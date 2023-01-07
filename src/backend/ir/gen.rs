use std::iter;

pub(super) use self::state::State;

use super::*;

fn mangle_method(name: &Ident, class: &Ident) -> Ident {
    Ident::from(format!("{}${}", name, class))
}

pub(crate) const CONCAT_STRINGS_FUNC: &str = "__concat_strings";
pub(crate) const REAL_MAIN: &str = "__real_main";

impl CFG {
    /** Built-in functions:
     * void printInt(int)
     * void printString(string)
     * void error()
     * int readInt()
     * string readString()
     */
    fn new(state: &mut State) -> Self {
        let built_in_functions = INITIAL_FUNCS
            .iter()
            .cloned()
            .chain(iter::once((
                "__concat_strings".to_string(),
                FunType {
                    ret_type: DataType::Nonvoid(NonvoidType::TString),
                    params: vec![NonvoidType::TString, NonvoidType::TString],
                },
            )))
            .map(|(name, fun_type)| {
                (
                    name,
                    IrFunction {
                        convention: CallingConvention::Cdecl,
                        entry: None,
                        params: fun_type
                            .params
                            .iter()
                            .map(|typ| state.fresh_reg(typ.clone().into()))
                            .collect(),
                        typ: fun_type,
                    },
                )
            })
            .collect::<HashMap<_, _>>();

        Self {
            blocks: vec![],
            current_block_idx: BasicBlockIdx(usize::MAX),
            current_func: Ident::new(),
            functions: built_in_functions,
        }
    }

    fn new_function(&mut self, mut id: Ident, fun_type: FunType, param_vars: Vec<Var>) {
        if id == "main" {
            id = REAL_MAIN.into();
        }
        self.current_func = id.clone();
        let entry = self.new_block();
        self.functions
            .insert(
                id,
                IrFunction {
                    convention: CallingConvention::StackVars,
                    entry: Some(entry),
                    typ: fun_type,
                    params: param_vars,
                },
            )
            .ok_or(())
            .unwrap_err(); // assert unique name
        self.make_current(entry);
        self.current_mut().entry = true;
    }

    fn new_block(&mut self) -> BasicBlockIdx {
        let new_idx = BasicBlockIdx(self.blocks.len());
        self.blocks.push(BasicBlock {
            func: self.current_func.clone(),
            ..BasicBlock::empty(new_idx)
        });
        new_idx
    }

    fn current_mut(&mut self) -> &mut BasicBlock {
        let idx = self.current_block_idx;
        &mut self[idx]
    }

    fn make_current(&mut self, idx: BasicBlockIdx) {
        self.current_block_idx = idx;
    }

    /// Called only for function with our call conventions, i.e. emitted by the compiler.
    pub fn variables_in_function(&self, func_name: &Ident) -> HashSet<Var> {
        let entry = self.functions.get(func_name).unwrap().entry.unwrap(); // see above.
        let mut variables = HashSet::new();

        fn dfs_variables(
            cfg: &CFG,
            visited: &mut HashSet<BasicBlockIdx>,
            block_idx: BasicBlockIdx,
            variables: &mut HashSet<Var>,
        ) {
            if visited.contains(&block_idx) {
                return;
            } else {
                visited.insert(block_idx);
            }
            cfg[block_idx].defined_variables(variables);
            for succ in cfg[block_idx].successors.iter().copied() {
                dfs_variables(cfg, visited, succ, variables)
            }
        }

        dfs_variables(
            self,
            &mut HashSet::<BasicBlockIdx>::new(),
            entry,
            &mut variables,
        );
        variables
    }

    fn link_succ_and_pred(&mut self) {
        let entries = self
            .functions
            .values()
            .map(|cfg_func| cfg_func.entry)
            .flatten()
            .collect::<Vec<_>>();
        let mut visited = vec![];
        for entry in entries {
            // eprintln!("\nIn linking succ and pred, so far visited: {:?}", visited);
            visited.clear();
            self.dfs_linking(entry, &mut visited);
        }
    }

    fn dfs_linking(&mut self, current: BasicBlockIdx, visited: &mut Vec<BasicBlockIdx>) {
        if visited.contains(&current) {
            return;
        }
        visited.push(current);
        match self[current].end_type {
            Some(EndType::Goto(bl)) => {
                self[current].successors.push(bl);
                self[bl].predecessors.push(current);
                self.dfs_linking(bl, visited);
            }
            Some(EndType::IfElse(_, bl1, bl2)) => {
                self[current].successors.extend([bl1, bl2]);
                self[bl1].predecessors.push(current);
                self[bl2].predecessors.push(current);
                self.dfs_linking(bl1, visited);
                self.dfs_linking(bl2, visited);
            }
            Some(EndType::Return(_)) | None => (), // function end,
        };
    }
}

impl BasicBlock {
    pub fn empty(idx: BasicBlockIdx) -> Self {
        Self {
            func: Ident::new(),
            idx,
            entry: false,
            quadruples: vec![],
            successors: vec![],
            predecessors: vec![],
            end_type: None,
            phi_nodes: VecMap::new(),
        }
    }

    fn assert_sane(&self) {
        assert!(self.quadruples.is_empty() || self.end_type.is_some())
    }

    pub fn is_empty(&self) -> bool {
        self.quadruples.is_empty()
    }

    fn defined_variables(&self, buf: &mut HashSet<Var>) {
        assert!(self.phi_nodes.is_empty());
        for var in self
            .quadruples
            .iter()
            .filter_map(|quadruple| match quadruple {
                Quadruple::BinOp(var, _, _, _)
                | Quadruple::RelOp(var, _, _, _)
                | Quadruple::UnOp(var, _, _)
                | Quadruple::Copy(var, _)
                | Quadruple::Set(var, _)
                | Quadruple::GetStrLit(var, _)
                | Quadruple::Call(var, _, _) => Some(*var),
                Quadruple::ArrLoad(_, _, _) => todo!(),
                Quadruple::ArrStore(_, _, _) => todo!(),
                Quadruple::DerefLoad(_, _) => todo!(),
                Quadruple::DerefStore(_, _) => todo!(),
            })
        {
            buf.insert(var);
        }
    }

    pub(super) fn all_variables(&self) -> HashSet<Var> {
        let mut vars = HashSet::<Var>::new();
        for quadruple in self.quadruples.iter() {
            match quadruple {
                Quadruple::BinOp(var1, var2, _, val) | Quadruple::RelOp(var1, var2, _, val) => {
                    vars.insert(*var1);
                    vars.insert(*var2);
                    if let Value::Variable(var) = val {
                        vars.insert(*var);
                    }
                }
                Quadruple::UnOp(var1, _, val) => {
                    vars.insert(*var1);
                    if let Value::Variable(var) = val {
                        vars.insert(*var);
                    }
                }
                Quadruple::Copy(var1, var2) => {
                    vars.insert(*var1);
                    vars.insert(*var2);
                }
                Quadruple::GetStrLit(var, _) | Quadruple::Set(var, _) => {
                    vars.insert(*var);
                }

                Quadruple::ArrLoad(_, _, _) => todo!(),
                Quadruple::ArrStore(_, _, _) => todo!(),
                Quadruple::DerefLoad(_, _) => todo!(),
                Quadruple::DerefStore(_, _) => todo!(),
                Quadruple::Call(var, _, args) => {
                    vars.insert(*var);
                    for arg in args {
                        if let Value::Variable(var) = arg {
                            vars.insert(*var);
                        }
                    }
                }
            };
        }
        vars
    }
}

mod state {
    use std::ops::{Deref, DerefMut};

    use rpds::RedBlackTreeMap as Map;
    use vector_map::VecMap;

    use crate::{
        backend::ir::{StringLiteral, VarType},
        frontend::semantic_analysis::ast::{Ident, NonvoidType},
    };

    use super::Var;

    pub struct StateGuard<'a>(&'a mut State, State);

    impl Drop for StateGuard<'_> {
        fn drop(&mut self) {
            // self.0.next_label_num = self.1.next_label_num;
            self.0.next_var_num = self.1.next_var_num;
            std::mem::swap(&mut self.0.string_literals, &mut self.1.string_literals);
            std::mem::swap(&mut self.0.var_types, &mut self.1.var_types);
        }
    }

    impl Deref for StateGuard<'_> {
        type Target = State;

        fn deref(&self) -> &Self::Target {
            &self.1
        }
    }
    impl DerefMut for StateGuard<'_> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.1
        }
    }

    pub struct State {
        /* state-like flow */
        pub string_literals: Vec<String>,
        next_var_num: usize,
        pub var_types: VecMap<Var, VarType>,

        /* env-like flow */
        var_location: Map<Ident, Var>,
    }
    impl State {
        pub(crate) fn new() -> Self {
            Self {
                next_var_num: 0,
                string_literals: Vec::new(),
                var_location: Map::new(),
                var_types: VecMap::new(),
            }
        }

        pub(crate) fn fresh_reg(&mut self, typ: VarType) -> Var {
            let var = Var(self.next_var_num);
            self.var_types.insert(var, typ);
            self.next_var_num += 1;
            var
        }

        pub(crate) fn get_var_type(&self, var: Var) -> Option<&VarType> {
            self.var_types.get(&var)
        }

        pub(crate) fn new_scope<'a>(&'a mut self) -> StateGuard<'a> {
            let string_literals = std::mem::take(&mut self.string_literals);
            StateGuard(
                self,
                Self {
                    var_location: self.var_location.clone(),
                    var_types: self.var_types.clone(),
                    string_literals,
                    ..*self
                },
            )
        }

        pub(crate) fn enter_new_frame_and_give_params(
            &mut self,
            params: impl Iterator<Item = (Ident, NonvoidType)>,
        ) -> Vec<Var> {
            self.var_location = Map::new();
            params
                .map(|(param_name, param_type)| self.declare_var(param_name, param_type.into()))
                .collect()
        }

        pub(crate) fn declare_var(&mut self, id: Ident, typ: VarType) -> Var {
            // assert!(!self.var_location.contains_key(&id));
            let var = Var(self.next_var_num);
            self.next_var_num += 1;
            self.var_location.insert_mut(id, var);
            self.var_types.insert(var, typ);
            var
        }

        pub(crate) fn retrieve_var(&mut self, id: &Ident) -> Var {
            *self
                .var_location
                .get(id)
                .unwrap_or_else(|| panic!("No var {} declared.", id))
        }

        pub(crate) fn register_literal(&mut self, string: String) -> StringLiteral {
            self.string_literals.push(string);
            StringLiteral(self.string_literals.len() - 1)
        }
    }
}

impl Program {
    pub fn ir(&self) -> Ir {
        let mut state = State::new();
        let mut cfg = CFG::new(&mut state);

        for func in self.0.iter() {
            let param_vars = state.enter_new_frame_and_give_params(
                func.params
                    .iter()
                    .map(|param| (param.name.clone(), param.type_.clone())),
            );
            cfg.new_function(func.name.clone(), func.fun_type(), param_vars);
            func.block.ir(&mut cfg, &mut state);
        }

        cfg.link_succ_and_pred();
        // eprintln!("BEFORE SSA: max var = {}", state.fresh_reg(None).0);
        // cfg.make_ssa(&mut state);
        // cfg.optimise_ssa();
        Ir {
            cfg,
            string_literals: state.string_literals,
        }
    }
}

impl Block {
    fn ir(&self, cfg: &mut CFG, state: &mut State) {
        for stmt in &self.1 {
            stmt.ir(cfg, state)
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ConditionalContext {
    block_true: BasicBlockIdx,
    block_false: BasicBlockIdx,
}

impl Stmt {
    fn ends_current_block(&self) -> bool {
        match &self.1 {
            StmtInner::Empty
            | StmtInner::Ass(_, _)
            | StmtInner::VarDecl(_)
            | StmtInner::Incr(_)
            | StmtInner::Decr(_)
            | StmtInner::SExp(_) => false,
            StmtInner::Return(_)
            | StmtInner::VoidReturn
            | StmtInner::Cond(_, _)
            | StmtInner::CondElse(_, _, _)
            | StmtInner::While(_, _)
            | StmtInner::For(_, _, _, _) => true,
            StmtInner::Block(block) => block.1.iter().any(|stmt| stmt.ends_current_block()),
        }
    }

    fn ir(&self, cfg: &mut CFG, state: &mut State) {
        match &self.1 {
            StmtInner::Empty => (),
            StmtInner::Block(block) => block.ir(cfg, &mut state.new_scope()),
            StmtInner::VarDecl(decls) => {
                for decl in decls.decls.iter() {
                    let var = state.declare_var(decl.name.clone(), decls.type_.clone().into());
                    if let Some(init) = decl.init.as_ref() {
                        let reg = init.ir(cfg, state, None);
                        match reg {
                            Value::Instant(i) => {
                                cfg.current_mut().quadruples.push(Quadruple::Set(var, i))
                            }
                            Value::Variable(reg) => {
                                cfg.current_mut().quadruples.push(Quadruple::Copy(var, reg))
                            }
                        }
                    } else {
                        // default value initialization
                        cfg.current_mut()
                            .quadruples
                            .push(Quadruple::Set(var, Instant(0)))
                    }
                }
            }
            StmtInner::Ass(lval, rval) => {
                let rval = rval.ir(cfg, state, None);
                let lval = lval.ir(cfg, state);
                let quadruple = match rval {
                    Value::Instant(i) => Quadruple::Set(lval, i),
                    Value::Variable(var) => Quadruple::Copy(lval, var),
                };
                cfg.current_mut().quadruples.push(quadruple);
            }
            StmtInner::Incr(lval) => {
                let lval = lval.ir(cfg, state);
                cfg.current_mut().quadruples.push(Quadruple::UnOp(
                    lval,
                    UnOpType::Inc,
                    Value::Variable(lval),
                ));
            }
            StmtInner::Decr(lval) => {
                let lval = lval.ir(cfg, state);
                cfg.current_mut().quadruples.push(Quadruple::UnOp(
                    lval,
                    UnOpType::Dec,
                    Value::Variable(lval),
                ));
            }
            StmtInner::Return(expr) => {
                let reg = expr.ir(cfg, state, None);
                cfg.current_mut().end_type = Some(EndType::Return(Some(reg)));
                let bl = cfg.new_block();
                cfg.make_current(bl);
            }
            StmtInner::VoidReturn => {
                cfg.current_mut().end_type = Some(EndType::Return(None));
                let bl = cfg.new_block();
                cfg.make_current(bl);
            }

            StmtInner::Cond(cond, then) => {
                let res = cond.ir(
                    cfg,
                    state,
                    Some(ConditionalContext {
                        block_true: todo!(),
                        block_false: todo!(),
                    }),
                );
                match res {
                    Value::Instant(i) => {
                        if *i == 0 {
                            // condition always false
                            // skip
                        } else {
                            // condition always true
                            then.ir(cfg, state)
                        }
                    }
                    Value::Variable(cond_var) => {
                        let pre_block = cfg.current_block_idx;
                        let then_block = cfg.new_block();
                        cfg.make_current(then_block);
                        then.ir(cfg, state);

                        let next_block = cfg.new_block();
                        cfg.make_current(next_block);

                        cfg[pre_block].end_type =
                            Some(EndType::IfElse(cond_var, then_block, next_block));
                        cfg[then_block]
                            .end_type
                            .get_or_insert(EndType::Goto(next_block));
                    }
                }
            }
            StmtInner::CondElse(cond, then, else_br) => {
                let res = cond.ir(
                    cfg,
                    state,
                    Some(ConditionalContext {
                        block_true: todo!(),
                        block_false: todo!(),
                    }),
                );
                match res {
                    Value::Instant(i) => {
                        if *i == 0 {
                            // condition always false
                            else_br.ir(cfg, state)
                        } else {
                            // condition always true
                            then.ir(cfg, state)
                        }
                    }
                    Value::Variable(cond_var) => {
                        let pre_block = cfg.current_block_idx;
                        let then_block = cfg.new_block();
                        cfg.make_current(then_block);
                        then.ir(cfg, state);

                        let else_block = cfg.new_block();
                        cfg.make_current(else_block);
                        else_br.ir(cfg, state);

                        let next_block = cfg.new_block();
                        cfg.make_current(next_block);

                        cfg[pre_block].end_type =
                            Some(EndType::IfElse(cond_var, then_block, else_block));
                        cfg[then_block]
                            .end_type
                            .get_or_insert(EndType::Goto(next_block));
                        cfg[else_block]
                            .end_type
                            .get_or_insert(EndType::Goto(next_block));
                    }
                }
            }

            StmtInner::SExp(expr) => {
                let _ = expr.ir(cfg, state, None);
            }

            StmtInner::While(cond, body) => {
                let pre_block = cfg.current_block_idx;
                let cond_block = cfg.new_block();
                cfg.make_current(cond_block);

                let res = cond.ir(
                    cfg,
                    state,
                    Some(ConditionalContext {
                        block_true: todo!(),
                        block_false: todo!(),
                    }),
                );
                match res {
                    Value::Instant(i) => {
                        if *i == 0 {
                            // condition always false
                            // skip
                        } else {
                            // condition always true
                            // make infinite loop in a new block
                            let loop_block = cfg.new_block();
                            cfg[pre_block].end_type = Some(EndType::Goto(loop_block));
                            cfg[loop_block].end_type = Some(EndType::Goto(loop_block));
                            cfg.make_current(loop_block);
                            body.ir(cfg, state);
                        }
                    }
                    Value::Variable(cond_var) => {
                        let loop_block = cfg.new_block();
                        cfg.make_current(loop_block);
                        body.ir(cfg, state);

                        let next_block = cfg.new_block();
                        cfg.make_current(next_block);

                        cfg[pre_block].end_type = Some(EndType::Goto(cond_block));
                        cfg[cond_block].end_type =
                            Some(EndType::IfElse(cond_var, loop_block, next_block));
                        cfg[loop_block].end_type = Some(EndType::Goto(cond_block));
                    }
                }
            }

            StmtInner::For(_, _, _, _) => todo!(),
        }
    }
}

// FIXME: make Instants typed, which would allow for string instants!
// Update: string instants impossible anyway.

impl Expr {
    fn ends_current_block(&self) -> bool {
        match &self.1 {
            ExprInner::Id(_)
            | ExprInner::IntLit(_)
            | ExprInner::BoolLit(_)
            | ExprInner::StringLit(_)
            | ExprInner::Null(_) => false,
            ExprInner::Op(op) => match op {
                Op::UnOp(_, opnd) => opnd.ends_current_block(),
                Op::BinOp(_, a, b) => a.ends_current_block() || b.ends_current_block(),
                Op::LogOp(_log_op, _, _) => true,
            },
            ExprInner::LVal(lval) => lval.ends_current_block(),
        }
    }

    fn ir(&self, cfg: &mut CFG, state: &mut State, cond_ctx: Option<ConditionalContext>) -> Value {
        match &self.1 {
            ExprInner::Op(op) => match op {
                Op::UnOp(un_op, a) => match un_op {
                    ast::UnOpType::Neg => match a.ir(cfg, state, cond_ctx) {
                        Value::Instant(i) => Value::Instant(Instant(-*i)),
                        val @ Value::Variable(var) => {
                            let typ = state.get_var_type(var).unwrap();
                            let tmp = state.fresh_reg(typ.clone());
                            cfg.current_mut().quadruples.push(Quadruple::UnOp(
                                tmp,
                                UnOpType::Neg,
                                val,
                            ));
                            Value::Variable(tmp)
                        }
                    },
                    ast::UnOpType::Not => match a.ir(cfg, state, cond_ctx) {
                        Value::Instant(i) => Value::Instant(i.not()),
                        val @ Value::Variable(var) => {
                            let typ = state.get_var_type(var).unwrap();
                            let tmp = state.fresh_reg(typ.clone());
                            cfg.current_mut().quadruples.push(Quadruple::UnOp(
                                tmp,
                                UnOpType::Not,
                                val,
                            ));
                            Value::Variable(tmp)
                        }
                    },
                },
                Op::BinOp(bin_op, a, b) => {
                    let a_val = a.ir(cfg, state, cond_ctx);
                    let b_val = b.ir(cfg, state, cond_ctx);

                    if let (Value::Instant(a_i), Value::Instant(b_i)) = (a_val, b_val) {
                        // TODO: optimise
                        return Value::Instant(match bin_op {
                            ast::BinOpType::IntOp(op) => match op {
                                IntOpType::IntRet(op) => match op {
                                    IntRetType::Mul => Instant(*a_i * *b_i),
                                    IntRetType::Div => Instant(*a_i / *b_i),
                                    IntRetType::Mod => Instant(*a_i % *b_i),
                                    IntRetType::Sub => Instant(*a_i - *b_i),
                                },
                                IntOpType::BoolRet(op) => match op {
                                    BoolRetType::Gt => Instant::bool(a_i > b_i),
                                    BoolRetType::Ge => Instant::bool(a_i >= b_i),
                                    BoolRetType::Lt => Instant::bool(a_i < b_i),
                                    BoolRetType::Le => Instant::bool(a_i <= b_i),
                                },
                            },
                            ast::BinOpType::Add => Instant(*a_i + *b_i),
                            ast::BinOpType::Eq => Instant::bool(a_i == b_i),
                            ast::BinOpType::NEq => Instant::bool(a_i != b_i),
                        });
                    }

                    let typ = match (a_val, b_val) {
                        (Value::Instant(_), Value::Instant(_)) => {
                            unreachable!("Case covered above")
                        }
                        (Value::Instant(_), Value::Variable(var))
                        | (Value::Variable(var), Value::Instant(_))
                        | (Value::Variable(var), Value::Variable(_)) => match bin_op {
                            ast::BinOpType::IntOp(_) => VarType::INT,
                            ast::BinOpType::Add | ast::BinOpType::Eq | ast::BinOpType::NEq => {
                                state.get_var_type(var).unwrap().clone()
                            }
                        },
                    };

                    let a_var = match a_val {
                        Value::Instant(i) => {
                            let var = state.fresh_reg(VarType::INT);
                            cfg.current_mut().quadruples.push(Quadruple::Set(var, i));
                            var
                        }
                        Value::Variable(var) => var,
                    };
                    let tmp = state.fresh_reg(typ.clone());
                    match bin_op {
                        ast::BinOpType::IntOp(op) => match op {
                            IntOpType::IntRet(op) => {
                                let op = match op {
                                    IntRetType::Sub => BinOpType::Sub,
                                    IntRetType::Mul => BinOpType::Mul,
                                    IntRetType::Div => BinOpType::Div,
                                    IntRetType::Mod => BinOpType::Mod,
                                };
                                cfg.current_mut()
                                    .quadruples
                                    .push(Quadruple::BinOp(tmp, a_var, op, b_val));
                                Value::Variable(tmp)
                            }
                            IntOpType::BoolRet(op) => {
                                let op = match op {
                                    BoolRetType::Gt => RelOpType::Gt,
                                    BoolRetType::Ge => RelOpType::Ge,
                                    BoolRetType::Lt => RelOpType::Lt,
                                    BoolRetType::Le => RelOpType::Le,
                                };
                                cfg.current_mut()
                                    .quadruples
                                    .push(Quadruple::RelOp(tmp, a_var, op, b_val));
                                Value::Variable(tmp)
                            }
                        },
                        ast::BinOpType::Eq => {
                            cfg.current_mut().quadruples.push(Quadruple::RelOp(
                                tmp,
                                a_var,
                                RelOpType::Eq,
                                b_val,
                            ));
                            Value::Variable(tmp)
                        }
                        ast::BinOpType::NEq => {
                            cfg.current_mut().quadruples.push(Quadruple::RelOp(
                                tmp,
                                a_var,
                                RelOpType::NEq,
                                b_val,
                            ));
                            Value::Variable(tmp)
                        }
                        ast::BinOpType::Add if matches!(typ, VarType::Ptr(PtrVarType::String)) => {
                            let b_var = match b_val {
                                Value::Instant(_) => unreachable!("Strings must not be instants"),
                                Value::Variable(var) => var,
                            };

                            cfg.current_mut().quadruples.push(Quadruple::Call(
                                tmp,
                                CONCAT_STRINGS_FUNC.to_string(),
                                vec![Value::Variable(a_var), Value::Variable(b_var)],
                            ));
                            Value::Variable(tmp)
                        }
                        ast::BinOpType::Add => {
                            cfg.current_mut().quadruples.push(Quadruple::BinOp(
                                tmp,
                                a_var,
                                BinOpType::Add,
                                b_val,
                            ));
                            Value::Variable(tmp)
                        }
                    }
                }
                Op::LogOp(log_op, a, b) => {
                    let a_res = a.ir(cfg, state, cond_ctx);
                    match (a_res, log_op) {
                        (Value::Instant(i), LogOpType::And) if *i == 0 => {
                            Value::Instant(Instant::bool(false))
                        }
                        (Value::Instant(i), LogOpType::And) if *i != 0 => {
                            b.ir(cfg, state, cond_ctx)
                        }
                        (Value::Instant(i), LogOpType::Or) if *i == 0 => b.ir(cfg, state, cond_ctx),
                        (Value::Instant(i), LogOpType::Or) if *i != 0 => {
                            Value::Instant(Instant::bool(true))
                        }
                        (Value::Instant(_), _) => unreachable!(),
                        (Value::Variable(a_var), _) => {
                            let pre_block_idx = cfg.current_block_idx;
                            let check_b_block_idx = cfg.new_block();
                            cfg.make_current(check_b_block_idx);

                            let b_res = b.ir(cfg, state, cond_ctx);
                            cfg.make_current(pre_block_idx);

                            match (b_res, log_op) {
                                (Value::Instant(i), LogOpType::And) if *i == 0 => {
                                    Value::Instant(Instant::bool(false))
                                }
                                (Value::Instant(i), LogOpType::And) if *i != 0 => a_res,
                                (Value::Instant(i), LogOpType::Or) if *i == 0 => a_res,
                                (Value::Instant(i), LogOpType::Or) if *i != 0 => {
                                    Value::Instant(Instant::bool(true))
                                }
                                (Value::Instant(_), _) => unreachable!(),
                                (Value::Variable(b_var), _) => {
                                    let then_block_idx = cfg.new_block();
                                    let else_block_idx = cfg.new_block();
                                    let next_block_idx = cfg.new_block();

                                    // cfg[pre_block_idx].successors.extend([check_b_block_idx, else_block_idx]);
                                    // cfg[check_b_block_idx].successors.extend([then_block_idx, else_block_idx]);
                                    // cfg[then_block_idx].successors.extend([next_block_idx]);
                                    // cfg[else_block_idx].successors.extend([next_block_idx]);

                                    cfg[then_block_idx].end_type =
                                        Some(EndType::Goto(next_block_idx));
                                    cfg[else_block_idx].end_type =
                                        Some(EndType::Goto(next_block_idx));

                                    let res_var = state.fresh_reg(VarType::BOOL);
                                    match log_op {
                                        LogOpType::And => {
                                            cfg[pre_block_idx].end_type = Some(EndType::IfElse(
                                                a_var,
                                                check_b_block_idx,
                                                else_block_idx,
                                            ));
                                            cfg[check_b_block_idx].end_type =
                                                Some(EndType::IfElse(
                                                    b_var,
                                                    then_block_idx,
                                                    else_block_idx,
                                                ));
                                        }
                                        LogOpType::Or => {
                                            cfg[pre_block_idx].end_type = Some(EndType::IfElse(
                                                a_var,
                                                else_block_idx,
                                                check_b_block_idx,
                                            ));
                                            cfg[check_b_block_idx].end_type =
                                                Some(EndType::IfElse(
                                                    b_var,
                                                    else_block_idx,
                                                    then_block_idx,
                                                ));
                                        }
                                    }

                                    cfg.make_current(next_block_idx);
                                    Value::Variable(res_var)
                                }
                            }
                        }
                    }
                }
            },
            ExprInner::Id(id) => {
                Value::Variable(state.retrieve_var(id)) // TODO: do not mutate variable if used as rval
            }
            ExprInner::IntLit(n) => Value::Instant(Instant(*n)),
            ExprInner::BoolLit(b) => Value::Instant(Instant::bool(*b)),
            ExprInner::StringLit(s) => {
                let reg = state.fresh_reg(VarType::STRING);
                let lit = state.register_literal(s.clone());
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::GetStrLit(reg, lit));
                Value::Variable(reg)
            }
            ExprInner::Null(typ) => {
                let reg = state.fresh_reg(typ.clone().into());
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::Set(reg, Instant(0)));
                Value::Variable(reg)
            }
            ExprInner::LVal(lval) => Value::Variable(lval.ir(cfg, state)),
        }
    }
}

impl LVal {
    #[inline(always)]
    const fn ends_current_block(&self) -> bool {
        false
    }

    fn ir(&self, cfg: &mut CFG, state: &mut State) -> Var {
        let typ = if let DataType::Nonvoid(nonvoid) = self.2.borrow().as_ref().unwrap() {
            Some(nonvoid.clone().into())
        } else {
            None
        };
        match &self.1 {
            LValInner::Id(var_id) => state.retrieve_var(var_id),
            LValInner::FunCall { name, args } => {
                let args = args.iter().map(|arg| arg.ir(cfg, state, None)).collect();
                let retvar = state.fresh_reg(typ.unwrap_or(VarType::INT));
                cfg.current_mut().quadruples.push(Quadruple::Call(
                    retvar,
                    if name == "main" {
                        "real_main".into()
                    } else {
                        name.clone()
                    },
                    args,
                ));
                retvar
            }

            LValInner::FieldAccess(_, _) => todo!(),
            LValInner::ArrSub(_, _) => todo!(),
            LValInner::MethodCall {
                object,
                method_name,
                args,
            } => todo!(),
            LValInner::New(_) => todo!(),
        }
    }
}
