use either::Either;
use enum_as_inner::EnumAsInner;

use std::{fmt, iter};

pub(super) use self::state::State;

use super::*;

#[allow(unused)]
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
        let entry = self.new_block(BasicBlockKind::Initial);
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
        self.make_current(entry, "CFG::new_function");
        self.current_mut().entry = true;
    }

    fn new_block(&mut self, kind: BasicBlockKind) -> BasicBlockIdx {
        let new_idx = BasicBlockIdx(self.blocks.len());
        self.blocks.push(BasicBlock {
            _func: self.current_func.clone(),
            ..BasicBlock::empty(new_idx, kind)
        });
        new_idx
    }

    fn current_mut(&mut self) -> &mut BasicBlock {
        let idx = self.current_block_idx;
        &mut self[idx]
    }

    fn make_current(&mut self, idx: BasicBlockIdx, who: &str) {
        self.current_block_idx = idx;
        eprintln!("{}: Made {} current block.", who, idx.0);
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
            Some(EndType::IfElse(_, _, _, bl1, bl2)) => {
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
    pub fn empty(idx: BasicBlockIdx, kind: BasicBlockKind) -> Self {
        Self {
            _func: Ident::new(),
            _idx: idx,
            _kind: kind,
            entry: false,
            quadruples: vec![],
            successors: vec![],
            predecessors: vec![],
            end_type: None,
            phi_nodes: VecMap::new(),
        }
    }

    // fn assert_sane(&self) {
    //     assert!(self.quadruples.is_empty() || self.end_type.is_some())
    // }

    pub fn is_empty(&self) -> bool {
        self.quadruples.is_empty()
    }

    fn set_end_type(&mut self, new_end_type: EndType) {
        if let Some(ref old_end_type) = self.end_type {
            eprintln!(
                "WARNING: replacing end_type for block {} (kind {:?}); it was {:?}, now {:?}",
                self._idx.0, self._kind, old_end_type, new_end_type
            )
        }
        self.end_type = Some(new_end_type);
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
                .map(|(param_name, param_type)| {
                    let var = self.fresh_reg(param_type.into());
                    self.declare_var(param_name, var);
                    var
                })
                .collect()
        }

        pub(crate) fn declare_var(&mut self, id: Ident, var: Var) {
            self.var_location.insert_mut(id, var);
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
            eprintln!("\nEmitting IR for function: {}", &func.name);
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
    pre_block: BasicBlockIdx,
    block_true: BasicBlockIdx,
    block_false: BasicBlockIdx,
    block_next: BasicBlockIdx,
}

#[derive(Debug, EnumAsInner)]
pub enum ValueFut<'a> {
    Instant(Instant),
    VariableFut(&'a Expr),
}
impl<'a> ValueFut<'a> {
    fn ir(
        &'a self,
        cfg: &mut CFG,
        state: &mut State,
        cond_ctx: Option<ConditionalContext>,
    ) -> Value {
        match self {
            ValueFut::Instant(i) => Value::Instant(*i),
            ValueFut::VariableFut(fut) => Value::Variable(fut.ir(cfg, state, cond_ctx).unwrap()),
        }
    }
}

impl Stmt {
    fn ir(&self, cfg: &mut CFG, state: &mut State) {
        match &self.1 {
            StmtInner::Empty => (),
            StmtInner::Block(block) => block.ir(cfg, &mut state.new_scope()),
            StmtInner::VarDecl(decls) => {
                for decl in decls.decls.iter() {
                    let var = state.fresh_reg(decls.type_.clone().into());
                    if let Some(init) = decl.init.as_ref() {
                        let fut = init.ir_fut();
                        match fut {
                            ValueFut::Instant(i) => {
                                cfg.current_mut().quadruples.push(Quadruple::Set(var, i))
                            }
                            ValueFut::VariableFut(init) => {
                                let reg = init.ir(cfg, state, None).unwrap();
                                cfg.current_mut().quadruples.push(Quadruple::Copy(var, reg))
                            }
                        }
                    } else {
                        // default value initialization
                        cfg.current_mut()
                            .quadruples
                            .push(Quadruple::Set(var, Instant(0)))
                    }
                    state.declare_var(decl.name.clone(), var);
                }
            }
            StmtInner::Ass(lval, rval) => {
                let rval_fut = rval.ir_fut();
                let lval = lval.ir(cfg, state, None).unwrap();
                let quadruple = match rval_fut {
                    ValueFut::Instant(i) => Quadruple::Set(lval, i),
                    ValueFut::VariableFut(rval) => {
                        let var = rval.ir(cfg, state, None).unwrap();
                        Quadruple::Copy(lval, var)
                    }
                };
                cfg.current_mut().quadruples.push(quadruple);
            }
            StmtInner::Incr(lval) => {
                let lval = lval.ir(cfg, state, None).unwrap();
                cfg.current_mut().quadruples.push(Quadruple::UnOp(
                    lval,
                    UnOpType::Inc,
                    Value::Variable(lval),
                ));
            }
            StmtInner::Decr(lval) => {
                let lval = lval.ir(cfg, state, None).unwrap();
                cfg.current_mut().quadruples.push(Quadruple::UnOp(
                    lval,
                    UnOpType::Dec,
                    Value::Variable(lval),
                ));
            }
            StmtInner::Return(expr) => {
                let reg = expr.ir_fut().ir(cfg, state, None);
                eprintln!(
                    "Because of Return, setting {} end_type to Return({:?})",
                    cfg.current_block_idx.0, reg
                );
                cfg.current_mut().set_end_type(EndType::Return(Some(reg)));
                let bl = cfg.new_block(BasicBlockKind::AfterReturn);
                cfg.make_current(bl, "Return");
            }
            StmtInner::VoidReturn => {
                cfg.current_mut().set_end_type(EndType::Return(None));
                let bl = cfg.new_block(BasicBlockKind::AfterReturn);
                cfg.make_current(bl, "Void return");
            }

            StmtInner::Cond(cond, then) => {
                let cond_fut = cond.ir_fut();
                match cond_fut {
                    ValueFut::Instant(i) => {
                        if *i == 0 {
                            // condition always false
                            // skip
                        } else {
                            // condition always true
                            then.ir(cfg, state)
                        }
                    }
                    ValueFut::VariableFut(cond) => {
                        let pre_block = cfg.current_block_idx;
                        let then_block = cfg.new_block(BasicBlockKind::IfThen);
                        let next_block = cfg.new_block(BasicBlockKind::IfNext);

                        cond.ir(
                            cfg,
                            state,
                            Some(ConditionalContext {
                                pre_block,
                                block_true: then_block,
                                block_false: next_block,
                                block_next: next_block,
                            }),
                        )
                        .ok_or(())
                        .unwrap_err(); // we should not return any var from the cond

                        cfg.make_current(then_block, "Cond then");
                        then.ir(cfg, state);

                        cfg.make_current(next_block, "Cond next");

                        // FIXME: this should be set inside cond
                        // cfg[pre_block].end_type =
                        //     Some(EndType::IfElse(cond_var, then_block, next_block));
                        cfg[then_block]
                            .end_type
                            .get_or_insert(EndType::Goto(next_block));
                    }
                }
            }

            StmtInner::CondElse(cond, then, else_br) => {
                let cond_fut = cond.ir_fut();
                match cond_fut {
                    ValueFut::Instant(i) => {
                        if *i == 0 {
                            // condition always false
                            else_br.ir(cfg, state)
                        } else {
                            // condition always true
                            then.ir(cfg, state)
                        }
                    }
                    ValueFut::VariableFut(cond) => {
                        let pre_block = cfg.current_block_idx;
                        let then_block = cfg.new_block(BasicBlockKind::IfElseThen);
                        let else_block = cfg.new_block(BasicBlockKind::IfElseElse);
                        let next_block = cfg.new_block(BasicBlockKind::IfElseNext);

                        cond.ir(
                            cfg,
                            state,
                            Some(ConditionalContext {
                                pre_block,
                                block_true: then_block,
                                block_false: else_block,
                                block_next: next_block,
                            }),
                        )
                        .ok_or(())
                        .unwrap_err(); // we should not return any var from the cond

                        cfg.make_current(then_block, "CondElse then");
                        then.ir(cfg, state);

                        cfg.make_current(else_block, "CondElse else");
                        else_br.ir(cfg, state);

                        // FIXME: this should be set inside cond
                        // cfg[pre_block].end_type =
                        //     Some(EndType::IfElse(cond_var, then_block, else_block));
                        cfg[then_block]
                            .end_type
                            .get_or_insert(EndType::Goto(next_block));
                        cfg[else_block]
                            .end_type
                            .get_or_insert(EndType::Goto(next_block));
                        cfg.make_current(next_block, "CondElse next");
                    }
                }
            }

            StmtInner::SExp(expr) => {
                let _ = expr.ir_fut().ir(cfg, state, None);
            }

            StmtInner::While(cond, body) => {
                let pre_block = cfg.current_block_idx;
                let cond_fut = cond.ir_fut();
                match cond_fut {
                    ValueFut::Instant(i) => {
                        if *i == 0 {
                            // condition always false
                            // skip
                        } else {
                            // condition always true
                            // make infinite loop in a new block
                            let loop_block = cfg.new_block(BasicBlockKind::InfiniteLoop);
                            cfg[pre_block].set_end_type(EndType::Goto(loop_block));
                            cfg[loop_block].set_end_type(EndType::Goto(loop_block));
                            cfg.make_current(loop_block, "While infinite loop");
                            body.ir(cfg, state);
                        }
                    }
                    ValueFut::VariableFut(cond) => {
                        let cond_block = cfg.new_block(BasicBlockKind::WhileCond);
                        let loop_block = cfg.new_block(BasicBlockKind::WhileBody);
                        let next_block = cfg.new_block(BasicBlockKind::WhileNext);

                        cfg[pre_block].set_end_type(EndType::Goto(cond_block));

                        cfg.make_current(cond_block, "While cond");
                        cond.ir(
                            cfg,
                            state,
                            Some(ConditionalContext {
                                pre_block: cond_block,
                                block_true: loop_block,
                                block_false: next_block,
                                block_next: next_block,
                            }),
                        )
                        .ok_or(())
                        .unwrap_err(); // we should not return any var from the cond

                        cfg.make_current(loop_block, "While body");
                        body.ir(cfg, state);
                        cfg.current_mut().set_end_type(EndType::Goto(cond_block));

                        cfg.make_current(next_block, "While next");
                        // FIXME: this should be set inside cond
                        // cfg[cond_block].end_type =
                        //     Some(EndType::IfElse(cond_var, loop_block, next_block));
                    }
                }
            }

            StmtInner::For(_, _, _, _) => todo!(),
        }
    }
}

// FIXME: make Instants typed, which would allow for string instants!
// Update: string instants impossible anyway.

fn make_conditional_context(cfg: &mut CFG, state: &mut State) -> (ConditionalContext, Var) {
    let res = state.fresh_reg(VarType::BOOL);
    let pre_block = cfg.current_block_idx;
    let then_block = cfg.new_block(BasicBlockKind::MadeCondCtxThen);
    let else_block = cfg.new_block(BasicBlockKind::MadeCondCtxElse);
    let next_block = cfg.new_block(BasicBlockKind::MadeCondCtxNext);
    cfg[then_block]
        .quadruples
        .push(Quadruple::Set(res, Instant::bool(true)));
    cfg[then_block].set_end_type(EndType::Goto(next_block));
    cfg[else_block]
        .quadruples
        .push(Quadruple::Set(res, Instant::bool(false)));
    cfg[else_block].set_end_type(EndType::Goto(next_block));

    let ctx = ConditionalContext {
        pre_block,
        block_true: then_block,
        block_false: else_block,
        block_next: next_block,
    };
    eprintln!("Made cond ctx: {:#?}.", ctx);

    (ctx, res)
}

struct DebugExprInner<'a>(&'a ExprInner);
impl<'a> fmt::Debug for DebugExprInner<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            ExprInner::Op(op) => match op {
                Op::UnOp(op, _) => match op {
                    ast::UnOpType::Neg => f.write_str("Neg"),
                    ast::UnOpType::Not => f.write_str("Not"),
                },
                Op::BinOp(op, _, _) => match op {
                    ast::BinOpType::IntOp(op) => match op {
                        IntOpType::IntRet(op) => write!(f, "{:?}", op),
                        IntOpType::BoolRet(op) => write!(f, "{:?}", op),
                    },
                    ast::BinOpType::Add => f.write_str("Add"),
                    ast::BinOpType::Eq => f.write_str("Eq"),
                    ast::BinOpType::NEq => f.write_str("NEq"),
                },
                Op::LogOp(op, _, _) => write!(f, "{:?}", op),
            },
            ExprInner::Id(_) => f.debug_struct("Id").finish_non_exhaustive(),
            ExprInner::IntLit(_) => f.debug_struct("IntLit").finish_non_exhaustive(),
            ExprInner::BoolLit(_) => f.debug_struct("BoolLit").finish_non_exhaustive(),
            ExprInner::StringLit(_) => f.debug_struct("StringLit").finish_non_exhaustive(),
            ExprInner::Null(_) => f.debug_struct("Null").finish_non_exhaustive(),
            ExprInner::LVal(_) => f.debug_struct("LVal").finish_non_exhaustive(),
        }
    }
}

fn finish_cond_ctx_leaf(
    cfg: &mut CFG,
    state: &mut State,
    var: Var,
    cond_ctx: Option<ConditionalContext>,
    from: &str,
) -> Option<Var> {
    if let Some(cond_ctx) = cond_ctx {
        assert!(matches!(
            state.get_var_type(var).unwrap(),
            VarType::Simple(SimpleVarType::Bool)
        ));
        cfg[cond_ctx.pre_block].set_end_type(EndType::IfElse(
            var,
            RelOpType::NEq,
            Value::Instant(Instant(0)),
            cond_ctx.block_true,
            cond_ctx.block_false,
        ));
        eprintln!(
            "Because of {}, set {} end_type to IfElse(then: {}, else: {}).",
            from, cond_ctx.pre_block.0, cond_ctx.block_true.0, cond_ctx.block_false.0
        );
        None
    } else {
        Some(var)
    }
}

impl Expr {
    fn debug_surface<'a>(&'a self) -> DebugExprInner<'a> {
        DebugExprInner(self)
    }

    fn ir_fut(&self) -> ValueFut {
        eprintln!("ir_fut of {:#?}", &self.debug_surface());
        match match &self.1 {
            ExprInner::Id(_) => None,
            ExprInner::IntLit(i) => Some(Instant(*i)),
            ExprInner::BoolLit(b) => Some(Instant::bool(*b)),
            ExprInner::StringLit(_) => None,
            ExprInner::Op(op) => match op {
                Op::UnOp(op, exp) => exp.ir_fut().as_instant().copied().map(|i| match op {
                    ast::UnOpType::Neg => Instant(-i.0),
                    ast::UnOpType::Not => i.not(),
                }),
                Op::BinOp(op, exp1, exp2) => exp1.ir_fut().as_instant().copied().and_then(|i1| {
                    exp2.ir_fut().as_instant().copied().and_then(|i2| {
                        Some(match op {
                            ast::BinOpType::IntOp(op) => match op {
                                IntOpType::IntRet(op) => Instant(match op {
                                    IntRetType::Mul => i1.0 * i2.0,
                                    IntRetType::Div => i1.0 / i2.0,
                                    IntRetType::Mod => i1.0 % i2.0,
                                    IntRetType::Sub => i1.0 - i2.0,
                                }),
                                IntOpType::BoolRet(op) => Instant::bool(match op {
                                    BoolRetType::Gt => i1.0 > i2.0,
                                    BoolRetType::Ge => i1.0 >= i2.0,
                                    BoolRetType::Lt => i1.0 < i2.0,
                                    BoolRetType::Le => i1.0 <= i2.0,
                                }),
                            },
                            ast::BinOpType::Add => Instant(i1.0 + i2.0),
                            ast::BinOpType::Eq => Instant::bool(i1 == i2),
                            ast::BinOpType::NEq => Instant::bool(i1 != i2),
                        })
                    })
                }),
                Op::LogOp(op, exp1, exp2) => match (exp1.ir_fut(), exp2.ir_fut(), op) {
                    (ValueFut::Instant(i1), ValueFut::Instant(i2), op) => {
                        Some(Instant::bool(match op {
                            LogOpType::And => i1.0 != 0 && i2.0 != 0,
                            LogOpType::Or => i1.0 != 0 || i2.0 != 0,
                        }))
                    }
                    (ValueFut::Instant(i), ValueFut::VariableFut(_), LogOpType::And) => {
                        (i.0 == 0).then_some(i)
                    }
                    (ValueFut::Instant(i), ValueFut::VariableFut(_), LogOpType::Or) => {
                        (i.0 != 0).then_some(i)
                    }
                    (ValueFut::VariableFut(_), ValueFut::Instant(_), LogOpType::And)
                    | (ValueFut::VariableFut(_), ValueFut::Instant(_), LogOpType::Or)
                    | (ValueFut::VariableFut(_), ValueFut::VariableFut(_), LogOpType::And)
                    | (ValueFut::VariableFut(_), ValueFut::VariableFut(_), LogOpType::Or) => None,
                },
            },
            ExprInner::LVal(_) => None,
            ExprInner::Null(_) => None,
        } {
            Some(i) => ValueFut::Instant(i),
            None => ValueFut::VariableFut(self),
        }
    }

    fn ir(
        &self,
        cfg: &mut CFG,
        state: &mut State,
        cond_ctx: Option<ConditionalContext>,
    ) -> Option<Var> {
        eprintln!(
            "ir of {:#?} with cond_ctx {:?}",
            &self.debug_surface(),
            cond_ctx
        );
        let res = match &self.1 {
            ExprInner::Op(op) => match op {
                Op::UnOp(un_op, expr) => match un_op {
                    ast::UnOpType::Neg => {
                        let var = expr.ir(cfg, state, cond_ctx).unwrap();
                        let typ = state.get_var_type(var).unwrap();
                        let tmp = state.fresh_reg(typ.clone());
                        cfg.current_mut().quadruples.push(Quadruple::UnOp(
                            tmp,
                            UnOpType::Neg,
                            Value::Variable(var),
                        ));
                        Some(tmp)
                    }

                    ast::UnOpType::Not => {
                        let (cond_ctx, res) =
                            cond_ctx.map(|ctx| (ctx, None)).unwrap_or_else(|| {
                                let (ctx, res) = make_conditional_context(cfg, state);
                                (ctx, Some(res))
                            });
                        expr.ir(
                            cfg,
                            state,
                            Some(ConditionalContext {
                                block_true: cond_ctx.block_false,
                                block_false: cond_ctx.block_true,
                                ..cond_ctx
                            }),
                        );
                        res
                    }
                },

                Op::BinOp(bin_op, a, b) => {
                    let op = match bin_op {
                        ast::BinOpType::IntOp(op) => match op {
                            IntOpType::IntRet(op) => Either::Right(match op {
                                IntRetType::Mul => BinOpType::Mul,
                                IntRetType::Div => BinOpType::Div,
                                IntRetType::Mod => BinOpType::Mod,
                                IntRetType::Sub => BinOpType::Sub,
                            }),
                            IntOpType::BoolRet(op) => Either::Left(match op {
                                BoolRetType::Gt => RelOpType::Gt,
                                BoolRetType::Ge => RelOpType::Ge,
                                BoolRetType::Lt => RelOpType::Lt,
                                BoolRetType::Le => RelOpType::Le,
                            }),
                        },
                        ast::BinOpType::Add => Either::Right(BinOpType::Add),
                        ast::BinOpType::Eq => Either::Left(RelOpType::Eq),
                        ast::BinOpType::NEq => Either::Left(RelOpType::NEq),
                    };

                    match op {
                        Either::Left(rel_op) => {
                            let (cond_ctx, res) =
                                cond_ctx.map(|ctx| (ctx, None)).unwrap_or_else(|| {
                                    let (ctx, res) = make_conditional_context(cfg, state);
                                    (ctx, Some(res))
                                });

                            // TODO: consider bool jump matrix for bool equality comparisons

                            let a_val = a.ir_fut().ir(cfg, state, None); // None context will result in materialising a bool
                            let b_val = b.ir_fut().ir(cfg, state, None); // in case of bool equality comparison

                            let (a_var, b_val, rel_op) = match (a_val, b_val) {
                                (Value::Instant(_), Value::Instant(_)) => unreachable!(),
                                (Value::Variable(a_var), b_val) => (a_var, b_val, rel_op),
                                (b_val @ Value::Instant(_), Value::Variable(a_var)) => {
                                    (a_var, b_val, rel_op.reversed_params())
                                }
                            };

                            cfg[cond_ctx.pre_block].set_end_type(EndType::IfElse(
                                a_var,
                                rel_op,
                                b_val,
                                cond_ctx.block_true,
                                cond_ctx.block_false,
                            ));
                            eprintln!(
                                "Because of RelOp, set {} end_type to IfElse(then: {}, else: {}).",
                                cond_ctx.pre_block.0, cond_ctx.block_true.0, cond_ctx.block_false.0
                            );
                            cfg.make_current(cond_ctx.block_next, "RelOp");

                            res
                        }
                        Either::Right(bin_op) => {
                            let a_val = a.ir_fut().ir(cfg, state, None); // ints should not be computed in cond context.
                            let b_val = b.ir_fut().ir(cfg, state, None);

                            let typ = match (a_val, b_val) {
                                (Value::Instant(_), Value::Instant(_)) => {
                                    unreachable!("Instants impossible")
                                }
                                // TODO: optimise comutative operations
                                (Value::Instant(_), Value::Variable(var))
                                | (Value::Variable(var), Value::Instant(_))
                                | (Value::Variable(var), Value::Variable(_)) => match bin_op {
                                    BinOpType::Add => state.get_var_type(var).unwrap().clone(),
                                    BinOpType::Sub
                                    | BinOpType::Mul
                                    | BinOpType::Div
                                    | BinOpType::Mod
                                    | BinOpType::And
                                    | BinOpType::Or
                                    | BinOpType::Xor => {
                                        assert!(matches!(
                                            state.get_var_type(var).unwrap(),
                                            VarType::Simple(SimpleVarType::Int)
                                        ));
                                        VarType::INT
                                    }
                                },
                            };
                            let (a_var, b_val) = match (a_val, b_val) {
                                (Value::Instant(_), Value::Instant(_)) => unreachable!(),
                                (Value::Variable(a_var), b_val) => (a_var, b_val),
                                (b_val @ Value::Instant(_), Value::Variable(a_var))
                                    if bin_op.is_commutative() =>
                                {
                                    (a_var, b_val)
                                }
                                (Value::Instant(a_i), b_val @ Value::Variable(_)) => {
                                    let a_var = state.fresh_reg(VarType::INT); // no String instants
                                    cfg.current_mut()
                                        .quadruples
                                        .push(Quadruple::Set(a_var, a_i));
                                    (a_var, b_val)
                                }
                            };

                            let tmp = state.fresh_reg(typ.clone());
                            if matches!(typ, VarType::Ptr(PtrVarType::String)) {
                                assert!(matches!(bin_op, BinOpType::Add));
                                let b_var = b_val.into_variable().unwrap();
                                cfg.current_mut().quadruples.push(Quadruple::Call(
                                    tmp,
                                    CONCAT_STRINGS_FUNC.to_string(),
                                    vec![Value::Variable(a_var), Value::Variable(b_var)],
                                ));
                            } else {
                                cfg.current_mut()
                                    .quadruples
                                    .push(Quadruple::BinOp(tmp, a_var, bin_op, b_val));
                            }

                            Some(tmp)
                        }
                    }
                }

                Op::LogOp(op, a, b) => {
                    let (cond_ctx, res) = cond_ctx.map(|ctx| (ctx, None)).unwrap_or_else(|| {
                        let (ctx, res) = make_conditional_context(cfg, state);
                        (ctx, Some(res))
                    });
                    match (a.ir_fut(), b.ir_fut(), op) {
                        (ValueFut::Instant(_), ValueFut::Instant(_), _) => {
                            unreachable!("Instants impossible")
                        }
                        (ValueFut::Instant(i), ValueFut::VariableFut(_), LogOpType::And)
                            if i.0 == 0 =>
                        {
                            unreachable!("Instants impossible")
                        }
                        (ValueFut::Instant(i), ValueFut::VariableFut(_), LogOpType::Or)
                            if i.0 != 0 =>
                        {
                            unreachable!("Instants impossible")
                        }
                        // reduction to second operand
                        (ValueFut::Instant(_), ValueFut::VariableFut(b), LogOpType::And)
                        | (ValueFut::Instant(_), ValueFut::VariableFut(b), LogOpType::Or) => {
                            b.ir(cfg, state, Some(cond_ctx)).ok_or(()).unwrap_err();
                        }

                        (ValueFut::VariableFut(a), ValueFut::Instant(i), LogOpType::And) => {
                            let cond_ctx = if i.0 == 0 {
                                ConditionalContext {
                                    block_true: cond_ctx.block_false,
                                    block_false: cond_ctx.block_false,
                                    ..cond_ctx
                                }
                            } else {
                                cond_ctx
                            };
                            a.ir(cfg, state, Some(cond_ctx)).ok_or(()).unwrap_err();
                        }
                        (ValueFut::VariableFut(a), ValueFut::Instant(i), LogOpType::Or) => {
                            let cond_ctx = if i.0 != 0 {
                                ConditionalContext {
                                    block_true: cond_ctx.block_true,
                                    block_false: cond_ctx.block_true,
                                    ..cond_ctx
                                }
                            } else {
                                cond_ctx
                            };
                            a.ir(cfg, state, Some(cond_ctx)).ok_or(()).unwrap_err();
                        }
                        (ValueFut::VariableFut(a), ValueFut::VariableFut(b), _) => {
                            let check_b_block_idx = cfg.new_block(BasicBlockKind::LogOpSecondCheck);

                            // cfg.make_current(, "LogOp");
                            let a_cond_ctx = match op {
                                LogOpType::And => ConditionalContext {
                                    block_true: check_b_block_idx,
                                    block_false: cond_ctx.block_false,
                                    ..cond_ctx
                                },
                                LogOpType::Or => ConditionalContext {
                                    block_true: cond_ctx.block_true,
                                    block_false: check_b_block_idx,
                                    ..cond_ctx
                                },
                            };
                            let b_cond_ctx = ConditionalContext {
                                pre_block: check_b_block_idx,
                                block_true: cond_ctx.block_true,
                                block_false: cond_ctx.block_false,
                                ..cond_ctx
                            };

                            a.ir(cfg, state, Some(a_cond_ctx)).ok_or(()).unwrap_err();

                            cfg.make_current(check_b_block_idx, "LogOp check_b");
                            b.ir(cfg, state, Some(b_cond_ctx)).ok_or(()).unwrap_err();
                        }
                    };
                    res
                }
            },

            ExprInner::Id(id) => {
                let var = state.retrieve_var(id);
                finish_cond_ctx_leaf(cfg, state, var, cond_ctx, "Expr Id")
            }

            ExprInner::IntLit(_) => unreachable!("Impossible instants"),

            ExprInner::BoolLit(_) => unreachable!("Impossible instants"),

            ExprInner::StringLit(s) => {
                let reg = state.fresh_reg(VarType::STRING);
                let lit = state.register_literal(s.clone());
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::GetStrLit(reg, lit));
                Some(reg)
            }

            ExprInner::Null(typ) => {
                let reg = state.fresh_reg(typ.clone().into());
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::Set(reg, Instant(0)));
                Some(reg)
            }

            ExprInner::LVal(lval) => lval.ir(cfg, state, cond_ctx),
        };
        if let Some(cond_ctx) = cond_ctx {
            cfg.make_current(cond_ctx.block_next, "Expr finish");
        }
        res
    }
}

impl LVal {
    fn ir(
        &self,
        cfg: &mut CFG,
        state: &mut State,
        cond_ctx: Option<ConditionalContext>,
    ) -> Option<Var> {
        let typ = if let DataType::Nonvoid(nonvoid) = self.2.borrow().as_ref().unwrap() {
            Some(nonvoid.clone().into())
        } else {
            None
        };
        let res = match &self.1 {
            LValInner::Id(var_id) => {
                let var = state.retrieve_var(var_id);
                finish_cond_ctx_leaf(cfg, state, var, cond_ctx, "LVal Id")
            }
            LValInner::FunCall { name, args } => {
                let args = args
                    .iter()
                    .map(|arg| arg.ir_fut().ir(cfg, state, None))
                    .collect();
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
                finish_cond_ctx_leaf(cfg, state, retvar, cond_ctx, "FunCall Id")
            }

            LValInner::FieldAccess(_, _) => todo!(),
            LValInner::ArrSub(_, _) => todo!(),
            LValInner::MethodCall {
                ..
                // object,
                // method_name,
                // args,
            } => todo!(),
            LValInner::New(_) => todo!(),
        };
        if let Some(cond_ctx) = cond_ctx {
            cfg.make_current(cond_ctx.block_next, "LVal finish");
        }
        res
    }
}

// TODO: tests for programs that reduce at compile time, e.g. if( 1 + 3 == 4).
