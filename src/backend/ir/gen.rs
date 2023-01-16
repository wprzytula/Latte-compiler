use either::Either;
use enum_as_inner::EnumAsInner;
use log::{debug, info, trace, warn};

use std::{fmt, iter};

use crate::backend::ir::gen::state::VariableKind;

pub(super) use self::state::State;

use super::*;

fn mangle_method(name: &str, class: &str) -> Ident {
    Ident::from(format!("{}__{}", class, name))
}

pub(crate) const CONCAT_STRINGS_FUNC: &str = "__concat_strings";
pub(crate) const NEW_FUNC: &str = "__new";
pub(crate) const SELF: &str = "self";

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
                CONCAT_STRINGS_FUNC.to_string(),
                FunType {
                    ret_type: DataType::Nonvoid(NonvoidType::TString),
                    params: vec![NonvoidType::TString, NonvoidType::TString],
                },
            )))
            .chain(iter::once((
                NEW_FUNC.to_string(),
                FunType {
                    ret_type: DataType::Nonvoid(NonvoidType::TClass("ANY".into())), // TODO
                    params: vec![NonvoidType::TString, NonvoidType::TString],
                },
            )))
            .map(|(name, fun_type)| {
                (
                    name,
                    IrFunction {
                        convention: CallingConvention::CdeclFFI,
                        entry: None,
                        params: fun_type
                            .params
                            .iter()
                            .map(|typ| state.fresh_reg(typ.clone().into()))
                            .collect(),
                        typ: fun_type,
                        self_var: None,
                    },
                )
            })
            .collect::<HashMap<_, _>>();

        Self {
            blocks: vec![],
            current_block_idx: BasicBlockIdx(usize::MAX),
            current_func: Ident::new(),
            functions: built_in_functions,
            classes: vec![],
            current_class: None,
        }
    }

    fn new_function(
        &mut self,
        id: Ident,
        fun_type: FunType,
        param_vars: Vec<Var>,
        self_var: Option<Var>,
    ) {
        self.current_func = id.clone();
        let entry = self.new_block(BasicBlockKind::Initial);
        self.functions
            .insert(
                id,
                IrFunction {
                    convention: CallingConvention::SimpleCdecl,
                    entry: Some(entry),
                    typ: fun_type,
                    params: param_vars,
                    self_var,
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
        trace!("{}: Made {} current block.", who, idx.0);
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
            // trace!("\nIn linking succ and pred, so far visited: {:?}", visited);
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
            kind,
            entry: false,
            quadruples: vec![],
            successors: vec![],
            predecessors: vec![],
            end_type: None,
            phi_nodes: VecMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.quadruples.is_empty()
    }

    fn set_end_type(&mut self, new_end_type: EndType) {
        if let Some(ref old_end_type) = self.end_type {
            warn!(
                "WARNING: replacing end_type for block {} (kind {:?}); it was {:?}, now {:?}",
                self._idx.0, self.kind, old_end_type, new_end_type
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
                | Quadruple::Call(var, _, _)
                | Quadruple::VirtualCall(var, _, _, _)
                | Quadruple::DerefLoad(var, _) => Some(*var),
                Quadruple::DerefStore(_, _) => None,
                Quadruple::VstStore(_, _) => None,
                Quadruple::InPlaceUnOp(_, _) => None,
                Quadruple::ArrLoad(_, _, _) => todo!(),
                Quadruple::ArrStore(_, _, _) => todo!(),
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
                Quadruple::DerefLoad(var, mem) => {
                    vars.insert(*var);
                    vars.insert(mem.base);
                }
                Quadruple::DerefStore(val, mem) => {
                    vars.insert(mem.base);
                    if let Value::Variable(var) = val {
                        vars.insert(*var);
                    }
                }
                Quadruple::Call(var, _, args) => {
                    vars.insert(*var);
                    for arg in args {
                        if let Value::Variable(var) = arg {
                            vars.insert(*var);
                        }
                    }
                }
                Quadruple::InPlaceUnOp(_, loc) => {
                    vars.insert(loc.var());
                }
                Quadruple::VstStore(_, mem) => {
                    vars.insert(mem.base);
                }
                Quadruple::VirtualCall(var, obj_var, _, args) => {
                    vars.insert(*var);
                    vars.insert(*obj_var);
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

    use log::{debug, info};
    use rpds::RedBlackTreeMap as Map;
    use vector_map::VecMap;

    use crate::{
        backend::ir::{StringLiteral, VarType},
        frontend::semantic_analysis::ast::{Ident, NonvoidType},
    };

    use super::{ClassIdx, Var};

    pub struct StateGuard<'a>(&'a mut State, State);

    impl Drop for StateGuard<'_> {
        fn drop(&mut self) {
            self.0.next_var_num = self.1.next_var_num;
            std::mem::swap(&mut self.0.string_literals, &mut self.1.string_literals);
            std::mem::swap(&mut self.0.var_types, &mut self.1.var_types);
            std::mem::swap(&mut self.0.class_mapping, &mut self.1.class_mapping);
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

    #[derive(Debug, Clone, Copy)]
    pub(crate) enum VariableKind {
        StackVar(Var),
        ClassField,
    }

    pub struct State {
        /* state-like flow */
        pub string_literals: Vec<String>,
        next_var_num: usize,
        pub var_types: VecMap<Var, VarType>,
        next_class_num: usize,
        class_mapping: VecMap<Ident, ClassIdx>,

        /* env-like flow */
        var_location: Map<Ident, VariableKind>,
    }
    impl State {
        pub(crate) fn new() -> Self {
            Self {
                next_var_num: 0,
                string_literals: Vec::new(),
                var_location: Map::new(),
                var_types: VecMap::new(),
                next_class_num: 0,
                class_mapping: VecMap::new(),
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
            let class_mapping = std::mem::replace(&mut self.class_mapping, VecMap::new());
            StateGuard(
                self,
                Self {
                    var_location: self.var_location.clone(),
                    var_types: self.var_types.clone(),
                    string_literals,
                    class_mapping,
                    ..*self
                },
            )
        }

        pub(crate) fn declare_and_give_params(
            &mut self,
            params: impl Iterator<Item = (Ident, NonvoidType)>,
        ) -> Vec<Var> {
            params
                .map(|(param_name, param_type)| {
                    let var = self.fresh_reg(param_type.into());
                    self.declare_var(param_name, VariableKind::StackVar(var));
                    var
                })
                .collect()
        }

        pub(crate) fn declare_var(&mut self, id: Ident, variable_kind: VariableKind) {
            debug!("Declared var: {} -> {:?}", &id, variable_kind);
            self.var_location.insert_mut(id, variable_kind);
        }

        pub(crate) fn retrieve_var(&mut self, id: &Ident) -> VariableKind {
            *self
                .var_location
                .get(id)
                .unwrap_or_else(|| panic!("Var \"{}\" not declared.", id))
        }

        pub(crate) fn register_literal(&mut self, string: String) -> StringLiteral {
            self.string_literals.push(string);
            StringLiteral(self.string_literals.len() - 1)
        }

        pub(crate) fn register_class_if_not_yet_registered(&mut self, class: Ident) -> ClassIdx {
            if let Some(idx) = self.class_mapping.get(&class) {
                *idx
            } else {
                info!("Registered class {}", &class);
                let class_idx = ClassIdx(self.next_class_num);
                self.next_class_num += 1;
                self.class_mapping.insert(class, class_idx);
                class_idx
            }
        }

        pub(crate) fn retrieve_class_idx(&self, class: &Ident) -> ClassIdx {
            *self
                .class_mapping
                .get(class)
                .unwrap_or_else(|| panic!("Class {} not registered", class))
        }
    }
}

impl Program {
    pub fn ir(&self) -> Ir {
        let mut state = State::new();
        let mut cfg = CFG::new(&mut state);

        Self::ir_for_classes(&mut cfg, &mut state, &self.1);

        Self::ir_for_functions(&mut cfg, &mut state, &self.0);

        cfg.link_succ_and_pred();
        // debug!("BEFORE SSA: max var = {}", state.fresh_reg(None).0);
        // cfg.make_ssa(&mut state);
        // cfg.optimise_ssa();
        Ir {
            cfg,
            string_literals: state.string_literals,
        }
    }

    fn ir_for_classes(cfg: &mut CFG, state: &mut State, class_defs: &[ClassDef]) {
        info!("Registering classes:");
        // Register classes
        for ClassDef {
            class, base_class, ..
        } in class_defs
        {
            let class_idx = state.register_class_if_not_yet_registered(class.clone());
            let base_idx = base_class
                .as_ref()
                .map(|base| state.register_class_if_not_yet_registered(base.clone()));

            cfg.classes
                .push(Class::new(class.clone(), class_idx, base_idx));
        }

        info!("Declaring fields:");
        // Declare fields
        for ClassDef {
            class: class_name,
            class_block: ClassBlock(class_items),
            base_class,
            ..
        } in class_defs
        {
            let state: &mut State = &mut state.new_scope();
            let class_idx = state.retrieve_class_idx(class_name);
            let base_class_idx = base_class
                .as_ref()
                .map(|base| state.retrieve_class_idx(base));

            // Add base class fields to variable env
            // ASSUMPTION: Base class declared before derived -> fulfilled by toposort
            if let Some(base_class_idx) = base_class_idx {
                let base_fields = cfg.classes[base_class_idx.0].fields.clone();
                for (field_name, _field) in &base_fields {
                    state.declare_var(field_name.clone(), VariableKind::ClassField);
                }
                cfg.classes[class_idx.0].size += base_fields.len() + 1/*VST*/;
                cfg.classes[class_idx.0].fields = base_fields;
            }

            // Add fields to variable env
            for class_item in class_items {
                match class_item {
                    ClassItem::Field(_, typ, field_name) => {
                        cfg.classes[class_idx.0].add_field(field_name.clone(), typ.clone().into());
                        state.declare_var(field_name.clone(), VariableKind::ClassField);
                    }
                    ClassItem::Method(_) => (), // not yet
                }
            }
        }

        info!("Declaring methods:");
        // Declare methods
        for ClassDef {
            class: class_name,
            class_block,
            ..
        } in class_defs
        {
            let class_idx = state.register_class_if_not_yet_registered(class_name.clone());

            if let Some(base_idx) = cfg.classes[class_idx.0].base_idx {
                let base_class = &cfg.classes[base_idx.0];
                let (base_methods, base_next_method_idx) =
                    (base_class.methods.clone(), base_class.next_method_idx);
                let class = &mut cfg.classes[class_idx.0];
                class.methods = base_methods;
                class.next_method_idx = base_next_method_idx;
            }

            for class_item in &class_block.0 {
                match class_item {
                    ClassItem::Field(_, _, _) => (),
                    ClassItem::Method(method) => {
                        let mangled_name = mangle_method(&method.name, &class_name);

                        // `self` is added as the last parameter in IrFunction
                        let method_type = {
                            let mut fun_type = method.fun_type();
                            fun_type
                                .params
                                .push(NonvoidType::TClass(class_name.clone()));
                            fun_type
                        };
                        let self_var = state.fresh_reg(VarType::class(class_name.clone()));
                        let mut param_vars = state.declare_and_give_params(
                            method
                                .params
                                .iter()
                                .map(|param| (param.name.clone(), param.type_.clone())),
                        );
                        param_vars.push(self_var);

                        // Store params mapping for method definitions below
                        let params = method
                            .params
                            .iter()
                            .map(|param| param.name.clone())
                            .chain(iter::once(SELF.to_string()))
                            .zip(param_vars.iter().copied())
                            .collect::<VecMap<_, _>>();
                        cfg.classes[class_idx.0].add_method(
                            method.name.clone(),
                            mangled_name.clone(),
                            params,
                            method_type.ret_type.clone(),
                        );

                        cfg.new_function(mangled_name, method_type, param_vars, Some(self_var));
                    }
                }
            }
        }

        info!("Defining methods:");
        // Declare fields and define methods
        for ClassDef {
            class: class_name,
            class_block: ClassBlock(class_items),
            base_class,
            ..
        } in class_defs
        {
            let state: &mut State = &mut state.new_scope();
            let class_idx = state.retrieve_class_idx(class_name);
            let base_class_idx = base_class
                .as_ref()
                .map(|base| state.retrieve_class_idx(base));

            // Add base class fields to variable env
            // ASSUMPTION: Base class declared before derived -> fulfilled by toposort
            if let Some(base_class_idx) = base_class_idx {
                let base_fields = cfg.classes[base_class_idx.0].fields.clone();
                for (field_name, _field) in &base_fields {
                    state.declare_var(field_name.clone(), VariableKind::ClassField);
                }
                // cfg.classes[class_idx.0].size += base_fields.len();
                // cfg.classes[class_idx.0].fields = base_fields;
            }

            // Add fields to variable env
            for class_item in class_items {
                match class_item {
                    ClassItem::Field(_, _, field_name) => {
                        // cfg.classes[class_idx.0].add_field(field_name.clone(), typ.clone().into());
                        state.declare_var(field_name.clone(), VariableKind::ClassField);
                    }
                    ClassItem::Method(_) => (), // not yet
                }
            }

            cfg.current_class = Some(class_idx);
            for class_item in class_items {
                match class_item {
                    ClassItem::Field(_, _, _) => (), // already covered
                    ClassItem::Method(method) => {
                        let mangled_name = mangle_method(&method.name, &class_name);

                        // New-scoped state
                        let method_state = &mut state.new_scope();

                        // Parameters
                        for (param_name, param_var) in cfg.classes[class_idx.0]
                            .methods
                            .get/*_mut*/(&method.name)
                            .unwrap()
                            .params
                            /*.drain()*/.clone()
                        {
                            method_state.declare_var(param_name, VariableKind::StackVar(param_var));
                        }

                        // Self
                        let ir_func = cfg.functions.get(&mangled_name).unwrap();
                        let self_var = ir_func.self_var.unwrap();
                        method_state.declare_var(SELF.into(), VariableKind::StackVar(self_var));

                        // current_func to properly name blocks
                        cfg.current_func = mangled_name.clone();

                        debug!("Emitting IR for method: {}:{}", class_name, &method.name);
                        cfg.make_current(ir_func.entry.unwrap(), "Defining methods");
                        method.block.ir(cfg, method_state);
                    }
                }
            }
        }
        cfg.current_class = None;
    }

    fn ir_for_functions(cfg: &mut CFG, state: &mut State, fun_defs: &[FunDef]) {
        for func in fun_defs {
            let param_vars = state.declare_and_give_params(
                func.params
                    .iter()
                    .map(|param| (param.name.clone(), param.type_.clone())),
            );
            cfg.new_function(func.name.clone(), func.fun_type(), param_vars, None);
            debug!("\nEmitting IR for function: {}", &func.name);
            func.block.ir(cfg, &mut state.new_scope());
        }
    }
}

impl Block {
    fn ir(&self, cfg: &mut CFG, state: &mut State) -> bool {
        self.1
            .iter()
            .map(|stmt| stmt.ir(cfg, state))
            .any(|definitive_return| definitive_return)
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
    fn ir(&self, cfg: &mut CFG, state: &mut State) -> bool {
        match &self.1 {
            StmtInner::Empty => false,

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
                    state.declare_var(decl.name.clone(), VariableKind::StackVar(var));
                }
                false
            }

            StmtInner::Ass(lval, rval) => {
                let rval_fut = rval.ir_fut();
                let loc = lval.ir(cfg, state, None);
                let quadruple = match (loc, rval_fut) {
                    (Loc::Var(lvar), ValueFut::Instant(i)) => Quadruple::Set(lvar, i),
                    (Loc::Var(lvar), ValueFut::VariableFut(rvar_fut)) => {
                        let rvar = rvar_fut.ir(cfg, state, None).unwrap();
                        Quadruple::Copy(lvar, rvar)
                    }
                    (Loc::Mem(lmem), rval) => {
                        let rval = rval.ir(cfg, state, None);
                        Quadruple::DerefStore(rval, lmem)
                    }
                };
                cfg.current_mut().quadruples.push(quadruple);
                false
            }

            StmtInner::Incr(lval) => {
                let lval = lval.ir(cfg, state, None);
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::InPlaceUnOp(InPlaceUnOpType::Inc, lval));
                false
            }

            StmtInner::Decr(lval) => {
                let lval = lval.ir(cfg, state, None);
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::InPlaceUnOp(InPlaceUnOpType::Dec, lval));
                false
            }

            StmtInner::Return(expr) => {
                let reg = expr.ir_fut().ir(cfg, state, None);
                trace!(
                    "Because of Return, setting {} end_type to Return({:?})",
                    cfg.current_block_idx.0,
                    reg
                );
                cfg.current_mut().set_end_type(EndType::Return(Some(reg)));
                true
            }
            StmtInner::VoidReturn => {
                cfg.current_mut().set_end_type(EndType::Return(None));
                true
            }

            StmtInner::Cond(cond, then) => {
                let cond_fut = cond.ir_fut();
                match cond_fut {
                    ValueFut::Instant(i) => {
                        if *i == 0 {
                            // condition always false
                            // skip
                            false
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

                        false
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
                        let then_definitive_return = then.ir(cfg, state);

                        cfg.make_current(else_block, "CondElse else");
                        let else_definitive_return = else_br.ir(cfg, state);

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

                        then_definitive_return && else_definitive_return
                    }
                }
            }

            StmtInner::SExp(expr) => {
                let _ = expr.ir_fut().ir(cfg, state, None);
                false
            }

            StmtInner::While(cond, body) => {
                let pre_block = cfg.current_block_idx;
                let cond_fut = cond.ir_fut();
                match cond_fut {
                    ValueFut::Instant(i) => {
                        if *i == 0 {
                            // condition always false
                            // skip
                            false
                        } else {
                            // condition always true
                            // make infinite loop in a new block
                            let loop_block = cfg.new_block(BasicBlockKind::InfiniteLoop);
                            cfg[pre_block].set_end_type(EndType::Goto(loop_block));
                            cfg[loop_block].set_end_type(EndType::Goto(loop_block));
                            cfg.make_current(loop_block, "While infinite loop");
                            body.ir(cfg, state);

                            true // never escapes
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
                        false
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
    trace!("Made cond ctx: {:#?}.", ctx);

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
        trace!(
            "Because of {}, set {} end_type to IfElse(then: {}, else: {}).",
            from,
            cond_ctx.pre_block.0,
            cond_ctx.block_true.0,
            cond_ctx.block_false.0
        );
        None
    } else {
        Some(var)
    }
}

impl Expr {
    #[allow(unused)]
    fn debug_surface<'a>(&'a self) -> DebugExprInner<'a> {
        DebugExprInner(self)
    }

    fn ir_fut(&self) -> ValueFut {
        // trace!("ir_fut of {:#?}", &self.debug_surface());
        match match &self.1 {
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
            ExprInner::Null(_) => Some(Instant(0)),
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
        // trace!(
        //    "ir of {:#?} with cond_ctx {:?}",
        //     &self.debug_surface(),
        //     cond_ctx
        // );
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
                            trace!(
                                "Because of RelOp, set {} end_type to IfElse(then: {}, else: {}).",
                                cond_ctx.pre_block.0,
                                cond_ctx.block_true.0,
                                cond_ctx.block_false.0
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

            ExprInner::Null(_typ) => unreachable!("Impossible instants"), /* {
            let reg = state.fresh_reg(typ.clone().into());
            cfg.current_mut()
            .quadruples
            .push(Quadruple::Set(reg, Instant(0)));
            Some(reg)
            } */
            ExprInner::LVal(lval) => {
                let loc = lval.ir(cfg, state, None);
                let var = match loc {
                    Loc::Var(var) => var,
                    Loc::Mem(mem) => {
                        let var = state.fresh_reg(lval.typ().unwrap());
                        cfg.current_mut()
                            .quadruples
                            .push(Quadruple::DerefLoad(var, mem));
                        var
                    }
                };
                finish_cond_ctx_leaf(cfg, state, var, cond_ctx, "Expr LVal")
            }
        };
        if let Some(cond_ctx) = cond_ctx {
            cfg.make_current(cond_ctx.block_next, "Expr finish");
            assert!(res.is_none());
        }
        res
    }
}

impl LVal {
    fn typ(&self) -> Option<VarType> {
        Some(self.2.borrow().clone()?.into_nonvoid().ok()?.into())
    }

    fn ir(&self, cfg: &mut CFG, state: &mut State, cond_ctx: Option<ConditionalContext>) -> Loc {
        let typ = self.typ();
        match &self.1 {
            LValInner::Id(var_id) => {
                let var_kind = state.retrieve_var(var_id);
                let loc = match var_kind {
                    VariableKind::StackVar(var) => Loc::Var(var),
                    VariableKind::ClassField => {
                        let current_class_idx = cfg.current_class.unwrap();
                        let this_var = {
                            let current_func = &cfg.current_func;
                            cfg.functions.get(current_func).unwrap().self_var.unwrap()
                        };
                        let offset = cfg.classes[current_class_idx.0]
                            .fields
                            .get(var_id)
                            .unwrap()
                            .offset;
                        Loc::Mem(Mem {
                            base: this_var,
                            offset,
                        })
                    }
                };
                loc
            }

            LValInner::FunCall { name, args } => {
                let args = args
                    .iter()
                    .map(|arg| arg.ir_fut().ir(cfg, state, None))
                    .collect();
                let retvar = state.fresh_reg(typ.unwrap_or(VarType::INT));
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::Call(retvar, name.clone(), args));
                Loc::Var(retvar)
            }

            LValInner::FieldAccess(lval, field_name) => {
                let lvar = match lval.ir(cfg, state, cond_ctx) {
                    Loc::Var(var) => var,
                    Loc::Mem(mem) => {
                        let var = state.fresh_reg(lval.typ().unwrap());
                        cfg.current_mut()
                            .quadruples
                            .push(Quadruple::DerefLoad(var, mem));
                        var
                    }
                };
                let typ = state.get_var_type(lvar).unwrap();
                // let typ_front = lval.2.borrow_mut().as_ref().unwrap().clone();
                // assert_eq!(typ_front.into() as VarType, *typ);

                let class_name = typ.as_class().unwrap();
                let class_idx = state.retrieve_class_idx(&class_name);
                let class_info = cfg.classes.get(class_idx.0).unwrap();
                let field = class_info.resolve_field(&cfg.classes, field_name);
                let field_offset = field.offset;
                // let res = state.fresh_reg(field.typ.clone());
                // cfg.current_mut().quadruples.push(Quadruple::DerefLoad(res, Loc::Mem(Mem{base: lvar, offset: field_offset})));
                Loc::Mem(Mem {
                    base: lvar,
                    offset: field_offset,
                })
            }

            LValInner::ArrSub(_, _) => todo!(),

            LValInner::MethodCall {
                object,
                method_name,
                args,
            } => {
                let typ = object.typ().unwrap();
                let this_class_name = typ.as_class().unwrap();
                let this_class_idx = state.retrieve_class_idx(this_class_name);

                let loc = object.ir(cfg, state, None);
                let object = match loc {
                    Loc::Var(var) => var,
                    Loc::Mem(mem) => {
                        let var = state.fresh_reg(typ.clone());
                        cfg.current_mut()
                            .quadruples
                            .push(Quadruple::DerefLoad(var, mem));
                        var
                    }
                };
                let args = args
                    .iter()
                    .map(|arg| arg.ir_fut().ir(cfg, state, None))
                    .chain(iter::once(Value::Variable(object)))
                    .collect::<Vec<_>>();
                // assert_eq!(
                //     args.len(),
                //     cfg.functions.get(&mangled_name).unwrap().params.len()
                // );
                let method = cfg.classes[this_class_idx.0]
                    .methods
                    .get(method_name)
                    .unwrap();
                let method_idx = method.idx;
                let rettype = method
                    .rettype
                    .clone()
                    .into_nonvoid()
                    .unwrap_or(NonvoidType::TInt)
                    .into();
                let retvar = state.fresh_reg(rettype);

                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::VirtualCall(retvar, object, method_idx, args));
                Loc::Var(retvar)
            }

            LValInner::New(typ) => match typ {
                NewType::TClass(name) => {
                    let class_idx = state.retrieve_class_idx(name);
                    let class_size = cfg.classes[class_idx.0].size;
                    let var = state.fresh_reg(VarType::class(name.clone()));
                    cfg.current_mut().quadruples.push(Quadruple::Call(
                        var,
                        NEW_FUNC.into(),
                        vec![Value::Instant(Instant((class_size * QUADWORD_SIZE) as i64))],
                    ));
                    cfg.current_mut().quadruples.push(Quadruple::VstStore(
                        class_idx,
                        Mem {
                            base: var,
                            offset: 0,
                        },
                    ));

                    Loc::Var(var)
                }
                NewType::TIntArr(_)
                | NewType::TStringArr(_)
                | NewType::TBooleanArr(_)
                | NewType::TClassArr(_, _) => todo!(),
            },
        }
    }
}
