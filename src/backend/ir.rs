use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, Index, IndexMut},
};

use vector_map::VecMap;

use crate::frontend::semantic_analysis::{
    ast::{self, *},
    FunType,
};

use self::state::State;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Instant(Instant),
    Variable(Var),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Instant(pub i64);
impl Instant {
    fn bool(b: bool) -> Self {
        Self(i64::from(b))
    }
    fn not(&self) -> Self {
        Self(!(self.0 != 0) as i64)
    }
}
impl Deref for Instant {
    type Target = i64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub enum Label {
    Num(usize),
    Func(Ident),
    Method(Ident, Ident),
}

#[derive(Debug)]
pub enum BinOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
}

#[derive(Debug)]
pub enum UnOpType {
    Not,
    Neg,
    Inc,
    Dec,
}

#[derive(Debug)]
pub enum RelOpType {
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    NEq,
}

fn mangle_method(name: &Ident, class: &Ident) -> Ident {
    Ident::from(format!("{}${}", name, class))
}

fn rename_var(var: &mut Var, prev: Var, current: Var) {
    if *var == prev {
        *var = current;
        println!("Renaming usage Var {} -> {}", prev.0, current.0);
    }
}
fn rename_val(val: &mut Value, prev: Var, current: Var) {
    if let Value::Variable(var) = val {
        rename_var(var, prev, current)
    }
}

#[derive(Debug)]
pub enum Quadruple {
    BinOp(Var, Var, BinOpType, Value), // dst, op1, op, op2
    RelOp(Var, Var, RelOpType, Value), // dst, op1, op, op2
    UnOp(Var, UnOpType, Value),        // dst, op,
    Copy(Var, Var),
    Set(Var, Instant),

    Call(Var, Ident, Vec<Value>),

    ArrLoad(Var, Var, Value),  // (dst, arr, idx)
    ArrStore(Var, Value, Var), // (arr, idx, src)
    DerefLoad(Var, Var),       // (dst, ptr)
    DerefStore(Value, Var),    // (src, ptr)
}

impl Quadruple {
    fn rename_usages(&mut self, prev: Var, current: Var) {
        match self {
            Quadruple::BinOp(_, var1, _, val) | Quadruple::RelOp(_, var1, _, val) => {
                rename_var(var1, prev, current);
                rename_val(val, prev, current);
            }
            Quadruple::UnOp(_, _, val) => rename_val(val, prev, current),
            Quadruple::Copy(_, var) => rename_var(var, prev, current),
            Quadruple::Set(_, _) => (),
            Quadruple::Call(_, _, vals) => {
                for val in vals {
                    rename_val(val, prev, current);
                }
            }
            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),
            Quadruple::DerefLoad(_, _) => todo!(),
            Quadruple::DerefStore(_, _) => todo!(),
        }
    }

    fn assigns_to_var(&self, var: Var) -> bool {
        match self {
            Quadruple::BinOp(ass, _, _, _)
            | Quadruple::RelOp(ass, _, _, _)
            | Quadruple::UnOp(ass, _, _)
            | Quadruple::Copy(ass, _)
            | Quadruple::Set(ass, _)
            | Quadruple::Call(ass, _, _) => *ass == var,
            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),
            Quadruple::DerefLoad(_, _) => todo!(),
            Quadruple::DerefStore(_, _) => todo!(),
        }
    }

    fn rename_assignment(&mut self, prev: Var, current: Var) {
        println!("Renaming assignment of Var {} -> {}", prev.0, current.0);
        assert!(self.assigns_to_var(prev));
        match self {
            Quadruple::BinOp(ass, _, _, _)
            | Quadruple::RelOp(ass, _, _, _)
            | Quadruple::UnOp(ass, _, _)
            | Quadruple::Copy(ass, _)
            | Quadruple::Set(ass, _)
            | Quadruple::Call(ass, _, _) => *ass = current,
            Quadruple::ArrLoad(_, _, _) => todo!(),
            Quadruple::ArrStore(_, _, _) => todo!(),
            Quadruple::DerefLoad(_, _) => todo!(),
            Quadruple::DerefStore(_, _) => todo!(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BasicBlockIdx(usize);

#[derive(Debug)]
pub struct CFG {
    pub blocks: Vec<BasicBlock>,
    current_block_idx: BasicBlockIdx,
    pub function_entries: HashMap<Ident, (BasicBlockIdx, FunType)>,
}

impl CFG {
    fn new() -> Self {
        Self {
            blocks: vec![],
            current_block_idx: BasicBlockIdx(usize::MAX),
            function_entries: HashMap::new(),
        }
    }

    fn new_function(&mut self, id: Ident, fun_type: FunType) {
        let entry = self.new_block();
        self.function_entries
            .insert(id, (entry, fun_type))
            .ok_or(())
            .unwrap_err(); // assert unique name
        self.make_current(entry);
        self.current_mut().entry = true;
    }

    fn new_block(&mut self) -> BasicBlockIdx {
        self.blocks.push(BasicBlock::empty());
        BasicBlockIdx(self.blocks.len() - 1)
    }

    fn current_mut(&mut self) -> &mut BasicBlock {
        let idx = self.current_block_idx;
        &mut self[idx]
    }

    fn make_current(&mut self, idx: BasicBlockIdx) {
        self.current_block_idx = idx;
    }

    pub fn variables_in_function(&self, func_name: &Ident) -> HashSet<Var> {
        let (entry, _) = *self.function_entries.get(func_name).unwrap();
        let mut variables = HashSet::new();

        fn dfs_variables(cfg: &CFG, block_idx: BasicBlockIdx, variables: &mut HashSet<Var>) {
            cfg[block_idx].defined_variables(variables);
            for succ in cfg[block_idx].successors.iter().copied() {
                dfs_variables(cfg, succ, variables)
            }
        }

        dfs_variables(self, entry, &mut variables);

        variables
    }

    fn link_succ_and_pred(&mut self) {
        let entries = self
            .function_entries
            .values()
            .map(|(entry, _)| entry)
            .copied()
            .collect::<Vec<_>>();
        let mut visited = vec![];
        for entry in entries {
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

    fn make_ssa(&mut self, state: &mut State) {
        let mut phi_to_replace = vec![];

        for (idx, block) in self.blocks.iter_mut().enumerate() {
            let this = BasicBlockIdx(idx);
            let vars = block.all_variables();

            if block.predecessors.len() > 1 {
                // insert (possibly reduntant) phi nodes
                for var in vars.iter().copied() {
                    block.phi_nodes.insert(
                        var,
                        block.predecessors.iter().map(|pred| (*pred, var)).collect(),
                    );
                }
            }

            for var in vars.iter().copied() {
                let mut current;

                // proceed with renaming variables

                if block.predecessors.len() > 1 {
                    // in case of a phi node block, variables are defined in phi nodes.
                    let phi_node = block.phi_nodes.remove(&var).unwrap();
                    current = state.fresh_reg(None);
                    block.phi_nodes.insert(current, phi_node);
                } else {
                    // in case of a non-phi node block, variables are defined first in quadruples, so we need to rename them there.
                    // but we don't rename variable uses until the first definition of that variable. Hence we do noop rename here.
                    current = var;
                }

                for quadruple in block.quadruples.iter_mut() {
                    quadruple.rename_usages(var, current);
                    if quadruple.assigns_to_var(var) {
                        current = state.fresh_reg(None);
                        quadruple.rename_assignment(var, current);
                    }
                }

                if current != var {
                    // if we did rename some variable, let's rename end info as well.
                    if let Some(ref mut end_info) = block.end_type {
                        end_info.rename_usages(var, current)
                    }

                    // if we did rename some variable, let's propagate the change to our successors' phi nodes.
                    for succ in block.successors.iter().copied() {
                        phi_to_replace.push((succ, this, var, current));
                    }
                }
            }
        }

        // Actually update successors' phi nodes.
        for (succ, this, var, current) in phi_to_replace {
            for (_, phi_node) in self[succ].phi_nodes.iter_mut() {
                for used in phi_node
                    .iter_mut()
                    .filter_map(|(pred, then)| (*pred == this).then(|| then))
                    .filter(|used| **used == var)
                {
                    *used = current;
                }
            }
        }
    }

    /// Removes redundant variables and their corresponding phi nodes.
    fn optimise_ssa(&mut self) {
        println!("Removing redundant variables:");

        fn exactly_one_mapping(phi_node: &VecMap<BasicBlockIdx, Var>) -> Option<Var> {
            let mut one_mapping = None;
            for (_edge, mapping) in phi_node {
                match one_mapping {
                    Some(one_mapping) if one_mapping == *mapping => (),
                    Some(_) => return None,
                    None => one_mapping = Some(*mapping),
                }
            }
            one_mapping
        }

        let redundant_phi_nodes = self
            .blocks
            .iter()
            .map(|block| {
                block.phi_nodes.iter().filter_map(|(var, phi_node)| {
                    exactly_one_mapping(phi_node).map(|mapping| {
                        (*var, mapping) // (to_be_replaced, replaced_with)
                    })
                })
            })
            .flatten();

        let redundant_copied_vars = self
            .blocks
            .iter()
            .map(|block| {
                block.quadruples.iter().filter_map(|quadruple| {
                    if let Quadruple::Copy(to, from) = quadruple {
                        Some((*to, *from))
                    } else {
                        None
                    }
                })
            })
            .flatten();

        let redundant_vars = redundant_phi_nodes
            .chain(redundant_copied_vars)
            .collect::<Vec<_>>();

        for (redundant_var, replacement_var) in redundant_vars {
            println!(
                "Removing redundant var {}, replacing with {}.",
                redundant_var.0, replacement_var.0
            );
            for block in self.blocks.iter_mut() {
                block.phi_nodes.remove(&redundant_var);
                for (_, phi_node) in block.phi_nodes.iter_mut() {
                    for (_, var) in phi_node.iter_mut() {
                        rename_var(var, redundant_var, replacement_var)
                    }
                }
                for quadruple in block.quadruples.iter_mut() {
                    quadruple.rename_usages(redundant_var, replacement_var);
                }
                block.quadruples.retain(|quadruple| match *quadruple {
                    Quadruple::Copy(to, from) if to == redundant_var => {
                        assert_eq!(from, replacement_var);
                        false
                    }
                    _ => true,
                });
                if let Some(ref mut end_info) = block.end_type {
                    end_info.rename_usages(redundant_var, replacement_var);
                }
            }
        }
    }
}

impl Index<BasicBlockIdx> for CFG {
    type Output = BasicBlock;

    fn index(&self, index: BasicBlockIdx) -> &Self::Output {
        &self.blocks[index.0]
    }
}
impl IndexMut<BasicBlockIdx> for CFG {
    fn index_mut(&mut self, index: BasicBlockIdx) -> &mut Self::Output {
        &mut self.blocks[index.0]
    }
}

#[derive(Debug)]
pub enum EndType {
    Goto(BasicBlockIdx),
    IfElse(Var, BasicBlockIdx, BasicBlockIdx),
    Return(Option<Value>),
}

impl EndType {
    fn rename_usages(&mut self, prev: Var, current: Var) {
        match self {
            EndType::IfElse(var, _, _) => rename_var(var, prev, current),
            EndType::Return(Some(val)) => rename_val(val, prev, current),
            EndType::Return(None) => (),
            EndType::Goto(_) => (),
        }
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub quadruples: Vec<Quadruple>,
    pub successors: Vec<BasicBlockIdx>,
    pub predecessors: Vec<BasicBlockIdx>,
    entry: bool,
    pub end_type: Option<EndType>,
    phi_nodes: VecMap<Var, VecMap<BasicBlockIdx, Var>>,
}

impl BasicBlock {
    pub fn empty() -> Self {
        Self {
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

    fn all_variables(&self) -> HashSet<Var> {
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
                Quadruple::Set(var, _) => {
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

    use crate::frontend::semantic_analysis::ast::{Ident, NonvoidType};

    use super::Var;

    pub struct StateGuard<'a>(&'a mut State, State);

    impl Drop for StateGuard<'_> {
        fn drop(&mut self) {
            // self.0.next_label_num = self.1.next_label_num;
            self.0.next_var_num = self.1.next_var_num;
            std::mem::swap(&mut self.0.string_literals, &mut self.1.string_literals);
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
        string_literals: Vec<String>,
        next_var_num: usize,
        var_location: Map<Ident, Var>,
        var_types: Vec<Option<NonvoidType>>,
    }
    impl State {
        pub(super) fn new() -> Self {
            Self {
                next_var_num: 0,
                string_literals: Vec::new(),
                var_location: Map::new(),
                var_types: Vec::new(),
            }
        }

        pub(super) fn fresh_reg(&mut self, typ: Option<NonvoidType>) -> Var {
            let var = Var(self.next_var_num);
            self.next_var_num += 1;
            var
        }

        pub(super) fn get_var_type(&self, var: Var) -> &Option<NonvoidType> {
            &self.var_types[var.0]
        }

        pub(super) fn new_scope<'a>(&'a mut self) -> StateGuard<'a> {
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

        pub(super) fn enter_new_frame(
            &mut self,
            params: impl Iterator<Item = (Ident, NonvoidType)>,
        ) {
            self.var_location = Map::new();
            for (param_name, param_type) in params {
                self.declare_var(param_name, param_type);
            }
        }

        pub(super) fn declare_var(&mut self, id: Ident, typ: NonvoidType) -> Var {
            // assert!(!self.var_location.contains_key(&id));
            let var = Var(self.next_var_num);
            self.next_var_num += 1;
            self.var_location.insert_mut(id, var);
            var
        }

        pub(super) fn retrieve_var(&mut self, id: &Ident) -> Var {
            *self
                .var_location
                .get(id)
                .unwrap_or_else(|| panic!("No var {} declared.", id))
        }

        pub(super) fn register_literal(&mut self, string: String) -> usize {
            self.string_literals.push(string);
            self.string_literals.len() - 1
        }
    }
}

impl Program {
    pub fn ir(&self) -> CFG {
        let mut state = State::new();
        let mut cfg = CFG::new();
        for func in self.0.iter() {
            state.enter_new_frame(
                func.params
                    .iter()
                    .map(|param| (param.name.clone(), param.type_.clone())),
            );
            cfg.new_function(func.name.clone(), func.fun_type());
            func.block.ir(&mut cfg, &mut state);
        }

        cfg.link_succ_and_pred();
        println!("BEFORE SSA: max var = {}", state.fresh_reg(None).0);
        cfg.make_ssa(&mut state);
        cfg.optimise_ssa();
        cfg
    }
}

impl Block {
    fn ir(&self, cfg: &mut CFG, state: &mut State) {
        for stmt in &self.1 {
            stmt.ir(cfg, state)
        }
    }
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
                    let var = state.declare_var(decl.name.clone(), decls.type_.clone());
                    if let Some(init) = decl.init.as_ref() {
                        let reg = init.ir(cfg, state);
                        match reg {
                            Value::Instant(i) => {
                                cfg.current_mut().quadruples.push(Quadruple::Set(var, i))
                            }
                            Value::Variable(reg) => {
                                cfg.current_mut().quadruples.push(Quadruple::Copy(var, reg))
                            }
                        }
                    }
                }
            }
            StmtInner::Ass(lval, rval) => {
                let rval = rval.ir(cfg, state);
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
                let reg = expr.ir(cfg, state);
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
                let res = cond.ir(cfg, state);
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
                        cfg[then_block].end_type = Some(EndType::Goto(next_block));
                    }
                }
            }
            StmtInner::CondElse(cond, then, else_br) => {
                let res = cond.ir(cfg, state);
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
                        cfg[then_block].end_type = Some(EndType::Goto(next_block));
                        cfg[else_block].end_type = Some(EndType::Goto(next_block));
                    }
                }
            }

            StmtInner::SExp(expr) => {
                let _ = expr.ir(cfg, state);
            }

            StmtInner::While(cond, body) => {
                let pre_block = cfg.current_block_idx;
                let cond_block = cfg.new_block();
                cfg.make_current(cond_block);

                let res = cond.ir(cfg, state);
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
                Op::LogOp(log_op, _, _) => true,
            },
            ExprInner::LVal(lval) => lval.ends_current_block(),
        }
    }

    fn ir(&self, cfg: &mut CFG, state: &mut State) -> Value {
        match &self.1 {
            ExprInner::Op(op) => match op {
                Op::UnOp(un_op, a) => match un_op {
                    ast::UnOpType::Neg => match a.ir(cfg, state) {
                        Value::Instant(i) => Value::Instant(Instant(-*i)),
                        var => {
                            let tmp = state.fresh_reg(None);
                            cfg.current_mut().quadruples.push(Quadruple::UnOp(
                                tmp,
                                UnOpType::Neg,
                                var,
                            ));
                            Value::Variable(tmp)
                        }
                    },
                    ast::UnOpType::Not => match a.ir(cfg, state) {
                        Value::Instant(i) => Value::Instant(i.not()),
                        var => {
                            let tmp = state.fresh_reg(None);
                            cfg.current_mut().quadruples.push(Quadruple::UnOp(
                                tmp,
                                UnOpType::Not,
                                var,
                            ));
                            Value::Variable(tmp)
                        }
                    },
                },
                Op::BinOp(bin_op, a, b) => {
                    let a_val = a.ir(cfg, state);
                    let b_val = b.ir(cfg, state);

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

                    let tmp = state.fresh_reg(None);
                    let a_var = match a_val {
                        Value::Instant(i) => {
                            let var = state.fresh_reg(None);
                            cfg.current_mut().quadruples.push(Quadruple::Set(var, i));
                            var
                        }
                        Value::Variable(var) => var,
                    };
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
                        ast::BinOpType::Add => {
                            // TODO: string addition
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
                    let a_res = a.ir(cfg, state);
                    match (a_res, log_op) {
                        (Value::Instant(i), LogOpType::And) if *i == 0 => {
                            Value::Instant(Instant::bool(false))
                        }
                        (Value::Instant(i), LogOpType::And) if *i != 0 => b.ir(cfg, state),
                        (Value::Instant(i), LogOpType::Or) if *i == 0 => b.ir(cfg, state),
                        (Value::Instant(i), LogOpType::Or) if *i != 0 => {
                            Value::Instant(Instant::bool(true))
                        }
                        (Value::Instant(_), _) => unreachable!(),
                        (Value::Variable(a_var), _) => {
                            let pre_block_idx = cfg.current_block_idx;
                            let check_b_block_idx = cfg.new_block();
                            cfg.make_current(check_b_block_idx);

                            let b_res = b.ir(cfg, state);
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

                                    let res_var = state.fresh_reg(Some(NonvoidType::TBoolean));
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
                let reg = state.fresh_reg(Some(NonvoidType::TString));
                let lit = state.register_literal(s.clone());
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::Set(reg, Instant(lit as i64)));
                Value::Variable(reg)
            }
            ExprInner::Null(typ) => Value::Instant(Instant(0)),
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
            Some(nonvoid.clone())
        } else {
            None
        };
        match &self.1 {
            LValInner::Id(var_id) => state.retrieve_var(var_id),
            LValInner::FunCall { name, args } => {
                let args = args.iter().map(|arg| arg.ir(cfg, state)).collect();
                let retvar = state.fresh_reg(typ);
                cfg.current_mut()
                    .quadruples
                    .push(Quadruple::Call(retvar, name.clone(), args));
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
