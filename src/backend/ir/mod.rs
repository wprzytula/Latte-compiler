mod gen;
mod opts;

pub(crate) use gen::{CONCAT_STRINGS_FUNC, REAL_MAIN};

use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, Index, IndexMut},
};

use vector_map::VecMap;

use crate::frontend::semantic_analysis::{
    ast::{self, *},
    FunType, INITIAL_FUNCS,
};

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

#[derive(Debug, Clone)]
pub enum VarType {
    Simple(SimpleVarType),
    Ptr(PtrVarType),
    ArrSimple(SimpleVarType),
    ArrPtr(PtrVarType),
}

impl VarType {
    const INT: VarType = VarType::Simple(SimpleVarType::Int);
    const BOOL: VarType = VarType::Simple(SimpleVarType::Bool);

    /**
     * Strings are represented as pointers to memory that is laid out as follows:
     * [usize_b1, usize_b2, usize_b3, usize_b4, b1, b2, b3, ..., bn]
     * where b1b2b3...bn is the string content and usize is equal to n.
     * */
    const STRING: VarType = VarType::Ptr(PtrVarType::String);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringLiteral(pub usize);

#[derive(Debug, Clone)]
pub enum SimpleVarType {
    Int,
    Bool,
}

#[derive(Debug, Clone)]
pub enum PtrVarType {
    String,
    Class(Ident),
}

impl From<NonvoidType> for VarType {
    fn from(t: NonvoidType) -> Self {
        match t {
            NonvoidType::TInt => VarType::INT,
            NonvoidType::TString => VarType::STRING,
            NonvoidType::TBoolean => VarType::BOOL,
            NonvoidType::TClass(_) => todo!(),
            NonvoidType::TIntArr => todo!(),
            NonvoidType::TStringArr => todo!(),
            NonvoidType::TBooleanArr => todo!(),
            NonvoidType::TClassArr(_) => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Quadruple {
    BinOp(Var, Var, BinOpType, Value), // dst, op1, op, op2
    RelOp(Var, Var, RelOpType, Value), // dst, op1, op, op2
    UnOp(Var, UnOpType, Value),        // dst, op,

    Copy(Var, Var),
    Set(Var, Instant),
    GetStrLit(Var, StringLiteral),

    Call(Var, Ident, Vec<Value>),

    ArrLoad(Var, Var, Value),  // (dst, arr, idx)
    ArrStore(Var, Value, Var), // (arr, idx, src)
    DerefLoad(Var, Var),       // (dst, ptr)
    DerefStore(Value, Var),    // (src, ptr)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BasicBlockIdx(usize);

#[derive(Debug, Clone, Copy)]
pub enum CallingConvention {
    StackVars,
    Cdecl,
}

#[derive(Debug)]
pub struct Ir {
    pub(crate) cfg: CFG,
    pub(crate) string_literals: Vec<String>,
}

#[derive(Debug)]
pub struct IrFunction {
    pub convention: CallingConvention,
    pub entry: Option<BasicBlockIdx>,
    pub typ: FunType,
    pub params: Vec<Var>,
}

#[derive(Debug)]
pub struct CFG {
    pub blocks: Vec<BasicBlock>,
    current_block_idx: BasicBlockIdx,
    current_func: Ident,
    pub functions: HashMap<Ident, IrFunction>,
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

#[derive(Debug)]
pub struct BasicBlock {
    idx: BasicBlockIdx,
    func: Ident,
    pub quadruples: Vec<Quadruple>,
    pub successors: Vec<BasicBlockIdx>,
    pub predecessors: Vec<BasicBlockIdx>,
    entry: bool,
    pub end_type: Option<EndType>,
    phi_nodes: VecMap<Var, VecMap<BasicBlockIdx, Var>>,
}
