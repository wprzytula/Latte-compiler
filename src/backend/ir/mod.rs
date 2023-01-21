mod gen;
// mod opts;
mod ra;

pub(crate) use gen::{CONCAT_STRINGS_FUNC, NEW_FUNC};

use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, Index, IndexMut},
};

use enum_as_inner::EnumAsInner;
use vector_map::VecMap;

use crate::frontend::semantic_analysis::{
    ast::{self, *},
    FunType, INITIAL_FUNCS,
};

use self::ra::FlowAnalysis;

use super::asmgen::QUADWORD_SIZE;

#[derive(Debug, Clone, Copy, EnumAsInner)]
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

#[derive(Debug, Clone, Copy)]
pub struct Mem {
    pub base: Var,
    pub offset: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Loc {
    Var(Var),
    Mem(Mem),
}
impl Loc {
    pub fn var(&self) -> Var {
        match self {
            Loc::Var(var) => *var,
            Loc::Mem(mem) => mem.base,
        }
    }
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
impl BinOpType {
    pub fn is_commutative(&self) -> bool {
        match self {
            BinOpType::Add | BinOpType::Mul | BinOpType::And | BinOpType::Or | BinOpType::Xor => {
                true
            }

            BinOpType::Sub | BinOpType::Div | BinOpType::Mod => false,
        }
    }
}

#[derive(Debug)]
pub enum UnOpType {
    Not,
    Neg,
}

#[derive(Debug)]
pub enum InPlaceUnOpType {
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
impl RelOpType {
    fn reversed_params(&self) -> Self {
        match self {
            RelOpType::Gt => RelOpType::Le,
            RelOpType::Ge => RelOpType::Lt,
            RelOpType::Lt => RelOpType::Ge,
            RelOpType::Le => RelOpType::Gt,
            RelOpType::Eq => RelOpType::Eq,
            RelOpType::NEq => RelOpType::NEq,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
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

    fn class(name: Ident) -> Self {
        Self::Ptr(PtrVarType::Class(name))
    }

    fn as_class(&self) -> Option<&Ident> {
        self.as_ptr().and_then(|ptr| ptr.as_class())
    }

    #[allow(unused)]
    fn into_class(self) -> Option<Ident> {
        self.into_ptr().ok().and_then(|res| res.into_class().ok())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringLiteral(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum SimpleVarType {
    Int,
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
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
            NonvoidType::TClass(name) => VarType::Ptr(PtrVarType::Class(name)),
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
    InPlaceUnOp(InPlaceUnOpType, Loc),

    Copy(Var, Var),
    Set(Var, Instant),
    GetStrLit(Var, StringLiteral),

    Call(Var, Ident, Vec<Value>),

    ArrLoad(Var, Var, Value),  // (dst, arr, idx)
    ArrStore(Var, Value, Var), // (arr, idx, src)

    DerefLoad(Var, Mem),    // (dst, ptr)
    DerefStore(Value, Mem), // (src, ptr)

    VstStore(ClassIdx, Mem),

    VirtualCall(Var, Var, usize, Vec<Value>), // (retvar, object, method_idx, args)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BasicBlockIdx(usize);

#[derive(Debug, Clone, Copy)]
pub enum CallingConvention {
    SimpleCdecl,
    CdeclFFI,
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
    pub self_var: Option<Var>, // Only for classes
}

#[derive(Debug)]
pub struct CFG {
    pub blocks: Vec<BasicBlock>,
    current_block_idx: BasicBlockIdx,
    current_func: Ident,
    current_class: Option<ClassIdx>,
    pub functions: HashMap<Ident, IrFunction>,
    pub classes: Vec<Class>,
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
    IfElse(Var, RelOpType, Value, BasicBlockIdx, BasicBlockIdx),
    Return(Option<Value>),
}

#[derive(Debug)]
pub enum BasicBlockKind {
    Other,
    Initial,
    AfterReturn,
    WhileCond,
    WhileBody,
    WhileNext,
    InfiniteLoop,
    LogOpSecondCheck,
    IfElseThen,
    IfElseElse,
    IfElseNext,
    IfThen,
    IfNext,
    MadeCondCtxThen,
    MadeCondCtxElse,
    MadeCondCtxNext,
}

#[derive(Debug)]
pub struct BasicBlock {
    _idx: BasicBlockIdx,
    _func: Ident,
    pub kind: BasicBlockKind,
    pub quadruples: Vec<Quadruple>,
    pub successors: Vec<BasicBlockIdx>,
    pub predecessors: Vec<BasicBlockIdx>,
    entry: bool,
    pub end_type: Option<EndType>,
    phi_nodes: VecMap<Var, VecMap<BasicBlockIdx, Var>>,
    flow_analysis: FlowAnalysis,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ClassIdx(pub usize);

#[derive(Debug, Clone)]
pub struct Field {
    offset: usize,
    _typ: VarType,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub mangled_name: Ident,
    pub idx: usize,
    pub params: VecMap<Ident, Var>,
    pub rettype: DataType,
}

#[derive(Debug)]
pub struct Class {
    pub name: Ident,
    _idx: ClassIdx,
    base_idx: Option<ClassIdx>,
    pub size: usize, // num elems, TODO: for virtual classes add VST size
    pub fields: VecMap<Ident, Field>,
    pub methods: VecMap<Ident, Method>, // src_name -> (mangled_name, method_idx, map<param_name -> param_var>)
    next_method_idx: usize,
}

impl Class {
    fn new(name: Ident, idx: ClassIdx, base_idx: Option<ClassIdx>) -> Self {
        Self {
            name,
            _idx: idx,
            base_idx,
            size: 1, /*VST size*/
            fields: VecMap::new(),
            methods: VecMap::new(),
            next_method_idx: 0,
        }
    }

    fn add_field(&mut self, name: Ident, typ: VarType) {
        let field_idx = self.fields.len() + /*VST ptr size*/1;
        self.fields.insert(
            name,
            Field {
                _typ: typ,
                offset: field_idx * QUADWORD_SIZE,
            },
        );
        self.size += 1;
    }

    fn resolve_field<'a>(&'a self, classes: &'a Vec<Class>, field_name: &Ident) -> &'a Field {
        if let Some(field) = self.fields.get(field_name) {
            return field;
        }
        let mut class = self;
        while let Some(base_idx) = class.base_idx {
            class = &classes[base_idx.0];
            if let Some(field) = class.fields.get(field_name) {
                return field;
            }
        }
        unreachable!();
    }

    fn add_method(
        &mut self,
        src_name: Ident,
        mangled_name: Ident,
        params: VecMap<Ident, Var>,
        rettype: DataType,
    ) {
        let method_idx =
            if let Some(method_idx) = self.methods.get(&src_name).map(|method| method.idx) {
                // override
                method_idx
            } else {
                // add new
                let method_idx = self.next_method_idx;
                self.next_method_idx += 1;
                method_idx
            };
        self.methods.insert(
            src_name,
            Method {
                mangled_name,
                idx: method_idx,
                params,
                rettype,
            },
        );
    }

    pub(crate) fn vst_name(&self) -> Ident {
        format!("{}___VST", &self.name)
    }
}
