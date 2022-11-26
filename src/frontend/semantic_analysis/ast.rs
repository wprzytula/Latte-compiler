use smallvec::SmallVec;
use std::{
    fmt::{self, Display},
    rc::Rc,
};

#[derive(Debug)]
pub struct Program(pub Vec<TopDef>);

pub type Int = i64;

#[derive(Debug)]
pub enum TopDef {
    FunDef(FunDef),
    Class(Ident, Option<Ident>, ClassBlock), // (class, base_class, block)
}

#[derive(Debug)]
pub struct FunDef {
    pub ret_type: DataType,
    pub name: Ident,
    pub params: Vec<Param>,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub type_: NonvoidType,
    pub name: Ident,
}

#[derive(Debug)]
pub struct ClassBlock(pub Vec<ClassItem>);

#[derive(Debug)]
pub enum ClassItem {
    Decl(DataDecl),
    FunDef(FunDef),
}

#[derive(Debug, Clone)]
pub struct SingleDecl {
    pub name: Ident,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct DataDecl {
    pub type_: NonvoidType,
    pub decls: SmallVec<[SingleDecl; 3]>,
}

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Block(Block),
    VarDecl(DataDecl),
    Ass(LVal, Expr),
    Incr(LVal),
    Decr(LVal),
    Return(Expr),
    VoidReturn,
    Cond(Expr, Box<Stmt>),
    CondElse(Expr, Box<Stmt>, Box<Stmt>),
    While(Expr, Box<Stmt>),
    SExp(Expr),
    For(NonvoidType, Ident, Expr, Box<Stmt>), // (elem_type, elem_name, array_expr, body)
}

#[derive(Debug, Clone)]
pub enum LVal {
    Id(Ident),
    LField(Box<LVal>, Ident),
    LArr(Box<LVal>, Expr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NonvoidType {
    TInt,
    TString,
    TBoolean,
    TArr(Box<NonvoidType>),
    Class(Ident),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataType {
    TVoid,
    Nonvoid(NonvoidType),
}

impl From<NonvoidType> for DataType {
    fn from(nonvoid: NonvoidType) -> Self {
        Self::Nonvoid(nonvoid)
    }
}

impl PartialEq<DataType> for NonvoidType {
    fn eq(&self, other: &DataType) -> bool {
        if let DataType::Nonvoid(nonvoid) = other {
            nonvoid == other
        } else {
            false
        }
    }
}

impl PartialEq<NonvoidType> for DataType {
    fn eq(&self, other: &NonvoidType) -> bool {
        other.eq(self)
    }
}

#[derive(Debug, Clone)]
pub enum NewType {
    TInt,
    TString,
    TBoolean,
    TArr(Box<NonvoidType>, Int),
    Class(Ident),
}

pub type RetType = DataType;

#[derive(Debug, Clone)]
pub enum UnOpType {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinOpType {
    IntOp(IntOpType),
    Eq,
    NEq,
}

#[derive(Debug, Clone)]
pub enum IntOpType {
    IntRet(IntRetType),
    BoolRet(BoolRetType),
}

#[derive(Debug, Clone)]
pub enum IntRetType {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
}

#[derive(Debug, Clone)]
pub enum BoolRetType {
    Gt,
    Ge,
    Lt,
    Le,
}

#[derive(Debug, Clone)]
pub enum Op {
    UnOp(UnOpType, Box<Expr>),
    BinOp(BinOpType, Box<Expr>, Box<Expr>),
    LogOp(LogOpType, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum LogOpType {
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Op(Op),
    Id(Ident),
    IntLit(Int),
    BoolLit(bool),
    StringLit(String),
    FunCall {
        name: Ident,
        args: Vec<Box<Expr>>,
    },
    ArrSub(Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, Ident),
    MethodCall {
        object: Box<Expr>,
        method_name: Ident,
        args: Vec<Box<Expr>>,
    },
    Null(NonvoidType),
    New(NewType),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident(Rc<String>);
impl From<String> for Ident {
    fn from(s: String) -> Self {
        Self(Rc::new(s))
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// semantics: the bool signifies _certainty_ of the return
// (i.e., the execution that can never reach behind that statement)
pub type StmtRetType = Option<(RetType, bool)>;

/* #[derive(PartialEq, Eq, Debug)]
pub struct ExpRich<R: Sized>(pub ExpRichNode<R>, pub R);

#[derive(PartialEq, Eq, Debug)]
pub enum ExpRichNode<R: Sized> {
    Lit(i32),
    VarRef(Ident),
    Mul(Box<ExpRich<R>>, Box<ExpRich<R>>),
    Sub(Box<ExpRich<R>>, Box<ExpRich<R>>),
    Add(Box<ExpRich<R>>, Box<ExpRich<R>>),
    Div(Box<ExpRich<R>>, Box<ExpRich<R>>),
}

impl<R: Sized> ExpRich<R> {
    pub fn enrich<RProd, F>(combine: &F, base: &RProd, exp: Expr) -> Self
    where
        F: Fn(&R, &R) -> R,
        RProd: Fn() -> R,
    {
        match exp {
            Exp::Lit(i) => ExpRich(ExpRichNode::Lit(i), base()),
            Exp::VarRef(v) => ExpRich(ExpRichNode::VarRef(v), base()),
            Exp::Mul(l, r) => {
                let l = Self::enrich(combine, base, *l);
                let r = Self::enrich(combine, base, *r);
                let rich = combine(&l.1, &r.1);
                ExpRich(ExpRichNode::Mul(Box::new(l), Box::new(r)), rich)
            }
            Exp::Sub(l, r) => {
                let l = Self::enrich(combine, base, *l);
                let r = Self::enrich(combine, base, *r);
                let rich = combine(&l.1, &r.1);
                ExpRich(ExpRichNode::Sub(Box::new(l), Box::new(r)), rich)
            }
            Exp::Add(l, r) => {
                let l = Self::enrich(combine, base, *l);
                let r = Self::enrich(combine, base, *r);
                let rich = combine(&l.1, &r.1);
                ExpRich(ExpRichNode::Add(Box::new(l), Box::new(r)), rich)
            }
            Exp::Div(l, r) => {
                let l = Self::enrich(combine, base, *l);
                let r = Self::enrich(combine, base, *r);
                let rich = combine(&l.1, &r.1);
                ExpRich(ExpRichNode::Div(Box::new(l), Box::new(r)), rich)
            }
        }
    }
}

fn iter_exp<Acc, F>(f: &F, acc: &mut Acc, exp: &Exp)
where
    F: Fn(&mut Acc, &Exp),
{
    f(acc, exp);
    match exp {
        Exp::Lit(_) | Exp::VarRef(_) => (),
        Exp::Mul(a, b) | Exp::Sub(a, b) | Exp::Add(a, b) | Exp::Div(a, b) => {
            iter_exp(f, acc, a);
            iter_exp(f, acc, b);
        }
    }
}

pub trait VarIdx: Copy {
    fn next_free_idx(&mut self) -> Self;
    fn default() -> Self;
}

#[derive(Debug)]
pub struct SymbolTable<VarId: VarIdx> {
    next_free_idx: VarId,
    pub variable_mapping: HashMap<VarName, (VarId, usize)>,
}

impl<VarId: VarIdx> SymbolTable<VarId> {
    fn register_variable(&mut self, var_name: &VarName, line: usize) {
        if !self.variable_mapping.contains_key(var_name) {
            self.variable_mapping
                .insert(var_name.clone(), (self.next_free_idx.next_free_idx(), line));
        }
    }

    fn register_var_ref(&mut self, exp: &Exp, line: usize) {
        if let Exp::VarRef(var_name) = exp {
            self.register_variable(var_name, line);
        }
    }

    pub fn retrieve_var_id(&self, var_name: &VarName) -> Option<(VarId, usize)> {
        self.variable_mapping.get(var_name).copied()
    }

    pub fn build(ast: &AST<Exp>) -> Self {
        let mut new = SymbolTable {
            next_free_idx: VarId::default(),
            variable_mapping: HashMap::new(),
        };
        for (line, stmt) in ast.0.iter().enumerate() {
            match stmt {
                Stmt::Print(exp) => {
                    iter_exp(
                        &|new, exp| Self::register_var_ref(new, exp, line),
                        &mut new,
                        exp,
                    );
                }
                Stmt::Ass(var_name, exp) => {
                    new.register_variable(var_name, line);
                    iter_exp(
                        &|new, exp| Self::register_var_ref(new, exp, line),
                        &mut new,
                        exp,
                    );
                }
            }
        }

        new
    }

    pub fn next_free_idx(&self) -> VarId {
        self.next_free_idx
    }
}
 */
