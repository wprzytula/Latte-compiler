use antlr_rust::token::GenericToken;
use enum_as_inner::EnumAsInner;
use smallvec::SmallVec;
use std::{
    borrow::Cow,
    cell::{Ref, RefCell},
    fmt::{self, Display, Write},
    hash::{self, Hash},
    ops::Deref,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos {
    pub line: isize,
    pub column: isize,
}
impl From<Ref<'_, GenericToken<Cow<'_, str>>>> for Pos {
    fn from(token_start: Ref<GenericToken<Cow<str>>>) -> Self {
        Self {
            line: token_start.line,
            column: token_start.column,
        }
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "in line {}:{}", self.line, self.column)
    }
}

#[derive(Debug)]
pub struct Program(pub Vec<FunDef>, pub Vec<ClassDef>);

pub type Int = i64;

#[derive(Debug)]
pub enum TopDef {
    FunDef(FunDef),
    ClassDef(ClassDef),
}

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub pos: Pos,
    pub class: Ident,
    pub base_class: Option<Ident>,
    pub class_block: ClassBlock,
}

impl PartialEq for ClassDef {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.class == other.class
    }
}

impl Eq for ClassDef {}

impl Hash for ClassDef {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.pos.hash(state);
        self.class.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct FunDef {
    pub pos: Pos,
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

#[derive(Debug, Clone)]
pub struct ClassBlock(pub Vec<ClassItem>);

#[derive(Debug, Clone)]
pub enum ClassItem {
    Field(Pos, NonvoidType, Ident),
    Method(FunDef),
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
pub struct Block(pub Pos, pub Vec<Stmt>);

#[derive(Debug, Clone)]
pub struct Stmt(pub Pos, pub StmtInner);
impl Deref for Stmt {
    type Target = StmtInner;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}
impl Stmt {
    pub fn pos(&self) -> Pos {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum StmtInner {
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
pub struct LVal(pub Pos, pub LValInner, pub RefCell<Option<DataType>>);
impl Deref for LVal {
    type Target = LValInner;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}
impl LVal {
    pub fn pos(&self) -> Pos {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum LValInner {
    Id(Ident),
    FieldAccess(Box<LVal>, Ident),
    ArrSub(Box<LVal>, Expr),
    FunCall {
        name: Ident,
        args: Vec<Box<Expr>>,
    },
    MethodCall {
        object: Box<LVal>,
        method_name: Ident,
        args: Vec<Box<Expr>>,
    },
    New(NewType),
}

#[derive(Clone, Debug)]
pub enum NonvoidType {
    TInt,
    TString,
    TBoolean,
    TClass(Ident),
    TIntArr,
    TStringArr,
    TBooleanArr,
    TClassArr(Ident),
}
impl Display for NonvoidType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NonvoidType::TInt => write!(f, "int"),
            NonvoidType::TString => write!(f, "string"),
            NonvoidType::TBoolean => write!(f, "boolean"),
            NonvoidType::TClass(id) => write!(f, "{}", id),
            NonvoidType::TIntArr => write!(f, "int[]"),
            NonvoidType::TStringArr => write!(f, "string[]"),
            NonvoidType::TBooleanArr => write!(f, "boolean[]"),
            NonvoidType::TClassArr(id) => write!(f, "{}[]", id),
        }
    }
}
impl NonvoidType {
    pub fn is_passed_by_ref(&self) -> bool {
        match self {
            NonvoidType::TInt | NonvoidType::TString | NonvoidType::TBoolean => false,
            NonvoidType::TClass(_)
            | NonvoidType::TIntArr
            | NonvoidType::TStringArr
            | NonvoidType::TBooleanArr
            | NonvoidType::TClassArr(_) => true,
        }
    }
    pub fn array_member_type(&self) -> Option<NonvoidType> {
        match self {
            NonvoidType::TIntArr => Some(Self::TInt),
            NonvoidType::TStringArr => Some(Self::TString),
            NonvoidType::TBooleanArr => Some(Self::TBoolean),
            NonvoidType::TClassArr(c) => Some(Self::TClass(c.clone())),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, EnumAsInner)]
pub enum DataType {
    TVoid,
    TExit,
    Nonvoid(NonvoidType),
}
impl Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataType::TVoid => f.write_str("void"),
            DataType::TExit => f.write_str("!"),
            DataType::Nonvoid(n) => n.fmt(f),
        }
    }
}
impl DataType {
    pub fn is_passed_by_ref(&self) -> bool {
        match self {
            DataType::TVoid | DataType::TExit => false,
            DataType::Nonvoid(n) => n.is_passed_by_ref(),
        }
    }
    pub fn array_member_type(&self) -> Option<NonvoidType> {
        match self {
            DataType::TVoid | DataType::TExit => None,
            DataType::Nonvoid(n) => n.array_member_type(),
        }
    }
}

impl From<NonvoidType> for DataType {
    fn from(nonvoid: NonvoidType) -> Self {
        Self::Nonvoid(nonvoid)
    }
}

pub struct DisplayArgs<'a>(pub &'a Vec<DataType>);
impl<'a> Display for DisplayArgs<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;
        let mut first = true;
        for arg in self.0 {
            if !first {
                f.write_str(", ")?;
            }
            arg.fmt(f)?;
            first = false;
        }
        f.write_char(')')
    }
}

pub struct DisplayParams<'a>(pub &'a Vec<NonvoidType>);
impl<'a> Display for DisplayParams<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;
        let mut first = true;
        for arg in self.0 {
            if !first {
                f.write_str(", ")?;
            }
            arg.fmt(f)?;
            first = false;
        }
        f.write_char(')')
    }
}

#[derive(Debug, Clone)]
pub enum NewType {
    TClass(Ident),
    TIntArr(Box<Expr>),
    TStringArr(Box<Expr>),
    TBooleanArr(Box<Expr>),
    TClassArr(Ident, Box<Expr>),
}
impl Display for NewType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NewType::TClass(id) => write!(f, "{}", id),
            NewType::TIntArr(len) => write!(f, "int[{:#?}]", len),
            NewType::TStringArr(len) => write!(f, "string[{:#?}]", len),
            NewType::TBooleanArr(len) => write!(f, "boolean[{:#?}]", len),
            NewType::TClassArr(id, len) => write!(f, "{}[{:#?}]", id, len),
        }
    }
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
    Add,
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
pub struct Expr(pub Pos, pub ExprInner, pub RefCell<Option<DataType>>);
impl Deref for Expr {
    type Target = ExprInner;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}
impl Expr {
    pub fn pos(&self) -> Pos {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum ExprInner {
    Op(Op),
    Id(Ident),
    IntLit(Int),
    BoolLit(bool),
    StringLit(String),
    Null(NonvoidType),
    LVal(Box<LVal>),
}

pub type Ident = String;

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
