use std::collections::HashMap;

struct Program(Vec<TopDef>);

type Int = i64;

enum TopDef {
    FunDef(FunDef),
    BaseClass(FunDef),
    DerivedClass(FunDef),
}

struct FunDef {
    type_: Type_,
    name: Ident,
    args: Vec<Arg>,
    block: Block,
}

struct Arg {
    type_: Type_,
    name: Ident,
}

struct ClassBlock(Vec<ClassItem>);

enum ClassItem {
    Decl(Decl),
    FunDef(FunDef),
}

struct VarDecl {
    type_: Type_,
    item: VarItem,
}

struct Block(Vec<Stmt>);

enum Stmt {
    Empty,
    Block(Block),
    VarDecl(VarDecl),
    Ass(Ident, Expr),
    Incr(Ident),
    Decr(Ident),
    Return(Expr),
    VoidReturn,
    Cond(Expr, Stmt),
    CondElse(Expr, Stmt, Stmt),
    While(Expr, Stmt),
    SExp(Expr),
}

enum DataType {
    TInt,
    TString,
    TBoolean,
    TVoid,
    TArr(Box<DataType>),
    Class(Ident)
}

type RetType = DataType;

enum VarItem {
    Uninit(Ident),
    Init(Ident, Expr),
}

enum UnOpType {
    Neg,
    Not,
}

enum BinOpType {
    IntOp(IntOpType),
    Eq,
    NEq,
}

enum IntOpType {
    IntRet(IntRetType),
    BoolRet(BoolRetType),
}

enum IntRetType {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
}

enum BoolRetType {
    Gt,
    Ge,
    Lt,
    Le,
}

enum Op {
    UnOp(UnOpType, Box<Expr>),
    BinOp(BinOpType, Box<Expr>, Box<Expr>),
    LogOp(LogOpType, Box<Expr>, Box<Expr>),
}

enum LogOpType {
    And,
    Or,
}

enum Expr {
    Op(Op),
    Id(Ident),
    IntLit(Int),
    BoolLit(bool),
    StringLit(String),
    FunCall {
        name: Ident,
        args: Vec<Box<Expr>>,
    },
    ArrSub(Expr, Expr),
    FieldAccess(Ident, Ident),
    MethodCall {
        var_name: Ident,
        method_name: Ident,
        args: Vec<Box<Expr>>,
    },
    Null(Type_),
}

struct Ident(String);

pub enum TypeCheckError {
    UndefinedVariable(Expr, Ident),
    UndefinedFunction(Expr, Ident),
    WrongArgNum(Expr, Ident, Int, Int),
    WrongArgTypes(Expr, Ident, Vec<DataType>, Vec<DataType>),
    NegWrongType(Expr),
    NotWrongType(Expr),
    MulWrongType(Expr),
    AddWrongType(Expr),
    RelWrongType(Expr),
    AndWrongType(Expr),
    OrWrongType(Expr),
    MultipleMain(FunDef),
    NoMain,
    ArgsInMain(FunDef),
    RepeatedParams(FunDef),
    VoidParams(FunDef),
    NoReturnInNonVoidFun(FunDef),
    ReturnTypeMismatch(FunDef, RetType, RetType),
    VoidVariables(Stmt),
    IncompatibleInitialization(Stmt, Expr, Ident, DataType, DataType),
    IncompatibleAssignment(Stmt, Expr, Ident, DataType, DataType),
    IncompatibleIncrementation(Stmt, Ident, DataType),
    IncompatibleDecrementation(Stmt, Ident, DataType),
    UndefinedVariableAssignment(Stmt, Ident),
    UndefinedVariableIncrementation(Stmt, Ident),
    UndefinedVariableDecrementation(Stmt, Ident),
    WrongConditionType(Stmt, Expr, DataType),
    PossibleNonReturn(Stmt, Stmt, StmtRetType, Stmt, StmtRetType),
    BreakOutsideLoop(Stmt),
    ContinueOutsideLoop(Stmt),
    IncompatibleBranchesType(Stmt, Stmt, StmtRetType, Stmt, StmtRetType),
    IncompatibleRetTypesInBlock(Stmt, StmtRetType, [Stmt], StmtRetType),
}

#[derive(PartialEq, Eq, Debug)]

pub struct ExpRich<R: Sized>(pub ExpRichNode<R>, pub R);

#[derive(PartialEq, Eq, Debug)]
pub enum ExpRichNode<R: Sized> {
    Lit(i32),
    VarRef(VarName),
    Mul(Box<ExpRich<R>>, Box<ExpRich<R>>),
    Sub(Box<ExpRich<R>>, Box<ExpRich<R>>),
    Add(Box<ExpRich<R>>, Box<ExpRich<R>>),
    Div(Box<ExpRich<R>>, Box<ExpRich<R>>),
}

impl<R: Sized> ExpRich<R> {
    pub fn enrich<RProd, F>(combine: &F, base: &RProd, exp: Exp) -> Self
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
