use std::ops::Deref;

use super::{
    ast::*,
    env::{DoubleDeclarationError, Env, FunType, MissingDeclarationError},
};

use either::Either;
use enum_as_inner::EnumAsInner;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypeCheckError {
    #[error(
        "{}: Function {} was applied {} arguments, but expected {}.",
        pos,
        func,
        actual,
        expected
    )]
    WrongFuncArgNum {
        pos: Pos,
        func: Ident,
        expected: usize,
        actual: usize,
    },

    #[error("")] // TODO
    WrongMethodArgNum {
        pos: Pos,
        class: Ident,
        method: Ident,
        expected: usize,
        actual: usize,
    },

    #[error(
        "{}: Function {} was applied arguments of nonmatching types: expected {}, got {}.",
        pos,
        func,
        DisplayParams(expected),
        DisplayArgs(actual)
    )]
    WrongArgTypes {
        pos: Pos,
        func: Ident,
        expected: Vec<NonvoidType>,
        actual: Vec<DataType>,
    },

    #[error("")]
    NegWrongType(Expr, DataType),

    #[error("")]
    NotWrongType(Expr, DataType),

    #[error("")]
    MulWrongType(Expr, DataType),

    #[error("")]
    AddWrongTypes(Expr, DataType, DataType),

    #[error("")]
    RelWrongType(Expr, DataType),

    #[error("")]
    EqWrongTypes(Pos, Expr, DataType, BinOpType, Expr, DataType),

    #[error("")]
    NoMain,

    #[error("")]
    ArgsInMain(Pos, FunDef),

    #[error("")]
    RepeatedParams(FunDef), // currently served by DoubleDeclaration

    #[error("")]
    NoReturnInNonVoidFun(FunDef), // currently served by ReturnTypeMismatch? Nope

    #[error("")]
    ReturnTypeMismatch(FunDef, RetType, RetType), // unused?

    #[error("")]
    IncompatibleInitialization(Stmt, Expr, Ident, NonvoidType, DataType),

    #[error("")]
    IncompatibleAssignment(Stmt, Expr, LVal, NonvoidType, DataType),

    #[error("")]
    IncompatibleIncrementation(Stmt, LVal, NonvoidType),

    #[error("")]
    IncompatibleDecrementation(Stmt, LVal, NonvoidType),

    #[error("")]
    WrongConditionType(Stmt, Expr, DataType),

    // #[error("")]
    // PossibleNonReturn {
    //     surr: Stmt,
    //     if_br: Stmt,
    //     if_ret: StmtRetType,
    //     else_br: Stmt,
    //     else_ret: StmtRetType,
    // },
    #[error("")]
    IncompatibleBranchesType {
        surr: Stmt,
        stmt1: Stmt,
        stmt1_ret_type: StmtRetType,
        stmt2: Stmt,
        stmt2_ret_type: StmtRetType,
    },

    #[error("")]
    IncompatibleRetTypesInFunction(FunDef, DataType, Block, StmtRetType),

    #[error(transparent)] // FIXME
    MissingDeclaration(#[from] MissingDeclarationError),

    #[error("")]
    DoubleDeclaration(#[from] DoubleDeclarationError),

    #[error("")]
    BadForElemType(Stmt, NonvoidType, DataType),

    #[error("")]
    IntBinOpWrongType(Expr, DataType),

    #[error("")]
    LogOpWrongType(Expr, DataType),

    #[error("")]
    BadArrType(Expr, DataType), // TODO

    #[error("")]
    BadArrIndex(Expr, DataType), // TODO

    #[error("")]
    NonObjectFieldAccess(Expr, DataType), // TODO

    #[error("")]
    WrongMainRetType(Pos, DataType),

    #[error("")]
    InvalidReturnType(Pos, /* Ident,  */ DataType, DataType), // (allowed, attempted)
}

impl From<Either<DoubleDeclarationError, MissingDeclarationError>> for TypeCheckError {
    fn from(e: Either<DoubleDeclarationError, MissingDeclarationError>) -> Self {
        match e {
            Either::Left(de) => de.into(),
            Either::Right(me) => me.into(),
        }
    }
}

#[derive(Debug, EnumAsInner)]
enum Constexpr {
    Bool(bool),
    String(String),
    Int(Int),
    Null,
}

impl PartialEq for Constexpr {
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
            && match (self, other) {
                (Constexpr::Bool(_), Constexpr::Bool(_))
                | (Constexpr::String(_), Constexpr::String(_))
                | (Constexpr::Int(_), Constexpr::Int(_)) => true,
                _ => panic!("Can't compare Constexpr of different types!"),
            }
    }

    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            _ => panic!("Can't compare Constexpr of different types!"),
        }
    }
}

impl Program {
    pub fn type_check(&self) -> Result<(), TypeCheckError> {
        let mut initial_env = Env::new();

        /* printInt */
        initial_env.declare_function(
            "printInt".to_owned().into(),
            FunType {
                ret_type: DataType::TVoid,
                params: vec![NonvoidType::TInt],
            },
        )?;

        /* printString */
        initial_env.declare_function(
            "printString".to_owned().into(),
            FunType {
                ret_type: DataType::TVoid,
                params: vec![NonvoidType::TString],
            },
        )?;

        /* error */
        initial_env.declare_function(
            "error".to_owned().into(),
            FunType {
                ret_type: DataType::TVoid,
                params: vec![],
            },
        )?;

        /* readInt */
        initial_env.declare_function(
            "readInt".to_owned().into(),
            FunType {
                ret_type: DataType::Nonvoid(NonvoidType::TInt),
                params: vec![],
            },
        )?;

        /* readString */
        initial_env.declare_function(
            "readString".to_owned().into(),
            FunType {
                ret_type: DataType::Nonvoid(NonvoidType::TString),
                params: vec![],
            },
        )?;

        self.type_check_with_env(&mut initial_env)
    }

    fn type_check_with_env(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        // First run - declare
        for top_def in self.0.iter() {
            match top_def {
                TopDef::FunDef(fun_def) => fun_def.declare(env)?,
                TopDef::Class(id, base_id, class_block) => {
                    env.declare_class(id.clone(), base_id.clone())?;
                    todo!()
                }
            }
        }

        if env.get_function_type(&"main".to_owned().into()).is_err() {
            return Err(TypeCheckError::NoMain);
        }

        // Second run - type check
        for top_def in self.0.iter() {
            match top_def {
                TopDef::FunDef(fun_def) => fun_def.type_check(&mut env.new_scope())?,
                TopDef::Class(id, base_id, class_block) => {
                    env.declare_class(id.clone(), base_id.clone())?;
                    todo!()
                }
            }
        }

        Ok(())
    }
}

impl FunDef {
    fn declare(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        let fun_type = FunType {
            ret_type: self.ret_type.clone(),
            params: self.params.iter().map(|arg| arg.type_.clone()).collect(),
        };
        if self.name.deref() == "main" {
            if !self.params.is_empty() {
                return Err(TypeCheckError::ArgsInMain(self.pos, self.clone()));
            }
            if !matches!(self.ret_type, DataType::Nonvoid(NonvoidType::TInt)) {
                return Err(TypeCheckError::WrongMainRetType(
                    self.pos,
                    self.ret_type.clone(),
                ));
            }
        }
        env.declare_function(self.name.clone(), fun_type)?;
        Ok(())
    }
    fn type_check(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        // Add params to env
        for param in self.params.iter() {
            env.declare_variable(param.name.clone(), param.type_.clone())?;
        }

        let body_ret_type = self.block.type_check(env, &self.ret_type)?;
        match (&self.ret_type, &body_ret_type) {
            (&DataType::TVoid, &None) | (&DataType::TVoid, &Some((DataType::TVoid, _))) => Ok(()), // Void return fulfilled
            (&DataType::Nonvoid(ref expected), &Some((DataType::Nonvoid(ref actual), true))) if expected == actual => Ok(()), // Matching nonvoid return
            (&DataType::Nonvoid(_), &None) | // Expected Nonvoid return, got void
            (&DataType::Nonvoid(_), &Some((_, false))) | // Expected Nonvoid return, got possible void
            (&DataType::TVoid, _) | // Expected Void return, got possible different
            (_, _) // All other cases (incompatible types, etc.)
            => Err(TypeCheckError::IncompatibleRetTypesInFunction(
                self.clone(),
                self.ret_type.clone(),
                self.block.clone(),
                body_ret_type,
            )),
        }
    }
}

impl Block {
    fn type_check(
        &self,
        env: &mut Env,
        allowed_ret_type: &DataType,
    ) -> Result<StmtRetType, TypeCheckError> {
        // eprintln!("type checking block: {:#?}", self);
        let mut block_ret_type = None;
        for stmt in &self.1 {
            let stmt_ret_type = stmt.type_check(env, allowed_ret_type)?;
            match (stmt_ret_type, &block_ret_type) {
                (None, _) => (),
                (stmt_ret_type @ Some(_), None) => block_ret_type = stmt_ret_type,
                (Some((stmt_ret, stmt_certain)), Some((block_ret, block_certain))) => {
                    if stmt_ret == *block_ret {
                        block_ret_type = Some((stmt_ret, *block_certain || stmt_certain));
                    } else {
                        unreachable!(
                            "This is handled by restricting Returns to a certain type only"
                        )
                    }
                }
            }
        }
        Ok(block_ret_type)
    }
}

impl Stmt {
    fn type_check(
        &self,
        env: &mut Env,
        allowed_ret_type: &DataType,
    ) -> Result<StmtRetType, TypeCheckError> {
        // eprintln!("type checking stmt: {:#?}", self);
        let pos = self.0;
        match &self.1 {
            StmtInner::Empty => Ok(None),

            StmtInner::Block(block) => {
                let block_ret = block.type_check(&mut env.new_scope(), allowed_ret_type)?;
                Ok(block_ret)
            }

            StmtInner::VarDecl(decl) => {
                for single_decl in decl.decls.iter() {
                    if let Some(ref init_expr) = single_decl.init {
                        let (init_type, _) = init_expr.type_check(env)?;
                        if init_type != decl.type_ {
                            return Err(TypeCheckError::IncompatibleInitialization(
                                self.clone(),
                                init_expr.clone(),
                                single_decl.name.clone(),
                                decl.type_.clone(),
                                init_type,
                            ));
                        }
                    }
                    env.declare_variable(single_decl.name.clone(), decl.type_.clone())?;
                }
                Ok(None)
            }

            StmtInner::Ass(lval, expr) => {
                let (expr_type, _) = expr.type_check(env)?;
                let lval_type = lval.type_check(env)?;

                if expr_type != *lval_type {
                    return Err(TypeCheckError::IncompatibleAssignment(
                        self.clone(),
                        expr.clone(),
                        lval.clone(),
                        lval_type.clone(),
                        expr_type,
                    ));
                }
                Ok(None)
            }

            StmtInner::Incr(lval) => {
                let lval_type = lval.type_check(env)?;
                if !matches!(lval_type, NonvoidType::TInt) {
                    return Err(TypeCheckError::IncompatibleIncrementation(
                        self.clone(),
                        lval.clone(),
                        lval_type.clone(),
                    ));
                }
                Ok(None)
            }

            StmtInner::Decr(lval) => {
                let lval_type = lval.type_check(env)?;
                if !matches!(lval_type, NonvoidType::TInt) {
                    return Err(TypeCheckError::IncompatibleDecrementation(
                        self.clone(),
                        lval.clone(),
                        lval_type.clone(),
                    ));
                }
                Ok(None)
            }

            StmtInner::Return(ret) => {
                let (ret_type, _) = ret.type_check(env)?;
                if &ret_type == allowed_ret_type {
                    Ok(StmtRetType::Some((ret_type, true)))
                } else {
                    Err(TypeCheckError::InvalidReturnType(
                        pos,
                        allowed_ret_type.clone(),
                        ret_type,
                    ))
                }
            }

            StmtInner::VoidReturn => {
                if let &DataType::TVoid = allowed_ret_type {
                    Ok(StmtRetType::Some((DataType::TVoid, true)))
                } else {
                    Err(TypeCheckError::InvalidReturnType(
                        pos,
                        allowed_ret_type.clone(),
                        DataType::TVoid,
                    ))
                }
            }

            StmtInner::Cond(condition, body) | StmtInner::While(condition, body) => {
                let (cond_type, constval) = condition.type_check(env)?;
                match (&cond_type, constval) {
                    (DataType::Nonvoid(NonvoidType::TBoolean), None) => {
                        let then_stmt_ret = body.type_check(env, allowed_ret_type)?;
                        Ok(then_stmt_ret.map(|(ret, _)| (ret, false))) // ret certainty is lost
                    }
                    (DataType::Nonvoid(NonvoidType::TBoolean), Some(Constexpr::Bool(true))) => {
                        if matches!(self.1, StmtInner::While(_, _)) {
                            body.type_check(env, allowed_ret_type).map(|ret| {
                                ret.map(|(ret, _certain)| {
                                    (ret, true) // while(true) will certainly return, otherwise it would loop infinitely (no breaks in Latte)
                                })
                            })
                        } else {
                            body.type_check(env, allowed_ret_type)
                        }
                    }
                    (DataType::Nonvoid(NonvoidType::TBoolean), Some(Constexpr::Bool(false))) => {
                        Ok({
                            body.type_check(env, allowed_ret_type)?;
                            None
                        })
                    }
                    (DataType::Nonvoid(NonvoidType::TBoolean), _) => unreachable!(),
                    _ => Err(TypeCheckError::WrongConditionType(
                        self.clone(),
                        condition.clone(),
                        cond_type,
                    )),
                }
            }

            StmtInner::CondElse(condition, then_stmt, else_stmt) => {
                let (ret_type, constval) = condition.type_check(&mut env.new_scope())?;
                let then_stmt_ret = then_stmt.type_check(env, allowed_ret_type)?;
                let else_stmt_ret = else_stmt.type_check(env, allowed_ret_type)?;
                match (&ret_type, constval) {
                    (DataType::Nonvoid(NonvoidType::TBoolean), None) => {
                        match (then_stmt_ret, else_stmt_ret) {
                            (None, None) => Ok(None),
                            (None, Some(ret_type)) | (Some(ret_type), None) => {
                                Ok(Some((ret_type.0, false)))
                            }
                            (Some(then_ret_type), Some(else_ret_type)) => {
                                match (then_ret_type, else_ret_type) {
                                    ((typ1, certain1), (typ2, certain2)) if typ1 == typ2 => {
                                        Ok(Some((typ1, certain1 && certain2)))
                                    }
                                    (then_ret_type, else_ret_type) => {
                                        Err(TypeCheckError::IncompatibleBranchesType {
                                            surr: self.clone(),
                                            stmt1: then_stmt.deref().clone(),
                                            stmt1_ret_type: Some(then_ret_type),
                                            stmt2: else_stmt.deref().clone(),
                                            stmt2_ret_type: Some(else_ret_type),
                                        })
                                    }
                                }
                            }
                        }
                    }
                    (DataType::Nonvoid(NonvoidType::TBoolean), Some(Constexpr::Bool(true))) => {
                        Ok(then_stmt_ret)
                    }
                    (DataType::Nonvoid(NonvoidType::TBoolean), Some(Constexpr::Bool(false))) => {
                        Ok(else_stmt_ret)
                    }
                    (DataType::Nonvoid(NonvoidType::TBoolean), _) => unreachable!(),
                    _ => Err(TypeCheckError::WrongConditionType(
                        self.clone(),
                        condition.clone(),
                        ret_type,
                    )),
                }
            }

            StmtInner::SExp(e) => {
                e.type_check(env)?;
                Ok(None)
            }

            StmtInner::For(elem_type, elem_name, array_expr, body) => {
                let (iterable_type, _) = array_expr.type_check(env)?;
                // if let DataType::Nonvoid(NonvoidType::TInt) = iterable_type {
                //     if **x != *elem_type {
                //         return Err(TypeCheckError::BadForElemType(
                //             self.clone(),
                //             elem_type.clone(),
                //             iterable_type,
                //         ));
                //     }
                // }
                todo!();
                let body_ret = {
                    let mut body_env = env.new_scope();
                    body_env.declare_variable(elem_name.clone(), elem_type.clone())?;
                    body.type_check(&mut body_env, allowed_ret_type)?
                };
                Ok(body_ret.map(|(ret, _)| (ret, false))) // ret certainty is lost
            }
        }
    }
}

impl LVal {
    fn type_check<'env>(&self, env: &'env Env) -> Result<&'env NonvoidType, TypeCheckError> {
        // (type, init)
        match self {
            LVal::Id(id) => env.get_variable_type(id).map_err(|e| e.into()),
            LVal::LField(_, _) => todo!(),
            LVal::LArr(_, _) => todo!(),
        }
    }
}

impl Expr {
    fn type_check(&self, env: &Env) -> Result<(DataType, Option<Constexpr>), TypeCheckError> {
        // (type, consteval)
        // eprintln!("type checking expr: {:#?}", self);
        let pos = self.0;
        match &self.1 {
            ExprInner::IntLit(i) => Ok((
                DataType::Nonvoid(NonvoidType::TInt),
                Some(Constexpr::Int(*i)),
            )),

            ExprInner::BoolLit(b) => Ok((
                DataType::Nonvoid(NonvoidType::TBoolean),
                Some(Constexpr::Bool(*b)),
            )),

            ExprInner::StringLit(s) => Ok((
                DataType::Nonvoid(NonvoidType::TString),
                Some(Constexpr::String(s.clone())),
            )),

            ExprInner::Op(op) => match op {
                Op::UnOp(un_op_type, expr) => {
                    let (expr_type, constval) = expr.type_check(env)?;
                    match un_op_type {
                        UnOpType::Not => {
                            if matches!(expr_type, DataType::Nonvoid(NonvoidType::TBoolean)) {
                                Ok((
                                    DataType::Nonvoid(NonvoidType::TBoolean),
                                    constval.map(|constval| {
                                        if let Constexpr::Bool(b) = constval {
                                            Constexpr::Bool(!b)
                                        } else {
                                            unreachable!()
                                        }
                                    }),
                                ))
                            } else {
                                Err(TypeCheckError::NotWrongType(
                                    expr.deref().clone(),
                                    expr_type,
                                ))
                            }
                        }
                        UnOpType::Neg => {
                            if matches!(expr_type, DataType::Nonvoid(NonvoidType::TInt)) {
                                Ok((
                                    DataType::Nonvoid(NonvoidType::TInt),
                                    constval.map(|constval| {
                                        if let Constexpr::Int(i) = constval {
                                            Constexpr::Int(-i)
                                        } else {
                                            unreachable!()
                                        }
                                    }),
                                ))
                            } else {
                                Err(TypeCheckError::NegWrongType(
                                    expr.deref().clone(),
                                    expr_type,
                                ))
                            }
                        }
                    }
                }

                Op::BinOp(bin_op_type, expr1, expr2) => {
                    let (expr1_type, constval1) = expr1.type_check(env)?;
                    let (expr2_type, constval2) = expr2.type_check(env)?;

                    match bin_op_type {
                        BinOpType::IntOp(int_op) => {
                            if let (
                                &DataType::Nonvoid(NonvoidType::TInt),
                                &DataType::Nonvoid(NonvoidType::TInt),
                            ) = (&expr1_type, &expr2_type)
                            {
                                let ret = match int_op {
                                    IntOpType::IntRet(int_ret_op) => {
                                        let constval_ret = if let (
                                            Some(Constexpr::Int(i1)),
                                            Some(Constexpr::Int(i2)),
                                        ) = (constval1, constval2)
                                        {
                                            Some(Constexpr::Int(match int_ret_op {
                                                IntRetType::Mul => i1 * i2,
                                                IntRetType::Div => i1 / i2,
                                                IntRetType::Mod => i1 % i2,
                                                IntRetType::Sub => i1 - i2,
                                            }))
                                        } else {
                                            None
                                        };
                                        (DataType::Nonvoid(NonvoidType::TInt), constval_ret)
                                    }
                                    IntOpType::BoolRet(bool_ret_op) => {
                                        let constval_ret = if let (
                                            Some(Constexpr::Int(i1)),
                                            Some(Constexpr::Int(i2)),
                                        ) = (constval1, constval2)
                                        {
                                            Some(Constexpr::Bool(match bool_ret_op {
                                                BoolRetType::Gt => i1 > i2,
                                                BoolRetType::Ge => i1 >= i2,
                                                BoolRetType::Lt => i1 < i2,
                                                BoolRetType::Le => i1 <= i2,
                                            }))
                                        } else {
                                            None
                                        };
                                        (DataType::Nonvoid(NonvoidType::TBoolean), constval_ret)
                                    }
                                };
                                Ok(ret)
                            } else {
                                if !matches!(expr1_type, DataType::Nonvoid(NonvoidType::TInt)) {
                                    return Err(TypeCheckError::IntBinOpWrongType(
                                        expr1.deref().clone(),
                                        expr1_type,
                                    ));
                                } else {
                                    return Err(TypeCheckError::IntBinOpWrongType(
                                        expr2.deref().clone(),
                                        expr2_type,
                                    ));
                                }
                            }
                        }

                        op @ BinOpType::Eq | op @ BinOpType::NEq => {
                            match (&expr1_type, &expr2_type) {
                                (
                                    &DataType::Nonvoid(NonvoidType::TInt),
                                    &DataType::Nonvoid(NonvoidType::TInt),
                                )
                                | (
                                    &DataType::Nonvoid(NonvoidType::TString),
                                    &DataType::Nonvoid(NonvoidType::TString),
                                )
                                | (
                                    &DataType::Nonvoid(NonvoidType::TBoolean),
                                    &DataType::Nonvoid(NonvoidType::TBoolean),
                                ) => {
                                    let constval = if constval1.is_some() && constval2.is_some() {
                                        Some(Constexpr::Bool(if matches!(op, BinOpType::Eq) {
                                            constval1 == constval2
                                        } else {
                                            // BinOpType::NEq
                                            constval1 != constval2
                                        }))
                                    } else {
                                        None
                                    };
                                    Ok((DataType::Nonvoid(NonvoidType::TBoolean), constval))
                                }
                                _ => Err(TypeCheckError::EqWrongTypes(
                                    pos,
                                    expr1.deref().clone(),
                                    expr1_type,
                                    bin_op_type.clone(),
                                    expr2.deref().clone(),
                                    expr2_type,
                                )),
                            }
                        }
                        BinOpType::Add => match (&expr1_type, &expr2_type) {
                            (
                                &DataType::Nonvoid(NonvoidType::TInt),
                                &DataType::Nonvoid(NonvoidType::TInt),
                            ) => {
                                let constval = if let (Some(constval1), Some(constval2)) =
                                    (constval1, constval2)
                                {
                                    Some(Constexpr::Int(
                                        constval1.into_int().unwrap()
                                            + constval2.into_int().unwrap(),
                                    ))
                                } else {
                                    None
                                };
                                Ok((DataType::Nonvoid(NonvoidType::TInt), constval))
                            }
                            (
                                &DataType::Nonvoid(NonvoidType::TString),
                                DataType::Nonvoid(NonvoidType::TString),
                            ) => {
                                let constval = if let (Some(constval1), Some(constval2)) =
                                    (constval1, constval2)
                                {
                                    Some(Constexpr::String(
                                        constval1.into_string().unwrap()
                                            + &constval2.into_string().unwrap(),
                                    ))
                                } else {
                                    None
                                };
                                Ok((DataType::Nonvoid(NonvoidType::TString), constval))
                            }
                            _ => Err(TypeCheckError::AddWrongTypes(
                                self.clone(),
                                expr1_type,
                                expr2_type,
                            )),
                        },
                    }
                }

                Op::LogOp(log_op_type, expr1, expr2) => {
                    let (expr1_type, constval1) = expr1.type_check(env)?;
                    let (expr2_type, constval2) = expr2.type_check(env)?;

                    if let (
                        &DataType::Nonvoid(NonvoidType::TBoolean),
                        &DataType::Nonvoid(NonvoidType::TBoolean),
                    ) = (&expr1_type, &expr2_type)
                    {
                        let constval_ret =
                            if let (Some(Constexpr::Bool(b1)), Some(Constexpr::Bool(b2))) =
                                (constval1, constval2)
                            {
                                Some(Constexpr::Bool(match log_op_type {
                                    LogOpType::And => b1 && b2,
                                    LogOpType::Or => b1 || b2,
                                }))
                            } else {
                                None
                            };
                        Ok((DataType::Nonvoid(NonvoidType::TBoolean), constval_ret))
                    } else {
                        if !matches!(expr1_type, DataType::Nonvoid(NonvoidType::TBoolean)) {
                            return Err(TypeCheckError::LogOpWrongType(
                                expr1.deref().clone(),
                                expr1_type,
                            ));
                        } else {
                            return Err(TypeCheckError::LogOpWrongType(
                                expr2.deref().clone(),
                                expr2_type,
                            ));
                        }
                    }
                }
            },

            ExprInner::Id(id) => {
                let var_type = env.get_variable_type(id)?;
                Ok((var_type.clone().into(), None))
            }

            ExprInner::FunCall { name, args } => {
                let func = env.get_function_type(name)?.clone();
                if args.len() != func.params.len() {
                    return Err(TypeCheckError::WrongFuncArgNum {
                        pos,
                        func: name.clone(),
                        expected: func.params.len(),
                        actual: args.len(),
                    });
                }
                let arg_types = args
                    .iter()
                    .map(|arg| arg.type_check(env).map(|(t, _)| t))
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;

                if arg_types != func.params {
                    return Err(TypeCheckError::WrongArgTypes {
                        pos,
                        func: name.clone(),
                        expected: func.params.clone(),
                        actual: arg_types,
                    });
                }

                Ok((func.ret_type.clone(), None))
            }

            ExprInner::ArrSub(arr, idx) => {
                todo!();
                let (arr_type, _) = arr.type_check(env)?;
                // let elt_type = if let DataType::Nonvoid(NonvoidType::TIntArr) = arr_type {
                //     // *inner_type
                //     todo!()
                // } else {
                //     return Err(TypeCheckError::BadArrType(arr.deref().clone(), arr_type));
                // };

                // let (idx_type, _) = idx.type_check(env)?;
                // if !matches!(idx_type, DataType::Nonvoid(NonvoidType::TInt)) {
                //     return Err(TypeCheckError::BadArrIndex(idx.deref().clone(), idx_type));
                // }

                // Ok((elt_type.into(), None))
            }

            ExprInner::FieldAccess(object, field) => {
                todo!();
                let (object_type, _) = object.type_check(env)?;
                let class = if let DataType::Nonvoid(NonvoidType::Class(name)) = object_type {
                    name
                } else {
                    return Err(TypeCheckError::NonObjectFieldAccess(
                        self.clone(),
                        object_type,
                    ));
                };
                let field_type = env.get_field_type(class, field.clone())?;

                Ok((field_type.clone().into(), None))
            }

            ExprInner::MethodCall {
                object,
                method_name,
                args,
            } => {
                todo!();
                let (object_type, _) = object.type_check(env)?;
                let class = if let DataType::Nonvoid(NonvoidType::Class(name)) = object_type {
                    name
                } else {
                    return Err(TypeCheckError::NonObjectFieldAccess(
                        self.clone(),
                        object_type,
                    ));
                };
                let method = env.resolve_method(class, method_name.clone())?;

                if args.len() != method.params.len() {
                    return Err(TypeCheckError::WrongFuncArgNum {
                        pos,
                        func: method_name.clone(),
                        expected: method.params.len(),
                        actual: args.len(),
                    });
                }

                let arg_types = args
                    .iter()
                    .map(|arg| arg.type_check(env).map(|(t, _)| t))
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;

                if arg_types != method.params {
                    return Err(TypeCheckError::WrongArgTypes {
                        pos,
                        func: method_name.clone(),
                        expected: method.params.clone(),
                        actual: arg_types,
                    });
                }

                Ok((method.ret_type.clone(), None))
            }

            ExprInner::Null(data_type) => Ok((data_type.clone().into(), Some(Constexpr::Null))),

            ExprInner::New(new_type) => {
                let data_type = match new_type {
                    NewType::TInt => DataType::Nonvoid(NonvoidType::TInt),
                    NewType::TString => DataType::Nonvoid(NonvoidType::TString),
                    NewType::TBoolean => DataType::Nonvoid(NonvoidType::TBoolean),
                    NewType::Class(c) => DataType::Nonvoid(NonvoidType::Class(c.clone())),
                    NewType::TIntArr(_) => todo!(),
                    NewType::TStringArr(_) => todo!(),
                    NewType::TBooleanArr(_) => todo!(),
                    NewType::TClassArr(_, _) => todo!(),
                };
                Ok((data_type, None))
            }
        }
    }
}

/* TODO:
        circular inheritance
        associativity
*/
