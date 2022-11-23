use std::ops::Deref;

use super::{
    ast::*,
    env::{DoubleDeclarationError, Env, FunType, MissingDeclarationError},
};

use either::Either;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypeCheckError {
    // UndefinedVariable(Expr, Ident),
    // UndefinedFunction(Expr, Ident),
    #[error("")]
    WrongFuncArgNum {
        func: Ident,
        expected: usize,
        actual: usize,
    },

    #[error("")]
    WrongMethodArgNum {
        surr: Expr,
        class: Ident,
        method: Ident,
        expected: usize,
        actual: usize,
    },

    #[error("")]
    WrongArgTypes {
        func: Ident,
        expected: Vec<DataType>,
        actual: Vec<DataType>,
    },

    #[error("")]
    NegWrongType(Expr, DataType),

    #[error("")]
    NotWrongType(Expr, DataType),

    #[error("")]
    MulWrongType(Expr, DataType),

    #[error("")]
    AddWrongType(Expr, DataType),

    #[error("")]
    RelWrongType(Expr, DataType),

    #[error("")]
    EqWrongTypes(Expr, DataType, BinOpType, Expr, DataType),

    #[error("")]
    AndWrongType(Expr, DataType),

    #[error("")]
    OrWrongType(Expr, DataType),

    #[error("")]
    MultipleMain(FunDef),

    #[error("")]
    NoMain,

    #[error("")]
    ArgsInMain(FunDef),

    #[error("")]
    RepeatedParams(FunDef),

    #[error("")]
    VoidParams(FunDef),

    #[error("")]
    NoReturnInNonVoidFun(FunDef),

    #[error("")]
    ReturnTypeMismatch(FunDef, RetType, RetType),

    #[error("")]
    VoidVariables(Stmt),

    #[error("")]
    IncompatibleInitialization(Stmt, Expr, Ident, DataType, DataType),

    #[error("")]
    IncompatibleAssignment(Stmt, Expr, Ident, DataType, DataType),

    #[error("")]
    IncompatibleIncrementation(Stmt, Ident, DataType),

    #[error("")]
    IncompatibleDecrementation(Stmt, Ident, DataType),

    #[error("")]
    UndefinedVariableAssignment(Stmt, Ident),

    #[error("")]
    UndefinedVariableIncrementation(Stmt, Ident),

    #[error("")]
    UndefinedVariableDecrementation(Stmt, Ident),

    #[error("")]
    WrongConditionType(Stmt, Expr, DataType),

    #[error("")]
    PossibleNonReturn {
        surr: Stmt,
        if_br: Stmt,
        if_ret: StmtRetType,
        else_br: Stmt,
        else_ret: StmtRetType,
    },

    #[error("")]
    BreakOutsideLoop(Stmt),

    #[error("")]
    ContinueOutsideLoop(Stmt),

    #[error("")]
    IncompatibleBranchesType {
        surr: Stmt,
        stmt1: Stmt,
        stmt1_ret_type: StmtRetType,
        stmt2: Stmt,
        stmt2_ret_type: StmtRetType,
    },

    #[error("")]
    IncompatibleRetTypesInBlock(Stmt, StmtRetType, Block, StmtRetType), // FIXME

    #[error(transparent)] // FIXME
    MissingDeclaration(#[from] MissingDeclarationError),

    #[error("")]
    DoubleDeclaration(#[from] DoubleDeclarationError),

    #[error("")]
    PossiblyUninitVariableAccess(Ident),

    #[error("")]
    BadForElemType(DataType, DataType),

    #[error("")]
    IntBinOpWrongType(Expr, DataType),

    #[error("")]
    LogOpWrongType(Expr, DataType),

    #[error("")]
    BadArrType(DataType),

    #[error("")]
    BadArrIndex(DataType),

    #[error("")]
    NonObjectFieldAccess(DataType),
}

impl From<Either<DoubleDeclarationError, MissingDeclarationError>> for TypeCheckError {
    fn from(e: Either<DoubleDeclarationError, MissingDeclarationError>) -> Self {
        match e {
            Either::Left(de) => de.into(),
            Either::Right(me) => me.into(),
        }
    }
}

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
    fn type_check(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        // First run
        for top_def in self.0.iter() {
            match top_def {
                TopDef::FunDef(fun_def) => {
                    let fun_type = FunType {
                        ret_type: fun_def.ret_type.clone(),
                        params: fun_def.args.iter().map(|arg| arg.type_.clone()).collect(),
                    };
                    env.declare_function(fun_def.name.clone(), fun_type)?;
                }
                TopDef::Class(id, base_id, class_block) => {
                    env.declare_class(id.clone(), base_id.clone())?;
                }
            }
        }
        Ok(())
    }
}

impl Block {
    fn type_check(&self, env: &mut Env) -> Result<StmtRetType, TypeCheckError> {
        let mut block_ret_type = None;
        for stmt in &self.0 {
            let stmt_ret_type = stmt.type_check(env)?;
            match (stmt_ret_type, &block_ret_type) {
                (None, _) => (),
                (stmt_ret_type @ Some(_), None) => block_ret_type = stmt_ret_type,
                (Some((stmt_ret, stmt_certain)), Some((block_ret, block_certain))) => {
                    if stmt_ret == *block_ret {
                        block_ret_type = Some((stmt_ret, *block_certain || stmt_certain));
                    } else {
                        return Err(TypeCheckError::IncompatibleRetTypesInBlock(
                            stmt.clone(),
                            Some((stmt_ret, stmt_certain)),
                            self.clone(),
                            block_ret_type,
                        ));
                    }
                }
            }
        }
        Ok(block_ret_type)
    }
}

impl Stmt {
    fn type_check(&self, env: &mut Env) -> Result<StmtRetType, TypeCheckError> {
        match self {
            Stmt::Empty => Ok(None),

            Stmt::Block(block) => {
                let block_ret = block.type_check(&mut env.new_scope())?;
                Ok(block_ret)
            }

            Stmt::VarDecl(decl) => {
                if let Some(ref init_expr) = decl.init {
                    let (init_type, _) = init_expr.type_check(env)?;
                    if init_type != decl.type_ {
                        return Err(TypeCheckError::IncompatibleInitialization(
                            self.clone(),
                            init_expr.clone(),
                            decl.name.clone(),
                            decl.type_.clone(),
                            init_type,
                        ));
                    }
                }
                env.declare_variable(decl.name.clone(), decl.type_.clone(), decl.init.is_some())?;
                Ok(None)
            }

            Stmt::Ass(id, expr) => {
                let (expr_type, _) = expr.type_check(env)?;
                let (var_type, _) = env.get_variable_type(id)?;
                if expr_type == *var_type {
                    return Err(TypeCheckError::IncompatibleAssignment(
                        self.clone(),
                        expr.clone(),
                        id.clone(),
                        var_type.clone(),
                        expr_type,
                    ));
                }
                env.init_variable(id)?;
                Ok(None)
            }

            Stmt::Incr(id) => {
                let (var_type, init) = env.get_variable_type(id)?;
                if !matches!(var_type, &DataType::TInt) {
                    return Err(TypeCheckError::IncompatibleIncrementation(
                        self.clone(),
                        id.clone(),
                        var_type.clone(),
                    ));
                }
                if !init {
                    return Err(TypeCheckError::PossiblyUninitVariableAccess(id.clone()));
                }
                Ok(None)
            }

            Stmt::Decr(id) => {
                let (var_type, init) = env.get_variable_type(id)?;
                if !matches!(var_type, &DataType::TInt) {
                    return Err(TypeCheckError::IncompatibleDecrementation(
                        self.clone(),
                        id.clone(),
                        var_type.clone(),
                    ));
                }
                if !init {
                    return Err(TypeCheckError::PossiblyUninitVariableAccess(id.clone()));
                }
                Ok(None)
            }

            Stmt::Return(ret) => {
                let (ret_type, _) = ret.type_check(env)?;
                Ok(StmtRetType::Some((ret_type, true)))
            }

            Stmt::VoidReturn => Ok(StmtRetType::Some((DataType::TVoid, true))),

            Stmt::Cond(condition, body) | Stmt::While(condition, body) => {
                let (cond_type, constval) = condition.type_check(env)?;
                match (&cond_type, constval) {
                    (DataType::TBoolean, None) => {
                        let then_stmt_ret = body.type_check(env)?;
                        Ok(then_stmt_ret.map(|(ret, _)| (ret, false))) // ret certainty is lost
                    }
                    (DataType::TBoolean, Some(Constexpr::Bool(true))) => body.type_check(env),
                    (DataType::TBoolean, Some(Constexpr::Bool(false))) => Ok({
                        body.type_check(env)?;
                        None
                    }),
                    (DataType::TBoolean, _) => unreachable!(),
                    _ => Err(TypeCheckError::WrongConditionType(
                        self.clone(),
                        condition.clone(),
                        cond_type,
                    )),
                }
            }

            Stmt::CondElse(condition, then_stmt, else_stmt) => {
                let (ret_type, constval) = condition.type_check(&mut env.new_scope())?;
                let then_stmt_ret = then_stmt.type_check(env)?;
                let else_stmt_ret = else_stmt.type_check(env)?;
                match (&ret_type, constval) {
                    (DataType::TBoolean, None) => {
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
                    (DataType::TBoolean, Some(Constexpr::Bool(true))) => Ok(then_stmt_ret),
                    (DataType::TBoolean, Some(Constexpr::Bool(false))) => Ok(else_stmt_ret),
                    (DataType::TBoolean, _) => unreachable!(),
                    _ => Err(TypeCheckError::WrongConditionType(
                        self.clone(),
                        condition.clone(),
                        ret_type,
                    )),
                }
            }

            Stmt::SExp(e) => {
                e.type_check(env)?;
                Ok(None)
            }

            Stmt::For(elem_type, elem_name, array_expr, body) => {
                let (iterable_type, _) = array_expr.type_check(env)?;
                if let DataType::TArr(ref x) = iterable_type {
                    if **x != *elem_type {
                        return Err(TypeCheckError::BadForElemType(
                            elem_type.clone(),
                            iterable_type,
                        ));
                    }
                }
                let body_ret = body.type_check(&mut env.new_scope())?;
                Ok(body_ret.map(|(ret, _)| (ret, false))) // ret certainty is lost
            }
        }
    }
}

impl Expr {
    fn type_check(&self, env: &Env) -> Result<(DataType, Option<Constexpr>), TypeCheckError> {
        // (type, consteval)
        match self {
            Expr::IntLit(i) => Ok((DataType::TInt, Some(Constexpr::Int(*i)))),

            Expr::BoolLit(b) => Ok((DataType::TInt, Some(Constexpr::Bool(*b)))),

            Expr::StringLit(s) => Ok((DataType::TInt, Some(Constexpr::String(s.clone())))),

            Expr::Op(op) => match op {
                Op::UnOp(un_op_type, expr) => {
                    let (expr_type, constval) = expr.type_check(env)?;
                    match un_op_type {
                        UnOpType::Not => {
                            if matches!(expr_type, DataType::TBoolean) {
                                Ok((
                                    DataType::TBoolean,
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
                            if matches!(expr_type, DataType::TInt) {
                                Ok((
                                    DataType::TInt,
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
                            if let (&DataType::TInt, &DataType::TInt) = (&expr1_type, &expr2_type) {
                                let constval_ret = match int_op {
                                    IntOpType::IntRet(int_ret_op) => {
                                        if let (
                                            Some(Constexpr::Int(i1)),
                                            Some(Constexpr::Int(i2)),
                                        ) = (constval1, constval2)
                                        {
                                            Some(Constexpr::Int(match int_ret_op {
                                                IntRetType::Mul => i1 * i2,
                                                IntRetType::Div => i1 / i2,
                                                IntRetType::Mod => i1 % i2,
                                                IntRetType::Add => i1 + i2,
                                                IntRetType::Sub => i1 - i2,
                                            }))
                                        } else {
                                            None
                                        }
                                    }
                                    IntOpType::BoolRet(bool_ret_op) => {
                                        if let (
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
                                        }
                                    }
                                };
                                Ok((DataType::TInt, constval_ret))
                            } else {
                                if !matches!(expr1_type, DataType::TInt) {
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

                        BinOpType::Eq | BinOpType::NEq => match (&expr1_type, &expr2_type) {
                            (t @ &DataType::TInt, &DataType::TInt)
                            | (t @ &DataType::TString, &DataType::TString)
                            | (t @ &DataType::TBoolean, &DataType::TBoolean) => todo!(),
                            _ => Err(TypeCheckError::EqWrongTypes(
                                expr1.deref().clone(),
                                expr1_type,
                                bin_op_type.clone(),
                                expr2.deref().clone(),
                                expr2_type,
                            )),
                        },
                    }
                }

                Op::LogOp(log_op_type, expr1, expr2) => {
                    let (expr1_type, constval1) = expr1.type_check(env)?;
                    let (expr2_type, constval2) = expr2.type_check(env)?;

                    if let (&DataType::TBoolean, &DataType::TBoolean) = (&expr1_type, &expr2_type) {
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
                        Ok((DataType::TBoolean, constval_ret))
                    } else {
                        if !matches!(expr1_type, DataType::TBoolean) {
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

            Expr::Id(id) => {
                let (var_type, initialised) = env.get_variable_type(id)?;
                if !initialised {
                    Err(TypeCheckError::PossiblyUninitVariableAccess(id.clone()))
                } else {
                    Ok((var_type.clone(), None))
                }
            }

            Expr::FunCall { name, args } => {
                let func = env.get_function_type(name)?;
                if args.len() != func.params.len() {
                    return Err(TypeCheckError::WrongFuncArgNum {
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
                        func: name.clone(),
                        expected: func.params.clone(),
                        actual: arg_types,
                    });
                }

                Ok((func.ret_type.clone(), None))
            }

            Expr::ArrSub(arr, idx) => {
                let (arr_type, _) = arr.type_check(env)?;
                let elt_type = if let DataType::TArr(inner_type) = arr_type {
                    *inner_type
                } else {
                    return Err(TypeCheckError::BadArrType(arr_type));
                };

                let (idx_type, _) = idx.type_check(env)?;
                if !matches!(idx_type, DataType::TInt) {
                    return Err(TypeCheckError::BadArrIndex(idx_type));
                }

                Ok((elt_type, None))
            }

            Expr::FieldAccess(object, field) => {
                let (object_type, _) = object.type_check(env)?;
                let class = if let DataType::Class(name) = object_type {
                    name
                } else {
                    return Err(TypeCheckError::NonObjectFieldAccess(object_type));
                };
                let field_type = env.get_field_type(class, field.clone())?;

                Ok((field_type.clone(), None))
            }

            Expr::MethodCall {
                object,
                method_name,
                args,
            } => {
                let (object_type, _) = object.type_check(env)?;
                let class = if let DataType::Class(name) = object_type {
                    name
                } else {
                    return Err(TypeCheckError::NonObjectFieldAccess(object_type));
                };
                let method = env.resolve_method(class, method_name.clone())?;

                if args.len() != method.params.len() {
                    return Err(TypeCheckError::WrongFuncArgNum {
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
                        func: method_name.clone(),
                        expected: method.params.clone(),
                        actual: arg_types,
                    });
                }

                Ok((method.ret_type.clone(), None))
            }

            Expr::Null(data_type) => Ok((data_type.clone(), Some(Constexpr::Null))),

            Expr::New(new_type) => {
                let data_type = match new_type {
                    NewType::TInt => DataType::TInt,
                    NewType::TString => DataType::TString,
                    NewType::TBoolean => DataType::TBoolean,
                    NewType::Class(c) => DataType::Class(c.clone()),
                    NewType::TArr(inner_type, _len) => DataType::TArr(inner_type.clone()),
                };
                Ok((data_type, None))
            }
        }
    }
}

/* TODO: usage of a variable initialised in both if branches...
        circular inheritance
*/
