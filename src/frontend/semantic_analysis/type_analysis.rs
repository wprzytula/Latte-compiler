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
    IncompatibleInitialization(Stmt, Expr, Ident, NonvoidType, DataType),

    #[error("")]
    IncompatibleAssignment(Stmt, Expr, LVal, NonvoidType, DataType),

    #[error("")]
    IncompatibleIncrementation(Stmt, LVal, NonvoidType),

    #[error("")]
    IncompatibleDecrementation(Stmt, LVal, NonvoidType),

    #[error("")]
    UndefinedVariableAssignment(Stmt, LVal),

    #[error("")]
    UndefinedVariableIncrementation(Stmt, LVal),

    #[error("")]
    UndefinedVariableDecrementation(Stmt, LVal),

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
    IncompatibleRetTypesInBlock(Stmt, StmtRetType, Block, StmtRetType), // FIXME: particular Stmt is against generalised Block (no explicit offensor)

    #[error("")]
    IncompatibleRetTypesInFunction(FunDef, DataType, Block, StmtRetType),

    #[error(transparent)] // FIXME
    MissingDeclaration(#[from] MissingDeclarationError),

    #[error("")]
    DoubleDeclaration(#[from] DoubleDeclarationError),

    #[error("")]
    PossiblyUninitAccess(LVal),

    #[error("")]
    PossiblyUninitVariableAccess(Ident),

    #[error("")]
    BadForElemType(NonvoidType, DataType),

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
    pub fn type_check(&self) -> Result<(), TypeCheckError> {
        let mut initial_env = Env::new();
        initial_env.declare_function("printInt".to_owned().into(), FunType {
             ret_type: DataType::Nonvoid(NonvoidType::TInt),
              params: vec![NonvoidType::TInt]
        })?;
        initial_env.declare_function("printBoolean".to_owned().into(), FunType {
            ret_type: DataType::Nonvoid(NonvoidType::TBoolean),
             params: vec![NonvoidType::TBoolean]
        })?;
        initial_env.declare_function("printString".to_owned().into(), FunType {
            ret_type: DataType::Nonvoid(NonvoidType::TString),
             params: vec![NonvoidType::TString]
        })?;

        self.type_check_with_env(&mut initial_env)
    }

    fn type_check_with_env(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        // First run
        for top_def in self.0.iter() {
            match top_def {
                TopDef::FunDef(fun_def) => fun_def.type_check(env)?,
                TopDef::Class(id, base_id, class_block) => {
                    env.declare_class(id.clone(), base_id.clone())?;
                }
            }
        }
        Ok(())
    }
}

impl FunDef {
    fn type_check(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        let fun_type = FunType {
            ret_type: self.ret_type.clone(),
            params: self.params.iter().map(|arg| arg.type_.clone()).collect(),
        };
        env.declare_function(self.name.clone(), fun_type)?;

        let body_ret_type = self.block.type_check(env)?;
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
    fn type_check(&self, env: &mut Env) -> Result<StmtRetType, TypeCheckError> {
        eprintln!("type checking block: {:#?}", self);
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
        eprintln!("type checking stmt: {:#?}", self);
        match self {
            Stmt::Empty => Ok(None),

            Stmt::Block(block) => {
                let block_ret = block.type_check(&mut env.new_scope())?;
                Ok(block_ret)
            }

            Stmt::VarDecl(decl) => {
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
                    env.declare_variable(
                        single_decl.name.clone(),
                        decl.type_.clone(),
                        single_decl.init.is_some(),
                    )?;
                }
                Ok(None)
            }

            Stmt::Ass(lval, expr) => {
                let (expr_type, _) = expr.type_check(env)?;
                let (lval_type, _was_init) = lval.type_check_and_make_init(env)?;

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

            Stmt::Incr(lval) => {
                let (lval_type, init) = lval.type_check_and_get_is_init(env)?;
                if !matches!(lval_type, NonvoidType::TInt) {
                    return Err(TypeCheckError::IncompatibleIncrementation(
                        self.clone(),
                        lval.clone(),
                        lval_type.clone(),
                    ));
                }
                if !init {
                    return Err(TypeCheckError::PossiblyUninitAccess(lval.clone()));
                }
                Ok(None)
            }

            Stmt::Decr(lval) => {
                let (lval_type, init) = lval.type_check_and_get_is_init(env)?;
                if !matches!(lval_type, NonvoidType::TInt) {
                    return Err(TypeCheckError::IncompatibleDecrementation(
                        self.clone(),
                        lval.clone(),
                        lval_type.clone(),
                    ));
                }
                if !init {
                    return Err(TypeCheckError::PossiblyUninitAccess(lval.clone()));
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
                    (DataType::Nonvoid(NonvoidType::TBoolean), None) => {
                        let then_stmt_ret = body.type_check(env)?;
                        Ok(then_stmt_ret.map(|(ret, _)| (ret, false))) // ret certainty is lost
                    }
                    (DataType::Nonvoid(NonvoidType::TBoolean), Some(Constexpr::Bool(true))) => {
                        body.type_check(env)
                    }
                    (DataType::Nonvoid(NonvoidType::TBoolean), Some(Constexpr::Bool(false))) => {
                        Ok({
                            body.type_check(env)?;
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

            Stmt::CondElse(condition, then_stmt, else_stmt) => {
                let (ret_type, constval) = condition.type_check(&mut env.new_scope())?;
                let then_stmt_ret = then_stmt.type_check(env)?;
                let else_stmt_ret = else_stmt.type_check(env)?;
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

            Stmt::SExp(e) => {
                e.type_check(env)?;
                Ok(None)
            }

            Stmt::For(elem_type, elem_name, array_expr, body) => {
                let (iterable_type, _) = array_expr.type_check(env)?;
                if let DataType::Nonvoid(NonvoidType::TArr(ref x)) = iterable_type {
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

impl LVal {
    fn type_check_and_make_init<'env>(
        &self,
        env: &'env mut Env,
    ) -> Result<(&'env NonvoidType, bool), TypeCheckError> {
        // (type, prev_init)
        match self {
            LVal::Id(id) => env
                .get_variable_type_and_make_init(id)
                .map_err(|e| e.into()),
            LVal::LField(_, _) => todo!(),
            LVal::LArr(_, _) => todo!(),
        }
    }

    fn type_check_and_get_is_init<'env>(
        &self,
        env: &'env Env,
    ) -> Result<(&'env NonvoidType, bool), TypeCheckError> {
        // (type, init)
        match self {
            LVal::Id(id) => env.get_variable_type_and_is_init(id).map_err(|e| e.into()),
            LVal::LField(_, _) => todo!(),
            LVal::LArr(_, _) => todo!(),
        }
    }
}

impl Expr {
    fn type_check(&self, env: &Env) -> Result<(DataType, Option<Constexpr>), TypeCheckError> {
        // (type, consteval)
        eprintln!("type checking expr: {:#?}", self);
        match self {
            Expr::IntLit(i) => Ok((
                DataType::Nonvoid(NonvoidType::TInt),
                Some(Constexpr::Int(*i)),
            )),

            Expr::BoolLit(b) => Ok((
                DataType::Nonvoid(NonvoidType::TBoolean),
                Some(Constexpr::Bool(*b)),
            )),

            Expr::StringLit(s) => Ok((
                DataType::Nonvoid(NonvoidType::TString),
                Some(Constexpr::String(s.clone())),
            )),

            Expr::Op(op) => match op {
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
                                Ok((DataType::Nonvoid(NonvoidType::TInt), constval_ret))
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
                                    t @ &DataType::Nonvoid(NonvoidType::TInt),
                                    &DataType::Nonvoid(NonvoidType::TInt),
                                )
                                | (
                                    t @ &DataType::Nonvoid(NonvoidType::TString),
                                    &DataType::Nonvoid(NonvoidType::TString),
                                )
                                | (
                                    t @ &DataType::Nonvoid(NonvoidType::TBoolean),
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
                                    Ok((t.clone(), constval))
                                }
                                _ => Err(TypeCheckError::EqWrongTypes(
                                    expr1.deref().clone(),
                                    expr1_type,
                                    bin_op_type.clone(),
                                    expr2.deref().clone(),
                                    expr2_type,
                                )),
                            }
                        }
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

            Expr::Id(id) => {
                let (var_type, initialised) = env.get_variable_type_and_is_init(id)?;
                if !initialised {
                    Err(TypeCheckError::PossiblyUninitVariableAccess(id.clone()))
                } else {
                    Ok((var_type.clone().into(), None))
                }
            }

            Expr::FunCall { name, args } => {
                let func = env.get_function_type(name)?.clone();
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
                let elt_type = if let DataType::Nonvoid(NonvoidType::TArr(inner_type)) = arr_type {
                    *inner_type
                } else {
                    return Err(TypeCheckError::BadArrType(arr_type));
                };

                let (idx_type, _) = idx.type_check(env)?;
                if !matches!(idx_type, DataType::Nonvoid(NonvoidType::TInt)) {
                    return Err(TypeCheckError::BadArrIndex(idx_type));
                }

                Ok((elt_type.into(), None))
            }

            Expr::FieldAccess(object, field) => {
                let (object_type, _) = object.type_check(env)?;
                let class = if let DataType::Nonvoid(NonvoidType::Class(name)) = object_type {
                    name
                } else {
                    return Err(TypeCheckError::NonObjectFieldAccess(object_type));
                };
                let field_type = env.get_field_type(class, field.clone())?;

                Ok((field_type.clone().into(), None))
            }

            Expr::MethodCall {
                object,
                method_name,
                args,
            } => {
                let (object_type, _) = object.type_check(env)?;
                let class = if let DataType::Nonvoid(NonvoidType::Class(name)) = object_type {
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

            Expr::Null(data_type) => Ok((data_type.clone().into(), Some(Constexpr::Null))),

            Expr::New(new_type) => {
                let data_type = match new_type {
                    NewType::TInt => DataType::Nonvoid(NonvoidType::TInt),
                    NewType::TString => DataType::Nonvoid(NonvoidType::TString),
                    NewType::TBoolean => DataType::Nonvoid(NonvoidType::TBoolean),
                    NewType::Class(c) => DataType::Nonvoid(NonvoidType::Class(c.clone())),
                    NewType::TArr(inner_type, _len) => {
                        DataType::Nonvoid(NonvoidType::TArr(inner_type.clone()))
                    }
                };
                Ok((data_type, None))
            }
        }
    }
}

/* TODO: usage of a variable initialised in both if branches...
        circular inheritance
        associativity
*/
