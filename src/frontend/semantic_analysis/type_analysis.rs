use std::{iter, ops::Deref};

use super::{
    ast::*,
    env::{
        CircularInheritanceError, DoubleFieldDeclarationError, DoubleMethodDeclarationError,
        DoubleVariableDeclarationError, Env, FunType, MissingBaseClassDeclarationError,
        MissingClassDeclarationError,
    },
};

use enum_as_inner::EnumAsInner;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypeCheckError {
    #[error(
        "{}: Function {} was applied {} argument(s), but expected {}.",
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

    #[error(
        "{}: Method {}::{} was applied {} argument(s), but expected {}.",
        pos,
        class,
        method,
        actual,
        expected
    )]
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
    WrongFuncArgTypes {
        pos: Pos,
        func: Ident,
        expected: Vec<NonvoidType>,
        actual: Vec<DataType>,
    },

    #[error(
        "{}: Method {}::{} was applied arguments of nonmatching types: expected {}, got {}.",
        pos,
        class,
        method,
        DisplayParams(expected),
        DisplayArgs(actual)
    )]
    WrongMethodArgTypes {
        pos: Pos,
        class: Ident,
        method: Ident,
        expected: Vec<NonvoidType>,
        actual: Vec<DataType>,
    },

    #[error(
        "{}: Method {}::{} overrides a base method with incompatible parameters: base {}, derived {}.",
        pos,
        class,
        method,
        DisplayParams(base_params),
        DisplayParams(actual_params),
    )]
    WrongMethodOverrideParams {
        pos: Pos,
        class: Ident,
        method: Ident,
        base_params: Vec<NonvoidType>,
        actual_params: Vec<NonvoidType>,
    },

    #[error(
        "{}: Method {}::{} overrides a base method with incompatible return type: base ret {}, derived ret {}.",
        pos,
        class,
        method,
        base_ret,
        actual_ret,
    )]
    WrongMethodOverrideRet {
        pos: Pos,
        class: Ident,
        method: Ident,
        base_ret: RetType,
        actual_ret: RetType,
    },

    #[error("{}: Incompatible type of Neg operation operand: {1}", .0.pos())]
    NegWrongType(Expr, DataType),

    #[error("{}: Incompatible type of Not operation operand: {1}", .0.pos())]
    NotWrongType(Expr, DataType),

    #[error("{}: Incompatible type of Add operation operands: {1}, {2}", .0.pos())]
    AddWrongTypes(Expr, DataType, DataType),

    #[error("{0}: Incompatible types of Eq/NEq operation operands: {2}, {5}")]
    EqWrongTypes(Pos, Expr, DataType, BinOpType, Expr, DataType),

    #[error("No main function")]
    NoMain,

    #[error("Main function must have no parameters")]
    ParamsInMain(Pos, FunDef),

    #[error("")]
    RepeatedParams(FunDef), // currently served by DoubleDeclaration

    #[error("")]
    NoReturnInNonVoidFun(FunDef), // currently served by ReturnTypeMismatch? Nope

    #[error("")]
    ReturnTypeMismatch(FunDef, RetType, RetType), // unused?

    #[error("{}: Incompatible type for initialising data of type {3}: {4}", .0.pos())]
    IncompatibleInitialization(Stmt, Expr, Ident, NonvoidType, DataType),

    #[error("{}: Value is not an lvalue; got type: {1}", .0.pos())]
    InvalidLVal(LVal, DataType),

    #[error("{}: Incompatible type for assigning value to data of type {3}: {4}", .0.pos())]
    IncompatibleAssignment(Stmt, Expr, LVal, DataType, DataType),

    #[error("{}: Cannot increment data of type {2}", .0.pos())]
    IncompatibleIncrementation(Stmt, LVal, DataType),

    #[error("{}: Cannot decrement data of type {2}", .0.pos())]
    IncompatibleDecrementation(Stmt, LVal, DataType),

    #[error("{}: Bad condition type: {2}", .0.pos())]
    WrongConditionType(Stmt, Expr, DataType),

    #[error("{}: Possible non return from non-void function {}", .0.pos, .0.name)]
    PossibleNonReturnFromFunction(FunDef),

    #[error("{}: Incompatible type returned from function: expected {1}, got {3}", .0.pos)]
    IncompatibleRetTypesInFunction(FunDef, DataType, Block, DataType),

    #[error("")]
    BadForElemType(Stmt, NonvoidType, DataType),

    #[error("{}: Bad type for binary operation over Ints: {1}", .0.pos())]
    IntBinOpWrongType(Expr, DataType),

    #[error("{}: Bad type for logical operation: {1}", .0.pos())]
    LogOpWrongType(Expr, DataType),

    #[error("{}: Value is not an array, but instead has type: {1}", .0.pos())]
    BadArrType(LVal, DataType),

    #[error("{}: Value is not a valid array index, but instead has type: {1}", .0.pos())]
    BadArrIndex(Expr, DataType),

    #[error("{}: Value, whose {2} field access was attempted, is not an object, but instead has type: {1}", .0.pos())]
    NonObjectFieldAccess(LVal, DataType, Ident),

    #[error("{}: Value, whose {2}() method call was attempted, is not an object, but instead has type: {1}", .0.pos())]
    NonObjectMethodCall(LVal, DataType, Ident),

    #[error("{0}: Class {1} does not have a field {2}")]
    NoSuchField(Pos, Ident, Ident),

    #[error("{0}: Class {1} does not have a method {2}()")]
    NoSuchMethod(Pos, Ident, Ident),

    #[error("{}: Bad return type for main function: {1}", .0)]
    WrongMainRetType(Pos, DataType),

    #[error("{0}: Returning {2} in function declared to return {1}")]
    InvalidReturnType(Pos, /* Ident,  */ DataType, DataType),

    #[error("{}: Multiple definitions of function {}", .0.pos, .0.name)]
    MultipleFunctionDefinition(FunDef),

    #[error("{}: Multiple declarations of variable {}", .0.pos(), .1)]
    MultipleVariableDeclaration(Stmt, Ident),

    #[error("{0}: Multiple declarations of field {2} on class {1}")]
    MultipleFieldDeclaration(Pos, Ident, Ident, NonvoidType),

    #[error("{0}: Multiple definitions of method {2} on class {1}")]
    MultipleMethodDefinitions(Pos, Ident, Ident, FunType),

    #[error("{}: Multiple definitions of class {}", .0, .1)]
    MultipleClassDefinition(Pos, Ident),

    #[error("{}: Multiple parameters in function {} have same name: {}", .0.pos, .0.name, 1)]
    RepeatedParamsNames(FunDef, Ident),

    #[error("{0}: Attempted to access undeclared variable {1}")]
    UndeclaredVariableAccess(Pos, Ident),

    #[error("{0}: Attempted to call undefined function {1}")]
    UndefinedFunctionCall(Pos, Ident),

    #[error("{0}: Bad type of length of array in 'new' expression: {3}")]
    BadNewArrLen(Pos, NewType, Expr, DataType),

    #[error("Base class {1} of class {0} does not exist.")]
    NonexistingBaseClass(Ident, Ident),

    #[error("{0}: Referred to undefined class {1}.")]
    NonexistingClass(Pos, Ident),

    #[error(transparent)]
    CircularInheritance(#[from] CircularInheritanceError),

    #[error("{0}: Illegal null cast to {1}.")]
    IllegalNullCast(Pos, NonvoidType),
}

#[derive(Debug, EnumAsInner)]
enum Constexpr {
    Bool(bool),
    String(String),
    Int(Int),
    // Null,
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
        initial_env
            .declare_function(
                "printInt".to_owned().into(),
                FunType {
                    ret_type: DataType::TVoid,
                    params: vec![NonvoidType::TInt],
                },
            )
            .unwrap();

        /* printString */
        initial_env
            .declare_function(
                "printString".to_owned().into(),
                FunType {
                    ret_type: DataType::TVoid,
                    params: vec![NonvoidType::TString],
                },
            )
            .unwrap();

        /* error */
        initial_env
            .declare_function(
                "error".to_owned().into(),
                FunType {
                    ret_type: DataType::TVoid,
                    params: vec![],
                },
            )
            .unwrap();

        /* readInt */
        initial_env
            .declare_function(
                "readInt".to_owned().into(),
                FunType {
                    ret_type: DataType::Nonvoid(NonvoidType::TInt),
                    params: vec![],
                },
            )
            .unwrap();

        /* readString */
        initial_env
            .declare_function(
                "readString".to_owned().into(),
                FunType {
                    ret_type: DataType::Nonvoid(NonvoidType::TString),
                    params: vec![],
                },
            )
            .unwrap();

        self.type_check_with_env(&mut initial_env)
    }

    fn type_check_with_env(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        // First run - declare topDef entities
        for fun_def in self.0.iter() {
            fun_def.declare(env)?;
        }
        for class_def in self.1.iter() {
            let ClassDef {
                pos,
                class,
                base_class,
                ..
            } = class_def;
            env.declare_class(class.clone(), base_class.clone())
                .map_err(|_| TypeCheckError::MultipleClassDefinition(*pos, class.clone()))?;
        }

        if env.get_function_type(&"main".to_owned().into()).is_err() {
            return Err(TypeCheckError::NoMain);
        }

        // Base classes check
        if let Err(err) = env.check_base_classes() {
            match err {
                either::Either::Left(MissingBaseClassDeclarationError(class, base)) => {
                    return Err(TypeCheckError::NonexistingBaseClass(class, base))
                }
                either::Either::Right(err @ CircularInheritanceError(_)) => {
                    return Err(TypeCheckError::CircularInheritance(err))
                }
            }
        }

        // env.topo_sort_classes(&mut self.1);

        // Second run - declare all class fields and methods - preferably in topological order of inheritance tree, from root to leavess
        for class_def in self.1.iter() {
            class_def.class_block.declare(&class_def.class, env)?;
        }

        // Third run - type check
        for fun_def in self.0.iter() {
            fun_def.type_check(&mut env.new_scope())?;
        }
        for class_def in self.1.iter() {
            let ClassDef {
                pos,
                class,
                base_class,
                class_block,
            } = class_def;
            class_block.type_check(class, &mut env.new_scope())?;
        }

        Ok(())
    }
}

impl ClassBlock {
    fn declare(&self, class: &Ident, env: &mut super::env::Env) -> Result<(), TypeCheckError> {
        for class_item in self.0.iter() {
            match class_item {
                ClassItem::Field(pos, nonvoid, name) => env
                    .declare_field(class.clone(), name.clone(), nonvoid.clone())
                    .map_err(|DoubleFieldDeclarationError(class, field, nonvoid)| {
                        TypeCheckError::MultipleFieldDeclaration(*pos, class, field, nonvoid)
                    })?,
                ClassItem::Method(fun_def) => env
                    .declare_method(class.clone(), fun_def.name.clone(), fun_def.fun_type())
                    .map_err(|DoubleMethodDeclarationError(class, field, method_type)| {
                        TypeCheckError::MultipleMethodDefinitions(
                            fun_def.pos,
                            class,
                            field,
                            method_type,
                        )
                    })?,
            }
        }
        Ok(())
    }

    fn type_check(&self, class: &Ident, env: &mut super::env::Env) -> Result<(), TypeCheckError> {
        // What if method signature conflicts with overridden superclass method signature?
        // for each class, for each method, I check that every method is a proper override of the base class' method.
        // Algorithm:
        // For each class:
        //   If base class exist:
        //      For each method:
        //        If method with same name is resolved in base class:
        //           For each parameter and for return type assert that class's method impl has types
        //           that are instances of the base class's method impl.
        if let Some(base_class) = env.get_class(class).unwrap().clone() {
            for class_item in self.0.iter() {
                if let ClassItem::Method(method) = class_item {
                    if let Ok(base_method) =
                        env.resolve_method(base_class.clone(), method.name.clone())
                    {
                        if !env.is_subtype(&method.ret_type, &base_method.ret_type) {
                            return Err(TypeCheckError::WrongMethodOverrideRet {
                                pos: method.pos,
                                class: class.clone(),
                                method: method.name.clone(),
                                base_ret: base_method.ret_type.clone(),
                                actual_ret: method.ret_type.clone(),
                            });
                        }
                        if iter::once(method.params.len() == base_method.params.len())
                            .chain(
                                method
                                    .fun_type()
                                    .params
                                    .into_iter()
                                    .zip(base_method.params.clone().into_iter())
                                    .map(|(method_param, base_param)| {
                                        env.is_subtype(&method_param.into(), &base_param.into())
                                    }),
                            )
                            .any(|valid| !valid)
                        {
                            return Err(TypeCheckError::WrongMethodOverrideParams {
                                pos: method.pos,
                                class: class.clone(),
                                method: method.name.clone(),
                                base_params: base_method.params.clone(),
                                actual_params: method.fun_type().params,
                            });
                        }
                    }
                }
            }
        }

        // Declare self as a variable
        env.declare_variable("self".to_owned().into(), NonvoidType::TClass(class.clone()))
            .unwrap();

        // For each field, declare it in the class block env as a variable
        for class_item in self.0.iter() {
            if let ClassItem::Field(pos, nonvoid, id) = class_item {
                if let NonvoidType::TClass(class) | NonvoidType::TClassArr(class) = nonvoid {
                    env.get_class(class)
                        .map_err(|MissingClassDeclarationError(class)| {
                            TypeCheckError::NonexistingClass(*pos, class)
                        })?;
                }
                env.declare_variable(id.clone(), nonvoid.clone()).unwrap();
            }
        }

        // For each method, type check it in class block env, which contains local fields as variables.
        for class_item in self.0.iter() {
            if let ClassItem::Method(method) = class_item {
                method.type_check(&mut env.new_scope())?;
            }
        }

        Ok(())
    }
}

impl FunDef {
    fn fun_type(&self) -> FunType {
        FunType {
            ret_type: self.ret_type.clone(),
            params: self.params.iter().map(|arg| arg.type_.clone()).collect(),
        }
    }

    fn declare(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        let fun_type = self.fun_type();
        if self.name.deref() == "main" {
            if !self.params.is_empty() {
                return Err(TypeCheckError::ParamsInMain(self.pos, self.clone()));
            }
            if !matches!(self.ret_type, DataType::Nonvoid(NonvoidType::TInt)) {
                return Err(TypeCheckError::WrongMainRetType(
                    self.pos,
                    self.ret_type.clone(),
                ));
            }
        }
        env.declare_function(self.name.clone(), fun_type)
            .map_err(|_| TypeCheckError::MultipleFunctionDefinition(self.clone()))?;
        Ok(())
    }
    fn type_check(&self, env: &mut Env) -> Result<(), TypeCheckError> {
        // check that ret type exists!
        if let DataType::Nonvoid(NonvoidType::TClass(ref class))
        | DataType::Nonvoid(NonvoidType::TClassArr(ref class)) = self.ret_type
        {
            env.get_class(class)
                .map_err(|MissingClassDeclarationError(class)| {
                    TypeCheckError::NonexistingClass(self.pos, class)
                })?;
        }

        // Add params to env
        for param in self.params.iter() {
            env.declare_variable(param.name.clone(), param.type_.clone())
                .map_err(|_| {
                    TypeCheckError::RepeatedParamsNames(self.clone(), param.name.clone())
                })?;
        }
        let body_ret_type = self.block.type_check(env, &self.ret_type)?;
        match (&self.ret_type, &body_ret_type) {
            (&DataType::TVoid, &None) | (&DataType::TVoid, &Some((DataType::TVoid, _))) => Ok(()), // Void return fulfilled
            (&DataType::Nonvoid(ref expected), &Some((DataType::Nonvoid(ref got), true))) if env.is_subtype(&got.clone().into(), &expected.clone().into()) => Ok(()), // Matching nonvoid return
            (&DataType::Nonvoid(ref expected), &Some((ref got, false))) if env.is_subtype(got, &expected.clone().into()) => Err(TypeCheckError::PossibleNonReturnFromFunction(self.clone())),
            (&DataType::Nonvoid(_), &None) | // Expected Nonvoid return, got void
            (&DataType::Nonvoid(_), &Some((_, false))) | // Expected Nonvoid return, got possible void
            (&DataType::TVoid, _) | // Expected Void return, got possible different
            (_, _) // All other cases (incompatible types, etc.)
            => Err(TypeCheckError::IncompatibleRetTypesInFunction(
                self.clone(),
                self.ret_type.clone(),
                self.block.clone(),
                body_ret_type.map(|(ret, _)| ret).unwrap_or(DataType::TVoid),
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
                    if env.is_subtype(&stmt_ret, block_ret) {
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
        let pos = self.0;
        match &self.1 {
            StmtInner::Empty => Ok(None),

            StmtInner::Block(block) => {
                let block_ret = block.type_check(&mut env.new_scope(), allowed_ret_type)?;
                Ok(block_ret)
            }

            StmtInner::VarDecl(decl) => {
                if let NonvoidType::TClass(ref c) | NonvoidType::TClassArr(ref c) = decl.type_ {
                    env.get_class(c)
                        .map_err(|MissingClassDeclarationError(class)| {
                            TypeCheckError::NonexistingClass(pos, class)
                        })?;
                }
                for single_decl in decl.decls.iter() {
                    if let Some(ref init_expr) = single_decl.init {
                        let (init_type, _) = init_expr.type_check(env)?;
                        if !env.is_subtype(&init_type, &decl.type_.clone().into()) {
                            return Err(TypeCheckError::IncompatibleInitialization(
                                self.clone(),
                                init_expr.clone(),
                                single_decl.name.clone(),
                                decl.type_.clone(),
                                init_type,
                            ));
                        }
                    }
                    env.declare_variable(single_decl.name.clone(), decl.type_.clone())
                        .map_err(|DoubleVariableDeclarationError(var)| {
                            TypeCheckError::MultipleVariableDeclaration(self.clone(), var)
                        })?;
                }
                Ok(None)
            }

            StmtInner::Ass(lval, expr) => {
                let (expr_type, _) = expr.type_check(env)?;
                let (lval_type, _, is_valid_lval) = lval.type_check(env)?;
                if !is_valid_lval {
                    return Err(TypeCheckError::InvalidLVal(lval.clone(), lval_type));
                }
                if !env.is_subtype(&expr_type, &lval_type) {
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
                let (lval_type, _, is_valid_lval) = lval.type_check(env)?;
                if !is_valid_lval {
                    return Err(TypeCheckError::InvalidLVal(lval.clone(), lval_type));
                }
                if !matches!(lval_type, DataType::Nonvoid(NonvoidType::TInt)) {
                    return Err(TypeCheckError::IncompatibleIncrementation(
                        self.clone(),
                        lval.clone(),
                        lval_type.clone(),
                    ));
                }
                Ok(None)
            }

            StmtInner::Decr(lval) => {
                let (lval_type, _, is_valid_lval) = lval.type_check(env)?;
                if !is_valid_lval {
                    return Err(TypeCheckError::InvalidLVal(lval.clone(), lval_type));
                }
                if !matches!(lval_type, DataType::Nonvoid(NonvoidType::TInt)) {
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
                if env.is_subtype(&ret_type, allowed_ret_type) {
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
                                    ((typ1, certain1), (typ2, certain2))/*  if typ1 == typ2 */ => {
                                        Ok(Some((typ1, certain1 && certain2)))
                                    }
                                    // (_then_ret_type, _else_ret_type) => {
                                    //     unreachable!("This is handled by restricting Returns to a certain type only")
                                    // }
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
                if let Some(iterable_elem_type) = iterable_type.array_member_type() {
                    if !env.is_subtype(&iterable_elem_type.into(), &elem_type.clone().into()) {
                        return Err(TypeCheckError::BadForElemType(
                            self.clone(),
                            elem_type.clone(),
                            iterable_type,
                        ));
                    }
                } else {
                    // Not an iterable
                    return Err(TypeCheckError::BadForElemType(
                        self.clone(),
                        elem_type.clone(),
                        iterable_type,
                    ));
                }
                let body_ret = {
                    let mut body_env = env.new_scope();
                    body_env
                        .declare_variable(elem_name.clone(), elem_type.clone())
                        .unwrap(); // This is in a new scope, so it must not fail.
                    body.type_check(&mut body_env, allowed_ret_type)?
                };
                Ok(body_ret.map(|(ret, _)| (ret, false))) // ret certainty is lost - body can be executed 0 times
            }
        }
    }
}

impl LVal {
    fn type_check<'env>(
        &self,
        env: &'env Env,
    ) -> Result<(DataType, Option<Constexpr>, bool), TypeCheckError> {
        // (type, constval, is_lval)
        let pos = self.0;
        match &self.1 {
            LValInner::Id(id) => {
                let nonvoid = env
                    .get_variable_type(id)
                    .map_err(|_| TypeCheckError::UndeclaredVariableAccess(pos, id.clone()))?;
                Ok((DataType::Nonvoid(nonvoid.clone()), None, true))
            }

            LValInner::FunCall { name, args } => {
                let func = env
                    .get_function_type(name)
                    .map_err(|_| TypeCheckError::UndefinedFunctionCall(pos, name.clone()))?
                    .clone();

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

                if arg_types
                    .clone()
                    .into_iter()
                    .zip(func.params.clone().into_iter())
                    .map(|(method_param, base_param)| {
                        env.is_subtype(&method_param.into(), &base_param.into())
                    })
                    .any(|valid| !valid)
                {
                    return Err(TypeCheckError::WrongFuncArgTypes {
                        pos,
                        func: name.clone(),
                        expected: func.params.clone(),
                        actual: arg_types,
                    });
                }

                Ok((
                    func.ret_type.clone(),
                    None,
                    func.ret_type.is_passed_by_ref(),
                ))
            }

            LValInner::ArrSub(arr, idx) => {
                let (arr_type, _, _) = arr.type_check(env)?;
                let elt_type = match arr_type {
                    DataType::Nonvoid(NonvoidType::TIntArr) => NonvoidType::TInt,
                    DataType::Nonvoid(NonvoidType::TBooleanArr) => NonvoidType::TBoolean,
                    DataType::Nonvoid(NonvoidType::TStringArr) => NonvoidType::TString,
                    DataType::Nonvoid(NonvoidType::TClassArr(class_name)) => {
                        NonvoidType::TClass(class_name)
                    }
                    _ => return Err(TypeCheckError::BadArrType(arr.deref().clone(), arr_type)),
                };

                let (idx_type, _) = idx.type_check(env)?;
                if !matches!(idx_type, DataType::Nonvoid(NonvoidType::TInt)) {
                    return Err(TypeCheckError::BadArrIndex(idx.clone(), idx_type));
                }

                Ok((elt_type.into(), None, true))
            }

            LValInner::FieldAccess(object, field) => {
                let (object_type, _, _) = object.type_check(env)?;

                // special case for array len
                if object_type.array_member_type().is_some() {
                    // it means that this is an array of some type
                    return Ok((DataType::Nonvoid(NonvoidType::TInt), None, false));
                }

                let class = if let DataType::Nonvoid(NonvoidType::TClass(name)) = object_type {
                    name
                } else {
                    return Err(TypeCheckError::NonObjectFieldAccess(
                        self.clone(),
                        object_type,
                        field.clone(),
                    ));
                };
                let field_type = env
                    .resolve_field_type(class.clone(), field.clone())
                    .map_err(|_| TypeCheckError::NoSuchField(self.pos(), class, field.clone()))?;

                Ok((field_type.clone().into(), None, true))
            }

            LValInner::MethodCall {
                object,
                method_name,
                args,
            } => {
                let (object_type, _, _) = object.type_check(env)?;
                let class = if let DataType::Nonvoid(NonvoidType::TClass(name)) = object_type {
                    name
                } else {
                    return Err(TypeCheckError::NonObjectMethodCall(
                        self.clone(),
                        object_type,
                        method_name.clone(),
                    ));
                };
                let method = env
                    .resolve_method(class.clone(), method_name.clone())
                    .map_err(|_| {
                        TypeCheckError::NoSuchMethod(self.pos(), class.clone(), method_name.clone())
                    })?;

                if args.len() != method.params.len() {
                    return Err(TypeCheckError::WrongMethodArgNum {
                        pos,
                        class,
                        method: method_name.clone(),
                        expected: method.params.len(),
                        actual: args.len(),
                    });
                }

                let arg_types = args
                    .iter()
                    .map(|arg| arg.type_check(env).map(|(t, _)| t))
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;

                if arg_types
                    .clone()
                    .into_iter()
                    .zip(method.params.clone().into_iter())
                    .map(|(method_param, base_param)| {
                        env.is_subtype(&method_param.into(), &base_param.into())
                    })
                    .any(|valid| !valid)
                {
                    return Err(TypeCheckError::WrongMethodArgTypes {
                        pos,
                        class,
                        method: method_name.clone(),
                        expected: method.params.clone(),
                        actual: arg_types,
                    });
                }

                Ok((
                    method.ret_type.clone(),
                    None,
                    method.ret_type.is_passed_by_ref(),
                ))
            }
            LValInner::New(new_type) => {
                let (data_type, len) = match new_type {
                    NewType::TInt => (DataType::Nonvoid(NonvoidType::TInt), None),
                    NewType::TString => (DataType::Nonvoid(NonvoidType::TString), None),
                    NewType::TBoolean => (DataType::Nonvoid(NonvoidType::TBoolean), None),
                    NewType::TClass(c) => {
                        env.get_class(c)
                            .map_err(|MissingClassDeclarationError(class)| {
                                TypeCheckError::NonexistingClass(pos, class)
                            })?;
                        (DataType::Nonvoid(NonvoidType::TClass(c.clone())), None)
                    }
                    NewType::TIntArr(len) => (DataType::Nonvoid(NonvoidType::TIntArr), Some(len)),
                    NewType::TStringArr(len) => {
                        (DataType::Nonvoid(NonvoidType::TStringArr), Some(len))
                    }
                    NewType::TBooleanArr(len) => {
                        (DataType::Nonvoid(NonvoidType::TBooleanArr), Some(len))
                    }
                    NewType::TClassArr(class, len) => {
                        env.get_class(class)
                            .map_err(|MissingClassDeclarationError(class)| {
                                TypeCheckError::NonexistingClass(pos, class)
                            })?;
                        (
                            DataType::Nonvoid(NonvoidType::TClassArr(class.clone())),
                            Some(len),
                        )
                    }
                };
                if let Some(len) = len {
                    let (len_type, _) = len.type_check(env)?;
                    if !matches!(len_type, DataType::Nonvoid(NonvoidType::TInt)) {
                        return Err(TypeCheckError::BadNewArrLen(
                            pos,
                            new_type.clone(),
                            len.deref().clone(),
                            len_type,
                        ));
                    }
                }
                Ok((data_type, None, false))
            }
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
                                (
                                    &DataType::Nonvoid(NonvoidType::TClass(ref class1)),
                                    &DataType::Nonvoid(NonvoidType::TClass(_)),
                                ) if env.get_class(class1).is_err() => {
                                    return Err(TypeCheckError::NonexistingClass(
                                        pos,
                                        class1.clone(),
                                    ))
                                }
                                (
                                    &DataType::Nonvoid(NonvoidType::TClass(_)),
                                    &DataType::Nonvoid(NonvoidType::TClass(ref class2)),
                                ) if env.get_class(class2).is_err() => {
                                    return Err(TypeCheckError::NonexistingClass(
                                        pos,
                                        class2.clone(),
                                    ))
                                }
                                (
                                    &DataType::Nonvoid(NonvoidType::TClass(ref class1)),
                                    &DataType::Nonvoid(NonvoidType::TClass(ref class2)),
                                ) if env.is_subclass(class1, class2)
                                    || env.is_subclass(class1, class2) =>
                                {
                                    Ok((DataType::Nonvoid(NonvoidType::TBoolean), None))
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
                let var_type = env
                    .get_variable_type(id)
                    .map_err(|_| TypeCheckError::UndeclaredVariableAccess(pos, id.clone()))?;
                Ok((var_type.clone().into(), None))
            }

            ExprInner::Null(nonvoid) => {
                if match nonvoid {
                    NonvoidType::TClass(class) => env.get_class(class).is_ok(),
                    _ => false,
                } {
                    Ok((
                        nonvoid.clone().into(),
                        /* Some(Constexpr::Null) */ None,
                    ))
                } else {
                    Err(TypeCheckError::IllegalNullCast(pos, nonvoid.clone()))
                }
            }

            ExprInner::LVal(lval) => {
                let (data_type, constval, _) = lval.type_check(env)?;
                Ok((data_type, constval))
            }
        }
    }
}

/* TODO:
    - accept subclasses wherever superclass is allowed.
*/
