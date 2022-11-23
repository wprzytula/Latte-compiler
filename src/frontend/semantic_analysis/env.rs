use either::Either;
use rpds::RedBlackTreeMap as Map;

use super::ast::{DataType, Ident, RetType};

use thiserror::Error;

pub struct FunType {
    pub ret_type: RetType,
    pub params: Vec<DataType>,
}

enum Symbol {
    Var(Ident),
    Fun(Ident),
    Class(Ident),
    Method(Ident, Ident),
}

#[derive(Debug, Error)]
pub enum DoubleDeclarationError {
    #[error("Function {0} declared twice.")]
    Fun(Ident),
    #[error("Variable {0} declared twice.")]
    Var(Ident),
    #[error("Class {0} declared twice.")]
    Class(Ident),
    #[error("Class {0}: field {1} declared twice.")]
    Field(Ident, Ident),
    #[error("Class {0}: method {1} declared twice.")]
    Method(Ident, Ident),
}

#[derive(Debug, Error)]
pub enum MissingDeclarationError {
    #[error("Function {0} referred but never declared.")]
    Fun(Ident),
    #[error("Variable {0} referred but never declared.")]
    Var(Ident),
    #[error("Class {0} referred but never declared.")]
    Class(Ident),
    #[error("Class {0} referred as base for class {1} but never declared.")]
    BaseClass(Ident, Ident), // (missing, for subclass)
    #[error("Field {1} on class {0} referred but never declared.")]
    Field(Ident, Ident), // (class, field)
    #[error("Method {1} on class {0} referred but never declared.")]
    Method(Ident, Ident), // (class, method)
}

#[derive(Clone, Copy)]
enum EnvRun {
    CollectingIds,
    TypeChecking,
}

pub struct Env {
    current_run: EnvRun,
    current_scope: u32,
    variables: Map<Ident, (DataType, u32, bool)>, // name -> (type, scope declared, initialised)
    functions: Map<Ident, FunType>,               // name -> type
    classes: Map<Ident, Option<Ident>>,           // class -> base class
    fields: Map<(Ident, Ident), DataType>,        // (class, field) -> type
    methods: Map<(Ident, Ident), FunType>,        // (class, method) -> type
}

impl Env {
    pub fn new() -> Self {
        Self {
            current_run: EnvRun::CollectingIds,
            current_scope: 0,
            variables: Map::new(),
            functions: Map::new(),
            classes: Map::new(),
            fields: Map::new(),
            methods: Map::new(),
        }
    }

    pub fn new_scope(&self) -> Self {
        Self {
            current_run: self.current_run,
            current_scope: self.current_scope + 1,
            variables: self.variables.clone(),
            functions: self.functions.clone(),
            classes: self.classes.clone(),
            fields: self.fields.clone(),
            methods: self.methods.clone(),
        }
    }

    pub fn typecheck_phase(&mut self) {
        assert!(matches!(self.current_run, EnvRun::CollectingIds));
        self.current_run = EnvRun::TypeChecking;
    }

    pub fn declare_variable(
        &mut self,
        id: Ident,
        data_type: DataType,
        initialised: bool,
    ) -> Result<(), DoubleDeclarationError> {
        if let Some((_, scope, _)) = self.variables.get(&id) {
            if *scope == self.current_scope {
                return Err(DoubleDeclarationError::Var(id));
            }
        }
        self.variables
            .insert_mut(id, (data_type, self.current_scope, initialised));
        Ok(())
    }

    pub fn init_variable(&mut self, id: &Ident) -> Result<(), MissingDeclarationError> {
        self.variables
            .get_mut(id)
            .ok_or(MissingDeclarationError::Var(id.clone()))?
            .2 = true;
        Ok(())
    }

    pub fn get_variable_type(
        &self,
        id: &Ident,
    ) -> Result<(&DataType, bool), MissingDeclarationError> {
        self.variables
            .get(&id)
            .map(|type_scope_init| (&type_scope_init.0, type_scope_init.2))
            .ok_or(MissingDeclarationError::Var(id.clone()))
    }

    pub fn declare_function(
        &mut self,
        id: Ident,
        fun_type: FunType,
    ) -> Result<(), DoubleDeclarationError> {
        if self.functions.get(&id).is_some() {
            return Err(DoubleDeclarationError::Fun(id));
        }
        self.functions.insert_mut(id, fun_type);
        Ok(())
    }

    pub fn get_function_type(&self, id: &Ident) -> Result<&FunType, MissingDeclarationError> {
        self.functions
            .get(id)
            .ok_or(MissingDeclarationError::Fun(id.clone()))
    }

    pub fn declare_class(
        &mut self,
        id: Ident,
        base_id: Option<Ident>,
    ) -> Result<(), Either<DoubleDeclarationError, MissingDeclarationError>> {
        if self.classes.get(&id).is_some() {
            return Err(Either::Left(DoubleDeclarationError::Class(id)));
        }
        if let Some(ref base_id) = base_id {
            if self.classes.get(base_id).is_none() {
                return Err(Either::Right(MissingDeclarationError::BaseClass(
                    base_id.clone(),
                    id,
                )));
            }
        }
        self.classes.insert_mut(id, base_id);
        Ok(())
    }

    pub fn get_class(&self, id: &Ident) -> Result<(), MissingDeclarationError> {
        self.classes
            .get(id)
            .map(|_| ())
            .ok_or(MissingDeclarationError::Class(id.clone()))
    }

    pub fn declare_field(
        &mut self,
        class: Ident,
        id: Ident,
        data_type: DataType,
    ) -> Result<(), DoubleDeclarationError> {
        let class_id = (class, id);
        if self.fields.get(&(class_id)).is_some() {
            return Err(DoubleDeclarationError::Field(class_id.0, class_id.1));
        }
        self.fields.insert_mut(class_id, data_type);
        Ok(())
    }

    pub fn get_field_type(
        &self,
        class: Ident,
        id: Ident,
    ) -> Result<&DataType, MissingDeclarationError> {
        let class_id = (class, id);
        self.fields
            .get(&class_id)
            .ok_or(MissingDeclarationError::Field(class_id.0, class_id.1))
    }

    pub fn declare_method(
        &mut self,
        class: Ident,
        id: Ident,
        fun_type: FunType,
    ) -> Result<(), DoubleDeclarationError> {
        assert!(self.get_class(&class).is_ok());
        let class_id = (class, id);
        if self.methods.get(&class_id).is_some() {
            return Err(DoubleDeclarationError::Method(class_id.0, class_id.1));
        }
        self.functions.insert_mut(class_id.1, fun_type);
        Ok(())
    }

    pub fn resolve_method(
        &self,
        class: Ident,
        mut method: Ident,
    ) -> Result<&FunType, MissingDeclarationError> {
        let mut current_class = class.clone();
        loop {
            let pair = (current_class, method);
            if let Some(fun_type) = self.methods.get(&pair) {
                return Ok(fun_type);
            } else {
                current_class = pair.0;
                method = pair.1;
                match self.classes.get(&current_class) {
                    None => return Err(MissingDeclarationError::Class(class)),
                    Some(None) => return Err(MissingDeclarationError::Method(method, class)),
                    Some(Some(base_class)) => current_class = base_class.clone(),
                }
            }
        }
    }
}
