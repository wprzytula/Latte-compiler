use std::{
    collections::HashSet,
    fmt::{self, Display, Write},
};

use either::Either;
use lazy_static::lazy_static;
use rpds::RedBlackTreeMap as Map;
use thiserror::Error;

use super::ast::{DataType, Ident, NonvoidType, RetType};

#[derive(Debug, Clone)]
pub struct FunType {
    pub ret_type: RetType,
    pub params: Vec<NonvoidType>,
}

#[derive(Debug)]
pub struct DoubleFuncDeclarationError(pub Ident);

#[derive(Debug)]
pub struct DoubleVariableDeclarationError(pub Ident);
pub struct DoubleClassDeclarationError(pub Ident);
pub struct DoubleFieldDeclarationError(pub Ident, pub Ident, pub NonvoidType);
pub struct DoubleMethodDeclarationError(pub Ident, pub Ident, pub FunType);

pub struct MissingFuncDeclarationError(pub Ident);

pub struct MissingVariableDeclarationError(pub Ident);

#[derive(Debug)]
pub struct MissingClassDeclarationError(pub Ident);

pub struct MissingBaseClassDeclarationError(pub Ident, pub Ident);

pub struct MissingFieldError(pub Ident, pub Ident);

pub struct MissingMethodError(pub Ident, pub Ident);

#[derive(Debug, Error)]
#[error("Found circular inheritance: {}", CircularDependencyDisplay(&.0))]
pub struct CircularInheritanceError(pub Vec<Ident>);

struct CircularDependencyDisplay<'a>(&'a Vec<Ident>);
impl<'a> Display for CircularDependencyDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;
        let mut first = true;
        for arg in self.0 {
            if !first {
                f.write_str(" -> ")?;
            }
            arg.fmt(f)?;
            first = false;
        }
        f.write_char(')')
    }
}

lazy_static! {
    pub static ref INITIAL_FUNCS: Vec<(Ident, FunType)> = vec![
        (
            "printInt".to_owned(),
            FunType {
                ret_type: DataType::TVoid,
                params: vec![NonvoidType::TInt],
            }
        ),
        (
            "printString".to_owned().into(),
            FunType {
                ret_type: DataType::TVoid,
                params: vec![NonvoidType::TString]
            }
        ),
        (
            "error".to_owned().into(),
            FunType {
                ret_type: DataType::TVoid,
                params: vec![],
            }
        ),
        (
            "readInt".to_owned().into(),
            FunType {
                ret_type: DataType::Nonvoid(NonvoidType::TInt),
                params: vec![],
            }
        ),
        (
            "readString".to_owned().into(),
            FunType {
                ret_type: DataType::Nonvoid(NonvoidType::TString),
                params: vec![],
            }
        ),
    ];
}

pub struct Env {
    current_scope: u32,
    variables: Map<Ident, (NonvoidType, u32)>, // name -> (type, scope declared)
    functions: Map<Ident, FunType>,            // name -> type
    classes: Map<Ident, Option<Ident>>,        // class -> base class
    fields: Map<(Ident, Ident), NonvoidType>,  // (class, field) -> type
    methods: Map<(Ident, Ident), FunType>,     // (class, method) -> type
}

impl Env {
    pub fn new() -> Self {
        Self {
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
            current_scope: self.current_scope + 1,
            variables: self.variables.clone(),
            functions: self.functions.clone(),
            classes: self.classes.clone(),
            fields: self.fields.clone(),
            methods: self.methods.clone(),
        }
    }

    pub fn declare_variable(
        &mut self,
        id: Ident,
        data_type: NonvoidType,
    ) -> Result<(), DoubleVariableDeclarationError> {
        if let Some((_, scope)) = self.variables.get(&id) {
            if *scope == self.current_scope {
                return Err(DoubleVariableDeclarationError(id));
            }
        }
        self.variables
            .insert_mut(id, (data_type, self.current_scope));
        Ok(())
    }

    pub fn get_variable_type(
        &self,
        id: &Ident,
    ) -> Result<&NonvoidType, MissingVariableDeclarationError> {
        self.variables
            .get(id)
            .map(|type_scope_init| &type_scope_init.0)
            .ok_or(MissingVariableDeclarationError(id.clone()))
    }

    pub fn declare_function(
        &mut self,
        id: Ident,
        fun_type: FunType,
    ) -> Result<(), DoubleFuncDeclarationError> {
        if self.functions.get(&id).is_some() {
            return Err(DoubleFuncDeclarationError(id));
        }
        self.functions.insert_mut(id, fun_type);
        Ok(())
    }

    pub fn get_function_type(&self, id: &Ident) -> Result<&FunType, MissingFuncDeclarationError> {
        self.functions
            .get(id)
            .ok_or(MissingFuncDeclarationError(id.clone()))
    }

    pub fn declare_class(
        &mut self,
        id: Ident,
        base_id: Option<Ident>,
    ) -> Result<(), DoubleClassDeclarationError> {
        if self.classes.get(&id).is_some() {
            return Err(DoubleClassDeclarationError(id));
        }
        self.classes.insert_mut(id, base_id);
        Ok(())
    }

    pub fn check_base_classes(
        &self,
    ) -> Result<(), Either<MissingBaseClassDeclarationError, CircularInheritanceError>> {
        let mut already_checked = HashSet::<Ident>::with_capacity(self.classes.size());
        let mut visited_for_one_outer = HashSet::<&Ident>::new();
        let mut cycle_buf: Vec<Ident> = vec![];
        'outer: for (mut class, mut base_class) in self.classes.iter() {
            cycle_buf.clear();
            visited_for_one_outer.clear();
            loop {
                cycle_buf.push(class.clone());
                visited_for_one_outer.insert(class);
                if already_checked.contains(class) {
                    continue 'outer;
                }
                already_checked.insert(class.clone());
                if let Some(base) = base_class {
                    class = base;
                    base_class = self.classes.get(class).ok_or(Either::Left(
                        MissingBaseClassDeclarationError(class.clone(), base.clone()),
                    ))?;
                } else {
                    break;
                }
                if visited_for_one_outer.contains(class) {
                    cycle_buf.push(class.clone());
                    let cycle = cycle_buf.into_iter().skip_while(|c| c != class).collect();
                    return Err(Either::Right(CircularInheritanceError(cycle)));
                }
            }
        }
        Ok(())
    }

    pub fn get_class(&self, id: &Ident) -> Result<&Option<Ident>, MissingClassDeclarationError> {
        self.classes
            .get(id)
            .ok_or(MissingClassDeclarationError(id.clone()))
    }

    pub fn is_subtype<'a: 'b, 'b>(&'a self, data_type: &'b DataType, base_type: &DataType) -> bool {
        match (data_type, base_type) {
            (_, DataType::TExit) => unreachable!(),
            (DataType::TExit, _) => unreachable!(),
            (DataType::TVoid, DataType::TVoid) => true,
            (DataType::TVoid, DataType::Nonvoid(_)) | (DataType::Nonvoid(_), DataType::TVoid) => {
                false
            }
            (DataType::Nonvoid(nonvoid), DataType::Nonvoid(base_nonvoid)) => {
                match (nonvoid, base_nonvoid) {
                    (NonvoidType::TInt, NonvoidType::TInt)
                    | (NonvoidType::TString, NonvoidType::TString)
                    | (NonvoidType::TBoolean, NonvoidType::TBoolean)
                    | (NonvoidType::TIntArr, NonvoidType::TIntArr)
                    | (NonvoidType::TStringArr, NonvoidType::TStringArr)
                    | (NonvoidType::TBooleanArr, NonvoidType::TBooleanArr) => true,
                    (NonvoidType::TClass(class), NonvoidType::TClass(base_class)) => {
                        self.is_subclass(class, base_class)
                    }
                    (NonvoidType::TClassArr(class), NonvoidType::TClassArr(base_class)) => {
                        self.is_subclass(class, base_class)
                    }
                    _ => false,
                }
            }
        }
    }

    pub fn is_subclass<'a: 'b, 'b>(&'a self, mut class: &'b Ident, base: &Ident) -> bool {
        while class != base {
            if let Some(base_class) = self.classes.get(class).unwrap() {
                class = base_class;
            } else {
                return false;
            }
        }
        true
    }

    pub fn declare_field(
        &mut self,
        class: Ident,
        id: Ident,
        data_type: NonvoidType,
    ) -> Result<(), DoubleFieldDeclarationError> {
        let class_id = (class, id);
        if self.fields.get(&(class_id)).is_some() {
            return Err(DoubleFieldDeclarationError(
                class_id.0, class_id.1, data_type,
            ));
        }
        self.fields.insert_mut(class_id, data_type);
        Ok(())
    }

    pub fn resolve_field_type(
        &self,
        class: Ident,
        mut field_id: Ident,
    ) -> Result<&NonvoidType, MissingFieldError> {
        let mut current_class = class.clone();
        loop {
            let pair = (current_class, field_id);
            if let Some(nonvoid) = self.fields.get(&pair) {
                return Ok(nonvoid);
            } else {
                current_class = pair.0;
                field_id = pair.1;
                match self.classes.get(&current_class).unwrap() {
                    // We've already checked existence of base classes (I hope...)
                    None => return Err(MissingFieldError(current_class, field_id)),
                    Some(base_class) => current_class = base_class.clone(),
                }
            }
        }
    }

    pub fn declare_method(
        &mut self,
        class: Ident,
        id: Ident,
        fun_type: FunType,
    ) -> Result<(), DoubleMethodDeclarationError> {
        assert!(self.get_class(&class).is_ok());
        let class_id = (class, id);
        if self.methods.get(&class_id).is_some() {
            return Err(DoubleMethodDeclarationError(
                class_id.0, class_id.1, fun_type,
            ));
        }
        self.methods.insert_mut(class_id, fun_type);
        Ok(())
    }

    pub fn resolve_method(
        &self,
        class: Ident,
        mut method: Ident,
    ) -> Result<&FunType, MissingMethodError> {
        let mut current_class = class.clone();
        loop {
            let pair = (current_class, method);
            if let Some(fun_type) = self.methods.get(&pair) {
                return Ok(fun_type);
            } else {
                current_class = pair.0;
                method = pair.1;
                match self.classes.get(&current_class).unwrap() {
                    // We've already checked existence of base classes (I hope...)
                    None => return Err(MissingMethodError(method, class)),
                    Some(base_class) => current_class = base_class.clone(),
                }
            }
        }
    }

    // pub(crate) fn topo_sort_classes<'a>(&self, classes: &'a mut [ClassDef]) {
    //     let class_def_set = classes.iter().map(|def| (&def.class, def)).collect::<HashMap<&'a Ident, &'a ClassDef>>();
    //     let leaves = {
    //         let mut leaf_candidates = class_def_set.clone();
    //         for class_def in class_def_set.values() {
    //             if let Some(ref base) = class_def.base_class {
    //                 leaf_candidates.remove(base);
    //             }
    //         }
    //         leaf_candidates
    //     };
    //     let next_level = leaves.values().filter_map(|def| def.base_class.as_ref());
    // }
}
