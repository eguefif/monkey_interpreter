use std::fmt;

use crate::{
    environment::Environment,
    parser::ast_types::{BlockStatement, Identifier},
};

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Int(Int),
    Str(Str),
    Bool(BoolObject),
    Return(Box<Object>),
    Let(Box<Variable>),
    Function(Func),
    Null,
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjectType::Int(value) => write!(f, "{}", value),
            ObjectType::Str(value) => write!(f, "{}", value),
            ObjectType::Bool(value) => write!(f, "{}", value),
            ObjectType::Return(value) => write!(f, "{}", value),
            ObjectType::Let(value) => write!(f, "{}", value),
            ObjectType::Function(value) => write!(f, "{}", value),
            ObjectType::Null => write!(f, "Null"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub value: Object,
    pub name: String,
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Object {
    pub obj_type: ObjectType,
    pub inspect: String,
}

impl Object {
    pub fn new(obj_type: ObjectType) -> Self {
        let inspect = format!("{obj_type}");
        Self { obj_type, inspect }
    }

    pub fn new_from(obj: &Object) -> Self {
        match &obj.obj_type {
            ObjectType::Int(value) => Self {
                inspect: obj.inspect.clone(),
                obj_type: ObjectType::Int(Int { value: value.value }),
            },
            ObjectType::Str(value) => Self {
                inspect: obj.inspect.clone(),
                obj_type: ObjectType::Str(Str {
                    value: value.value.clone(),
                }),
            },
            ObjectType::Bool(value) => Self {
                inspect: obj.inspect.clone(),
                obj_type: ObjectType::Bool(BoolObject { value: value.value }),
            },
            _ => Self {
                inspect: "null".to_string(),
                obj_type: ObjectType::Null,
            },
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect)
    }
}

#[derive(Debug, PartialEq)]
pub struct Int {
    pub value: i128,
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Str {
    pub value: String,
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct BoolObject {
    pub value: bool,
}

impl fmt::Display for BoolObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Func {
    params: Vec<Identifier>,
    body: BlockStatement,
    env: Environment,
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut params = String::new();
        for (i, param) in self.params.iter().enumerate() {
            if i == self.params.len() - 1 {
                params.push_str(format!("{}", param.token.litteral).as_str());
            } else {
                params.push_str(format!("{}, ", param.token.litteral).as_str());
            }
        }
        write!(f, "fn ({}) {{\n{}\n}}", params, self.body)
    }
}
