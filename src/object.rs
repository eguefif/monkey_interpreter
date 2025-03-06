use std::fmt;

use crate::{
    builtin::BuiltinType,
    parser::ast_types::{BlockStatement, Identifier},
};

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    BuiltIn(BuiltinType),
    Int(Int),
    Str(Str),
    Array(Array),
    Bool(BoolObject),
    Return(Box<Object>),
    Let(Box<Variable>),
    Function(Func),
    Null,
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjectType::Array(value) => write!(f, "{}", value),
            ObjectType::Int(value) => write!(f, "{}", value),
            ObjectType::BuiltIn(value) => write!(f, "{}", value),
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
            ObjectType::Function(value) => {
                let mut new_params: Vec<Identifier> = Vec::new();
                for param in value.params.iter() {
                    new_params.push(param.clone());
                }
                Self {
                    inspect: obj.inspect.clone(),
                    obj_type: ObjectType::Function(Func {
                        params: new_params,
                        body: value.body.clone(),
                    }),
                }
            }
            ObjectType::Array(array) => {
                let mut elements: Vec<Object> = Vec::new();
                for elem in array.elements.iter() {
                    elements.push(Object::new_from(elem));
                }
                Self {
                    inspect: obj.inspect.clone(),
                    obj_type: ObjectType::Array(Array { elements }),
                }
            }
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
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
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

#[derive(Debug, PartialEq)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut elements = String::new();
        for e in self.elements.iter() {
            elements.push_str(format!("{}", e).as_str());
        }
        elements.pop();
        elements.pop();
        write!(f, "[{}]", elements)
    }
}
