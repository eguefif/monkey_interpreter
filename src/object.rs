use std::{
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
};

use crate::{
    builtin::BuiltinType,
    parser::ast_types::{BlockStatement, Identifier},
};

#[derive(Debug, PartialEq, Eq)]
pub enum ObjectType {
    BuiltIn(BuiltinType),
    Int(Int),
    Str(Str),
    Array(Array),
    Hash(HashM),
    Bool(BoolObject),
    Return(Box<Object>),
    Let(Box<Variable>),
    Function(Func),
    Null,
}

impl Hash for ObjectType {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            ObjectType::Hash(value) => format!("{}", value).hash(state),
            ObjectType::Array(value) => format!("{}", value).hash(state),
            ObjectType::Int(value) => format!("{}", value).hash(state),
            ObjectType::BuiltIn(value) => format!("{}", value).hash(state),
            ObjectType::Str(value) => format!("{}", value).hash(state),
            ObjectType::Bool(value) => format!("{}", value).hash(state),
            ObjectType::Return(value) => format!("{}", value).hash(state),
            ObjectType::Let(value) => format!("{}", value).hash(state),
            ObjectType::Function(value) => format!("{}", value).hash(state),
            ObjectType::Null => format!("Null").hash(state),
        }
    }
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjectType::Hash(value) => write!(f, "{}", value),
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

#[derive(Debug, PartialEq, Eq)]
pub struct Variable {
    pub value: Object,
    pub name: String,
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
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
            ObjectType::Hash(hash) => {
                let mut elements: HashMap<Object, Object> = HashMap::new();
                for (key, value) in hash.elements.iter() {
                    elements.insert(Object::new_from(key), Object::new_from(value));
                }
                Self {
                    inspect: obj.inspect.clone(),
                    obj_type: ObjectType::Hash(HashM { elements }),
                }
            }
            _ => Self {
                inspect: "null".to_string(),
                obj_type: ObjectType::Null,
            },
        }
    }
}

impl Hash for Object {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.obj_type.hash(state);
        self.inspect.hash(state);
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Int {
    pub value: i128,
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Str {
    pub value: String,
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BoolObject {
    pub value: bool,
}

impl fmt::Display for BoolObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut elements = String::new();
        for e in self.elements.iter() {
            elements.push_str(format!("{}, ", e).as_str());
        }
        elements.pop();
        elements.pop();
        write!(f, "[{}]", elements)
    }
}

#[derive(Debug, Eq)]
pub struct HashM {
    pub elements: HashMap<Object, Object>,
}

impl fmt::Display for HashM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut elements = String::new();
        for (key, value) in self.elements.iter() {
            elements.push_str(format!("{}: {}, ", key, value).as_str());
        }
        elements.pop();
        elements.pop();
        write!(f, "[{}]", elements)
    }
}

impl PartialEq for HashM {
    fn eq(&self, other: &HashM) -> bool {
        let str_self = format!("{}", self);
        let other_self = format!("{}", other);
        str_self == other_self
    }
    fn ne(&self, other: &HashM) -> bool {
        let str_self = format!("{}", self);
        let other_self = format!("{}", other);
        str_self != other_self
    }
}
