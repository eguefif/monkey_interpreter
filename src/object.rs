use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Int(Int),
    Str(Str),
    Bool(BoolObject),
    Return(Box<Object>),
    Let(Box<Variable>),
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

/*
#[derive(Debug, PartialEq)]
pub struct Null {}

impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Null")
    }
}
*/
