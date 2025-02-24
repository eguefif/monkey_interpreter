use std::fmt;

pub enum ObjectType {
    Int(Int),
    Str(Str),
    Bool(Bool),
    Null(Null),
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjectType::Int(value) => write!(f, "Integer: {}", value),
            ObjectType::Str(value) => write!(f, "String: {}", value),
            ObjectType::Bool(value) => write!(f, "Boolean: {}", value),
            ObjectType::Null(value) => write!(f, "Null"),
        }
    }
}

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

pub struct Int {
    pub value: i64,
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct Str {
    pub value: String,
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Integer: {}", self.value)
    }
}

pub struct Bool {
    pub value: bool,
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Boolean: {}", self.value)
    }
}

pub struct Null {}

impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Null")
    }
}
