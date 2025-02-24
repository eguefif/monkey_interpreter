use std::fmt;

enum ObjectType {
    Int(i64),
    Str(String),
    Bool(bool),
    Null,
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjectType::Int(value) => write!(f, "Integer: {}", value),
            ObjectType::Str(value) => write!(f, "String: {}", value),
            ObjectType::Bool(value) => write!(f, "Boolean: {}", value),
            ObjectType::Null => write!(f, "Null"),
        }
    }
}

struct Object {
    obj_type: ObjectType,
    inspect: String,
}

impl Object {
    pub fn new(obj_type: ObjectType) -> Self {
        Self {
            obj_type,
            inspect: "5".to_string(),
        }
    }
}
