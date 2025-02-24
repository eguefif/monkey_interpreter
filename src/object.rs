use std::fmt;

enum ObjectType {
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

struct Int {
    value: i64,
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

struct Str {
    value: String,
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Integer: {}", self.value)
    }
}

struct Bool {
    value: bool,
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Boolean: {}", self.value)
    }
}

struct Null {}

impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Null")
    }
}
