use std::fmt;

use crate::object::{Int, Object, ObjectType};

#[derive(Debug, PartialEq)]
pub enum BuiltinType {
    Len,
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinType::Len => write!(f, "len"),
        }
    }
}
pub fn len(str: String) -> usize {
    str.len()
}

pub fn is_builtin(name: &str) -> bool {
    match name {
        "len" => true,
        _ => false,
    }
}

pub fn make_builtin(name: &str) -> Object {
    match name {
        "len" => Object::new(ObjectType::BuiltIn(BuiltinType::Len)),
        _ => Object::new(ObjectType::Null),
    }
}

pub fn evaluate_builtin(name: BuiltinType, args: Vec<Object>) -> Result<Object, String> {
    match name {
        BuiltinType::Len => apply_len(args),
    }
}

fn apply_len(args: Vec<Object>) -> Result<Object, String> {
    if args.len() > 1 {
        return Err(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    if let ObjectType::Str(ref value) = args[0].obj_type {
        Ok(Object::new(ObjectType::Int(Int {
            value: value.value.len() as i128,
        })))
    } else {
        match args[0].obj_type {
            ObjectType::Int(_) => {
                return Err("argument to 'len' not supported, got INTEGER".to_string())
            }

            ObjectType::BuiltIn(_) => {
                return Err("argument to 'len' not supported, got BUILTIN".to_string())
            }

            ObjectType::Bool(_) => {
                return Err("argument to 'len' not supported, got BOOLEAN".to_string())
            }
            ObjectType::Return(_) => {
                return Err("argument to 'len' not supported, got RETURN".to_string())
            }

            ObjectType::Let(_) => {
                return Err("argument to 'len' not supported, got LET".to_string())
            }

            ObjectType::Function(_) => {
                return Err("argument to 'len' not supported, got FUNCTION".to_string())
            }

            ObjectType::Null => return Err("argument to 'len' not supported, got NULL".to_string()),
            ObjectType::Str(_) => return Err("IMPOSSIBLE to have a string here".to_string()),
        }
    }
}
