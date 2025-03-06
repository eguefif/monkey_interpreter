use std::fmt;

use crate::object::{Int, Object, ObjectType};

#[derive(Debug, PartialEq)]
pub enum BuiltinType {
    Len,
    First,
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinType::Len => write!(f, "len"),
            BuiltinType::First => write!(f, "first"),
        }
    }
}
pub fn len(str: String) -> usize {
    str.len()
}

pub fn is_builtin(name: &str) -> bool {
    match name {
        "len" => true,
        "first" => true,
        _ => false,
    }
}

pub fn make_builtin(name: &str) -> Object {
    match name {
        "len" => Object::new(ObjectType::BuiltIn(BuiltinType::Len)),
        "first" => Object::new(ObjectType::BuiltIn(BuiltinType::First)),
        _ => Object::new(ObjectType::Null),
    }
}

pub fn evaluate_builtin(name: BuiltinType, args: Vec<Object>) -> Result<Object, String> {
    match name {
        BuiltinType::Len => apply_len(args),
        BuiltinType::First => apply_first(args),
    }
}

fn apply_len(args: Vec<Object>) -> Result<Object, String> {
    if args.len() > 1 {
        return Err(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match args[0].obj_type {
        ObjectType::Str(ref value) => Ok(Object::new(ObjectType::Int(Int {
            value: value.value.len() as i128,
        }))),
        ObjectType::Array(ref value) => Ok(Object::new(ObjectType::Int(Int {
            value: value.elements.len() as i128,
        }))),
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

        ObjectType::Let(_) => return Err("argument to 'len' not supported, got LET".to_string()),

        ObjectType::Function(_) => {
            return Err("argument to 'len' not supported, got FUNCTION".to_string())
        }

        ObjectType::Null => return Err("argument to 'len' not supported, got NULL".to_string()),
    }
}

fn apply_first(args: Vec<Object>) -> Result<Object, String> {
    if args.len() > 1 || args.len() == 0 {
        return Err(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match args[0].obj_type {
        ObjectType::Array(ref value) => {
            if value.elements.len() == 0 {
                return Err("Out of bound, the array is empty".to_string());
            }
            let obj = &value.elements[0];
            Ok(Object::new_from(&obj))
        }
        ObjectType::Str(_) => {
            return Err("argument to 'first' not supported, got String".to_string())
        }
        ObjectType::Int(_) => {
            return Err("argument to 'first' not supported, got INTEGER".to_string())
        }

        ObjectType::BuiltIn(_) => {
            return Err("argument to 'first' not supported, got BUILTIN".to_string())
        }

        ObjectType::Bool(_) => {
            return Err("argument to 'first' not supported, got BOOLEAN".to_string())
        }
        ObjectType::Return(_) => {
            return Err("argument to 'first' not supported, got RETURN".to_string())
        }

        ObjectType::Let(_) => return Err("argument to 'first' not supported, got LET".to_string()),

        ObjectType::Function(_) => {
            return Err("argument to 'first' not supported, got FUNCTION".to_string())
        }

        ObjectType::Null => return Err("argument to 'first' not supported, got NULL".to_string()),
    }
}
