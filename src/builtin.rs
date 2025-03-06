use std::fmt;

use crate::object::{Array, Int, Object, ObjectType};

#[derive(Debug, PartialEq)]
pub enum BuiltinType {
    Len,
    First,
    Last,
    Rest,
    Push,
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinType::Len => write!(f, "len"),
            BuiltinType::First => write!(f, "first"),
            BuiltinType::Last => write!(f, "last"),
            BuiltinType::Rest => write!(f, "rest"),
            BuiltinType::Push => write!(f, "push"),
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
        "last" => true,
        "rest" => true,
        "push" => true,
        _ => false,
    }
}

pub fn make_builtin(name: &str) -> Object {
    match name {
        "len" => Object::new(ObjectType::BuiltIn(BuiltinType::Len)),
        "first" => Object::new(ObjectType::BuiltIn(BuiltinType::First)),
        "last" => Object::new(ObjectType::BuiltIn(BuiltinType::Last)),
        "rest" => Object::new(ObjectType::BuiltIn(BuiltinType::Rest)),
        "push" => Object::new(ObjectType::BuiltIn(BuiltinType::Push)),
        _ => Object::new(ObjectType::Null),
    }
}

pub fn evaluate_builtin(name: BuiltinType, args: Vec<Object>) -> Result<Object, String> {
    match name {
        BuiltinType::Len => apply_len(args),
        BuiltinType::First => apply_first(args),
        BuiltinType::Last => apply_last(args),
        BuiltinType::Rest => apply_rest(args),
        BuiltinType::Push => apply_push(args),
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

fn apply_last(args: Vec<Object>) -> Result<Object, String> {
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
            let obj = &value
                .elements
                .last()
                .expect("Out of bound, the array is empty");
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

fn apply_rest(args: Vec<Object>) -> Result<Object, String> {
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
            let mut rest = Vec::new();
            let mut it = value.elements.iter();
            it.next();
            for element in it {
                rest.push(Object::new_from(&element));
            }
            Ok(Object::new(ObjectType::Array(Array { elements: rest })))
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

fn apply_push(args: Vec<Object>) -> Result<Object, String> {
    if args.len() != 2 {
        return Err(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match args[0].obj_type {
        ObjectType::Array(ref value) => {
            let mut new_array = Vec::new();
            for element in value.elements.iter() {
                new_array.push(Object::new_from(element))
            }
            new_array.push(Object::new_from(&args[1]));
            Ok(Object::new(ObjectType::Array(Array {
                elements: new_array,
            })))
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
