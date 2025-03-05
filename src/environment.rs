use crate::builtin::{is_builtin, make_builtin};
use crate::object::{Object, Variable};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Environment {
    pub variables: HashMap<String, Object>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            outer: None,
        }
    }

    pub fn from_env(env: Rc<RefCell<Environment>>) -> Self {
        Self {
            variables: HashMap::new(),
            outer: Some(env.clone()),
        }
    }

    pub fn push(&mut self, var: Variable) {
        self.variables.insert(var.name.clone(), var.value);
    }

    pub fn display(&self) {
        for (name, obj) in self.variables.iter() {
            println!("{:?}: {:?}", name, obj);
        }
    }

    pub fn get_variable(&self, name: &str) -> Result<Object, String> {
        if let Some(value) = self.variables.get(name) {
            let obj = Object::new_from(value);
            return Ok(obj);
        } else {
            if let Some(ref outer) = self.outer {
                if let Ok(value) = outer.borrow().get_variable(name) {
                    let obj = Object::new_from(&value);
                    return Ok(obj);
                } else if is_builtin(name) {
                    return Ok(make_builtin(name));
                } else {
                    Err(format!("Unknown identifier: {name}"))
                }
            } else if is_builtin(name) {
                return Ok(make_builtin(name));
            } else {
                Err(format!("Unknown identifier: {name}"))
            }
        }
    }
}
