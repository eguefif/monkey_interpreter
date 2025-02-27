use crate::object::{Object, Variable};
use std::collections::HashMap;

pub struct Environment {
    pub variables: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn push(&mut self, var: Variable) {
        self.variables.insert(var.name.clone(), var.value);
    }

    pub fn get_variable(&self, name: &String) -> Result<Object, String> {
        if let Some(value) = self.variables.get(name) {
            let obj = Object::new_from(value);
            return Ok(obj);
        } else {
            Err("Unknown identifier".to_string())
        }
    }
}
