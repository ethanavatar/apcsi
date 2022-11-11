use std::collections::HashMap;

use crate::interpreter::InterpreterValue;

pub struct Environment {
    pub values: HashMap<String, InterpreterValue>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&InterpreterValue> {
        self.values.get(name)
    }

    pub fn define(&mut self, name: &str, value: InterpreterValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn set(&mut self, name: &str, value: InterpreterValue) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            return Ok(());
        }

        let err = format!("Undefined variable '{}'.", name);
        Err(err)
    }
}