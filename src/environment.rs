use crate::interpreter::InterpreterValue;

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    pub values: HashMap<String, InterpreterValue>,
    pub parent: Option<Box<Environment>>,
}

impl Environment {

    /// Creates a new scope
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    /// Creates a new scope with a parent scope
    pub fn new_with_parent(enclosing: Environment) -> Self {
        Self {
            values: HashMap::new(),
            parent: Some(Box::new(enclosing)),
        }
    }

    pub fn get_parent(&self) -> Option<&Environment> {
        match &self.parent {
            Some(parent) => Some(parent),
            None => None,
        }
    }

    pub fn is_defined(&self, name: &str) -> bool {
       let res =  self.parent
            .as_ref()
            .map(|parent| parent.is_defined(name))
            .unwrap_or(false)
            || self.values.contains_key(name);

        res
    }

    /// Returns the value of the variable with the given name.
    /// 
    /// Returns `None` if the variable is not defined in this environment.
    pub fn get(&self, name: &str) -> Option<&InterpreterValue> {
        self.parent.as_ref()
            .and_then(|parent| parent.get(name))
            .or_else(|| self.values.get(name))
    }

    /// Defines a new variable in the environment with the given name and value.
    pub fn define(&mut self, name: &str, value: &InterpreterValue) {
        self.values.insert(name.to_string(), value.clone());
    }

    /// Changes the value of an existing variable in the environment.
    /// 
    /// Returns ok if the variable exists and the value was changed.
    /// 
    /// Returns an error if the variable does not exist.
    pub fn set(&mut self, name: &str, value: &InterpreterValue) -> Result<(), String> {
        if !self.values.contains_key(name) {
            self.parent.as_mut()
                .map(|parent| parent.set(name, value))
                .unwrap_or(Err(format!("Undefined variable '{}'.", name)))
        } else {
            self.values.insert(name.to_string(), value.clone());
            Ok(())
        }
    }
}