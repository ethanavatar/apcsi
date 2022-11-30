use crate::environment::Environment;
use crate::statement::Statement;

#[derive(Clone, Debug)]
pub enum InterpreterValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(String, Statement, Vec<String>, Environment),
    List(Vec<InterpreterValue>),
    None,
}

impl InterpreterValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            InterpreterValue::Number(n) => *n != 0.0,
            InterpreterValue::String(s) => !s.is_empty(),
            InterpreterValue::Boolean(b) => *b,
            InterpreterValue::Function(_, _, _, _) => true, // This might be the wrong thing to do
            InterpreterValue::List(l) => !l.is_empty(),
            InterpreterValue::None => false,

            _ => unreachable!(),
        }
    }

    pub fn is_equal(&self, other: &InterpreterValue) -> bool {
        match (self, other) {
            (InterpreterValue::Number(n1), InterpreterValue::Number(n2)) => n1 == n2,
            (InterpreterValue::String(s1), InterpreterValue::String(s2)) => s1 == s2,
            (InterpreterValue::Boolean(b1), InterpreterValue::Boolean(b2)) => b1 == b2,
            (InterpreterValue::Function(_, _, _, _), InterpreterValue::Function(_, _, _, _)) => {
                // what if they're the same function?
                false
            }
            (InterpreterValue::List(_a), InterpreterValue::List(_b)) => {
                unimplemented!("Cannot compare lists")
            },
            (InterpreterValue::None, InterpreterValue::None) => true,
            _ => false,
        }
    }

    pub fn is_indexable(&self) -> bool {
        match self {
            InterpreterValue::String(_) => true,
            InterpreterValue::List(_) => true,
            _ => false,
        }
    }

    pub fn get(&self, i: usize) -> Option<InterpreterValue> {
        if self.is_indexable() {
            match self {
                InterpreterValue::String(s) => s.chars().nth(i).map(|c| InterpreterValue::String(c.to_string())),
                InterpreterValue::List(l) => l.get(i).cloned(),
                _ => None,
            }
        } else {
            panic!("Cannot index non-indexable value");
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            InterpreterValue::Number(n) => n.to_string(),
            InterpreterValue::String(s) => s.clone(),
            InterpreterValue::Boolean(b) => {
                match b {
                    true => "TRUE".to_string(),
                    false => "FALSE".to_string(),
                }
            },
            InterpreterValue::None => "NONE".to_string(),
            InterpreterValue::Function(n, _, p, _) => format!("<fn `{}`>({})", n, p.iter().map(|t| t.clone()).collect::<Vec<String>>().join(", ")),
            InterpreterValue::List(l) => format!("[{}]", l.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")),

            _ => unreachable!(),
        }
    }
}