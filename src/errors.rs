use std::fmt::Display;


#[derive(Debug, Clone)]
pub enum RuntimeError {
    ValueError(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::ValueError(msg) => write!(f, "ValueError: {}", msg),
        }
    }
}