use crate::environment::Environment;
use crate::interpreter::InterpreterValue;
use crate::scanner::Token;

use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Token),
    ListLiteral(Vec<Expr>),
    Unary(Token, Box<Expr>),
    Identifier(Token),
    Call(Token, Vec<Expr>),
    Get(Token, Box<Expr>),
}

pub trait ExprVisitor {
    fn visit_literal(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue;
    fn visit_grouping(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue;
    fn visit_unary(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue;
    fn visit_binary(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue;
    fn visit_identifier(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue;
    fn visit_call(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue;
    fn visit_list_literal(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue;
    fn visit_get(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue;
}

impl Expr {
    pub fn accept(&self, visitor: &mut impl ExprVisitor, env: &Environment) -> InterpreterValue {
        match self {
            e @ Expr::Binary(_, _, _) => visitor.visit_binary(e, env),
            e @ Expr::Grouping(_) => visitor.visit_grouping(e, env),
            e @ Expr::Literal(_) => visitor.visit_literal(e, env),
            e @ Expr::Unary(_, _) => visitor.visit_unary(e, env),
            e @ Expr::Identifier(_) => visitor.visit_identifier(e, env),
            e @ Expr::Call(_, _) => visitor.visit_call(e, env),
            e @ Expr::ListLiteral(_) => visitor.visit_list_literal(e, env),
            e @ Expr::Get(_, _) => visitor.visit_get(e, env),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(left, operator, right) => {
                write!(f, "({} {} {})", operator.lexeme, left, right)
            }
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
            Expr::Literal(token) => write!(f, "{}", token.lexeme),
            Expr::Unary(operator, expr) => write!(f, "({} {})", operator.lexeme, expr),
            Expr::Identifier(token) => unimplemented!("Environment lookup for {}", token.lexeme),
            Expr::Call(token, args) => {
                write!(f, "{}(", token.lexeme)?;
                for arg in args {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ")")
            }
            Expr::ListLiteral(exprs) => {
                write!(f, "[")?;
                for expr in exprs {
                    write!(f, "{}, ", expr)?;
                }
                write!(f, "]")
            }
            Expr::Get(token, expr) => write!(f, "{}[{}]", token.lexeme, expr),
        }
    }
}