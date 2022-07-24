use crate::lexer::Symbols;

#[derive(Debug, PartialEq)]
enum Operation {
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Compare(Symbols)
}

#[derive(Debug, PartialEq)]
enum Literal {
    Int(i32),
    Float(f32),
    String(String),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    BinaryOp(Operation, Box<Expr>, Box<Expr>),
    UnaryOp(String, Box<Expr>),
    Call(String),
    Grouping(Box<Expr>),
}

pub fn parse(tokens: Vec<Symbols>) -> Vec<Expr> {
    let mut result: Vec<Expr> = Vec::new();
    let mut index = 0;
    while index < tokens.len() {
        match descend(&tokens, index) {
            Some(expr) => result.push(expr),
            None => {}
        }
        index += 1;
    }
    result
}

fn parse_step(tokens: &Vec<Symbols>, index: usize) -> Expr {
    match &tokens[index] {
        Symbols::Identifier(ident) => Expr::Identifier(ident.to_string()),
        Symbols::Number(number) => Expr::Literal(Literal::Int(number.parse().unwrap())),
        Symbols::String(string) => Expr::Literal(Literal::String(string.to_string())),
        _ => match descend(tokens, index){
            Some(expr) => expr,
            None => panic!("Cannot assign to the name at position {}", index)
        }
    }
}

fn parse_binary_op(tokens: &Vec<Symbols>, index: usize) -> Option<Expr> {

    let operation_type: Operation = match tokens[index] {
        Symbols::Assign => Operation::Assign,
        Symbols::Plus => Operation::Plus,
        Symbols::Minus => Operation::Minus,
        Symbols::Star => Operation::Star,
        Symbols::Slash => Operation::Slash,
        Symbols::Greater => Operation::Compare(Symbols::Greater),
        Symbols::GreaterEqual => Operation::Compare(Symbols::GreaterEqual),
        Symbols::Less => Operation::Compare(Symbols::Less),
        Symbols::LessEqual => Operation::Compare(Symbols::LessEqual),
        Symbols::NotEqual => Operation::Compare(Symbols::NotEqual),
        Symbols::Equal => Operation::Compare(Symbols::Equal),
        _ => panic!("Invalid binary operation at position {}", index)
    };
    let left: Expr = parse_step(tokens, index - 1);
    let right: Expr = parse_step(tokens, index + 1);
    Some(Expr::BinaryOp(
        operation_type,
        Box::new(left),
        Box::new(right)
    ))
}

fn descend(tokens: &Vec<Symbols>, index: usize) -> Option<Expr> {
    match tokens[index] {
        Symbols::Assign
        | Symbols::Plus
        | Symbols::Minus
        | Symbols::Star
        | Symbols::Slash
        | Symbols::Greater
        | Symbols::GreaterEqual
        | Symbols::Less
        | Symbols::LessEqual
        | Symbols::NotEqual
        | Symbols::Equal => parse_binary_op(tokens, index),
        Symbols::LParen => todo!("parse_grouping"),

        _ => None
    }
}