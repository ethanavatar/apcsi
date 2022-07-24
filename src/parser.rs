use crate::lexer::Symbols;

#[derive(Debug, PartialEq)]
pub enum Operation {
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Compare(Symbols)
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    List(Vec<Expr>)
}

impl Literal {
    pub fn eval(&self) -> i32 {
        match self {
            Literal::Int(i) => *i,
            _ => 0
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    BinaryOp(Operation, Box<Expr>, Box<Expr>),
    UnaryOp(String, Box<Expr>),
    Call(String),
    Group(Box<Expr>),
    Index(Box<Expr>),
    Block(Vec<Expr>),

    Display(Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    RepeatNTimes(Box<Expr>, Box<Expr>),
    RepeatUntil(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn eval(&self) -> i32 {
        match self {
            Expr::Literal(literal) => literal.eval(),
            //Expr::Identifier(identifier) => identifier,
            Expr::BinaryOp(operation, left, right) => {
                let left_eval = left.eval();
                let right_eval = right.eval();
                match operation {
                    Operation::Plus => left_eval + right_eval,
                    Operation::Minus => left_eval - right_eval,
                    Operation::Star => left_eval * right_eval,
                    Operation::Slash => left_eval / right_eval,
                    _ => unimplemented!()
                    //Operation::Compare(symbol) => format!("{} {} {}", left_eval, symbol.to_string(), right_eval)
                }
            }
            
            Expr::Display(expr) => expr.eval(),
            _ => unimplemented!()
        }
    }
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
        Symbols::Identifier(ident) => match ident.as_str() {
            "true" | "false" => Expr::Literal(Literal::Bool(ident.parse::<bool>().unwrap())),
            _ => Expr::Identifier(ident.to_string())
        },
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

fn parse_list(tokens: &Vec<Symbols>, index: usize) -> Option<Vec<Expr>> {
    let mut result: Vec<Expr> = Vec::new();
    let mut i = index + 1;
    while i < tokens.len() {
        match &tokens[i] {
            Symbols::Comma => {
                result.push(parse_step(tokens, i - 1));
                println!("comma: {:?}", result);
                i += 1;
            }
            _ => {
                result.push(parse_step(tokens, i));
                println!("other: {:?}", result);
                i += 1;
            }
        }
    }
    Some(result)
}

fn descend(tokens: &Vec<Symbols>, index: usize) -> Option<Expr> {
    match tokens.get(index) {
        Some(Symbols::Assign)
        | Some(Symbols::Plus)
        | Some(Symbols::Minus)
        | Some(Symbols::Star)
        | Some(Symbols::Slash)
        | Some(Symbols::Greater)
        | Some(Symbols::GreaterEqual)
        | Some(Symbols::Less)
        | Some(Symbols::LessEqual)
        | Some(Symbols::NotEqual)
        | Some(Symbols::Equal) => parse_binary_op(tokens, index),
        Some(Symbols::LParen) => {
            let mut paren_count = 1;
            let mut i = index + 1;
            let mut inner: Vec<Symbols> = Vec::new();
            while paren_count > 0 {
                match tokens[i] {
                    Symbols::LParen => paren_count += 1,
                    Symbols::RParen => paren_count -= 1,
                    _ => {}
                };
                if paren_count > 0 {
                    inner.push(tokens[i].clone());
                    i += 1;
                }                
            }

            let mut i2 = index + 1;
            let mut result: Option<Expr> = None;
            while match descend(&tokens, i2 + 1) {
                Some(expr) => {result = Some(expr); false},
                None => {true}
            } { i2 += 1; }
            match result {
                Some(expr) => Some(Expr::Group(Box::new(expr))),
                None => panic!("Invalid grouping at position {}", index)
            }
        },
        Some(Symbols::LBrace) => {
            let mut paren_count = 1;
            let mut i = index + 1;
            let mut inner: Vec<Symbols> = Vec::new();
            while paren_count > 0 {
                match tokens[i] {
                    Symbols::LBrace => paren_count += 1,
                    Symbols::RBrace => paren_count -= 1,
                    _ => {}
                };
                if paren_count > 0 {
                    inner.push(tokens[i].clone());
                    i += 1;
                }
            }

            Some(Expr::Block(parse(inner)))
        },
        Some(Symbols::LBracket) => {
            let mut paren_count = 1;
            let mut i = index + 1;
            let mut inner: Vec<Symbols> = Vec::new();
            while paren_count > 0 {
                match tokens[i] {
                    Symbols::LBracket => paren_count += 1,
                    Symbols::RBracket => paren_count -= 1,
                    _ => {}
                };
                if paren_count > 0 {
                    inner.push(tokens[i].clone());
                    i += 1;
                }
            }

            let contents = match parse_list(&inner, 0) {
                Some(list) => list,
                None => panic!("Invalid list at position {}", index)
            };
            Some(Expr::Literal(Literal::List(contents)))
        },
        Some(Symbols::Identifier(ident)) => {
            match ident.as_str() {
                "DISPLAY" => {
                    let mut i2 = index + 1;
                    let mut operand: Option<Expr> = None;
                    while match descend(&tokens, i2 + 1) {
                        Some(expr) => {operand = Some(expr); false},
                        None => {true}
                    } { i2 += 1; }
                    match operand {
                        Some(expr) => Some(Expr::Display(Box::new(expr))),
                        None => panic!("Invalid grouping at position {}", index)
                    }
                },
                "RANDOM"

                | "NOT"
                | "AND"
                | "OR"

                | "IF"
                | "ELSE"
                | "REPEAT"
                | "UNTIL"
                | "TIMES"

                | "INSERT"
                | "APPEND"
                | "REMOVE"
                | "LENGTH"

                | "FOR"
                | "EACH"
                | "IN"

                | "PROCEDURE"
                | "RETURN" => {
                    todo!()
                }
                _ => {
                    None
                }
            }
        }
        _ => None
    }
}