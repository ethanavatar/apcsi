mod lexer;
mod parser;

use std::env;
use std::fs::File;
use std::io::Read;

fn run_program(program: &Vec<parser::Expr>) {
    let mut i = 0;
    while i < program.len() {
        match &program[i] {
            parser::Expr::Display(expr) => {
                println!("{}", expr.into_int());
            }
            _ => {}
        };
        i += 1;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let mut file = File::open(filename).expect("File not found");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Could not read file");

    let lex_result = lexer::lex(&contents);
    let parse_result = parser::parse(lex_result);
    run_program(&parse_result);
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_assign_identifier() {
        let lex_result = lexer::lex("a <- something");
        let parse_result = parser::parse(lex_result);
        assert_eq!(
            parse_result[0],
            parser::Expr::BinaryOp(
                parser::Operation::Assign,
                Box::new(parser::Expr::Identifier("a".to_string())),
                Box::new(parser::Expr::Identifier("something".to_string()))
            )
        );
    }

    #[test]
    fn test_assign_int_literal() {
        let lex_result = lexer::lex("a <- 1");
        let parse_result = parser::parse(lex_result);
        assert_eq!(
            parse_result[0],
            parser::Expr::BinaryOp(
                parser::Operation::Assign,
                Box::new(parser::Expr::Identifier("a".to_string())),
                Box::new(parser::Expr::Literal(parser::Literal::Int(1)))
            )
        );
    }

    #[test]
    fn test_assign_string_literal() {
        let lex_result = lexer::lex("a <- \"something\"");
        let parse_result = parser::parse(lex_result);
        assert_eq!(
            parse_result[0],
            parser::Expr::BinaryOp(
                parser::Operation::Assign,
                Box::new(parser::Expr::Identifier("a".to_string())),
                Box::new(parser::Expr::Literal(parser::Literal::String("something".to_string())))
            )
        );
    }

    #[test]
    fn test_assign_bool_literal() {
        let lex_result = lexer::lex("a <- true");
        let parse_result = parser::parse(lex_result);
        assert_eq!(
            parse_result[0],
            parser::Expr::BinaryOp(
                parser::Operation::Assign,
                Box::new(parser::Expr::Identifier("a".to_string())),
                Box::new(parser::Expr::Literal(parser::Literal::Bool(true)))
            )
        );
    }

    #[test]
    fn test_parens() {
        let lex_result = lexer::lex("a <- (1 + 2)");
        let parse_result = parser::parse(lex_result);
        assert_eq!(
            parse_result[0],
            parser::Expr::BinaryOp(
                parser::Operation::Assign,
                Box::new(parser::Expr::Identifier("a".to_string())),
                Box::new(parser::Expr::Group(
                    Box::new(parser::Expr::BinaryOp(
                        parser::Operation::Plus,
                        Box::new(parser::Expr::Literal(parser::Literal::Int(1))),
                        Box::new(parser::Expr::Literal(parser::Literal::Int(2)))
                    ))
                ))
            )
        );
    }

    #[test]
    fn test_index() {
        let lex_result = lexer::lex("a <- [1, 1]");
        let parse_result = parser::parse(lex_result);
        assert_eq!(
            parse_result[0],
            parser::Expr::BinaryOp(
                parser::Operation::Assign,
                Box::new(parser::Expr::Identifier("a".to_string())),
                Box::new(parser::Expr::Literal(parser::Literal::List(vec![
                    parser::Expr::Literal(parser::Literal::Int(1)),
                    parser::Expr::Literal(parser::Literal::Int(1))
                ])))
            )
        );
    }

    #[test]
    fn test_display() {
        let lex_result = lexer::lex("DISPLAY 1 + 1");
        let parse_result = parser::parse(lex_result);
        assert_eq!(
            parse_result[0],
            parser::Expr::Display(
                Box::new(parser::Expr::BinaryOp(
                    parser::Operation::Plus,
                    Box::new(parser::Expr::Literal(parser::Literal::Int(1))),
                    Box::new(parser::Expr::Literal(parser::Literal::Int(1)))
                ))
            )
        );
    }
}