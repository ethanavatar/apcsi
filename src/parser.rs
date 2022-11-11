use std::fmt::Display;

use crate::scanner::Token;
use crate::scanner::TokenId;

use crate::interpreter::ExprVisitor;
use crate::interpreter::InterpreterValue;
use crate::statement::Statement;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Token),
    Unary(Token, Box<Expr>),
}

impl Expr {
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> InterpreterValue {
        match self {
            e @ Expr::Binary(_, _, _) => visitor.visit_binary(e),
            e @ Expr::Grouping(_) => visitor.visit_grouping(e),
            e @ Expr::Literal(_) => visitor.visit_literal(e),
            e @ Expr::Unary(_, _) => visitor.visit_unary(e),
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
        }
    }
}
pub struct Parser {
    tokens: Vec<Token>,
    current: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0
        }
    }

    pub fn parse(&mut self, out: &mut Vec<Statement>) {
        while !self.is_at_end() {
            out.push(self.statement());
        }
    }

    fn statement(&mut self) -> Statement {
        if self.match_token(&vec![TokenId::Display]) {
            return self.display_statement();
        }

        return self.expression_statement();
    }

    fn expression_statement(&mut self) -> Statement {
        let expr = self.expression();
        self.consume(TokenId::Newline, "Expected '\\n' after expression.");
        Statement::Expression(expr)
    }

    fn display_statement(&mut self) -> Statement {
        let value = self.expression();
        self.consume(TokenId::Newline, "Expected '\\n' after value.");
        Statement::Display(value)
    }

    pub fn synchronize(&mut self) -> () {
        self.advance();

        while !self.is_at_end() {
            if self.previous().id == TokenId::Semicolon {
                return;
            }

            match self.peek().id {
                TokenId::Procedure
                | TokenId::ForEach
                | TokenId::If
                | TokenId::Repeat
                | TokenId::Display
                | TokenId::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek().id == TokenId::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&mut self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, id: &TokenId) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().id == *id
    }

    fn match_token(&mut self, token_ids: &Vec<TokenId>) -> bool {
        for id in token_ids {
            if self.check(&id) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, id: TokenId, message: &str) -> &Token {
        if self.check(&id) {
            return self.advance();
        }
        panic!("{}", message);
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while self.match_token(&vec![TokenId::BangEqual, TokenId::Equal]) {
            let operator = self.previous().clone();
            let right = self.comparison().clone();
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self.match_token(&vec![TokenId::Greater, TokenId::GreaterEqual, TokenId::Less, TokenId::LessEqual]) {
            let operator = self.previous().clone();
            let right = self.term().clone();
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while self.match_token(&vec![TokenId::Minus, TokenId::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor().clone();
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while self.match_token(&vec![TokenId::Slash, TokenId::Star]) {
            let operator = self.previous().clone();
            let right = self.unary().clone();
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        if self.match_token(&vec![TokenId::Bang, TokenId::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary().clone();
            return Expr::Unary(operator, Box::new(right));
        }

        self.primary()
    }

    fn primary(&mut self) -> Expr {
        if self.match_token(&vec![TokenId::False])
            || self.match_token(&vec![TokenId::True])
            || self.match_token(&vec![TokenId::None]) {
            return Expr::Literal(self.previous().clone());
        }

        if let TokenId::Number(_) = self.peek().id {
            return Expr::Literal(self.advance().clone());
        }

        if let TokenId::String(_) = self.peek().id {
            return Expr::Literal(self.advance().clone());
        }

        if self.match_token(&vec![TokenId::LParen]) {
            let expr = self.expression();
            self.consume(TokenId::RParen, "Expect ')' after expression.");
            return Expr::Grouping(Box::new(expr.clone()));
        }

        panic!("Expected expression.");
    }
}
