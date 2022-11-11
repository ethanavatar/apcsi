use crate::scanner::Token;
use crate::scanner::TokenId;

#[derive(Debug, Clone)]
enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Token),
    Unary(Token, Box<Expr>),
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

    fn is_at_end(&self) -> bool {
        self.peek().id == TokenId::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
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
            let operator = self.previous();
            let right = self.comparison();
            expr = Expr::Binary(Box::new(expr.clone()), operator.clone(), Box::new(right.clone()));
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self.match_token(&vec![TokenId::Greater, TokenId::GreaterEqual, TokenId::Less, TokenId::LessEqual]) {
            let operator = self.previous();
            let right = self.term();
            expr = Expr::Binary(Box::new(expr.clone()), operator.clone(), Box::new(right.clone()));
        }

        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while self.match_token(&vec![TokenId::Minus, TokenId::Plus]) {
            let operator = self.previous();
            let right = self.factor();
            expr = Expr::Binary(Box::new(expr.clone()), operator.clone(), Box::new(right.clone()));
        }

        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while self.match_token(&vec![TokenId::Slash, TokenId::Star]) {
            let operator = self.previous();
            let right = self.unary();
            expr = Expr::Binary(Box::new(expr.clone()), operator.clone(), Box::new(right.clone()));
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        if self.match_token(&vec![TokenId::Bang, TokenId::Minus]) {
            let operator = self.previous();
            let right = self.unary();
            return Expr::Unary(operator.clone(), Box::new(right.clone()));
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

    pub fn parse_tree(&mut self, out: &mut Vec<Expr>) {
        while !self.is_at_end() {
            out.push(self.expression());
        }
    }
}