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
    Identifier(Token),
}

impl Expr {
    pub fn accept(&self, visitor: &mut impl ExprVisitor) -> InterpreterValue {
        match self {
            e @ Expr::Binary(_, _, _) => visitor.visit_binary(e),
            e @ Expr::Grouping(_) => visitor.visit_grouping(e),
            e @ Expr::Literal(_) => visitor.visit_literal(e),
            e @ Expr::Unary(_, _) => visitor.visit_unary(e),
            e @ Expr::Identifier(_) => visitor.visit_identifier(e),
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
            
            let decl = self.declaration();
            decl.is_some()
                .then(|| out.push(decl.unwrap()));
        }
    }

    fn peek_next(&self) -> &Token {
        &self.tokens[self.current + 1]
    }

    fn declaration(&mut self) -> Option<Statement> {

        // match_token has an implicit advancement
        if self.match_token(&vec![TokenId::Newline]) {
            return None;
        }

        if let TokenId::Comment(_) = self.peek().id {
            self.advance();
            return None;
        }

        let current = self.peek().clone();
        if let tok @ Token { id: TokenId::Name(_), .. } = current {

            let name = self.consume(tok.clone().id, "Expected variable name.").clone();

            if !(self.peek_next().id == TokenId::Assign) {
                
                let err = format!("Expected '<-' after variable name '{}'. ({}, {})", name.lexeme, name.line, name.column);
                self.consume(TokenId::Assign, &err);
                let decl = self.variable_declaration(&name);
                return Some(decl)

            }

        }

        Some(self.statement())

        //self.synchronize();
        //None
    }

    fn variable_declaration(&mut self, name: &Token) -> Statement {
    
        let initializer = self.expression();

        let fail_msg = format!("Expected '\\n' after variable declaration. Found: {:?} instead", self.peek());
        self.consume(TokenId::Newline, &fail_msg);

        Statement::VariableDecl(name.clone(), initializer)
    }

    fn statement(&mut self) -> Statement {
        if self.match_token(&vec![TokenId::Display]) {
            return self.display_statement();
        }

        if self.match_token(&vec![TokenId::LBrace]) {
            return self.block_statement();
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
        
        let fail_msg = format!("Expected '\\n' or `Eof` after display statement. Found: {:?} instead", self.peek());
        self.consume_either(&vec![TokenId::Newline, TokenId::Eof], &fail_msg);

        Statement::Display(value)
    }

    fn block_statement(&mut self) -> Statement {
        let mut statements = Vec::new();
        while !self.check(&TokenId::RBrace) && !self.is_at_end() {

            let decl = self.declaration();
            decl.is_some()
                .then(|| statements.push(decl.unwrap()));
        }

        self.consume(TokenId::RBrace, "Expected '}' after block.");

        Statement::Block(statements)
    }

    pub fn synchronize(&mut self) -> () {
        self.advance();

        while !self.is_at_end() {
            if self.previous().id == TokenId::Newline {
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
        if self.current >= self.tokens.len() {
            panic!("Parser::peek() called when current >= tokens.len()");
        }

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

    fn consume_either(&mut self, id: &Vec<TokenId>, message: &str) -> &Token {
        for i in id {
            if self.check(i) {
                return self.advance();
            }
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

        if let TokenId::Name(_) = self.peek().id {
            return Expr::Identifier(self.advance().clone());
        }

        let err = format!("Expect expression. Found: {:?} instead", self.peek());
        panic!("{}", err);
    }
}
