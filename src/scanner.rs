use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenId {
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
  
    // One or two character tokens.
    Bang, BangEqual,
    Assign, Equal,
    Greater, GreaterEqual,
    Less, LessEqual,
  
    // Literals.
    Name(String), String(String), Number(u32),
  
    // Keywords.
    And, Or, Not, True, False, None,
    Procedure, Repeat, TIMES, UNTIL,
    ForEach, In,
    If, Else, 
    Display, Return,
  
    Newline,
    Eof
}

#[derive(Debug, Clone)]
pub struct Token {
    pub id: TokenId,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    fn new(id: TokenId, lexeme: String, line: usize, column: usize) -> Self {
        Self { id, lexeme, line, column }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub struct Scanner {
    source: String,
    current: usize,
    start: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            current: 0,
            start: 0,
            line: 1,
        }
    }

    /// Returns the next token in the source code.
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap()
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0'
        }

        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn match_string(&mut self) -> TokenId {
        while self.advance() != '"' {
            if self.is_at_end() || self.peek() == '\n' {
                panic!("Unterminated string.");
            }
        }
        let string_value = self.source[self.start + 1..self.current - 1].to_string();
        let id = TokenId::String(string_value);

        id
    }

    fn match_number(&mut self) -> TokenId {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let number_value = self.source[self.start..self.current].to_string();
        let id = TokenId::Number(number_value.parse().unwrap());

        id
    }

    fn match_name(&mut self) -> TokenId {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let name = self.source[self.start..self.current].to_string();
        let id = match name.as_str() {
            "AND" => TokenId::And,
            "OR" => TokenId::Or,
            "NOT" => TokenId::Not,
            "TRUE" => TokenId::True,
            "FALSE" => TokenId::False,
            "NONE" => TokenId::None,
            "PROCEDURE" => TokenId::Procedure,
            "REPEAT" => TokenId::Repeat,
            "TIMES" => TokenId::TIMES,
            "UNTIL" => TokenId::UNTIL,
            "FOREACH" => TokenId::ForEach,
            "IN" => TokenId::In,
            "IF" => TokenId::If,
            "ELSE" => TokenId::Else,
            "DISPLAY" => TokenId::Display,
            "RETURN" => TokenId::Return,
            _ => TokenId::Name(name),
        };

        id
    }

    pub fn scan_tokens(&mut self, out: &mut Vec<Token>) {
        while !self.is_at_end() {
            self.start = self.current;
            let tok = self.scan_token();
            tok.is_some()
                .then(|| out.push(tok.unwrap()));
        }

        out.push(Token::new(TokenId::Eof, "".to_string(), self.line, self.current));
    }

    fn scan_token(&mut self) -> Option<Token> {
        let c = self.advance();
        
        let id = match c {
            '(' => Some(TokenId::LParen),
            ')' => Some(TokenId::RParen),
            '{' => Some(TokenId::LBrace),
            '}' => Some(TokenId::RBrace),
            '[' => Some(TokenId::LBracket),
            ']' => Some(TokenId::RBracket),
            ',' => Some(TokenId::Comma),
            '.' => Some(TokenId::Dot),
            '-' => Some(TokenId::Minus),
            '+' => Some(TokenId::Plus),
            ';' => Some(TokenId::Semicolon),
            '/' => Some(TokenId::Slash),
            '*' => Some(TokenId::Star),
            '!' => Some(if self.match_char('=') {
                TokenId::BangEqual
            } else {
                TokenId::Bang
            }),
            '=' => Some(TokenId::Equal),
            '>' => Some(if self.match_char('=') {
                TokenId::GreaterEqual
            } else {
                TokenId::Greater
            }),

            '<' => Some(
                if self.match_char('=') {
                    TokenId::LessEqual
                } else if self.match_char('-') {
                    TokenId::Assign
                } else {
                    TokenId::Less
                }
            ),

            '"' => Some(self.match_string()),
            n if n.is_numeric() => Some(self.match_number()),
            a if a.is_alphabetic() => Some(self.match_name()),

            ' ' | '\r' | '\t' => None,
            '\n' => {
                self.line += 1;
                Some(TokenId::Newline)
            },
            '\0' => Some(TokenId::Eof),
            _ => None,
        };

        if let Some(id) = id {
            let tok = self.new_token(id, self.source[self.start..self.current].to_string());
            return Some(tok)
        }

        None
    }

    fn new_token(&self, id: TokenId, lexeme: String) -> Token {
        Token::new(id, lexeme, self.line, self.start)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}