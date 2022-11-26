use modulo::Mod;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenId {
    // Single-character tokens.
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
  
    // One or two character tokens.
    Bang, BangEqual,
    Assign, Equal,
    Greater, GreaterEqual,
    Less, LessEqual,
  
    // Literals.
    Name(String), String(String), Number(f64),
  
    // Keywords.
    And, Or, Not, True, False, None,
    Procedure, Repeat, TIMES, UNTIL,
    For, Each, In,
    If, Else, 
    Display, Return,

    Append, Insert, Remove, Length,

    Comment(String),
  
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

    /// Creates a new scanner with the given source code.
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            current: 0,
            start: 0,
            line: 1,
        }
    }

    /// Scans the source code and stores the resulting tokens in the `out` vector.
    pub fn scan_tokens(&mut self, out: &mut Vec<Token>) {
        while !self.is_at_end() {
            self.start = self.current;
            let tok = self.scan_token();
            tok.is_some()
                .then(|| out.push(tok.unwrap()));
        }

        out.push(Token::new(TokenId::Eof, "".to_string(), self.line, self.current));
    }

    /// Returns `true` if the scanner's current position is at the end of the source code.
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Returns the next unread token in the source code and advances the character pointer.
    /// 
    /// If the next position is at the end of the source code, returns a null character (`'\0'`), without advancing the character pointer.
    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0'
        }

        self.current += 1;
        self.source
            .chars()
            .nth(self.current - 1)
            .unwrap()
    }

    /// Returns the character at the current position without advancing the character pointer.
    /// 
    /// If the current position is at the end of the source code, returns a null character (`'\0'`).
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0'
        }
        
        self.source
            .chars()
            .nth(self.current)
            .unwrap()
    }

    /// Returns the character after the current position without advancing the character pointer.
    /// 
    /// If the next position is at the end of the source code, returns a null character (`'\0'`).
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0'
        }

        self.source
            .chars()
            .nth(self.current + 1)
            .unwrap()
    }

    /// Creates a new `Token` with the given `TokenId` and `lexeme`.
    fn new_token(&self, id: TokenId, lexeme: String) -> Token {
        Token::new(id, lexeme, self.line, self.start.modulo(self.line))
    }

    /// Returns `true` if the next character is the same as the given character, then advances the character pointer.
    /// 
    /// else, returns `false` without advancing the character pointer.
    /// 
    /// If the next position is at the end of the source code, returns `false` without advancing the character pointer.
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

    /// Called when a quote is found. Advances the character pointer until the next quote is found.
    ///
    /// Returns a `TokenId::String(String)` that contains the string literal found between the quotes.
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

    /// Called when a number is found. Advances the character pointer until the next character is not a digit or a decimal point.
    /// 
    /// Returns a `TokenId::Number(u32)` that contains the number literal found.
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

    /// Called when a name is found. Advances the character pointer until the next character is not a letter, digit, or underscore.
    /// 
    /// Returns a `TokenId::Name(String)` that contains the name found.
    /// 
    /// Alternatively, if the name is a reserved keyword, returns the corresponding `TokenId`.
    /// ```rust
    /// enum TokenId {
    ///     ...
    ///     // Keywords.
    ///     AND, OR, NOT, TRUE, FALSE, NONE,
    ///     PROCEDURE, REPEAT, TIMES, UNTIL,
    ///     FOREACH, IN,
    ///     IF, ELSE, 
    ///     DISPLAY, RETURN
    /// }
    /// ```
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
            "FOR" => TokenId::For,
            "EACH" => TokenId::Each,
            "IN" => TokenId::In,
            "IF" => TokenId::If,
            "ELSE" => TokenId::Else,
            "DISPLAY" => TokenId::Display,
            "RETURN" => TokenId::Return,
            "APPEND" => TokenId::Append,
            "REMOVE" => TokenId::Remove,
            "LENGTH" => TokenId::Length,
            "INSERT" => TokenId::Insert,
            _ => TokenId::Name(name),
        };

        id
    }

    /// Called when a comment is found
    /// 
    /// If the next character is a `/` (single line comment), advances the character pointer until the next newline is found.
    /// 
    /// If the next character is a `*` (multi-line comment), advances the character pointer until the next `*/` is found.
    fn match_comment(&mut self) -> TokenId {
        if self.match_char('/') {
                    
            while self.peek() != '\n' && !self.is_at_end() {
                self.advance();
            }
            return TokenId::Comment(self.source[self.start + 2..self.current].to_string())
        } else if self.match_char('*') {

            while self.peek() != '*' && self.peek_next() != '/' && !self.is_at_end() {
                self.advance();
            }
            if self.is_at_end() {
                panic!("Unterminated block comment.");
            }

            // Step over the closing '*/'
            self.advance();
            self.advance();

            return TokenId::Comment(self.source[self.start + 2..self.current - 1].to_string())
        }

        unreachable!("Input is always a comment.")
    }

    /// Matches the next character in the source code to a token and returns it.
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
            '/' => {
                match self.peek() {
                    '/' | '*' =>  Some(self.match_comment()),
                    _ => Some(TokenId::Slash)
                }
            },
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
}