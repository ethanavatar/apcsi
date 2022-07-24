
#[derive(Debug, PartialEq, Clone)]
pub enum Symbols {
    Assign,
    Comma,
    Dot,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Plus,
    Minus,
    Star,
    Slash,

    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,

    Identifier(String),
    Number(String),
    String(String),

    Unhandled
}

fn match_left_arrow(input: &str, index: &mut usize) -> Symbols {
    if *index + 1 >= input.len() {
        *index += 1;
        return Symbols::Less;
    }

    match input.chars().nth(*index + 1).unwrap() {
        '=' => {*index += 2; Symbols::LessEqual},
        '-' => {*index += 2; Symbols::Assign},
        _   => {*index += 1; Symbols::Less},
    }
}

fn match_right_arrow(input: &str, index: &mut usize) -> Symbols {
    if *index + 1 >= input.len() {
        *index += 1;
        return Symbols::Greater;
    }

    match input.chars().nth(*index + 1).unwrap() {
        '=' => {*index += 2; Symbols::GreaterEqual},
        _   => {*index += 1; Symbols::Greater},
    }
}

pub fn lex(input: &str) -> Vec<Symbols> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < input.len() {
        match input.chars().nth(i).unwrap() {
            '<' => {
                result.push(match_left_arrow(&input, &mut i));
            }
            '>' => {
                result.push(match_right_arrow(&input, &mut i));
            }
            ',' => {
                result.push(Symbols::Comma);
                i += 1;
            }
            '.' => {
                result.push(Symbols::Dot);
                i += 1;
            }
            '(' => {
                result.push(Symbols::LParen);
                i += 1;
            }
            ')' => {
                result.push(Symbols::RParen);
                i += 1;
            }
            '{' => {
                result.push(Symbols::LBrace);
                i += 1;
            }
            '}' => {
                result.push(Symbols::RBrace);
                i += 1;
            }
            '[' => {
                result.push(Symbols::LBracket);
                i += 1;
            }
            ']' => {
                result.push(Symbols::RBracket);
                i += 1;
            }
            '+' => {
                result.push(Symbols::Plus);
                i += 1;
            }
            '-' => {
                result.push(Symbols::Minus);
                i += 1;
            }
            '*' => {
                result.push(Symbols::Star);
                i += 1;
            }
            '/' => {
                result.push(Symbols::Slash);
                i += 1;
            }
            '=' => {
                result.push(Symbols::Equal);
                i += 1;
            }
            '!' => {
                if (i + 1) > input.len() {
                    result.push(Symbols::Unhandled);
                    break;
                }
                i += 1;
                result.push(
                match input.chars().nth(i) {
                    Some('=') => {i += 2; Symbols::NotEqual}
                    _   => {i += 1; Symbols::Unhandled}
                });
            }
            '"' => {
                let mut string = String::new();
                i += 1;
                while match input.chars().nth(i) {
                    Some(ch) => {
                        if ch != '"' {
                            true
                        } else {
                            false
                        }
                    },
                    None => {false}
                } {
                    string.push(input.chars().nth(i).unwrap());
                    i += 1;
                }

                result.push(Symbols::String(string));
            }
            '\n' | '\r' | '\t' | ' ' => {
                i += 1;
            }
            _ => {
                if input.chars().nth(i).unwrap().is_alphabetic() {
                    let mut identifier = String::new();
                    while i < input.len() && input.chars().nth(i).unwrap().is_alphanumeric() {
                        identifier.push(input.chars().nth(i).unwrap());
                        i += 1;
                    }
                    result.push(Symbols::Identifier(identifier));
                } else if input.chars().nth(i).unwrap().is_numeric() {
                    let mut number = String::new();
                    while i < input.len() && input.chars().nth(i).unwrap().is_numeric() {
                        number.push(input.chars().nth(i).unwrap());
                        i += 1;
                    }
                    result.push(Symbols::Number(number));
                } else {
                    result.push(Symbols::Unhandled);
                }
            }
        }
    }
    result
}


#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_left_arrow() {
        let result = lexer::lex("<-");
        assert_eq!(result[0], lexer::Symbols::Assign);

        let result = lexer::lex("<=");
        assert_eq!(result[0], lexer::Symbols::LessEqual);

        let result = lexer::lex("<-");
        assert_eq!(result[0], lexer::Symbols::Assign);
    }

    #[test]
    fn test_right_arrow() {
        let result = lexer::lex(">");
        assert_eq!(result[0], lexer::Symbols::Greater);

        let result = lexer::lex(">=");
        assert_eq!(result[0], lexer::Symbols::GreaterEqual);
    }

    #[test]
    fn test_comma() {
        let result = lexer::lex(",");
        assert_eq!(result[0], lexer::Symbols::Comma);
    }

    #[test]
    fn test_dot() {
        let result = lexer::lex(".");
        assert_eq!(result[0], lexer::Symbols::Dot);
    }

    #[test]
    fn test_paren() {
        let result = lexer::lex("(");
        assert_eq!(result[0], lexer::Symbols::LParen);

        let result = lexer::lex(")");
        assert_eq!(result[0], lexer::Symbols::RParen);
    }

    #[test]
    fn test_brace() {
        let result = lexer::lex("{");
        assert_eq!(result[0], lexer::Symbols::LBrace);

        let result = lexer::lex("}");
        assert_eq!(result[0], lexer::Symbols::RBrace);
    }

    #[test]
    fn test_bracket() {
        let result = lexer::lex("[");
        assert_eq!(result[0], lexer::Symbols::LBracket);

        let result = lexer::lex("]");
        assert_eq!(result[0], lexer::Symbols::RBracket);
    }

    #[test]
    fn test_plus() {
        let result = lexer::lex("+");
        assert_eq!(result[0], lexer::Symbols::Plus);
    }

    #[test]
    fn test_minus() {
        let result = lexer::lex("-");
        assert_eq!(result[0], lexer::Symbols::Minus);
    }

    #[test]
    fn test_star() {
        let result = lexer::lex("*");
        assert_eq!(result[0], lexer::Symbols::Star);
    }

    #[test]
    fn test_slash() {
        let result = lexer::lex("/");
        assert_eq!(result[0], lexer::Symbols::Slash);
    }

    #[test]
    fn test_equal() {
        let result = lexer::lex("=");
        assert_eq!(result[0], lexer::Symbols::Equal);
    }

    #[test]
    fn test_not_equal() {
        let result = lexer::lex("!=");
        assert_eq!(result[0], lexer::Symbols::NotEqual);
    }

    #[test]
    fn test_unhandled() {
        let result = lexer::lex("!");
        assert_eq!(result[0], lexer::Symbols::Unhandled);
    }

    #[test]
    fn test_whitespace() {
        let result = lexer::lex(" \t\n");
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn test_identifier() {
        let result = lexer::lex("abc");
        assert_eq!(result[0], lexer::Symbols::Identifier("abc".to_string()));
    }

    #[test]
    fn test_number() {
        let result = lexer::lex("123");
        assert_eq!(result[0], lexer::Symbols::Number("123".to_string()));
    }
}