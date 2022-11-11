use crate::{parser::Expr};
use crate::{scanner::TokenId};

#[derive(Debug, Clone)]
pub enum InterpreterValue {
    Number(f64),
    String(String),
    Boolean(bool),
    None,
}

pub trait ExprVisitor {
    fn visit_literal(&mut self, expr: &Expr) -> InterpreterValue;
    fn visit_grouping(&mut self, expr: &Expr) -> InterpreterValue;
    fn visit_unary(&mut self, expr: &Expr) -> InterpreterValue;
    fn visit_binary(&mut self, expr: &Expr) -> InterpreterValue;
}

pub struct Interpreter {

}

impl Interpreter {
    pub fn new() -> Self {
        Self {

        }
    }
    
    pub fn interpret(&mut self, expr: &Expr) -> () {
        let value = self.evaluate(expr);
        println!("{}", Self::to_string(&value));
    }

    fn evaluate(&mut self, expr: &Expr) -> InterpreterValue {
        expr.accept(self)
    }

    fn match_literal(expr: &Expr) -> InterpreterValue {
        match expr {
            Expr::Literal(token) => {
                match &token.id {
                    TokenId::Number(n) => {
                        let f = *n as f64;
                        InterpreterValue::Number(f)
                    },
                    TokenId::String(s) => InterpreterValue::String(s.clone()),
                    TokenId::True => InterpreterValue::Boolean(true),
                    TokenId::False => InterpreterValue::Boolean(false),
                    TokenId::None => InterpreterValue::None,
                    _ => panic!("Invalid literal token"),
                }
            },
            _ => unreachable!("Expected literal expression but got {:?}", expr),
        }
    }

    fn is_truthy(value: &InterpreterValue) -> bool {
        match value {
            InterpreterValue::None => false,
            InterpreterValue::Boolean(b) => *b,
            _ => true,
        }
    }

    fn is_equal(a: &InterpreterValue, b: &InterpreterValue) -> bool {
        match (a, b) {
            (InterpreterValue::Number(a), InterpreterValue::Number(b)) => a == b,
            (InterpreterValue::String(a), InterpreterValue::String(b)) => a == b,
            (InterpreterValue::Boolean(a), InterpreterValue::Boolean(b)) => a == b,
            (InterpreterValue::None, InterpreterValue::None) => true,
            _ => false,
        }
    }

    fn to_string(value: &InterpreterValue) -> String {
        match value {
            InterpreterValue::Number(n) => n.to_string(),
            InterpreterValue::String(s) => s.clone(),
            InterpreterValue::Boolean(b) => b.to_string(),
            InterpreterValue::None => "None".to_string(),
        }
    }
}

impl ExprVisitor for Interpreter {
    fn visit_literal(&mut self, expr: &Expr) -> InterpreterValue {
        Self::match_literal(expr)
    }

    fn visit_grouping(&mut self, expr: &Expr) -> InterpreterValue{
        match expr {
            Expr::Grouping(expr) => self.evaluate(expr),
            _ => unreachable!("Expected grouping expression but got {:?}", expr),
        }
    }

    fn visit_unary(&mut self, expr: &Expr) -> InterpreterValue {
        match expr {
            Expr::Unary(operator, operand) => {
                
                let right = self.evaluate(operand);

                match operator.id {
                    TokenId::Minus => {
                        if let InterpreterValue::Number(n) = right {
                            InterpreterValue::Number(-(n as f64))
                        } else {
                            panic!("Unary operator expected number but got {:?}", right);
                        }
                    }

                    TokenId::Bang => {
                        InterpreterValue::Boolean(!Self::is_truthy(&right))
                    }


                    _ => unreachable!("Expected unary operator but got {:?}", operator),
                }

            }
            _ => unreachable!("Expected unary expression but got {:?}", expr),
        }
    }

    fn visit_binary(&mut self, expr: &Expr) -> InterpreterValue {
        match expr {
            Expr::Binary(left, operator, right) => {

                let left = self.evaluate(left);
                let right = self.evaluate(right);

                match operator.id {



                    TokenId::Minus => {

                        if let InterpreterValue::Number(left) = left {
                            if let InterpreterValue::Number(right) = right {

                                // The actual return value
                                InterpreterValue::Number(left - right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else {
                            panic!("Binary operator expected number but got {:?}", left);
                        }

                    }



                    TokenId::Slash => {

                        if let InterpreterValue::Number(left) = left {
                            if let InterpreterValue::Number(right) = right {

                                // The actual return value
                                InterpreterValue::Number(left / right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else {
                            panic!("Binary operator expected number but got {:?}", left);
                        }


                    }



                    TokenId::Star => {

                        if let InterpreterValue::Number(left) = left {
                            if let InterpreterValue::Number(right) = right {

                                // The actual return value
                                InterpreterValue::Number(left * right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else {
                            panic!("Binary operator expected number but got {:?}", left);
                        }

                    }



                    TokenId::Plus => {
                        
                        if let InterpreterValue::Number(left) = left {
                            if let InterpreterValue::Number(right) = right {

                                // The actual return value
                                InterpreterValue::Number(left + right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else if let InterpreterValue::String(left) = left {
                            if let InterpreterValue::String(right) = right {

                                // The actual return value
                                InterpreterValue::String(format!("{}{}", left, right))

                            } else {
                                panic!("Binary operator expected string but got {:?}", right);
                            }
                        } else {
                            panic!("Binary operator expected number or string but got {:?}", left);
                        }

                    }


                    TokenId::Greater => {

                        if let InterpreterValue::Number(left) = left {
                            if let InterpreterValue::Number(right) = right {

                                // The actual return value
                                InterpreterValue::Boolean(left > right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else {
                            panic!("Binary operator expected number but got {:?}", left);
                        }

                    }



                    TokenId::GreaterEqual => {

                        if let InterpreterValue::Number(left) = left {
                            if let InterpreterValue::Number(right) = right {

                                // The actual return value
                                InterpreterValue::Boolean(left >= right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else {
                            panic!("Binary operator expected number but got {:?}", left);
                        }

                    }



                    TokenId::Less => {

                        if let InterpreterValue::Number(left) = left {
                            if let InterpreterValue::Number(right) = right {

                                // The actual return value
                                InterpreterValue::Boolean(left < right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else {
                            panic!("Binary operator expected number but got {:?}", left);
                        }

                    }



                    TokenId::LessEqual => {

                        if let InterpreterValue::Number(left) = left {
                            if let InterpreterValue::Number(right) = right {

                                // The actual return value
                                InterpreterValue::Boolean(left <= right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else {
                            panic!("Binary operator expected number but got {:?}", left);
                        }

                    }



                    TokenId::BangEqual => {
                        InterpreterValue::Boolean(!Self::is_equal(&left, &right))
                    }



                    TokenId::Equal => {
                        InterpreterValue::Boolean(Self::is_equal(&left, &right))
                    }






                    _ => unreachable!("Expected binary operator but got {:?}", operator),
                }

            }
            _ => unreachable!("Expected binary expression but got {:?}", expr),
        }
    }
}