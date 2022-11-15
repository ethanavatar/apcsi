use crate::statement::{Statement, StatementVisitor};
use crate::{parser::Expr};
use crate::{scanner::TokenId};
use crate::environment::Environment;

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
    fn visit_identifier(&mut self, expr: &Expr) -> InterpreterValue;
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new(env: Environment) -> Self {
        Self {
            environment: env,
        }
    }
    
    pub fn interpret(&mut self, statement: &Statement) -> Result<(), String> {
        self.execute(statement);

        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> () {
        match statement {
            display @ Statement::Display(_) => {
                display.accept(self)
            },
            assign @ Statement::VariableDecl(_, _) => {
                assign.accept(self)
            },
            Statement::Expression(expr) => {
                expr.accept(self);
            },
            block @ Statement::Block(_) => {
                block.accept(self)
            },
            if_statement @ Statement::If(_, _, _) => {
                if_statement.accept(self)
            },
            _ => ()
        }
    }

    fn execute_block(&mut self, statements: &Vec<Statement>, environment: &Environment) -> () {
        self.environment = environment.clone();

        for statement in statements {
            self.execute(statement);
        }

        let parent = self.environment.get_parent().unwrap();
        self.environment = parent.clone();
    }

    fn evaluate(&mut self, expr: &Expr) -> InterpreterValue {
        expr.accept(self)
    }

    fn match_literal(expr: &Expr) -> InterpreterValue {
        match expr {
            Expr::Literal(token) => {
                match &token.id {
                    TokenId::Number(n) => InterpreterValue::Number(*n),
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
            InterpreterValue::Boolean(b) => {
                match b {
                    true => "TRUE".to_string(),
                    false => "FALSE".to_string(),
                }
            },
            InterpreterValue::None => "NONE".to_string(),
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
                                return InterpreterValue::Number(left + right)

                            } else {
                                panic!("Binary operator expected number but got {:?}", right);
                            }
                        } else {
                            let right = Self::to_string(&right);
                            let left = Self::to_string(&left);

                            // The actual return value
                            return InterpreterValue::String(format!("{}{}", left, right))
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

    fn visit_identifier(&mut self, expr: &Expr) -> InterpreterValue {
        if let Expr::Identifier(tok) = expr {
            let res = self.environment.get(&tok.lexeme);
            match res {
                Some(value) => value.clone(),
                None => panic!("{}", format!("Name '{}' does not exist in the current context. ({}, {})", tok.lexeme, tok.line, tok.column)),
            }
        } else {
            unreachable!("Expected identifier expression but got {:?}", expr);
        }
    }
}


impl StatementVisitor<()> for Interpreter {
    fn visit_expression(&mut self, statement: &Statement) -> () {
        if let Statement::Expression(expr) = statement {
            self.evaluate(expr);

            return;
        }
        
        unreachable!("Expected expression statement but got {:?}", statement);
    }

    fn visit_display(&mut self, statement: &Statement) -> () {
        if let Statement::Display(expr) = statement {
            let value = self.evaluate(expr);
            println!("{}", Self::to_string(&value));

            return;
        }

        unreachable!("Expected display statement but got {:?}", statement);
    }

    fn visit_block(&mut self, block: &Statement) -> () {
        let statements = if let Statement::Block(s) = block {
            s
        } else {
            unreachable!("Expected block statement but got {:?}", block);
        };

        self.execute_block(statements, &Environment::new_with_parent(self.environment.clone()));
    }
    fn visit_if(&mut self, _if: &Statement) -> () {

        let (condition, then_branch, else_branch) = if let Statement::If(c, t, e) = _if {
            (c, t, e)
        } else {
            unreachable!("Expected if statement but got {:?}", _if);
        };

        if Self::is_truthy(&self.evaluate(condition)) {
            self.execute(then_branch);
        } else if let Some(else_branch) = *else_branch.clone() {
            self.execute(&else_branch);
        }
    }
    fn visit_procedure(&mut self, _proc: &Statement) -> () { unimplemented!() }
    fn visit_return(&mut self, _return: &Statement) -> () { unimplemented!() }
    fn visit_repeat(&mut self, _repeat: &Statement) -> () { unimplemented!() }
    fn visit_variable_decl(&mut self, variable: &Statement) -> () {
                
        if let Statement::VariableDecl(name, expr) = variable {
            let value = self.evaluate(expr);
            if self.environment.is_defined(&name.lexeme) {
                self.environment.set(&name.lexeme, &value).unwrap();
                return;
            }

            self.environment.define(name.lexeme.clone().as_str(), &value);
        } else {
            unreachable!("Expected variable statement but got {:?}", variable);
        }

    }
}