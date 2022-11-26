use crate::statement::{Statement, StatementVisitor};
use crate::{scanner::TokenId};
use crate::environment::Environment;
use crate::expr::{Expr, ExprVisitor};
use crate::scanner::Token;

#[derive(Clone, Debug)]
pub enum InterpreterValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(String, Statement, Vec<String>, Environment),
    None,
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
        self.execute(statement, &mut self.environment.clone());

        Ok(())
    }

    fn execute(&mut self, statement: &Statement, env: &mut Environment) -> () {
        match statement {
            s @ Statement::Display(_) =>           { s.accept(self, env) },
            s @ Statement::VariableDecl(_, _) =>   { s.accept(self, env) },
            s @ Statement::Expression(e) => { e.accept(self, env); },
            s @ Statement::Block(_) =>             { s.accept(self, env) },
            s @ Statement::If(_, _, _) =>          { s.accept(self, env) },
            s @ Statement::Procedure(_, _, _) =>   { s.accept(self, env) },
            s @ Statement::Call(_, _) =>           { s.accept(self, env) },
            s @ Statement::RepeatUntil(_, _) =>    { s.accept(self, env) },
            s @ Statement::RepeatTimes(_, _) =>    { s.accept(self, env) },
            _ => ()
        }
    }

    fn execute_block<'a>(&mut self, statements: &Vec<Statement>, environment: &'a mut Environment) -> &'a Environment {

        //println!("n defined: {}", environment.is_defined("n"));

        for statement in statements {
            self.execute(statement, environment);
        }

        environment.get_parent().unwrap()
    }

    fn evaluate(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue {
        expr.accept(self, env)
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
            InterpreterValue::Function(n, _, p, _) => format!("<fn `{}`>({})", n, p.iter().map(|t| t.clone()).collect::<Vec<String>>().join(", ")),
        }
    }

    fn call_void(&mut self, func: &InterpreterValue, args: &Vec<InterpreterValue>) -> () {
        match func {
            InterpreterValue::Function(name, stmt, p, env) => {

                let mut env = env.clone();

                for (i, param) in p.iter().enumerate() {
                    env.define(param, &args[i]);
                }

                self.execute(stmt, &mut env)
            },
            _ => panic!("Can only call functions"),
        }
    }
}

impl ExprVisitor for Interpreter {
    fn visit_literal(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue {
        Self::match_literal(expr)
    }

    fn visit_grouping(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue{
        match expr {
            Expr::Grouping(expr) => self.evaluate(expr, env),
            _ => unreachable!("Expected grouping expression but got {:?}", expr),
        }
    }

    fn visit_unary(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue {
        match expr {
            Expr::Unary(operator, operand) => {
                
                let right = self.evaluate(operand, env);

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

    fn visit_binary(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue {
        match expr {
            Expr::Binary(left, operator, right) => {

                let left = self.evaluate(left, env);
                let right = self.evaluate(right, env);

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

    fn visit_identifier(&mut self, expr: &Expr, env: &Environment) -> InterpreterValue {
        if let Expr::Identifier(tok) = expr {
            let res = env.get(&tok.lexeme);

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
    fn visit_expression(&mut self, statement: &Statement, env: &mut Environment) -> () {
        if let Statement::Expression(expr) = statement {
            self.evaluate(expr, env);

            return;
        }
        
        unreachable!("Expected expression statement but got {:?}", statement);
    }

    fn visit_display(&mut self, statement: &Statement, env: &mut Environment) -> () {
        if let Statement::Display(expr) = statement {
            let value = self.evaluate(expr, env);
            println!("{}", Self::to_string(&value));

            return;
        }

        unreachable!("Expected display statement but got {:?}", statement);
    }

    fn visit_block(&mut self, block: &Statement, env: &mut Environment) -> () {
        let statements = if let Statement::Block(s) = block {
            s
        } else {
            unreachable!("Expected block statement but got {:?}", block);
        };

        self.execute_block(statements, &mut Environment::new_with_parent(env.clone()));
    }
    fn visit_if(&mut self, r#if: &Statement, env: &mut Environment) -> () {

        let (
            condition,
            then_branch,
            else_branch
        ) = match r#if {
            Statement::If(c, t, e) => (c, t, *e.clone()),
            _ => unreachable!()
        };

        if Self::is_truthy(&self.evaluate(condition, env)) {
            self.execute(then_branch, env);
        } else if let Some(else_branch) = else_branch {
            self.execute(&else_branch, env);
        }
    }
    fn visit_procedure(&mut self, proc: &Statement, env: &mut Environment) -> () {
        let (
            name,
            params,
            body
        ) = match proc {
            Statement::Procedure(n, p, b) => (n, p, b),
            _ => unreachable!()
        };

        let mut env = Environment::new_with_parent(env.clone());
        for param in params {
            env.define(&param, &InterpreterValue::None);
        }

        let procedure = InterpreterValue::Function(
            name.lexeme.clone(),
            *body.clone(),
            params.clone(),
            env,
        );

        self.environment.define(name.lexeme.clone().as_str(), &procedure);
    }
    fn visit_return(&mut self, _return: &Statement, env: &mut Environment) -> () { unimplemented!() }
    fn visit_repeat_until(&mut self, repeat: &Statement, env: &mut Environment) -> () {

        let (break_expr, body) = match repeat {
            Statement::RepeatUntil(e, b) => (e, *b.clone()),
            _ => unreachable!()
        };

        let body_stmts = match body {
            Statement::Block(v) => *v,
            _ => panic!()
        };

        let mut env = env.clone();
        loop {
            env = self.execute_block(&body_stmts, &mut Environment::new_with_parent(env.clone())).clone();
            
            let cond = self.evaluate(break_expr, &env);
            if Self::is_truthy(&cond) {
                break;
            }
        }
    }
    fn visit_repeat_times(&mut self, repeat: &Statement, env: &mut Environment) -> () {

        let (times, body) = match repeat {
            Statement::RepeatTimes(e, b) => (e, *b.clone()),
            _ => unreachable!()
        };

        let times = match self.evaluate(times, env) {
            InterpreterValue::Number(n) => n as u64,
            _ => panic!("Expected integral value for repeat statement")
        };

        let body_stmts = if let Statement::Block(v) = body {
            *v
        } else { panic!() };

        let mut env = env.clone();
        for _ in 0..times {
            env = self.execute_block(&body_stmts, &mut Environment::new_with_parent(env.clone())).clone();
        }

    }
    fn visit_variable_decl(&mut self, variable: &Statement, env: &mut Environment) -> () {
                
        if let Statement::VariableDecl(name, expr) = variable {

            let value = self.evaluate(expr, env);

            if env.is_defined(&name.lexeme) {
                env.set(&name.lexeme, &value).unwrap();
                return;
            }

            env.define(name.lexeme.clone().as_str(), &value);

            self.environment = env.clone();

        } else {
            unreachable!("Expected variable statement but got {:?}", variable);
        }

    }

    fn visit_call(&mut self, call: &Statement, env: &mut Environment) -> () {
        let (callee, args) = if let Statement::Call(c, a) = call {
            (c, a)
        } else {
            unreachable!("Expected call statement but got {:?}", call);
        };

        let callee_value = {
            let val = env.get(&callee.lexeme)
                .unwrap_or_else(|| panic!("Undefined procedure '{}' ({}, {})", callee.lexeme, callee.line, callee.column));

            val.clone()
        };

        let mut args = args.iter()
            .map(|arg| self.evaluate(arg, env))
            .collect::<Vec<_>>();

        self.call_void(&callee_value, &args);
    }
}