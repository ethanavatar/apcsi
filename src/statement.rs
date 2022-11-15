use crate::{parser::Expr, scanner::Token};

pub trait StatementVisitor<R> {
    fn visit_block(&mut self, block: &Statement) -> R;
    fn visit_expression(&mut self, expression: &Statement) -> R;
    fn visit_if(&mut self, if_statement: &Statement) -> R;
    fn visit_display(&mut self, print: &Statement) -> R;
    fn visit_procedure(&mut self, procedure: &Statement) -> R;
    fn visit_return(&mut self, return_statement: &Statement) -> R;
    fn visit_repeat_until(&mut self, while_statement: &Statement) -> R;
    fn visit_repeat_times(&mut self, while_statement: &Statement) -> R;
    fn visit_variable_decl(&mut self, variable: &Statement) -> R;
    fn visit_call(&mut self, call_name: &Statement) -> R;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Box<Vec<Statement>>),
    Expression(Expr),
    If(Expr, Box<Statement>, Box<Option<Statement>>),
    Display(Expr),
    Procedure(Token, Vec<Token>, Box<Statement>),
    Return(Token, Expr),
    RepeatUntil(Expr, Box<Statement>),
    RepeatTimes(Expr, Box<Statement>),
    VariableDecl(Token, Expr),
    Call(Token, Vec<Token>),
}

impl Statement {
    pub fn accept<R>(&self, visitor: &mut impl StatementVisitor<R>) -> R {
        match self {
            s @ Statement::Block(_) => visitor.visit_block(s),
            s @ Statement::Expression(_) => visitor.visit_expression(s),
            s @ Statement::If(_, _, _) => visitor.visit_if(s),
            s @ Statement::Display(_) => visitor.visit_display(s),
            s @ Statement::Procedure(_, _, _) => visitor.visit_procedure(s),
            s @ Statement::Return(_, _) => visitor.visit_return(s),
            s @ Statement::RepeatUntil(_, _) => visitor.visit_repeat_until(s),
            s @ Statement::RepeatTimes(_, _) => visitor.visit_repeat_times(s),
            s @ Statement::VariableDecl(_, _) => visitor.visit_variable_decl(s),
            s @ Statement::Call(_, _) => visitor.visit_call(s),
        }
    }
}