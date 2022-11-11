use crate::{parser::Expr, scanner::Token};

pub trait StatementVisitor<R> {
    fn visit_block(&mut self, block: &Statement) -> R;
    fn visit_expression(&mut self, expression: &Statement) -> R;
    fn visit_if(&mut self, if_statement: &Statement) -> R;
    fn visit_display(&mut self, print: &Statement) -> R;
    fn visit_procedure(&mut self, procedure: &Statement) -> R;
    fn visit_return(&mut self, return_statement: &Statement) -> R;
    fn visit_repeat(&mut self, while_statement: &Statement) -> R;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Box<Statement>>),
    Expression(Expr),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    Display(Expr),
    Procedure(Token, Vec<Token>, Vec<Box<Statement>>),
    Return(Token, Expr),
    Repeat(Expr, Box<Statement>),
}

impl Statement {
    //                                TODO: This type bound  \/  will cause issues later.
    pub fn accept(&self, visitor: &mut impl StatementVisitor<()>) -> () {
        match self {
            s @ Statement::Block(_) => visitor.visit_block(s),
            s @ Statement::Expression(_) => visitor.visit_expression(s),
            s @ Statement::If(_, _, _) => visitor.visit_if(s),
            s @ Statement::Display(_) => visitor.visit_display(s),
            s @ Statement::Procedure(_, _, _) => visitor.visit_procedure(s),
            s @ Statement::Return(_, _) => visitor.visit_return(s),
            s @ Statement::Repeat(_, _) => visitor.visit_repeat(s),
        }
    }
}