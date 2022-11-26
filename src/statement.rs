use crate::environment::Environment;
use crate::expr::Expr;
use crate::scanner::Token;

pub trait StatementVisitor<R> {
    fn visit_block(&mut self, block: &Statement, env: &mut Environment) -> R;
    fn visit_expression(&mut self, expression: &Statement, env: &mut Environment) -> R;
    fn visit_if(&mut self, if_statement: &Statement, env: &mut Environment) -> R;
    fn visit_display(&mut self, print: &Statement, env: &mut Environment) -> R;
    fn visit_procedure(&mut self, procedure: &Statement, env: &mut Environment) -> R;
    fn visit_return(&mut self, return_statement: &Statement, env: &mut Environment) -> R;
    fn visit_repeat_until(&mut self, while_statement: &Statement, env: &mut Environment) -> R;
    fn visit_repeat_times(&mut self, while_statement: &Statement, env: &mut Environment) -> R;
    fn visit_for(&mut self, for_statement: &Statement, env: &mut Environment) -> R;
    fn visit_variable_decl(&mut self, variable: &Statement, env: &mut Environment) -> R;
    fn visit_call(&mut self, call_name: &Statement, env: &mut Environment) -> R;
    fn visit_list_operation(&mut self, append: &Statement, env: &mut Environment) -> R;
    fn visit_list_access(&mut self, access: &Statement, env: &mut Environment) -> R;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Box<Vec<Statement>>),
    Expression(Expr),
    If(Expr, Box<Statement>, Box<Option<Statement>>),
    Display(Expr),
    Procedure(Token, Vec<String>, Box<Statement>),
    Return(Expr),
    RepeatUntil(Expr, Box<Statement>),
    RepeatTimes(Expr, Box<Statement>),
    For(Token, Token, Box<Statement>),
    VariableDecl(Token, Expr),
    Call(Token, Vec<Expr>),
    ListOperation(Token, Token, Option<Expr>, Option<Expr>),
    ListAccess(Token, Expr, Expr),
}

impl Statement {
    pub fn accept<R>(&self, visitor: &mut impl StatementVisitor<R>, env: &mut Environment) -> R {
        match self {
            s @ Statement::Block(_) => visitor.visit_block(s, env),
            s @ Statement::Expression(_) => visitor.visit_expression(s, env),
            s @ Statement::If(_, _, _) => visitor.visit_if(s, env),
            s @ Statement::Display(_) => visitor.visit_display(s, env),
            s @ Statement::Procedure(_, _, _) => visitor.visit_procedure(s, env),
            s @ Statement::Return(_) => visitor.visit_return(s, env),
            s @ Statement::RepeatUntil(_, _) => visitor.visit_repeat_until(s, env),
            s @ Statement::RepeatTimes(_, _) => visitor.visit_repeat_times(s, env),
            s @ Statement::VariableDecl(_, _) => visitor.visit_variable_decl(s, env),
            s @ Statement::Call(_, _) => visitor.visit_call(s, env),
            s @ Statement::ListOperation(_, _, _, _) => visitor.visit_list_operation(s, env),
            s @ Statement::ListAccess(_, _, _) => visitor.visit_list_access(s, env),
            s @ Statement::For(_, _, _) => visitor.visit_for(s, env),
        }
    }
}