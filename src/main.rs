mod lexer;
mod parser;

fn main() {
    let lex_result = lexer::lex("a <- something");
    println!("{:?}", parser::parse(lex_result));

    let lex_result = lexer::lex("a <- 1");
    println!("{:?}", parser::parse(lex_result));

    let lex_result = lexer::lex("a <- \"string value\"");
    println!("{:?}", parser::parse(lex_result));
}