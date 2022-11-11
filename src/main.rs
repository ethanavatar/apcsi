mod scanner;
use scanner::Scanner;

mod parser;
use parser::Parser;

mod interpreter;
use interpreter::Interpreter;

mod statement;


use std::{path::PathBuf, io::Write, vec};

fn run_program(program: &str) {
    let mut scanner = Scanner::new(program);
    let mut tokens = vec![];
    scanner.scan_tokens(&mut tokens);

    //println!("\nTokens:");
    for token in tokens.clone() {
        //println!("{:?}", token);
    }

    let mut parser = Parser::new(tokens);
    let mut tree = vec![];
    parser.parse(&mut tree);


    //println!("\nExpression(s):");
    for node in &tree {
        //println!("{:?}", node);
    }

    //println!("\nOutput:");
    let mut interpreter = Interpreter::new();
    for node in &tree {
        interpreter.interpret(node);
    }
}

fn run_repl() -> ! {
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        run_program(&line);
    }
}

fn run_file(path: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let file_bytes = std::fs::read(path)?;
    let file_string = std::str::from_utf8(&file_bytes)?;
    run_program(file_string);

    Ok(())
}

fn main() {
    run_repl();
}