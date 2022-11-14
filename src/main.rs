mod scanner;
mod parser;
mod interpreter;
mod statement;
mod environment;

use clap::Parser;

use std::{path::PathBuf, io::Write, vec};

fn run_program(program: &str) {
    let mut scanner = scanner::Scanner::new(program);
    let mut tokens = vec![];
    scanner.scan_tokens(&mut tokens);

    let mut parser = parser::Parser::new(tokens);
    let mut tree = vec![];
    parser.parse(&mut tree);

    let env = environment::Environment::new();

    let mut interpreter = interpreter::Interpreter::new(env);
    for node in &tree {
        interpreter.interpret(node).unwrap();
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

#[derive(clap::Parser, Debug)]
#[command(
    author = "Ethan Evans",
    about = "A simple interpreter for the College Board's AP Computer Science Principles pseudocode language.",
    version
)]
struct Opts {
    #[arg(default_value = None)]
    file: Option<PathBuf>,
}


fn main() {

    let opts = Opts::parse();

    match opts.file {
        Some(path) => run_file(path).unwrap(),
        None => run_repl(),
    }
}