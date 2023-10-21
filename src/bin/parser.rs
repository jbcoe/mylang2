use std::io;
use std::io::BufRead;

use mylang2::lexer;
use mylang2::parser::Parser;

fn main() {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut line = String::new();
    let mut eof = false;

    let mut lines = vec![];

    while !eof {
        match handle.read_line(&mut line) {
            Ok(0) => {
                eof = true;
            }
            Ok(_) => {
                lines.push(line.clone());
                line.clear();
            }
            Err(_error) => {
                panic!("something went wrong");
            }
        }
    }
    let source = lines.join("");
    let tokens = lexer::Lexer::tokenize(&source);

    match Parser::parse_program(&tokens) {
        Ok(program) => {
            for statement in program.statements {
                println!("{:#?}", statement);
            }
        }
        Err(error) => {
            println!("error: {}", error.message);
        }
    }
}
