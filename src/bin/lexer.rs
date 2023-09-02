use std::io;
use std::io::BufRead;

use mylang2::lexer;
use mylang2::token::Kind;

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

    for token in tokens {
        match token.kind() {
            Kind::Whitespace | Kind::EndOfFile => {}
            Kind::Unknown => {
                println!(
                    "Token {{ kind:{:?} line:{} column:{}}}",
                    token.kind(),
                    lexer::get_line(&token),
                    lexer::get_column(&token),
                );
            }
            _ => {
                println!(
                    r#"Token {{ kind:{:?}, line:{}, column:{}, text: "{}"}}"#,
                    token.kind(),
                    lexer::get_line(&token),
                    lexer::get_column(&token),
                    token.text()
                );
            }
        }
    }
}
