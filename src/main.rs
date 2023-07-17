#![allow(dead_code)]
mod ast;
mod lexer;
mod parser;
use std::{io::Write, process::exit};

use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let prompt = ">> ";
    println!("Welcome to Monkey interpreter!");
    print!("{}", prompt);
    std::io::stdout().flush().unwrap();
    let lines = std::io::stdin().lines();
    for line in lines {
        if let Ok(line) = line {
            if line == "exit" {
                exit(0);
            }
            let lex = Lexer::new(line);
            let mut parser = Parser::new(lex);
            let program = parser.parse_program();
            println!("{}", program);
            print!("{}", prompt);
            std::io::stdout().flush().unwrap();
        }
    }
}
