#![allow(dead_code)]
mod ast;
mod lexer;
mod parser;
use std::{io::Write, process::exit};

use crate::lexer::{Lexer, Token};

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
            let mut lex = Lexer::new(line);
            while let Ok(token) = lex.next_token() {
                println!("{:?}", token);
                if token == Token::EOF {
                    break;
                }
            }
            print!("{}", prompt);
            std::io::stdout().flush().unwrap();
        }
    }
}
