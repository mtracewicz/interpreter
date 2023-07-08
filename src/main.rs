mod lexer;
use crate::lexer::Lexer;

fn main() {
    let mut lex = Lexer::new(String::from("let x = 5"));
}
