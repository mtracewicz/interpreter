use crate::lexer::{Lexer, Token};

pub enum Statment {
    Let(String, Expression),
}

pub enum Expression {
    IntegerLiteral(i32),
}

pub struct Program {
    statments: Vec<Statment>,
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lex: Lexer) -> Parser {
        let mut parser = Parser {
            lexer: lex,
            current_token: Token::Illegal,
            peek_token: Token::Illegal,
        };
        parser.next_token();
        parser.next_token();
        return parser;
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statments: vec![] };
        while self.current_token != Token::EOF {
            if let Some(statment) = self.parse_statment() {
                program.statments.push(statment);
            }
            self.next_token();
        }
        return program;
    }

    fn parse_statment(&mut self) -> Option<Statment> {
        match self.current_token {
            Token::Let => self.parse_let_statment(),
            _ => None,
        }
    }

    fn parse_let_statment(&mut self) -> Option<Statment> {
        let name: String;
        if let Token::Identifier(identifier) = self.peek_token.clone() {
            self.next_token();
            name = identifier;
        } else {
            return None;
        }
        if !self.expect_token(Token::Assign) {
            return None;
        }

        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Some(Statment::Let(name, Expression::IntegerLiteral(5)))
    }

    fn expect_token(&mut self, t: Token) -> bool {
        if self.peek_token == t {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;";

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(3, program.statments.len());
        let expected_identifiers = [String::from("x"), String::from("y"), String::from("foobar")];
        for (i, identifier) in expected_identifiers.iter().enumerate() {
            let statment = &program.statments[i];
            test_let_statment(identifier, statment);
        }
    }

    fn test_let_statment(identifier: &String, statment: &Statment) {
        if let Statment::Let(name, _exp) = statment {
            assert_eq!(identifier, name);
        } else {
            panic!("Not a let statment");
        }
    }
}
