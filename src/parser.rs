use crate::ast::{Expression, Precedence, Program, Statment};
use crate::lexer::{Lexer, Token};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    parsing_errors: Vec<ParsingError>,
}

#[derive(Debug, PartialEq)]
pub enum ParsingError {
    ExpectedIdentifier(Token),
    ExpectedAssign(Token),
}
type PrefixParserFn = fn(&mut Parser) -> Expression;
type InfixParsern = fn(&mut Parser, Expression) -> Expression;

impl Parser {
    pub fn new(lex: Lexer) -> Parser {
        let mut parser = Parser {
            lexer: lex,
            current_token: Token::Illegal,
            peek_token: Token::Illegal,
            parsing_errors: vec![],
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
            Token::Return => self.parse_return_statment(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statment(&mut self) -> Option<Statment> {
        let name: String;
        if let Token::Identifier(identifier) = self.peek_token.clone() {
            self.next_token();
            name = identifier;
        } else {
            self.parsing_errors
                .push(ParsingError::ExpectedIdentifier(self.peek_token.clone()));
            return None;
        }
        if !self.expect_token(Token::Assign) {
            self.parsing_errors
                .push(ParsingError::ExpectedAssign(self.peek_token.clone()));
            return None;
        }

        while self.current_token != Token::Semicolon && self.current_token != Token::EOF {
            self.next_token();
        }

        Some(Statment::Let(name, Expression::IntegerLiteral(5)))
    }

    fn parse_return_statment(&mut self) -> Option<Statment> {
        let literal = match self.peek_token.clone() {
            Token::Integer(i) => i.parse::<i32>().unwrap(),
            _ => 0,
        };
        while self.current_token != Token::Semicolon && self.current_token != Token::EOF {
            self.next_token();
        }

        Some(Statment::Return(Expression::IntegerLiteral(literal)))
    }

    fn parse_expression_statement(&mut self) -> Option<Statment> {
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        if let Some(exp) = expression {
            Some(Statment::Expression(exp))
        } else {
            None
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        if let Some(prefix) = self.prefix_parse_functions() {
            Some(prefix(self))
        } else {
            None
        }
    }

    fn prefix_parse_functions(&self) -> Option<PrefixParserFn> {
        match &self.current_token {
            Token::Identifier(_name) => Some(Parser::parse_identifier),
            Token::Integer(_) => Some(Parser::parse_integer_literal),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Expression {
        if let Token::Identifier(name) = self.current_token.clone() {
            Expression::Identifier(name)
        } else {
            panic!("Parse identifier called not on an identifier")
        }
    }

    fn parse_integer_literal(&mut self) -> Expression {
        if let Token::Integer(value) = self.current_token.clone() {
            Expression::IntegerLiteral(value.parse::<i32>().unwrap())
        } else {
            panic!("Parse identifier called not on an identifier")
        }
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
    fn test_let_parser() {
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

    #[test]
    fn test_return_parser() {
        let input = "
        return 0;
        return 128;
        return 5;";

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(3, program.statments.len());
        let expected_expressions = [
            Expression::IntegerLiteral(0),
            Expression::IntegerLiteral(128),
            Expression::IntegerLiteral(5),
        ];
        for (i, identifier) in expected_expressions.iter().enumerate() {
            let statment = &program.statments[i];
            test_return_statment(identifier, statment);
        }
    }

    #[test]
    fn test_identifier_parser() {
        let input = "foobar";
        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(0, parser.parsing_errors.len());
        assert_eq!(1, program.statments.len());
        let statment = program.statments.first().unwrap().clone();
        if let Statment::Expression(exp) = statment {
            if let Expression::Identifier(name) = exp {
                assert_eq!("foobar", name);
            }
        } else {
            panic!("Not an expression!");
        }
    }

    #[test]
    fn test_integer_literal_parser() {
        let input = "5;";
        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(0, parser.parsing_errors.len());
        assert_eq!(1, program.statments.len());
        let statment = program.statments.first().unwrap().clone();
        if let Statment::Expression(exp) = statment {
            if let Expression::IntegerLiteral(value) = exp {
                assert_eq!(5, *value);
            }
        } else {
            panic!("Not an expression!");
        }
    }

    #[test]
    fn test_parser_errors() {
        let input = "
            let x = 5;
            let = 10;
            let y 5;
            let z = 15;";

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let _program = parser.parse_program();
        assert_eq!(2, parser.parsing_errors.len());
        let expecte_errors = [
            ParsingError::ExpectedIdentifier(Token::Assign),
            ParsingError::ExpectedAssign(Token::Integer(String::from("5"))),
        ];
        for (expected_error, actual_error) in
            expecte_errors.iter().zip(parser.parsing_errors.iter())
        {
            assert_eq!(expected_error, actual_error);
        }
    }

    fn test_let_statment(identifier: &String, statment: &Statment) {
        if let Statment::Let(name, _exp) = statment {
            assert_eq!(identifier, name);
        } else {
            panic!("Not a let statment");
        }
    }

    fn test_return_statment(expression: &Expression, statment: &Statment) {
        if let Statment::Return(exp) = statment {
            assert_eq!(expression, exp);
        } else {
            panic!("Not a return statment");
        }
    }
}
