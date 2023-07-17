use crate::ast::{
    get_token_precedence, BlockStatment, Expression, InfixOperator, Precedence, PrefixOeprator,
    Program, Statment,
};
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
    ExpectedLeftParenthesis(Token),
    ExpectedRightParenthesis(Token),
    ExpectedLeftBrace(Token),
}
type PrefixParserFn = fn(&mut Parser) -> Expression;
type InfixParserFn = fn(&mut Parser, Expression) -> Expression;

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
        let left = if let Some(prefix) = Parser::prefix_parse_functions(&self.current_token) {
            Some(prefix(self))
        } else {
            None
        };
        if let Some(mut left_expression) = left {
            while self.peek_token != Token::Semicolon
                && precedence < get_token_precedence(&self.peek_token)
            {
                let infix_fn = Parser::infix_parse_function(&self.peek_token);
                if let Some(f) = infix_fn {
                    self.next_token();
                    left_expression = f(self, left_expression);
                } else {
                    return Some(left_expression);
                }
            }
            return Some(left_expression);
        } else {
            None
        }
    }

    fn prefix_parse_functions(token: &Token) -> Option<PrefixParserFn> {
        match token {
            Token::Identifier(_name) => Some(Parser::parse_identifier),
            Token::Integer(_) => Some(Parser::parse_integer_literal),
            Token::True | Token::False => Some(Parser::parse_bool_literal),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            Token::LeftParenthesis => Some(Parser::parse_grouped_expression),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_function_expression),
            _ => None,
        }
    }

    fn infix_parse_function(token: &Token) -> Option<InfixParserFn> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::LessThen
            | Token::GreaterThen
            | Token::Equal
            | Token::NotEqual => Some(Parser::parse_infix_expression),
            Token::LeftParenthesis => Some(Parser::parse_call_expression),
            _ => None,
        }
    }
    fn parse_call_expression(&mut self, left: Expression) -> Expression {
        Expression::Call(Box::new(left), self.parse_call_arguments())
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = vec![];
        if self.peek_token == Token::RightParenthesis {
            self.next_token();
            return args;
        }
        self.next_token();

        args.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest).unwrap());
        }

        if !self.expect_token(Token::RightParenthesis) {
            panic!("Missing right parsenthesis");
        }

        return args;
    }

    fn parse_function_expression(&mut self) -> Expression {
        if !self.expect_token(Token::LeftParenthesis) {
            panic!("Not a left parenthesis.");
        }
        let params = self.parse_parameters();
        if !self.expect_token(Token::LeftBrace) {
            panic!("Not a left brace.");
        }
        let body = self.parse_block_statment();
        return Expression::Function(params, body);
    }

    fn parse_parameters(&mut self) -> Vec<Expression> {
        let mut params = vec![];
        if self.peek_token == Token::RightParenthesis {
            self.next_token();
            return params;
        }
        self.next_token();

        if let Token::Identifier(name) = self.current_token.clone() {
            params.push(Expression::Identifier(name));
        } else {
            panic!("Expected an identifier");
        }

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            if let Token::Identifier(name) = self.current_token.clone() {
                params.push(Expression::Identifier(name));
            } else {
                panic!("Expected an identifier");
            }
        }

        if !self.expect_token(Token::RightParenthesis) {
            panic!("Missing right parsenthesis");
        }
        return params;
    }

    fn parse_if_expression(&mut self) -> Expression {
        if !self.expect_token(Token::LeftParenthesis) {
            self.parsing_errors
                .push(ParsingError::ExpectedLeftParenthesis(
                    self.current_token.clone(),
                ));
            panic!("Did not find left parenthesis");
        };
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest).unwrap();
        if !self.expect_token(Token::RightParenthesis) {
            self.parsing_errors
                .push(ParsingError::ExpectedRightParenthesis(
                    self.current_token.clone(),
                ));
            panic!("Did not find right parenthesis");
        };
        if !self.expect_token(Token::LeftBrace) {
            self.parsing_errors
                .push(ParsingError::ExpectedLeftBrace(self.current_token.clone()));
            panic!("Did not find left brace");
        };
        let consequence = self.parse_block_statment();

        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            if !self.expect_token(Token::LeftBrace) {
                panic!("Not a left brace");
            } else {
                Some(self.parse_block_statment())
            }
        } else {
            None
        };

        return Expression::If(Box::new(condition), consequence, alternative);
    }

    fn parse_block_statment(&mut self) -> BlockStatment {
        let mut block_statment = BlockStatment { statments: vec![] };
        self.next_token();
        while self.current_token != Token::RightBrace && self.current_token != Token::EOF {
            let statment = self.parse_statment();
            if let Some(statment) = statment {
                block_statment.statments.push(statment);
            }

            self.next_token();
        }

        return block_statment;
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();
        if let Some(exp) = self.parse_expression(Precedence::Lowest) {
            if self.expect_token(Token::RightParenthesis) {
                return exp;
            } else {
                self.parsing_errors
                    .push(ParsingError::ExpectedRightParenthesis(
                        self.current_token.clone(),
                    ));
                panic!("Did not find right parenthesis");
            }
        }
        panic!("Error parsing a grouped expression");
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let precendence = get_token_precedence(&self.current_token);
        let operator = InfixOperator::from(&self.current_token);
        self.next_token();
        Expression::Infix(
            Box::new(left),
            operator,
            Box::new(self.parse_expression(precendence).unwrap()),
        )
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
            panic!("Parse int called not on an int")
        }
    }

    fn parse_bool_literal(&mut self) -> Expression {
        match self.current_token {
            Token::True => Expression::Boolean(true),
            Token::False => Expression::Boolean(false),
            _ => panic!("Parse bool called not on a bool"),
        }
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let operator = PrefixOeprator::from(&self.current_token);
        self.next_token();
        if let Some(exp) = self.parse_expression(Precedence::Prefix) {
            Expression::Prefix(operator, Box::new(exp))
        } else {
            panic!("Cound not construct an expresion for a prefix operator");
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
    fn test_if_statment() {
        let input = "if (x < y) {x}";
        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(0, parser.parsing_errors.len());
        assert_eq!(1, program.statments.len());
        if let Statment::Expression(exp) = program.statments.first().unwrap() {
            if let Expression::If(condition, consequence, alternative) = exp {
                assert_eq!("(x < y)", condition.to_string());
                assert_eq!("x", consequence.to_string());
                assert!(!alternative.is_some())
            }
        } else {
            panic!("Not a statment expression.");
        }
    }

    #[test]
    fn test_if_else_statment() {
        let input = "if (x < y) {x} else {y}";
        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(0, parser.parsing_errors.len());
        assert_eq!(1, program.statments.len());
        if let Statment::Expression(exp) = program.statments.first().unwrap() {
            if let Expression::If(condition, consequence, alternative) = exp {
                assert_eq!("(x < y)", condition.to_string());
                assert_eq!("x", consequence.to_string());
                if let Some(alternative) = alternative {
                    assert_eq!("y", alternative.to_string());
                } else {
                    panic!("Missing alternative.");
                }
            }
        } else {
            panic!("Not an expression.");
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
            test_integer_literal(exp, 5)
        } else {
            panic!("Not an expression!");
        }
    }

    #[test]
    fn test_bool_literal_parser() {
        let input = "true; false;";
        let expected_output = [true, false];
        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(0, parser.parsing_errors.len());
        assert_eq!(2, program.statments.len());
        for (statment, desired_value) in program.statments.iter().zip(expected_output) {
            if let Statment::Expression(exp) = statment {
                test_bool_literal_expression(exp, desired_value);
            } else {
                panic!("Not an expression!");
            }
        }
    }

    #[test]
    fn test_function_literal_parser() {
        let input = "fn(x,y){ x+y; }";
        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(0, parser.parsing_errors.len());
        assert_eq!(1, program.statments.len());
        if let Statment::Expression(ex) = program.statments.first().unwrap().clone() {
            if let Expression::Function(params, body) = ex {
                assert_eq!(2, params.len());
                assert_eq!("x", params[0].to_string());
                assert_eq!("y", params[1].to_string());
                assert_eq!(1, body.statments.len());
                assert_eq!("(x + y)", body.statments.first().unwrap().to_string());
            }
        } else {
            panic!("Not an expression");
        }
    }

    #[test]
    fn test_params_parseing() {
        let inputs = [
            ("fn(){}", vec![]),
            ("fn(x){}", vec!["x"]),
            ("fn(x,y){}", vec!["x", "y"]),
            ("fn(x,y,z){}", vec!["x", "y", "z"]),
        ];
        for input in inputs {
            let lexer = Lexer::new(String::from(input.0));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert_eq!(1, program.statments.len());
            if let Statment::Expression(ex) = program.statments.first().unwrap().clone() {
                if let Expression::Function(params, _body) = ex {
                    assert_eq!(input.1.len(), params.len());
                    for (expected, actuall) in input.1.iter().zip(params.iter()) {
                        if let Expression::Identifier(name) = actuall {
                            assert_eq!(expected, name);
                        } else {
                            panic!("Not an identifier");
                        }
                    }
                } else {
                    panic!("Not a function Expression");
                }
            } else {
                panic!("Not a statment");
            }
        }
    }

    #[test]
    fn test_call_parsing() {
        let input = "add(1, 2*2, 3+4)";
        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(1, program.statments.len());
        if let Statment::Expression(ex) = program.statments.first().unwrap().clone() {
            if let Expression::Call(function, args) = ex {
                assert_eq!(3, args.len());
                assert_eq!("1", args[0].to_string());
                assert_eq!("(2 * 2)", args[1].to_string());
                assert_eq!("(3 + 4)", args[2].to_string());
                if **function != Expression::Identifier(String::from("add")) {
                    panic!("Not an identifier");
                }
            } else {
                panic!("Not a call Expression");
            }
        } else {
            panic!("Not a statment");
        }
    }

    fn test_bool_literal_expression(expression: &Expression, desired_value: bool) {
        if let Expression::Boolean(value) = expression {
            assert_eq!(desired_value, *value);
        } else {
            panic!("Not a boolean expression");
        }
    }

    fn test_integer_literal(expression: &Expression, desired_value: i32) {
        if let Expression::IntegerLiteral(value) = expression {
            assert_eq!(desired_value, *value);
        } else {
            panic!("Not an Integer Literal");
        }
    }

    #[test]
    fn test_prefix_parser() {
        let inputs = [("!5", "!", 5), ("-15", "-", 15)];
        for input in inputs.iter() {
            let lexer = Lexer::new(String::from(input.0));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert_eq!(0, parser.parsing_errors.len());
            assert_eq!(1, program.statments.len());
            let statment = program.statments.first().unwrap().clone();
            if let Statment::Expression(exp) = statment {
                if let Expression::Prefix(operator, right) = exp {
                    assert_eq!(input.1, operator.to_string());
                    test_integer_literal(right, input.2);
                } else {
                    panic!("Not a Prefix Expression");
                }
            } else {
                panic!("Not an expression!");
            }
        }
    }

    #[test]
    fn test_infix_parser() {
        let inputs = [
            ("5 + 5", 5, "+", 5),
            ("5 - 5", 5, "-", 5),
            ("5 * 5", 5, "*", 5),
            ("5 / 5", 5, "/", 5),
            ("5 < 5", 5, "<", 5),
            ("5 > 5", 5, ">", 5),
            ("5 == 5", 5, "==", 5),
            ("5 != 5", 5, "!=", 5),
        ];
        for input in inputs.iter() {
            let lexer = Lexer::new(String::from(input.0));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert_eq!(0, parser.parsing_errors.len());
            assert_eq!(1, program.statments.len());
            let statment = program.statments.first().unwrap().clone();
            if let Statment::Expression(exp) = statment {
                if let Expression::Infix(left, operator, right) = exp {
                    assert_eq!(input.2, operator.to_string());
                    test_integer_literal(left, input.1);
                    test_integer_literal(right, input.3);
                } else {
                    panic!("Not an Infix Expression");
                }
            } else {
                panic!("Not an expression!");
            }
        }
    }

    #[test]
    fn test_infix_bool_parser() {
        let inputs = [
            ("true == true", true, "==", true),
            ("false != true", false, "!=", true),
            ("false == false", false, "==", false),
        ];
        for input in inputs.iter() {
            let lexer = Lexer::new(String::from(input.0));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert_eq!(0, parser.parsing_errors.len());
            assert_eq!(1, program.statments.len());
            let statment = program.statments.first().unwrap().clone();
            if let Statment::Expression(exp) = statment {
                if let Expression::Infix(left, operator, right) = exp {
                    assert_eq!(input.2, operator.to_string());
                    test_bool_literal_expression(left, input.1);
                    test_bool_literal_expression(right, input.3);
                } else {
                    panic!("Not an Infix Expression");
                }
            } else {
                panic!("Not an expression!");
            }
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

    #[test]
    fn test_precedence() {
        let inputs = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];
        for input in inputs.iter() {
            let lexer = Lexer::new(String::from(input.0));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert_eq!(0, parser.parsing_errors.len());
            assert_eq!(input.1, program.to_string());
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
