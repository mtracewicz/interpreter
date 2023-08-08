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
    MissingPrefixParsFunction(Token),
    MissingInfixParseFunction(Token),
    ParseBoolDidNotGetBoolToken(Token),
    PrefixFunctionNotFound(Token),
}
type PrefixParserFn = fn(&mut Parser) -> Result<Expression, ParsingError>;
type InfixParserFn = fn(&mut Parser, Expression) -> Result<Expression, ParsingError>;

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
            match self.parse_statment() {
                Ok(statment) => program.statments.push(statment),
                Err(error) => self.parsing_errors.push(error),
            }
            self.next_token();
        }
        return program;
    }

    fn parse_statment(&mut self) -> Result<Statment, ParsingError> {
        match self.current_token {
            Token::Let => self.parse_let_statment(),
            Token::Return => self.parse_return_statment(),
            _ => self.parse_expression_statement(),
        }
    }

    fn read_till_next_statment(&mut self) {
        while self.current_token != Token::Semicolon && self.current_token != Token::EOF {
            self.next_token();
        }
    }

    fn parse_let_statment(&mut self) -> Result<Statment, ParsingError> {
        let name: String;
        if let Token::Identifier(identifier) = self.peek_token.clone() {
            self.next_token();
            name = identifier;
        } else {
            let invalid_token = self.peek_token.clone();
            self.read_till_next_statment();
            return Err(ParsingError::ExpectedIdentifier(invalid_token));
        }
        if !self.expect_token(Token::Assign) {
            let invalid_token = self.peek_token.clone();
            self.read_till_next_statment();
            return Err(ParsingError::ExpectedAssign(invalid_token));
        }

        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        self.read_till_next_statment();

        Ok(Statment::Let(name, value))
    }

    fn parse_return_statment(&mut self) -> Result<Statment, ParsingError> {
        self.next_token();
        let literal = self.parse_expression(Precedence::Lowest)?;
        self.read_till_next_statment();

        return Ok(Statment::Return(literal));
    }

    fn parse_expression_statement(&mut self) -> Result<Statment, ParsingError> {
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        return match expression {
            Ok(expr) => Ok(Statment::Expression(expr)),
            Err(err) => Err(err),
        };
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParsingError> {
        let left = if let Ok(prefix) = Parser::prefix_parse_functions(&self.current_token) {
            Ok(prefix(self))
        } else {
            Err(ParsingError::PrefixFunctionNotFound(
                self.current_token.clone(),
            ))
        };
        if let Ok(mut left_expression) = left {
            while self.peek_token != Token::Semicolon
                && precedence < get_token_precedence(&self.peek_token)
            {
                let infix_fn = Parser::infix_parse_function(&self.peek_token);
                if let Ok(f) = infix_fn {
                    self.next_token();
                    if let Ok(le) = left_expression {
                        left_expression = f(self, le);
                    } else {
                        return left_expression;
                    }
                } else {
                    return left_expression;
                }
            }
            return left_expression;
        } else {
            return left?;
        }
    }

    fn prefix_parse_functions(token: &Token) -> Result<PrefixParserFn, ParsingError> {
        match token {
            Token::Identifier(_name) => Ok(Parser::parse_identifier),
            Token::Integer(_) => Ok(Parser::parse_integer_literal),
            Token::True | Token::False => Ok(Parser::parse_bool_literal),
            Token::Bang | Token::Minus => Ok(Parser::parse_prefix_expression),
            Token::LeftParenthesis => Ok(Parser::parse_grouped_expression),
            Token::If => Ok(Parser::parse_if_expression),
            Token::Function => Ok(Parser::parse_function_expression),
            _ => Err(ParsingError::MissingPrefixParsFunction(token.clone())),
        }
    }

    fn infix_parse_function(token: &Token) -> Result<InfixParserFn, ParsingError> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::LessThen
            | Token::GreaterThen
            | Token::Equal
            | Token::NotEqual => Ok(Parser::parse_infix_expression),
            Token::LeftParenthesis => Ok(Parser::parse_call_expression),
            _ => Err(ParsingError::MissingInfixParseFunction(token.clone())),
        }
    }

    fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, ParsingError> {
        Ok(Expression::Call(
            Box::new(left),
            self.parse_call_arguments(),
        ))
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

    fn parse_function_expression(&mut self) -> Result<Expression, ParsingError> {
        if !self.expect_token(Token::LeftParenthesis) {
            panic!("Not a left parenthesis.");
        }
        let params = self.parse_parameters();
        if !self.expect_token(Token::LeftBrace) {
            panic!("Not a left brace.");
        }
        let body = self.parse_block_statment();
        return Ok(Expression::Function(params, body));
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

    fn parse_if_expression(&mut self) -> Result<Expression, ParsingError> {
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

        return Ok(Expression::If(
            Box::new(condition),
            consequence,
            alternative,
        ));
    }

    fn parse_block_statment(&mut self) -> BlockStatment {
        let mut block_statment = BlockStatment { statments: vec![] };
        self.next_token();
        while self.current_token != Token::RightBrace && self.current_token != Token::EOF {
            let statment = self.parse_statment();
            if let Ok(statment) = statment {
                block_statment.statments.push(statment);
            }

            self.next_token();
        }

        return block_statment;
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParsingError> {
        self.next_token();
        if let Ok(exp) = self.parse_expression(Precedence::Lowest) {
            if self.expect_token(Token::RightParenthesis) {
                return Ok(exp);
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

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParsingError> {
        let precendence = get_token_precedence(&self.current_token);
        let operator = InfixOperator::from(&self.current_token);
        self.next_token();
        Ok(Expression::Infix(
            Box::new(left),
            operator,
            Box::new(self.parse_expression(precendence).unwrap()),
        ))
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParsingError> {
        if let Token::Identifier(name) = self.current_token.clone() {
            Ok(Expression::Identifier(name))
        } else {
            panic!("Parse identifier called not on an identifier")
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParsingError> {
        if let Token::Integer(value) = self.current_token.clone() {
            if let Ok(i) = value.parse::<i32>() {
                Ok(Expression::IntegerLiteral(i))
            } else {
                panic!("Parse int got not an int");
            }
        } else {
            panic!("Parse int called not on an int")
        }
    }

    fn parse_bool_literal(&mut self) -> Result<Expression, ParsingError> {
        match self.current_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => Err(ParsingError::ParseBoolDidNotGetBoolToken(
                self.current_token.clone(),
            )),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParsingError> {
        let operator = PrefixOeprator::from(&self.current_token);
        self.next_token();
        if let Ok(exp) = self.parse_expression(Precedence::Prefix) {
            Ok(Expression::Prefix(operator, Box::new(exp)))
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
        let y = true;
        let foobar = y;";

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(3, program.statments.len());
        let expected_values = [
            (String::from("x"), "5"),
            (String::from("y"), "true"),
            (String::from("foobar"), "y"),
        ];
        for (i, value) in expected_values.iter().enumerate() {
            let statment = &program.statments[i];
            test_let_statment(&(&value.0, value.1), statment);
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

    fn test_let_statment(value: &(&String, &str), statment: &Statment) {
        if let Statment::Let(name, exp) = statment {
            assert_eq!(value.0, name);
            assert_eq!(value.1, exp.to_string());
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
