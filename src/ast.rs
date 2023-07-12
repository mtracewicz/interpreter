use std::fmt::Display;

use crate::lexer::Token;

pub struct Program {
    pub statments: Vec<Statment>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.statments.iter().for_each(|statment| {
            write!(f, "{}", statment).unwrap();
        });
        Ok(())
    }
}

pub enum Statment {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

impl Display for Statment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statment::Let(identifier, expression) => {
                write!(f, "let {} = {};", identifier, expression)
            }
            Statment::Return(expression) => write!(f, "return {};", expression),
            Statment::Expression(expression) => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntegerLiteral(i32),
    Boolean(bool),
    Identifier(String),
    Prefix(PrefixOeprator, Box<Expression>),
    Infix(Box<Expression>, InfixOperator, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IntegerLiteral(i) => write!(f, "{}", i),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::Prefix(op, exp) => write!(f, "({}{})", op, exp),
            Expression::Infix(l, o, r) => write!(f, "({} {} {})", l, o, r),
            Expression::Identifier(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PrefixOeprator {
    Bang,
    Minus,
}

impl Display for PrefixOeprator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOeprator::Bang => write!(f, "!"),
            PrefixOeprator::Minus => write!(f, "-"),
        }
    }
}

impl From<&Token> for PrefixOeprator {
    fn from(token: &Token) -> PrefixOeprator {
        match token {
            Token::Bang => PrefixOeprator::Bang,
            Token::Minus => PrefixOeprator::Minus,
            _ => panic!("Error converting token to operator"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum InfixOperator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    LessThen,
    GreaterThen,
    Equal,
    NotEqual,
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Asterisk => write!(f, "*"),
            InfixOperator::Slash => write!(f, "/"),
            InfixOperator::LessThen => write!(f, "<"),
            InfixOperator::GreaterThen => write!(f, ">"),
            InfixOperator::Equal => write!(f, "=="),
            InfixOperator::NotEqual => write!(f, "!="),
        }
    }
}

impl From<&Token> for InfixOperator {
    fn from(token: &Token) -> InfixOperator {
        match token {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Asterisk,
            Token::Slash => InfixOperator::Slash,
            Token::LessThen => InfixOperator::LessThen,
            Token::GreaterThen => InfixOperator::GreaterThen,
            Token::Equal => InfixOperator::Equal,
            Token::NotEqual => InfixOperator::NotEqual,
            _ => panic!("Error converting token to operator"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equal,
    LesserGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub fn get_token_precedence(token: &Token) -> Precedence {
    match token {
        Token::Equal => Precedence::Equal,
        Token::NotEqual => Precedence::Equal,
        Token::LessThen => Precedence::LesserGreater,
        Token::GreaterThen => Precedence::LesserGreater,
        Token::Minus => Precedence::Sum,
        Token::Plus => Precedence::Sum,
        Token::Slash => Precedence::Product,
        Token::Asterisk => Precedence::Product,
        _ => Precedence::Lowest,
    }
}
