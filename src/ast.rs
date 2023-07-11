use std::fmt::Display;

use crate::lexer::Token;

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
    Identifier(String),
    Prefix(String, Box<Expression>),
    Infix(Box<Expression>, String, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IntegerLiteral(i) => write!(f, "{}", i),
            Expression::Prefix(op, exp) => write!(f, "({}{})", op, exp),
            Expression::Infix(l, o, r) => write!(f, "({} {} {})", l, o, r),
            Expression::Identifier(i) => write!(f, "{}", i),
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
