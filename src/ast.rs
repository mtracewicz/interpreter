pub enum Statment {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntegerLiteral(i32),
    Identifier(String),
    Prefix(String, Box<Expression>),
    Infix(Box<Expression>, String, Box<Expression>),
}

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
