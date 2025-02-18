use crate::tokenizer::{Token, TokenType};

// TODO: add format display functions to transform everyting into a string we can just compare in
// tests

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Int(Integer),
    PrefixOp(PrefixExpression),
    InfixOp(InfixExpression),
    None,
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::with_capacity(200),
        }
    }

    pub fn len(&self) -> usize {
        self.statements.len()
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn get_value(&self) -> &str {
        self.token.litteral.as_str()
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Integer {
    pub token: Token,
    pub value: i128,
}

#[derive(Debug, PartialEq)]
pub enum PrefixType {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub prefix_type: PrefixType,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum InfixType {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Eq,
    Noteq,
    None,
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub infix_type: InfixType,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(left: Expression, right: Expression, token: Token) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            infix_type: get_infix_type(&token.token_type),
            token,
        }
    }
}
fn get_infix_type(token: &TokenType) -> InfixType {
    match token {
        TokenType::Plus => InfixType::Add,
        TokenType::Minus => InfixType::Sub,
        TokenType::Equal => InfixType::Eq,
        TokenType::Noteq => InfixType::Noteq,
        TokenType::Lt => InfixType::Lt,
        TokenType::Gt => InfixType::Gt,
        TokenType::Asterisk => InfixType::Mul,
        TokenType::Slash => InfixType::Div,
        _ => InfixType::None,
    }
}
