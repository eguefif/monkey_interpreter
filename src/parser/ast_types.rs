use crate::tokenizer::Token;

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

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Int(Integer),
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

#[derive(Debug)]
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

#[derive(Debug)]
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
