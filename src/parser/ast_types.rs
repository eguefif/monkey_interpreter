use crate::tokenizer::{Token, TokenType};
use std::fmt;

#[derive(Debug, PartialEq, PartialOrd)]
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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(_) => write!(f, ""),
            Statement::Return(_) => write!(f, ""),
            Statement::Expression(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Int(Integer),
    PrefixOp(PrefixExpression),
    InfixOp(InfixExpression),
    None,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(value) => write!(f, "{}", value),
            Expression::Int(value) => write!(f, "{}", value),
            Expression::PrefixOp(value) => write!(f, "{}", value),
            Expression::InfixOp(value) => write!(f, "{}", value),
            Expression::None => write!(f, ""),
        }
    }
}

#[derive(Debug)]
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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut statements_str = String::new();
        for statement in self.statements.iter() {
            statements_str.push_str(&format!("{}", statement))
        }
        write!(f, "{}", statements_str)
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub value: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.identifier, self.value)
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {}", self.return_value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
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

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, PartialEq)]
pub struct Integer {
    pub token: Token,
    pub value: i128,
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub enum PrefixType {
    Minus,
    Bang,
    Add,
    Mul,
    Div,
    Eq,
    Noteq,
    Gt,
    Lt,
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub prefix_type: PrefixType,
    pub right: Box<Expression>,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.token.litteral, *self.right)
    }
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

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} {} {})",
            *self.left, self.token.litteral, *self.right
        )
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
