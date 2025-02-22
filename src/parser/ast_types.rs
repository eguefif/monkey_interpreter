use crate::tokenizer::{Token, TokenType};
use std::fmt;

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

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(value) => write!(f, "{}", value),
            Statement::Return(value) => write!(f, "{}", value),
            Statement::Expression(value) => write!(f, "{}", value),
        }
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
        write!(f, "let {} = {};\n", self.identifier, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.return_value {
            None => write!(f, "return;"),
            Some(value) => write!(f, "return {};", value),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Int(Integer),
    Boolean(Bool),
    PrefixOp(PrefixExpression),
    InfixOp(InfixExpression),
    If(IfExpression),
    Function(FunctionExpression),
    CallExpression(CallExpression),
    None,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(value) => write!(f, "{}", value),
            Expression::Int(value) => write!(f, "{}", value),
            Expression::PrefixOp(value) => write!(f, "{}", value),
            Expression::InfixOp(value) => write!(f, "{}", value),
            Expression::Boolean(value) => write!(f, "{}", value),
            Expression::If(value) => write!(f, "{}", value),
            Expression::Function(value) => write!(f, "{}", value),
            Expression::CallExpression(value) => write!(f, "{}", value),
            Expression::None => write!(f, ""),
        }
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

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.expression {
            Expression::If(value) => write!(f, "{}\n", value),
            Expression::Function(value) => write!(f, "{}\n", value),
            _ => write!(f, "{};\n", self.expression),
        }
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

#[derive(Debug, PartialEq)]
pub struct Bool {
    pub token: Token,
    pub value: bool,
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Bool {
    pub fn new(token: Token) -> Self {
        let value = match token.token_type {
            TokenType::True => true,
            TokenType::False => false,
            _ => panic!("Bool can only be true or false"),
        };
        Self { value, token }
    }
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(alternative) = &self.alternative {
            write!(
                f,
                "if {}\n{{\n{}}} else {{\n{}}}",
                self.condition, self.consequence, alternative
            )
        } else {
            write!(f, "if {}\n{{{}}}", self.condition, self.consequence)
        }
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl PartialEq for BlockStatement {
    fn eq(&self, other: &Self) -> bool {
        if self.statements.len() != other.statements.len() {
            return false;
        }
        for (i, s) in self.statements.iter().enumerate() {
            if *s != other.statements[i] {
                return false;
            }
        }
        return true;
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut block = String::new();
        for statement in self.statements.iter() {
            block.push_str(&format!("{}", statement));
        }
        write!(f, "{}", block)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionExpression {
    pub token: Token,
    pub params: Vec<Identifier>,
    pub block: BlockStatement,
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut params = String::new();
        for (i, param) in self.params.iter().enumerate() {
            if i == self.params.len() - 1 {
                params.push_str(format!("{}", param.token.litteral).as_str());
            } else {
                params.push_str(format!("{}, ", param.token.litteral).as_str());
            }
        }
        write!(
            f,
            "fn {}({}) {{\n{}\n}}",
            self.token.litteral, params, self.block
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function call")
    }
}
