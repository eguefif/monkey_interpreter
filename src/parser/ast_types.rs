use crate::tokenizer::{Token, TokenType};
use std::{
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
};

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
    Index,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Expression {
    Identifier(Identifier),
    Int(Integer),
    Str(Str),
    Index(IndexExpression),
    Array(ArrayLitteral),
    Hash(HashLitteral),
    Boolean(Bool),
    PrefixOp(PrefixExpression),
    InfixOp(InfixExpression),
    If(IfExpression),
    Function(FunctionExpression),
    CallExpression(CallExpression),
    None,
}

impl Hash for Expression {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            Expression::Hash(value) => panic!("Expression null is not a hashmap key type"),
            Expression::Index(value) => value.hash(state),
            Expression::Array(value) => value.hash(state),
            Expression::Str(value) => value.hash(state),
            Expression::Identifier(value) => value.hash(state),
            Expression::Int(value) => value.hash(state),
            Expression::PrefixOp(value) => value.hash(state),
            Expression::InfixOp(value) => value.hash(state),
            Expression::Boolean(value) => value.hash(state),
            Expression::If(value) => value.hash(state),
            Expression::Function(value) => value.hash(state),
            Expression::CallExpression(value) => value.hash(state),
            Expression::None => panic!("Expression null is not a hashmap key type"),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Hash(value) => write!(f, "{}", value),
            Expression::Index(value) => write!(f, "{}", value),
            Expression::Array(value) => write!(f, "{}", value),
            Expression::Str(value) => write!(f, "{}", value),
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Integer {
    pub token: Token,
    pub value: i128,
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Str {
    pub token: Token,
    pub value: String,
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

impl fmt::Display for InfixType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            InfixType::Add => "+",
            InfixType::Sub => "-",
            InfixType::Mul => "*",
            InfixType::Div => "/",
            InfixType::Gt => ">",
            InfixType::Lt => "<",
            InfixType::Eq => "=",
            InfixType::Noteq => "!=",
            InfixType::None => "",
        };
        write!(f, "{text}")
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct FunctionExpression {
    pub token: Token,
    pub params: Vec<Identifier>,
    pub block: BlockStatement,
}

impl FunctionExpression {
    pub fn copy_params(&self) -> Vec<Identifier> {
        let mut retval: Vec<Identifier> = Vec::new();
        for param in self.params.iter() {
            let ident = Identifier {
                token: Token {
                    token_type: param.token.token_type.clone(),
                    litteral: param.token.litteral.clone(),
                },
                value: param.value.clone(),
            };
            retval.push(ident);
        }
        retval
    }

    pub fn copy_block(&self) -> BlockStatement {
        let mut statements: Vec<Statement> = Vec::new();

        for statement in self.block.statements.iter() {
            statements.push(statement.clone());
        }
        BlockStatement { statements }
    }
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
        write!(f, "fn({}) {{\n{}\n}}", params, self.block)
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut args = String::new();
        for arg in self.args.iter() {
            args.push_str(format!("{}, ", arg).as_str());
        }
        args.pop();
        args.pop();
        write!(f, "{}({})", self.function, args)
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ArrayLitteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl fmt::Display for ArrayLitteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = String::new();

        for element in self.elements.iter() {
            str.push_str(format!("{}, ", element).as_str());
        }
        str.pop();
        str.pop();

        write!(f, "[{}]", str)
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Debug, Clone, Eq)]
pub struct HashLitteral {
    pub token: Token,
    pub elements: HashMap<Expression, Expression>,
}

impl fmt::Display for HashLitteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = String::new();

        for (key, value) in self.elements.iter() {
            str.push_str(format!("{}: {}, ", key, value).as_str());
        }
        str.pop();
        str.pop();

        write!(f, "{{{}}}", str)
    }
}

impl PartialEq for HashLitteral {
    fn eq(&self, other: &HashLitteral) -> bool {
        let str_self = format!("{}", self);
        let other_self = format!("{}", other);
        str_self == other_self
    }
    fn ne(&self, other: &HashLitteral) -> bool {
        let str_self = format!("{}", self);
        let other_self = format!("{}", other);
        str_self != other_self
    }
}
