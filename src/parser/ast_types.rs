use crate::tokenizer::Token;

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

pub enum Statement {
    Let(LetStatement),
    Return,
    Expr,
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

pub struct LetStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub value: Expression,
}

pub struct Expression {
    pub value: String,
}

pub struct Identifier {
    pub token: Token,
}

impl Identifier {
    pub fn get_value(&self) -> &str {
        self.token.litteral.as_str()
    }
}
