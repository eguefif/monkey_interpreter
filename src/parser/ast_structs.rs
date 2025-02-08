use crate::parser::ast_traits::{Expression, Node, Statement};
use crate::tokenizer::Token;

struct Program<T: Statement> {
    statements: Vec<T>,
}

impl<T: Statement> Program<T> {
    pub fn new() -> Self {
        Self {
            statements: Vec::with_capacity(200),
        }
    }
}
/*
struct LetStatement<T: None, LetStatement> {
    token: Token,
    identifier: Identifier,
    value: T,
}

impl Node for LetStatement {
    fn token_litteral(&self) -> String {
        self.token.litteral
    }
}

impl Statement<T> for LetStatement {
    fn statement_node(&self) {}
}

struct Identifier {
    token: Token,
    value: String,
}

impl Node for Identifier {
    fn token_litteral(&self) -> String {
        self.value
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}
*/
