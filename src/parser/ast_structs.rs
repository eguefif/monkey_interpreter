use crate::parser::ast_traits::Statement;
use crate::tokenizer::Token;

struct Program {
    statements: Vec<impl Statement>,
}

impl Program{
    pub fn new() -> Self{
        Self {
            statements: Vec::with_capacity(200),
        }
    }
}

struct<T: Node, Statement> LetStatement<T> {
    token: Token,
    identifier: Identifier,
    value: T,
}

impl Node for LetStatement{
    fn token_litteral(&self) -> String{
        self.token.litteral
    }
}

impl Statement for LetStatement{
    fn statement_node(&self) {}
}

struct Identifier {
    token: Token,
    value: String,
}
