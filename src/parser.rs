pub mod ast_structs;
pub mod ast_traits;

use crate::tokenizer::lexer::Lexer;
use crate::tokenizer::Token;

pub struct Parser<'a> {
    lexer: 'a Lexer,
    curToken: Option<Token>,
    nextToken: Option<Token>,
}

impl Parser<'_> {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            lexer,
            curToken: None,
            nextToken: None,
        }
    }

    pub fn make_ast() -> String {
        todo!()
    }
}
