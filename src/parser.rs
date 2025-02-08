pub mod ast_structs;
pub mod ast_traits;

use crate::tokenizer::lexer::Lexer;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn make_ast(&mut self) -> String {
        todo!()
    }
}
