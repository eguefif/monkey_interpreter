use crate::parser::ast_structs::{Identifier, LetStatement, Program};
use crate::parser::ast_traits::Statement;
use crate::tokenizer::lexer::Lexer;

pub mod ast_structs;
pub mod ast_traits;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a, T: Statement> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn parse_program(&mut self) -> Program<T>
    where
        T: Statement,
    {
        let token = self.lexer.next().unwrap();
        let s1 = LetStatement {
            token: token.clone(),
            identifier: Identifier {
                token: token.clone(),
                value: token.litteral.clone(),
            },
            value: Identifier {
                token: token.clone(),
                value: token.litteral.clone(),
            },
        };
        let mut v: Vec<T> = Vec::new();
        v.push(s1);
        Program { statements: v }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::lexer::Lexer;

    #[test]
    fn it_should_parse_let_statement() {
        let input = "let x =5;
let y = 10;
let foobar = 838383;
    ";
        let lexer = Lexer::new(&input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program();
    }
}
