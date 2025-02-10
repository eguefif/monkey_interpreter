use crate::parser::ast_types::{Expression, Identifier, LetStatement, Program, Statement};
use crate::tokenizer::lexer::Lexer;

pub mod ast_types;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let token = self.lexer.next().unwrap();
        let s1 = LetStatement {
            token: token.clone(),
            identifier: Identifier {
                token: token.clone(),
                value: token.litteral.clone(),
            },
            value: Expression {
                value: token.litteral.clone(),
            },
        };
        let mut v: Vec<Statement> = Vec::new();
        let stm = Statement::Let(s1);
        v.push(stm);
        Some(Program { statements: v })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast_types::Statement::Let;
    use crate::tokenizer::lexer::Lexer;

    #[test]
    fn it_should_parse_let_statement() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;
    ";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();
        assert_eq!(program.len(), 3);

        let expected_identifiers = ["x", "y", "foobar"];
        for (i, stmt) in program.statements.iter().enumerate() {
            assert!(check_let_statement(stmt, expected_identifiers[i]));
        }
    }

    fn check_let_statement(smt: &Statement, identifiers: &str) -> bool {
        if let Let(smt) = smt {
            if smt.identifier.value.as_str() == identifiers {
                return true;
            }
        }
        false
    }
}
