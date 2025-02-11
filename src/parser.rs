use crate::parser::ast_types::{Expression, Identifier, LetStatement, Program, Statement};
use crate::tokenizer::lexer::Lexer;
use crate::tokenizer::{Token, TokenType};

pub mod ast_types;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut statements: Vec<Statement> = Vec::new();
        loop {
            if let Some(token) = self.lexer.next() {
                if let Some(statement) = self.parse_statement(token) {
                    statements.push(statement);
                }
            } else {
                break;
            }
        }
        Some(Program {
            statements: statements,
        })
    }

    fn parse_statement(&mut self, token: Token) -> Option<Statement> {
        match token.token_type {
            TokenType::Let => Some(Statement::Let(self.parse_letstatement(token))),
            _ => None,
        }
    }

    fn parse_letstatement(&mut self, token: Token) -> LetStatement {
        let ident = self
            .lexer
            .next()
            .expect("Error: end of file before end of let statement");
        if !self.expect_token(TokenType::Assign) {
            panic!("Error: let statement does not have =");
        }
        let value = self
            .lexer
            .next()
            .expect("Error: end of file before end of let statement");
        while let Some(token) = self.lexer.next() {
            if token.token_type == TokenType::Semicolon {
                break;
            }
        }
        LetStatement {
            token: token,
            identifier: Identifier { token: ident },
            value: Expression {
                value: "mock".to_string(),
            },
        }
    }

    fn expect_token(&mut self, token_type: TokenType) -> bool {
        let next = self
            .lexer
            .next()
            .expect("Error: reach the EOF. Should have a token");
        if next.token_type == token_type {
            return true;
        }
        return false;
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
            if smt.identifier.get_value() == identifiers {
                return true;
            }
        }
        false
    }
}
