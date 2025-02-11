use crate::parser::ast_types::{
    Expression, Identifier, LetStatement, Program, ReturnStatement, Statement,
};
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
            TokenType::Return => Some(Statement::Return(self.parse_returnstatement(token))),
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

    fn parse_returnstatement(&mut self, token: Token) -> ReturnStatement {
        while let Some(token) = self.lexer.next() {
            if token.token_type == TokenType::Semicolon {
                break;
            }
        }
        ReturnStatement {
            token: token,
            return_value: Expression {
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
        panic!(
            "Error: expect {:?} but got {:?}",
            next.token_type, token_type
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast_types::Statement::{Let, Return};
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

    #[test]
    fn it_should_parse_return_statement() {
        let input = "return 5;
return 10;
return add(5, 1);
;
    ";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();
        assert_eq!(program.len(), 3);
        for (i, stmt) in program.statements.iter().enumerate() {
            if let Return(stmt) = stmt {
                assert_eq!(stmt.token.token_type, TokenType::Return)
            } else {
                panic!("Failed: statement is None")
            }
        }
    }

    #[test]
    #[should_panic]
    fn it_should_panic_with_error_expect_token() {
        let input = "let x 3 = 5;
    ";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();
    }
}
