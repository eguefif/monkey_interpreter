use crate::parser::ast_types::{Expression, Identifier, LetStatement, Program, Statement};
use crate::tokenizer::lexer::Lexer;
use crate::tokenizer::Token;

pub mod ast_types;

pub fn parse_program(lexer: Lexer) -> Option<Program> {
    let mut statements: Vec<Statement> = Vec::new();
    for token in lexer {
        if let Some(statement) = parse_statement(token) {
            statements.push(statement);
        } else {
            break;
        }
    }
    Some(Program {
        statements: statements,
    })
}

fn parse_statement(token: Token) -> Option<Statement> {
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
    let stm = Statement::Let(s1);
    Some(stm)
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
        let program: Program = parse_program(lexer).unwrap();
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
