use ast_types::{Bool, InfixExpression, Integer, PrefixExpression, PrefixType};

use crate::parser::ast_types::{
    Expression, ExpressionStatement, Identifier, LetStatement, Precedence, Program,
    ReturnStatement, Statement,
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
            TokenType::Let => Some(Statement::Let(self.parse_let_statement(token))),
            TokenType::Return => Some(Statement::Return(self.parse_return_statement(token))),
            _ => Some(Statement::Expression(
                self.parse_expression_statement(token),
            )),
        }
    }

    fn parse_let_statement(&mut self, token: Token) -> LetStatement {
        let ident = self
            .lexer
            .next()
            .expect("Error: end of file before end of let statement");
        if !self.expect_token(TokenType::Assign) {
            panic!("Error: let statement does not have =");
        }
        let let_expression = if let Some(next_token) = self.lexer.next() {
            LetStatement {
                token: token.clone(),
                identifier: Identifier {
                    value: ident.litteral.clone(),
                    token: ident,
                },
                value: self.parse_expression(next_token, Precedence::Lowest),
            }
        } else {
            panic!("Missing right expression in let statement");
        };
        if let Some(next_token) = self.lexer.next() {
            if next_token.token_type != TokenType::Semicolon {
                panic!("Missing ; after let expression")
            }
        }
        let_expression
    }

    fn parse_return_statement(&mut self, token: Token) -> ReturnStatement {
        while let Some(token) = self.lexer.next() {
            if token.token_type == TokenType::Semicolon {
                break;
            }
        }
        ReturnStatement {
            token: token.clone(),
            return_value: Expression::Identifier(Identifier {
                value: token.litteral.clone(),
                token: token,
            }),
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

    fn parse_expression_statement(&mut self, token: Token) -> ExpressionStatement {
        let expression = self.parse_expression(token.clone(), Precedence::Lowest);

        if let Some(next_token) = self.lexer.peek() {
            if next_token.token_type == TokenType::Semicolon {
                self.lexer.next();
            }
        }
        ExpressionStatement { token, expression }
    }

    fn parse_expression(&mut self, token: Token, precedence: Precedence) -> Expression {
        let prefix = match token.token_type {
            TokenType::Ident => self.parse_identifier(token),
            TokenType::Int(value) => self.parse_number(token, value),
            TokenType::Bang => self.parse_bang(token),
            TokenType::Minus => self.parse_minus(token),
            TokenType::True | TokenType::False => self.parse_bool(token),
            _ => todo!("not yet implement {:?}", token),
        };
        let mut left_expression = prefix;
        while let Some(peek) = self.lexer.peek() {
            if peek.token_type != TokenType::Semicolon
                && precedence < get_precedence(&peek.token_type)
            {
                let next_token = self.lexer.next().expect("Infix expression has no operator");
                left_expression = self.parse_infix_expression(left_expression, next_token);
            } else {
                break;
            }
        }
        left_expression
    }

    fn parse_bool(&mut self, token: Token) -> Expression {
        Expression::Boolean(Bool::new(token))
    }

    fn parse_identifier(&mut self, token: Token) -> Expression {
        Expression::Identifier(Identifier {
            value: token.litteral.clone(),
            token: token,
        })
    }
    fn parse_number(&mut self, token: Token, value: i128) -> Expression {
        Expression::Int(Integer { value, token })
    }

    fn parse_bang(&mut self, token: Token) -> Expression {
        Expression::PrefixOp(PrefixExpression {
            token: token,
            prefix_type: PrefixType::Bang,
            right: {
                if let Some(token) = self.lexer.next() {
                    Box::new(self.parse_expression(token, Precedence::Prefix))
                } else {
                    panic!("Prefix operator has no right expression")
                }
            },
        })
    }

    fn parse_minus(&mut self, token: Token) -> Expression {
        Expression::PrefixOp(PrefixExpression {
            token: token,
            prefix_type: PrefixType::Minus,
            right: {
                if let Some(token) = self.lexer.next() {
                    Box::new(self.parse_expression(token, Precedence::Prefix))
                } else {
                    panic!("Prefix operator has no right expression")
                }
            },
        })
    }

    fn parse_infix_expression(&mut self, left: Expression, token: Token) -> Expression {
        let next_token = self
            .lexer
            .next()
            .expect("Infix expression has no right expression");
        let precedence = get_precedence(&token.token_type);
        let right = self.parse_expression(next_token, precedence);
        Expression::InfixOp(InfixExpression::new(left, right, token))
    }
}

fn get_precedence(token: &TokenType) -> Precedence {
    match token {
        TokenType::Minus => Precedence::Sum,
        TokenType::Plus => Precedence::Sum,
        TokenType::Asterisk => Precedence::Product,
        TokenType::Slash => Precedence::Product,
        TokenType::Equal => Precedence::Equals,
        TokenType::Noteq => Precedence::Equals,
        TokenType::Gt => Precedence::Lessgreater,
        TokenType::Lt => Precedence::Lessgreater,
        _ => Precedence::Lowest,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast_types::Statement::{Let, Return};
    use crate::parser::ast_types::{InfixType, PrefixType};
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

    #[test]
    fn it_should_parse_expression_identifier() {
        let input = "foobar;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::Identifier(ident) = &exp.expression {
                assert_eq!(ident.value, "foobar");
            } else {
                panic!("not an identifier");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_integer() {
        let input = "5";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::Int(num) = &exp.expression {
                assert_eq!(num.value, 5);
                assert_eq!(num.token.litteral, "5")
            } else {
                panic!("not an identifier");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_prefix_bang() {
        let input = "!5";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::PrefixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "!");
                assert_eq!(op.prefix_type, PrefixType::Bang);
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 5);
                }
            } else {
                panic!("not an identifier");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_prefix_sub() {
        let input = "-15";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::PrefixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "-");
                assert_eq!(op.prefix_type, PrefixType::Minus);
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                }
            } else {
                panic!("not an identifier");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_add() {
        let input = "3 + 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::InfixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "+");
                assert_eq!(op.infix_type, InfixType::Add);
                if let Expression::Int(int) = &*op.left {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("no left expression")
                }
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                } else {
                    panic!("no right expression")
                }
            } else {
                panic!("not an infix expression");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_sub() {
        let input = "3 - 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::InfixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "-");
                assert_eq!(op.infix_type, InfixType::Sub);
                if let Expression::Int(int) = &*op.left {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("no left expression")
                }
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                } else {
                    panic!("no right expression")
                }
            } else {
                panic!("not an infix expression");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_division() {
        let input = "3 / 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::InfixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "/");
                assert_eq!(op.infix_type, InfixType::Div);
                if let Expression::Int(int) = &*op.left {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("no left expression")
                }
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                } else {
                    panic!("no right expression")
                }
            } else {
                panic!("not an infix expression");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_mul() {
        let input = "3 * 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::InfixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "*");
                assert_eq!(op.infix_type, InfixType::Mul);
                if let Expression::Int(int) = &*op.left {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("no left expression")
                }
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                } else {
                    panic!("no right expression")
                }
            } else {
                panic!("not an infix expression");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_equal() {
        let input = "3 == 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::InfixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "==");
                assert_eq!(op.infix_type, InfixType::Eq);
                if let Expression::Int(int) = &*op.left {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("no left expression")
                }
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                } else {
                    panic!("no right expression")
                }
            } else {
                panic!("not an infix expression");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_not_equal() {
        let input = "3 != 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::InfixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "!=");
                assert_eq!(op.infix_type, InfixType::Noteq);
                if let Expression::Int(int) = &*op.left {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("no left expression")
                }
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                } else {
                    panic!("no right expression")
                }
            } else {
                panic!("not an infix expression");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_not_gt() {
        let input = "3 > 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::InfixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, ">");
                assert_eq!(op.infix_type, InfixType::Gt);
                if let Expression::Int(int) = &*op.left {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("no left expression")
                }
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                } else {
                    panic!("no right expression")
                }
            } else {
                panic!("not an infix expression");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_not_lt() {
        let input = "3 < 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!(program.len(), 1);
        if let Statement::Expression(exp) = &program.statements[0] {
            if let Expression::InfixOp(op) = &exp.expression {
                assert_eq!(op.token.litteral, "<");
                assert_eq!(op.infix_type, InfixType::Lt);
                if let Expression::Int(int) = &*op.left {
                    assert_eq!(int.value, 3);
                } else {
                    panic!("no left expression")
                }
                if let Expression::Int(int) = &*op.right {
                    assert_eq!(int.value, 15);
                } else {
                    panic!("no right expression")
                }
            } else {
                panic!("not an infix expression");
            }
        } else {
            panic!("Not a statement expression");
        }
    }

    #[test]
    fn it_should_parse_expression_infix_complex_expression() {
        let input = "3 < 15;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        assert_eq!("(3 < 15)", format!("{}", program).trim());
    }

    #[test]
    fn it_should_parse_expression_infix_complex_expressions() {
        let inputs = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];
        for input in inputs {
            let lexer = Lexer::new(input.0);
            let mut parser = Parser::new(lexer);
            let program: Program = parser.parse_program().unwrap();

            assert_eq!(input.1, format!("{}", program).trim());
        }
    }

    #[test]
    fn it_should_parse_expression_boolean() {
        let input = "true;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        let stmt = &program.statements[0];
        if let Statement::Expression(exp) = stmt {
            if let Expression::Boolean(value) = &exp.expression {
                assert!(value.value);
            } else {
                panic!("Fail: not a boolean expression")
            }
        } else {
            panic!("Fail: not a statement expression")
        }
    }

    #[test]
    fn it_should_parse_expression_boolean_in_a_let() {
        let input = "let a = true;";
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();

        let stmt = &program.statements[0];
        if let Statement::Let(exp) = stmt {
            if let Expression::Boolean(value) = &exp.value {
                assert!(value.value);
            } else {
                panic!("Fail: not a boolean expression: {:?}", stmt)
            }
        } else {
            panic!("Fail: not a statement expression")
        }
    }

    #[test]
    fn it_should_parse_bool_in_expression() {
        let inputs = [
            ("true", "true"),
            ("false", "false"),
            ("false != true", "(false != true)"),
            ("false == false", "(false == false)"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
        ];
        for input in inputs {
            let lexer = Lexer::new(input.0);
            let mut parser = Parser::new(lexer);
            let program: Program = parser.parse_program().unwrap();

            assert_eq!(input.1, format!("{program}"));
        }
    }

    #[test]
    fn it_should_parse_group_expression() {
        let inputs = [
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / ( 5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];
        for input in inputs {
            let lexer = Lexer::new(input.0);
            let mut parser = Parser::new(lexer);
            let program: Program = parser.parse_program().unwrap();

            assert_eq!(input.1, format!("{program}"));
        }
    }
}
