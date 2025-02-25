#![allow(unused_variables)]
use crate::evaluator::evaluate;
use crate::parser::Parser;
use crate::tokenizer::lexer::Lexer;
use std::fs;

pub mod evaluator;
pub mod object;
pub mod parser;
pub mod tokenizer;

pub fn interpret(filename: String) {
    let content = fs::read_to_string(filename).expect("Error: file does not exist");
    let lexer = Lexer::new(&content);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_program();
    if let Some(program) = ast {
        for statement in program.statements {
            evaluate(&statement);
        }
    }
}
