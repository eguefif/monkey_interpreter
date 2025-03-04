#![allow(unused_variables)]
use environment::Environment;

use crate::evaluator::eval_program;
use crate::parser::Parser;
use crate::tokenizer::lexer::Lexer;
use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

pub mod environment;
pub mod evaluator;
pub mod object;
pub mod parser;
pub mod tokenizer;

pub fn interpret(filename: String, env: Rc<RefCell<Environment>>) {
    let content = fs::read_to_string(filename).expect("Error: file does not exist");
    let lexer = Lexer::new(&content);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_program();
    if let Some(program) = ast {
        match eval_program(&program.statements, env.clone()) {
            Ok(output) => println!("{}", output),
            Err(output) => println!("{}", output),
        }
    }
}
