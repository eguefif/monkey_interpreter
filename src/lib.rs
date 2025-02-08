use crate::parser::Parser;
use crate::tokenizer::lexer::Lexer;
use std::fs;

pub mod parser;
pub mod tokenizer;

pub fn interpret(filename: String) {
    let content = fs::read_to_string(filename).expect("Error: file does not exist");
    let lexer = Lexer::new(&content);
    let parser = Parser::new(lexer);
}
