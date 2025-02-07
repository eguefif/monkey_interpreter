use crate::tokenizer::lexer::Lexer;
use std::fs;

pub mod tokenizer;

pub fn interpret(filename: String) {
    let content = fs::read_to_string(filename).expect("Error: file does not exist");
    let lexer = Lexer::new(&content);
    for token in lexer {
        println!("{token:?}");
    }
}
