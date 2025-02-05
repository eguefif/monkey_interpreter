use crate::tokenizer::{Token, TokenType};
use std::fs;

pub mod tokenizer;

pub fn print_file(filename: String) {
    let content = fs::read_to_string(filename).expect("Error: file does not exist");
    println!("{}", content);
    let t = TokenType::Int;
    let token = Token {
        token_type: t,
        litteral: "test".to_string(),
    };
    println!("{token:?}");
}
