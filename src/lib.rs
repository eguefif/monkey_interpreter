use std::fs;

pub mod tokenizer;

pub fn interpret(filename: String) {
    let content = fs::read_to_string(filename).expect("Error: file does not exist");
    println!("{content}");
}
