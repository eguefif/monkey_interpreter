use monkey_interpreter::{environment::Environment, interpret};
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut env = Environment::new();
    if args.len() != 2 {
        println!("Usage: monkey FILENAME")
    } else {
        let filename = args[1].clone();
        interpret(filename, &mut env);
    }
}
