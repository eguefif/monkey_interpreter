use monkey_interpreter::interpret;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: monkey FILENAME")
    } else {
        let filename = args[1].clone();
        interpret(filename);
    }
}
