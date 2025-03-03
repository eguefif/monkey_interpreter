use monkey_interpreter::{environment::Environment, interpret};
use std::cell::RefCell;
use std::env;
use std::rc::Rc;

fn main() {
    let args: Vec<String> = env::args().collect();
    let env = Rc::new(RefCell::new(Environment::new()));
    if args.len() != 2 {
        println!("Usage: monkey FILENAME")
    } else {
        let filename = args[1].clone();
        interpret(filename, env);
    }
}
