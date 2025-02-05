use monkey_interpreter::print_file;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: monkey FILENAME")
    } else {
        let filename = args[1].clone();
        print_file(filename);
    }
}
