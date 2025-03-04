use monkey_interpreter::environment::Environment;
use monkey_interpreter::evaluator::eval_program;
use monkey_interpreter::parser::Parser;
use monkey_interpreter::tokenizer::lexer::Lexer;
use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let env = Rc::new(RefCell::new(Environment::new()));
    println!(
        "Hello {}, this is Monk, your programming language",
        get_user()
    );
    println!("Feel free to type in commands");
    loop {
        print!(">> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut buffer)?;
        if buffer.as_str() == "exit" {
            break;
        }
        let lexer = Lexer::new(&buffer);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Error");
        match eval_program(&program.statements, env.clone()) {
            Ok(output) => println!("{}", output),
            Err(output) => println!("{}", output),
        }
        buffer.clear();
    }
    Ok(())
}

fn get_user() -> String {
    for var in std::env::vars() {
        if var.0 == "USER" {
            return var.1;
        }
    }
    return "".to_string();
}
