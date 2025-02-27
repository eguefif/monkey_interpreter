use monkey_interpreter::environment::Environment;
use monkey_interpreter::evaluator::eval_program;
use monkey_interpreter::parser::Parser;
use monkey_interpreter::tokenizer::lexer::Lexer;
use std::io;
use std::io::Write;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let mut env = Environment::new();
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
        match eval_program(&program.statements, &mut env) {
            Ok(output) => println!("{}", output),
            Err(output) => println!("{}", output),
        }
        println!("Env: {:?}", env);
        buffer.clear();
    }
    Ok(())
}
