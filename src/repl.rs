use monkey_interpreter::evaluator::eval_program;
use monkey_interpreter::parser::Parser;
use monkey_interpreter::tokenizer::lexer::Lexer;
use std::io;
use std::io::Write;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
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
        let output = eval_program(&program.statements);
        println!("{}", output);
        buffer.clear();
    }
    Ok(())
}
