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
        let prog = parser.parse_program().expect("Error");
        println!("{}", prog);
        buffer.clear();
    }
    Ok(())
}
