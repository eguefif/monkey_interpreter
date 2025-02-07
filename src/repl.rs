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
        let mut lexer = Lexer::new(&buffer);
        while let Some(token) = lexer.next() {
            println!("{token:?} ");
        }
        buffer.clear();
    }
    Ok(())
}
