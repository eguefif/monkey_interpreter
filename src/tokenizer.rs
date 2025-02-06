pub mod lexer;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,

    Ident,
    Int,

    Assign,
    Plus,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub litteral: String,
}

impl Token {
    pub fn new(token_type: TokenType, litteral: String) -> Self {
        Self {
            token_type,
            litteral,
        }
    }
}
