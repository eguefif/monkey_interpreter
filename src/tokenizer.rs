pub mod lexer;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,

    Ident,
    Int(i128),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Comma,
    Semicolon,
    Lt,
    Gt,
    Equal,
    Noteq,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
    If,
    Else,
    Return,
    False,
    True,
}

#[derive(Debug, PartialEq, Clone)]
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
