use core::fmt;

pub mod lexer;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,

    Ident,
    Int(i128),
    Str(String),

    Assign,

    Bang,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Equal,
    Noteq,

    Comma,
    Colon,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

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

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Lbracket => write!(f, "left bracket"),
            TokenType::Rbracket => write!(f, "right bracket"),
            TokenType::Lparen => write!(f, "left parenthesis"),
            TokenType::Rparen => write!(f, "right parenthesis"),
            _ => write!(f, ""),
        }
    }
}
