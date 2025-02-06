#![allow(dead_code)]
use crate::tokenizer::{Token, TokenType};
use std::str::Chars;

pub struct Lexer<'a> {
    iter: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a String) -> Self {
        Self {
            iter: content.chars(),
        }
    }
}
impl Iterator for Lexer<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.iter.next() {
            match next {
                '=' => Some(Token::new(TokenType::Assign, next.to_string())),
                '+' => Some(Token::new(TokenType::Plus, next.to_string())),
                ';' => Some(Token::new(TokenType::Semicolon, next.to_string())),
                ',' => Some(Token::new(TokenType::Comma, next.to_string())),
                '{' => Some(Token::new(TokenType::Lbrace, next.to_string())),
                '}' => Some(Token::new(TokenType::Rbrace, next.to_string())),
                '(' => Some(Token::new(TokenType::Lparen, next.to_string())),
                ')' => Some(Token::new(TokenType::Rparen, next.to_string())),
                _ => Some(Token::new(TokenType::Illegal, next.to_string())),
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_return_basic_tokens() {
        let input = "=+(){},;".to_string();
        let lexer = Lexer::new(&input);

        let expected_values = vec![
            Token {
                token_type: TokenType::Assign,
                litteral: "=".to_string(),
            },
            Token {
                token_type: TokenType::Plus,
                litteral: "+".to_string(),
            },
            Token {
                token_type: TokenType::Lparen,
                litteral: "(".to_string(),
            },
            Token {
                token_type: TokenType::Rparen,
                litteral: ")".to_string(),
            },
            Token {
                token_type: TokenType::Lbrace,
                litteral: "{".to_string(),
            },
            Token {
                token_type: TokenType::Rbrace,
                litteral: "}".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                litteral: ",".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                litteral: ";".to_string(),
            },
        ];
        let mut i = 0;
        for token in lexer {
            assert_eq!(token, expected_values[i]);
            i += 1;
        }
        assert_eq!(i, expected_values.len());
    }

    #[test]
    fn it_tokenize_all_token() {
        let input = "let file = 5;
let ten = 10;

let add = fn(x, y) {
x + y;
};
        "
        .to_string();
        let lexer = Lexer::new(&input);

        let expected_values = vec![
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "file".to_string()),
            Token::new(TokenType::Assign, '='.to_string()),
            Token::new(TokenType::Int, "5".to_string()),
            Token::new(TokenType::Semicolon, ';'.to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::Assign, '='.to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "add".to_string()),
            Token::new(TokenType::Assign, '='.to_string()),
            Token::new(TokenType::Function, "fn".to_string()),
            Token::new(TokenType::Lparen, '('.to_string()),
            Token::new(TokenType::Ident, "x".to_string()),
            Token::new(TokenType::Comma, ','.to_string()),
            Token::new(TokenType::Ident, "y".to_string()),
            Token::new(TokenType::Rparen, ')'.to_string()),
            Token::new(TokenType::Lbrace, '{'.to_string()),
            Token::new(TokenType::Ident, "x".to_string()),
            Token::new(TokenType::Plus, '+'.to_string()),
            Token::new(TokenType::Ident, "y".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Rbrace, '}'.to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
        ];

        let mut i = 0;
        for token in lexer {
            assert_eq!(token, expected_values[i]);
            i += 1;
        }
        assert_eq!(i, expected_values.len());
    }
}
