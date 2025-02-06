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
                '=' => Some(Token::new(TokenType::Assign, next)),
                '+' => Some(Token::new(TokenType::Plus, next)),
                ';' => Some(Token::new(TokenType::Semicolon, next)),
                ',' => Some(Token::new(TokenType::Comma, next)),
                '{' => Some(Token::new(TokenType::Lbrace, next)),
                '}' => Some(Token::new(TokenType::Rbrace, next)),
                '(' => Some(Token::new(TokenType::Lparen, next)),
                ')' => Some(Token::new(TokenType::Rparen, next)),
                _ => Some(Token::new(TokenType::Illegal, next)),
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
    fn it_should_return_tokens() {
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
}
