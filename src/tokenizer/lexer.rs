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
                '=' => Some(Token {
                    token_type: TokenType::Assign,
                    litteral: next.to_string(),
                }),
                '+' => Some(Token {
                    token_type: TokenType::Plus,
                    litteral: next.to_string(),
                }),
                '{' => Some(Token {
                    token_type: TokenType::Lbrace,
                    litteral: next.to_string(),
                }),
                '}' => Some(Token {
                    token_type: TokenType::Rbrace,
                    litteral: next.to_string(),
                }),
                ')' => Some(Token {
                    token_type: TokenType::Rparen,
                    litteral: next.to_string(),
                }),
                '(' => Some(Token {
                    token_type: TokenType::Lparen,
                    litteral: next.to_string(),
                }),
                ',' => Some(Token {
                    token_type: TokenType::Comma,
                    litteral: next.to_string(),
                }),
                ';' => Some(Token {
                    token_type: TokenType::Semicolon,
                    litteral: next.to_string(),
                }),
                _ => Some(Token {
                    token_type: TokenType::Illegal,
                    litteral: next.to_string(),
                }),
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
