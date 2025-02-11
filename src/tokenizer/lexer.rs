#![allow(dead_code)]
use crate::tokenizer::{Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a str) -> Self {
        Self {
            iter: content.chars().peekable(),
        }
    }

    pub fn get_next_non_whitespace_char(&mut self) -> Option<char> {
        loop {
            if let Some(current) = self.iter.next() {
                if !current.is_whitespace() {
                    return Some(current);
                }
            } else {
                return None;
            }
        }
    }

    pub fn read_symbol(&mut self, next: char) -> Option<Token> {
        let litteral = self.get_litteral(next);
        self.get_token(litteral)
    }

    fn get_litteral(&mut self, first_char: char) -> String {
        let mut token_litteral = String::with_capacity(30);
        token_litteral.push(first_char);
        loop {
            if let Some(next_peek) = self.iter.peek() {
                if is_letter(next_peek) {
                    token_litteral.push(*next_peek);
                    self.iter.next();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        token_litteral
    }

    fn get_token(&self, token_litteral: String) -> Option<Token> {
        match token_litteral.as_str() {
            "let" => Some(Token::new(TokenType::Let, token_litteral)),
            "fn" => Some(Token::new(TokenType::Function, token_litteral)),
            "if" => Some(Token::new(TokenType::If, token_litteral)),
            "else" => Some(Token::new(TokenType::Else, token_litteral)),
            "return" => Some(Token::new(TokenType::Return, token_litteral)),
            "true" => Some(Token::new(TokenType::True, token_litteral)),
            "false" => Some(Token::new(TokenType::False, token_litteral)),
            _ => {
                if let Ok(value) = token_litteral.parse::<i128>() {
                    return Some(Token::new(TokenType::Int(value), token_litteral));
                }
                return Some(Token::new(TokenType::Ident, token_litteral));
            }
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.get_next_non_whitespace_char() {
            match next {
                '=' => {
                    if let Some(peek) = self.iter.peek() {
                        if *peek == '=' {
                            self.iter.next();
                            return Some(Token::new(TokenType::Equal, "==".to_string()));
                        }
                    }
                    return Some(Token::new(TokenType::Assign, next.to_string()));
                }
                '+' => Some(Token::new(TokenType::Plus, next.to_string())),
                ';' => Some(Token::new(TokenType::Semicolon, next.to_string())),
                ',' => Some(Token::new(TokenType::Comma, next.to_string())),
                '{' => Some(Token::new(TokenType::Lbrace, next.to_string())),
                '}' => Some(Token::new(TokenType::Rbrace, next.to_string())),
                '(' => Some(Token::new(TokenType::Lparen, next.to_string())),
                ')' => Some(Token::new(TokenType::Rparen, next.to_string())),
                '<' => Some(Token::new(TokenType::Lt, next.to_string())),
                '>' => Some(Token::new(TokenType::Gt, next.to_string())),
                '-' => Some(Token::new(TokenType::Minus, next.to_string())),
                '!' => {
                    if let Some(peek) = self.iter.peek() {
                        if *peek == '=' {
                            self.iter.next();
                            return Some(Token::new(TokenType::Noteq, "!=".to_string()));
                        }
                    }
                    return Some(Token::new(TokenType::Bang, next.to_string()));
                }
                '*' => Some(Token::new(TokenType::Asterisk, next.to_string())),
                '/' => Some(Token::new(TokenType::Slash, next.to_string())),
                _ => {
                    if is_letter(&next) {
                        return self.read_symbol(next);
                    } else {
                        return Some(Token::new(TokenType::Illegal, next.to_string()));
                    }
                }
            }
        } else {
            None
        }
    }
}

fn is_letter(letter: &char) -> bool {
    letter.is_alphanumeric() || *letter == '_'
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

let result = add(file, ten);
!-/*;
5 < 10 > 5 == != ;
if (5 < 10) {
    return true;
} else {
    return false;
}
        "
        .to_string();
        let lexer = Lexer::new(&input);

        let expected_values = vec![
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "file".to_string()),
            Token::new(TokenType::Assign, '='.to_string()),
            Token::new(TokenType::Int(5), "5".to_string()),
            Token::new(TokenType::Semicolon, ';'.to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::Assign, '='.to_string()),
            Token::new(TokenType::Int(10), "10".to_string()),
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
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "result".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Ident, "add".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Ident, "file".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Bang, "!".to_string()),
            Token::new(TokenType::Minus, "-".to_string()),
            Token::new(TokenType::Slash, "/".to_string()),
            Token::new(TokenType::Asterisk, "*".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Int(5), "5".to_string()),
            Token::new(TokenType::Lt, "<".to_string()),
            Token::new(TokenType::Int(10), "10".to_string()),
            Token::new(TokenType::Gt, ">".to_string()),
            Token::new(TokenType::Int(5), "5".to_string()),
            Token::new(TokenType::Equal, "==".to_string()),
            Token::new(TokenType::Noteq, "!=".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::If, "if".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Int(5), "5".to_string()),
            Token::new(TokenType::Lt, "<".to_string()),
            Token::new(TokenType::Int(10), "10".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::Lbrace, "{".to_string()),
            Token::new(TokenType::Return, "return".to_string()),
            Token::new(TokenType::True, "true".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Rbrace, "}".to_string()),
            Token::new(TokenType::Else, "else".to_string()),
            Token::new(TokenType::Lbrace, "{".to_string()),
            Token::new(TokenType::Return, "return".to_string()),
            Token::new(TokenType::False, "false".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Rbrace, "}".to_string()),
        ];

        let mut i = 0;
        for token in lexer {
            assert_eq!(token, expected_values[i]);
            i += 1;
        }
        assert_eq!(i, expected_values.len());
    }
}
