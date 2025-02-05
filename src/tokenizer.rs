#[derive(Debug)]
pub enum TokenType {
    Int,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub litteral: String,
}
