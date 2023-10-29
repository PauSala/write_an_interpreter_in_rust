#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    MINUS,
    ASTERISK,
    BANG,
    SLASH,
    COMMA,
    EQ,
    NOTEQ,
    LT,
    GT,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_literal: TokenType, literal: String) -> Token {
        Token {
            token_type: token_literal,
            literal,
        }
    }
}
