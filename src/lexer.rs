use crate::token::{Token, TokenType};

pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer<'_> {
    pub fn new(str: &[u8]) -> Lexer {
        Lexer {
            input: str,
            position: 0,
            read_position: 1,
            ch: str[0],
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn is_letter(&mut self, ch: char) -> bool {
        'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
    }

    fn is_digit(&mut self, ch: char) -> bool {
        '0' <= ch && ch <= '9'
    }

    fn read_digit(&mut self) -> String {
        let position = self.position;
        while self.is_digit(self.ch as char) {
            self.read_char();
        }
        //TO DO -> what if is not UTF valid?
        let res = String::from_utf8(self.input[position..self.position].to_vec()).unwrap();
        res
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.is_letter(self.ch as char) {
            self.read_char();
        }
        //TO DO -> what if is not UTF valid?
        let res = String::from_utf8(self.input[position..self.position].to_vec()).unwrap();
        res
    }

    fn get_token(&mut self, token_literal: TokenType, literal: String) -> Token {
        let token = Token::new(token_literal, literal);
        self.read_char();
        token
    }

    fn consume_white_spaces(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        }
        self.input[self.read_position]
    }

    fn get_identifier_type(&mut self, identifier: &str) -> TokenType {
        match identifier {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.consume_white_spaces();
        let char = self.ch as char;
        match char {
            ';' => self.get_token(TokenType::SEMICOLON, ';'.to_string()),

            '(' => self.get_token(TokenType::LPAREN, '('.to_string()),

            ')' => self.get_token(TokenType::RPAREN, ')'.to_string()),

            ',' => self.get_token(TokenType::COMMA, ','.to_string()),

            '+' => self.get_token(TokenType::PLUS, '+'.to_string()),

            '-' => self.get_token(TokenType::MINUS, '-'.to_string()),

            '*' => self.get_token(TokenType::ASTERISK, '*'.to_string()),

            '/' => self.get_token(TokenType::SLASH, '/'.to_string()),

            '<' => self.get_token(TokenType::LT, '<'.to_string()),

            '>' => self.get_token(TokenType::GT, '>'.to_string()),

            '{' => self.get_token(TokenType::LBRACE, '{'.to_string()),

            '}' => self.get_token(TokenType::RBRACE, '}'.to_string()),

            '\0' => self.get_token(TokenType::EOF, '\0'.to_string()),

            '=' => {
                if self.peek_char() as char == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let mut literal = String::new();
                    literal.push(ch as char);
                    literal.push(self.ch as char);
                    return self.get_token(TokenType::EQ, "==".to_string());
                };
                return self.get_token(TokenType::ASSIGN, '='.to_string());
            }

            '!' => {
                if self.peek_char() as char == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let mut literal = String::new();
                    literal.push(ch as char);
                    literal.push(self.ch as char);
                    return self.get_token(TokenType::NOTEQ, "!=".to_string());
                };
                self.get_token(TokenType::BANG, '!'.to_string())
            }

            _ => {
                if self.is_letter(self.ch as char) {
                    let identifier = self.read_identifier();
                    return Token::new(self.get_identifier_type(&identifier), identifier);
                } else if self.is_digit(self.ch as char) {
                    let digit = self.read_digit();
                    return Token::new(TokenType::INT, digit);
                }
                Token::new(TokenType::ILLEGAL, self.ch.to_string())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_char_works() {
        let input = "{;.".to_string();
        let mut lexer = Lexer::new(input.as_bytes());
        assert_eq!(lexer.ch as char, '{' as char);
        lexer.read_char();
        assert_eq!(lexer.ch, ';' as u8);
        lexer.read_char();
        assert_eq!(lexer.ch, '.' as u8);
        lexer.read_char();
        assert_eq!(lexer.ch, 0);
    }

    #[test]
    fn next_token_works() {
        //Normal case
        let input = " =+;,{(".to_string();
        let mut lexer = Lexer::new(input.as_bytes());
        let token = lexer.next_token();
        assert_eq!(token.literal, '='.to_string());
        let token = lexer.next_token();
        assert_eq!(token.literal, '+'.to_string());
        let token = lexer.next_token();
        assert_eq!(token.literal, ';'.to_string());

        //EOF STRING
        let input = '\0'.to_string();
        let mut lexer = Lexer::new(input.as_bytes());
        let token = lexer.next_token();
        assert_eq!(token.literal, '\0'.to_string());
    }

    #[test]
    fn it_should_parse_identifiers() {
        let input = "+identifier=".to_string();
        let mut lexer = Lexer::new(input.as_bytes());
        let token = lexer.next_token();
        assert_eq!(token.literal, "+".to_string());

        let token = lexer.next_token();
        assert_eq!(token.literal, "identifier".to_string());

        let token = lexer.next_token();
        assert_eq!(token.literal, "=".to_string());
    }

    #[test]
    fn it_should_ignore_white_spaces() {
        //white spaces
        let input = "\r \t fn =\nlet".to_string();
        let mut lexer = Lexer::new(input.as_bytes());
        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::FUNCTION);

        let token = lexer.next_token();
        assert_eq!(token.literal, "=".to_string());

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::LET);
    }

    #[test]
    fn it_should_parse_integers() {
        let input = "let variable = 1984".to_string();
        let mut lexer = Lexer::new(input.as_bytes());
        //let
        lexer.next_token();
        //variable
        lexer.next_token();
        // =
        lexer.next_token();
        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::INT);
        assert_eq!(token.literal, "1984");
    }

    #[test]
    fn it_sould_parse_eq() {
        let input = "2 == 2".to_string();
        let mut lexer = Lexer::new(input.as_bytes());
        //2
        lexer.next_token();
        // ==
        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::EQ);
        assert_eq!(token.literal, "==");

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::INT);
        assert_eq!(token.literal, "2");
    }

    #[test]
    fn it_sould_parse_noteq() {
        let input = "2 != 2".to_string();
        let mut lexer = Lexer::new(input.as_bytes());
        //2
        lexer.next_token();
        // ==
        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::NOTEQ);
        assert_eq!(token.literal, "!=");

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::INT);
        assert_eq!(token.literal, "2");
    }
}
