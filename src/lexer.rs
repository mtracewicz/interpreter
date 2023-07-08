use std::usize;

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,
    Identifier(String),
    Integer(String),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Function,
    Let,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThen,
    GreaterThen,
    True,
    False,
    If,
    Else,
    Return,
    Equal,
    NotEqual,
}

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lex = Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lex.read_char();
        return lex;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            b'=' => Token::Assign,
            b'+' => Token::Plus,
            b'(' => Token::LeftParenthesis,
            b')' => Token::RightParenthesis,
            b'{' => Token::LeftBrace,
            b'}' => Token::RightBrace,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => return self.read_identifier(),
            b'0'..=b'9' => return self.read_integer(),
            0 => Token::EOF,
            _ => Token::Illegal,
        };
        self.read_char();
        return token;
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }
        let identifier = String::from_utf8_lossy(&self.input[position..self.position]).to_string();
        return match identifier.as_str() {
            "let" => Token::Let,
            "fn" => Token::Function,
            _ => Token::Identifier(identifier),
        };
    }

    fn read_integer(&mut self) -> Token {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        let number = String::from_utf8_lossy(&self.input[position..self.position]).to_string();
        return Token::Integer(number);
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

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from(
            "let five = 5;
            let ten = 10;

            let add = fn(x,y){
                x+y;
            };
            let result = add(five,ten);
            ",
        );
        let mut lexer = Lexer::new(input);
        let expected_tokens = [
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Integer(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Integer(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LeftParenthesis,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::RightParenthesis,
            Token::LeftBrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::RightBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::LeftParenthesis,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::RightParenthesis,
            Token::Semicolon,
            Token::EOF,
        ];

        for token in expected_tokens.into_iter() {
            assert_eq!(token, lexer.next_token());
        }
    }
}
