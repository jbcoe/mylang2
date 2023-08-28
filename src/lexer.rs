use crate::token::Kind;
use crate::token::Token;
use std::str;

pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    byte: u8,
}
impl<'a> Lexer<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Lexer {
        let mut lexer = Lexer {
            input: input.as_bytes(),
            position: 0,
            read_position: 0,
            byte: 0,
        };
        lexer.read_char();
        lexer
    }

    fn char(&self) -> char {
        char::from(self.byte)
    }

    fn read_char(&mut self) {
        self.byte = match self.input.get(self.read_position) {
            None => 0,
            Some(b) => {
                self.position = self.read_position;
                self.read_position += 1;
                *b
            }
        }
    }

    fn reset(&mut self, position: usize) {
        self.position = position;
        self.read_position = position + 1;
        self.byte = match self.input.get(self.position) {
            None => 0,
            Some(b) => *b,
        }
    }

    fn peek_char(&self) -> char {
        match self.input.get(self.read_position) {
            None => '\0',
            Some(c) => char::from(*c),
        }
    }

    fn text_range(&self, start: usize) -> &'a [u8] {
        &self.input[start..self.read_position]
    }

    fn char_token(&self, kind: Kind) -> Token<'a> {
        self.text_token(self.position, kind)
    }

    fn text_token(&self, start: usize, kind: Kind) -> Token<'a> {
        Token::new(self.text_range(start), start, kind)
    }

    fn read_whitespace(&mut self) -> Option<Token<'a>> {
        if !self.char().is_whitespace() {
            return None;
        }

        let start = self.position;
        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        Some(self.text_token(start, Kind::Whitespace))
    }

    fn read_identifier(&mut self) -> Option<Token<'a>> {
        if !self.char().is_ascii_alphabetic() {
            return None;
        }

        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
            self.read_char();
        }
        Some(self.text_token(start, Kind::Identifier))
    }

    fn read_keyword(&mut self) -> Option<Token<'a>> {
        if !self.char().is_ascii_alphabetic() {
            return None;
        }

        let start = self.position;
        while self.peek_char().is_ascii_alphabetic() {
            self.read_char();
        }
        if let Ok(token_text) = str::from_utf8(self.text_range(start)) {
            match token_text {
                "let" => return Some(self.text_token(start, Kind::Let)),
                _ => {
                    self.reset(start);
                    return None;
                }
            }
        }
        None
    }

    pub fn read_symbol(&mut self) -> Option<Token<'a>> {
        if self.char() == '=' {
            return Some(self.char_token(Kind::EqualSign));
        }
        return None;
    }

    pub fn read_integer(&mut self) -> Option<Token<'a>> {
        if !self.char().is_ascii_digit() {
            return None;
        }
        let start = self.position;
        while self.peek_char().is_ascii_digit() {
            self.read_char();
        }
        return Some(self.text_token(start, Kind::Integer));
    }

    pub fn read_token(&mut self) -> Token<'a> {
        if self.char() == '\0' {
            return Token::end_of_file(self.position);
        } else if let Some(t) = self.read_whitespace() {
            return t;
        } else if let Some(t) = self.read_symbol() {
            return t;
        } else if let Some(t) = self.read_keyword() {
            return t;
        } else if let Some(t) = self.read_integer() {
            return t;
        } else if let Some(t) = self.read_identifier() {
            return t;
        } else {
            return self.char_token(Kind::Unknown);
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        let token = self.read_token();
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! lexer_test_case {
        ( name: $test_name:ident, input: $input:expr, expected_tokens:$expected_tokens:expr,) => {
            #[test]
            fn $test_name() {
                let mut lexer = Lexer::new($input);
                for expected_token in $expected_tokens {
                    let mut t = lexer.next_token();
                    while t.kind() == Kind::Whitespace {
                        t = lexer.next_token();
                    }
                    assert_eq!(t.kind(), expected_token.1);
                    assert_eq!(t.text(), expected_token.0);
                }
                assert_eq!(lexer.next_token().kind(), Kind::EndOfFile);
            }
        };
        ( name: $test_name:ident, input: $input:expr, expected_tokens:$expected_tokens:expr, check_whitespace: true,) => {
            #[test]
            fn $test_name() {
                let mut lexer = Lexer::new($input);
                for expected_token in $expected_tokens {
                    let t = lexer.next_token();
                    assert_eq!(t.kind(), expected_token.1);
                    assert_eq!(t.text(), expected_token.0);
                }
                assert_eq!(lexer.next_token().kind(), Kind::EndOfFile);
            }
        };
    }

    lexer_test_case! {
        name: single_space,
        input: " ",
        expected_tokens: &[
            (" ", Kind::Whitespace),
        ],
        check_whitespace: true,
    }

    lexer_test_case! {
        name: space_and_tab,
        input: " \t",
        expected_tokens: &[
            (" \t", Kind::Whitespace),
        ],
        check_whitespace: true,
    }

    lexer_test_case! {
        name: let_keyword,
        input: "let",
        expected_tokens: &[
            ("let", Kind::Let),
        ],
    }

    lexer_test_case! {
        name: let_statement,
        input: "let x",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment,
        input: "let x = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }
}
