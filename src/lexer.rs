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
    #[must_use = "Creates a Lexer, has no side effects"]
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

    // Returns the current character.
    fn char(&self) -> char {
        char::from(self.byte)
    }

    // Reads the next character advancing the lexer.
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

    // Resets the lexer to a previous position.
    fn reset(&mut self, position: usize) {
        self.position = position;
        self.read_position = position + 1;
        self.byte = match self.input.get(self.position) {
            None => 0,
            Some(b) => *b,
        }
    }

    // Returns the next character without advancing the lexer.
    fn peek_char(&self) -> char {
        match self.input.get(self.read_position) {
            None => '\0',
            Some(c) => char::from(*c),
        }
    }

    // Returns the text between the start and the read position.
    fn text_range(&self, start: usize) -> &'a [u8] {
        &self.input[start..self.read_position]
    }

    // Returns a token for a single character.
    fn char_token(&self, kind: Kind) -> Token<'a> {
        self.text_token(self.position, kind)
    }

    // Returns a token for a range of text.
    fn text_token(&self, start: usize, kind: Kind) -> Token<'a> {
        Token::new(self.text_range(start), start, kind)
    }

    // Attempts to read a whitespace token.
    fn maybe_read_whitespace(&mut self) -> Option<Token<'a>> {
        if !self.char().is_whitespace() {
            return None;
        }

        let start = self.position;
        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        Some(self.text_token(start, Kind::Whitespace))
    }

    // Attempts to read an identifier token, potentially advancing the lexer.
    fn maybe_read_identifier(&mut self) -> Option<Token<'a>> {
        if !self.char().is_ascii_alphabetic() {
            return None;
        }

        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
            self.read_char();
        }
        Some(self.text_token(start, Kind::Identifier))
    }

    // Attempts to read a keyword token, potentially advancing the lexer.
    fn maybe_read_keyword(&mut self) -> Option<Token<'a>> {
        if !self.char().is_ascii_alphabetic() {
            return None;
        }

        let start = self.position;
        while self.peek_char().is_alphanumeric() {
            self.read_char();
        }
        if let Ok(token_text) = str::from_utf8(self.text_range(start)) {
            match token_text {
                "let" => return Some(self.text_token(start, Kind::Let)),
                "int1" => return Some(self.text_token(start, Kind::Int1)),
                "int2" => return Some(self.text_token(start, Kind::Int2)),
                "int4" => return Some(self.text_token(start, Kind::Int4)),
                "int8" => return Some(self.text_token(start, Kind::Int8)),
                "int16" => return Some(self.text_token(start, Kind::Int16)),
                "int32" => return Some(self.text_token(start, Kind::Int32)),
                "int64" => return Some(self.text_token(start, Kind::Int64)),
                _ => {
                    self.reset(start);
                    return None;
                }
            }
        }
        None
    }

    // Attempts to read a symbol token, potentially advancing the lexer.
    pub fn maybe_read_symbol(&mut self) -> Option<Token<'a>> {
        if self.char() == '=' {
            return Some(self.char_token(Kind::EqualSign));
        } else if self.char() == ':' {
            return Some(self.char_token(Kind::Colon));
        }
        return None;
    }

    // Attempts to read an integer token, potentially advancing the lexer.
    pub fn maybe_read_integer(&mut self) -> Option<Token<'a>> {
        if !self.char().is_ascii_digit() {
            return None;
        }
        let start = self.position;
        while self.peek_char().is_ascii_digit() {
            self.read_char();
        }
        return Some(self.text_token(start, Kind::Integer));
    }

    // Attempts to read a string token, potentially advancing the lexer.
    pub fn maybe_read_string(&mut self) -> Option<Token<'a>> {
        if self.char() != '"' {
            return None;
        }
        let start = self.position;
        self.read_char(); // consume the opening quote.
        while self.peek_char() != '"' {
            // Returns None if the string is incomplete.
            if self.peek_char() == '\0' {
                self.reset(start);
                return None;
            }
            self.read_char();
        }

        let token = self.text_token(start + 1, Kind::String);
        self.read_char(); // consume the closing quote.
        return Some(token);
    }

    // Reads the next token unconditionally advancing the lexer.
    pub fn read_token(&mut self) -> Token<'a> {
        if self.char() == '\0' {
            return Token::end_of_file(self.position);
        } else if let Some(t) = self.maybe_read_whitespace() {
            return t;
        } else if let Some(t) = self.maybe_read_symbol() {
            return t;
        } else if let Some(t) = self.maybe_read_keyword() {
            return t;
        } else if let Some(t) = self.maybe_read_string() {
            return t;
        } else if let Some(t) = self.maybe_read_integer() {
            return t;
        } else if let Some(t) = self.maybe_read_identifier() {
            return t;
        } else {
            let start = self.position;
            while self.char() != '\0' {
                self.read_char();
            }
            return self.text_token(start, Kind::Unknown);
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
        name: let_statement_with_assignment_to_integer,
        input: "let x = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment_to_integer_i1,
        input: "let x: int1 = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            (":", Kind::Colon),
            ("int1", Kind::Int1),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment_to_integer_i2,
        input: "let x: int2 = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            (":", Kind::Colon),
            ("int2", Kind::Int2),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment_to_integer_i4,
        input: "let x: int4 = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            (":", Kind::Colon),
            ("int4", Kind::Int4),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment_to_integer_i8,
        input: "let x: int8 = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            (":", Kind::Colon),
            ("int8", Kind::Int8),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment_to_integer_i16,
        input: "let x: int16 = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            (":", Kind::Colon),
            ("int16", Kind::Int16),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment_to_integer_i32,
        input: "let x: int32 = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            (":", Kind::Colon),
            ("int32", Kind::Int32),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment_to_integer_i64,
        input: "let x: int64 = 5",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            (":", Kind::Colon),
            ("int64", Kind::Int64),
            ("=", Kind::EqualSign),
            ("5", Kind::Integer),
        ],
    }

    lexer_test_case! {
        name: let_statement_with_assignment_to_string,
        input: r#"let x: int64 = "five""#,
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            (":", Kind::Colon),
            ("int64", Kind::Int64),
            ("=", Kind::EqualSign),
            ("five", Kind::String),
        ],
    }

    lexer_test_case! {
        name: incomplete_string,
        input: r#""oops"#,
        expected_tokens: &[
            (r#""oops"#, Kind::Unknown),
        ],
    }
}
