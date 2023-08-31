use std::str;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Kind {
    Colon,
    EndOfFile,
    EqualSign,
    Identifier,
    Int1,
    Int16,
    Int2,
    Int32,
    Int4,
    Int64,
    Int8,
    Integer,
    Let,
    String,
    Unknown,
    Whitespace,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    text: &'a [u8],
    offset: usize,
    kind: Kind,
}

impl<'a> Token<'a> {
    pub const fn new(text: &'a [u8], offset: usize, kind: Kind) -> Token<'a> {
        Token { text, offset, kind }
    }

    pub const fn kind(&self) -> Kind {
        self.kind
    }

    pub fn text(&self) -> &str {
        str::from_utf8(self.text).unwrap()
    }

    pub const fn offset(&self) -> usize {
        self.offset
    }

    pub const fn end_of_file(offset: usize) -> Token<'static> {
        Token {
            text: &[],
            offset,
            kind: Kind::EndOfFile,
        }
    }
}
