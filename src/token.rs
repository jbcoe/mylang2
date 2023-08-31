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
    Mut,
    String,
    Unknown,
    Whitespace,
}

pub struct Token<'a> {
    text: &'a [u8],
    offset: usize,
    kind: Kind,
}

impl core::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("text", &self.text())
            .field("offset", &self.offset())
            .field("kind", &self.kind())
            .finish()
    }
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

pub(crate) static KEYWORDS: phf::Map<&'static str, Kind> = phf::phf_map! {
    "let"=> Kind::Let,
    "mut"=> Kind::Mut,
    "int1"=> Kind::Int1,
    "int2"=> Kind::Int2,
    "int4"=> Kind::Int4,
    "int8"=> Kind::Int8,
    "int16"=> Kind::Int16,
    "int32"=> Kind::Int32,
    "int64"=> Kind::Int64,
    "integer"=> Kind::Integer,
    "string"=> Kind::String,
};
