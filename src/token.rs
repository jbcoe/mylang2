use std::collections::HashMap;
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

pub(crate) struct Keywords {
    keywords: HashMap<String, Kind>,
}

impl Keywords {
    pub(crate) fn new() -> Keywords {
        let mut keywords = HashMap::new();
        keywords.insert("let".to_string(), Kind::Let);
        keywords.insert("mut".to_string(), Kind::Mut);
        keywords.insert("int1".to_string(), Kind::Int1);
        keywords.insert("int2".to_string(), Kind::Int2);
        keywords.insert("int4".to_string(), Kind::Int4);
        keywords.insert("int8".to_string(), Kind::Int8);
        keywords.insert("int16".to_string(), Kind::Int16);
        keywords.insert("int32".to_string(), Kind::Int32);
        keywords.insert("int64".to_string(), Kind::Int64);
        keywords.insert("integer".to_string(), Kind::Integer);
        keywords.insert("string".to_string(), Kind::String);
        Keywords { keywords }
    }

    pub(crate) fn get(&self, text: &str) -> Option<Kind> {
        self.keywords.get(text).copied()
    }
}
