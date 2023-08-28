#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Kind {
    Identifier,
    Integer,
    Let,
    SemiColon,
    EqualSign,
    Whitespace,
    EndOfFile,
    Unknown,
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

    pub fn text(&self) -> String {
        String::from_utf8(self.text.to_vec()).unwrap()
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
