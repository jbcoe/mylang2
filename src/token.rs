use std::str;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Kind {
    And,
    Arrow,
    Assign,
    Colon,
    Comma,
    Comment,
    DecimalLiteral,
    Divide,
    EndOfFile,
    Equal,
    False,
    FloatLiteral,
    Fn,
    Greater,
    GreaterOrEqual,
    Identifier,
    If,
    IntegerLiteral,
    LeftBrace,
    LeftParenthesis,
    LeftSquareBracket,
    Less,
    LessOrEqual,
    Let,
    Minus,
    Mut,
    NotEqual,
    Or,
    Plus,
    Return,
    RightBrace,
    RightParenthesis,
    RightSquareBracket,
    Semicolon,
    Star,
    String,
    StringLiteral,
    True,
    Unknown,
    Whitespace,
    XOr,
}

pub struct Token<'a> {
    source: &'a [u8],
    offset: usize,
    len: usize,
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
    pub const fn new(source: &'a [u8], offset: usize, len: usize, kind: Kind) -> Token<'a> {
        Token {
            source,
            offset,
            len,
            kind,
        }
    }

    pub const fn kind(&self) -> Kind {
        self.kind
    }

    pub fn source(&self) -> &[u8] {
        self.source
    }

    pub fn text(&self) -> &str {
        match self.kind {
            Kind::EndOfFile => "<EOF>",
            _ => str::from_utf8(&self.source[self.offset..self.offset + self.len]).unwrap(),
        }
    }

    pub const fn offset(&self) -> usize {
        self.offset
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const fn is_empty(&self) -> bool {
        self.len > 0
    }

    pub const fn end_of_file(offset: usize) -> Token<'static> {
        Token {
            source: &[],
            offset,
            len: 0,
            kind: Kind::EndOfFile,
        }
    }
}

pub(crate) static KEYWORDS: phf::Map<&'static str, Kind> = phf::phf_map! {
    "and" => Kind::And,
    "false" => Kind::False,
    "fn"=> Kind::Fn,
    "if" => Kind::If,
    "let"=> Kind::Let,
    "mut"=> Kind::Mut,
    "or" => Kind::Or,
    "return"=> Kind::Return,
    "true" => Kind::True,
    "xor" => Kind::XOr,
};
