pub enum Statement<'a> {
    Let(LetStatement<'a>),
}

pub enum Expression<'a> {
    IntegerLiteral(&'a str),
    DecimalLiteral(&'a str),
}

pub struct Type<'a> {
    pub name: &'a str,
}

pub struct Indentifier<'a> {
    pub name: &'a str,
}

pub struct LetStatement<'a> {
    pub identifier: Indentifier<'a>,
    pub ttype: Type<'a>,
    pub expression: Box<Expression<'a>>,
}

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}
