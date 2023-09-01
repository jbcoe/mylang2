#[derive(Debug)]
pub enum Statement<'a> {
    Let(LetStatement<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug)]
pub enum Expression<'a> {
    IntegerLiteral(&'a str),
    Identifier(Indentifier<'a>),
    BinaryExpression(BinaryExpression<'a>),
}

#[derive(Debug)]
pub struct Type<'a> {
    pub name: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Divide,
    Plus,
    Minus,
    Star,
}

#[derive(Debug)]
pub struct BinaryExpression<'a> {
    pub operator: BinaryOperator,
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct Indentifier<'a> {
    pub name: &'a str,
}

#[derive(Debug)]
pub struct LetStatement<'a> {
    pub identifier: Indentifier<'a>,
    pub ttype: Type<'a>,
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}
