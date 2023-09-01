pub enum Statement {
    Let(LetStatement),
}

pub enum Expression {
    IntegerLiteral(String),
    DecimalLiteral(String),
}

pub struct Type {
    pub name: String,
}

pub struct Indentifier {
    pub name: String,
}

pub struct LetStatement {
    pub identifier: Indentifier,
    pub ttype: Type,
    pub expression: Box<Expression>,
}

pub struct Program {
    pub statements: Vec<Statement>,
}
