#[derive(Debug)]
pub enum Statement<'a> {
    If(IfStatement<'a>),
    Let(LetStatement<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
    Expression(Expression<'a>),
    Return(ReturnStatement<'a>),
}

#[derive(Debug)]
pub struct Parameter<'a> {
    pub identifier: Identifier<'a>,
    pub ttype: Type<'a>,
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub identifier: Identifier<'a>,
    pub parameters: Vec<Parameter<'a>>,
    pub return_type: Type<'a>,
}

#[derive(Debug)]
pub struct FunctionCall<'a> {
    pub identifier: Identifier<'a>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    BooleanLiteral(BooleanLiteral),
    BinaryExpression(BinaryExpression<'a>),
    Identifier(Identifier<'a>),
    IntegerLiteral(IntegerLiteral<'a>),
    FloatLiteral(FloatLiteral<'a>),
    FunctionCall(FunctionCall<'a>),
}

#[derive(Debug)]
pub struct IntegerLiteral<'a> {
    pub text: &'a str,
}

#[derive(Debug)]
pub struct FloatLiteral<'a> {
    pub text: &'a str,
}

#[derive(Debug)]
pub struct BooleanLiteral {
    pub value: bool,
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
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    XOr,
}

#[derive(Debug)]
pub struct BinaryExpression<'a> {
    pub operator: BinaryOperator,
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct Identifier<'a> {
    pub name: &'a str,
}

#[derive(Debug)]
pub struct LetStatement<'a> {
    pub identifier: Identifier<'a>,
    pub ttype: Type<'a>,
    pub mutable: bool,
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct ReturnStatement<'a> {
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct IfStatement<'a> {
    pub condition: Box<Expression<'a>>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}
