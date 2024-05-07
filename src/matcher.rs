#![macro_use]

use crate::ast::{BinaryOperator, Expression, Parameter, Statement, Type};

pub trait ExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool;
}

pub trait StatementMatcher {
    fn matches(&self, statement: &Statement) -> bool;
}

pub trait ParameterMatcher {
    fn matches(&self, parameter: &Parameter) -> bool;
}

pub trait TypeMatcher {
    fn matches(&self, ttype: &Type) -> bool;
}

pub struct NamedTypeMatcher {
    name: String,
}

impl NamedTypeMatcher {
    pub fn new(name: String) -> Box<NamedTypeMatcher> {
        Box::new(NamedTypeMatcher { name })
    }
}

impl TypeMatcher for NamedTypeMatcher {
    fn matches(&self, ttype: &Type) -> bool {
        ttype.name == self.name
    }
}

pub struct AnyMatcher {
    _private: (),
}

impl AnyMatcher {
    pub fn new() -> Box<AnyMatcher> {
        Box::new(AnyMatcher { _private: () })
    }
}

impl TypeMatcher for AnyMatcher {
    fn matches(&self, _ttype: &Type) -> bool {
        true
    }
}

impl StatementMatcher for AnyMatcher {
    fn matches(&self, _statement: &Statement) -> bool {
        true
    }
}

impl ExpressionMatcher for AnyMatcher {
    fn matches(&self, _expression: &Expression) -> bool {
        true
    }
}

impl ParameterMatcher for AnyMatcher {
    fn matches(&self, _parameter: &Parameter) -> bool {
        true
    }
}

pub struct NamedParameterMatcher {
    identifier: String,
    ttype: Box<dyn TypeMatcher>,
}

impl NamedParameterMatcher {
    pub fn new(identifier: String, ttype: Box<dyn TypeMatcher>) -> Box<NamedParameterMatcher> {
        Box::new(NamedParameterMatcher { identifier, ttype })
    }
}

impl ParameterMatcher for NamedParameterMatcher {
    fn matches(&self, parameter: &Parameter) -> bool {
        self.identifier == parameter.identifier.name && self.ttype.matches(&parameter.ttype)
    }
}

pub struct LetStatementMatcher {
    identifier: String,
    ttype: Box<dyn TypeMatcher>,
    mutable: bool,
    expression: Box<dyn ExpressionMatcher>,
}

impl LetStatementMatcher {
    pub fn new(
        identifier: String,
        ttype: Box<dyn TypeMatcher>,
        mutable: bool,
        expression: Box<dyn ExpressionMatcher>,
    ) -> Box<LetStatementMatcher> {
        Box::new(LetStatementMatcher {
            identifier,
            ttype,
            mutable,
            expression,
        })
    }
}

impl StatementMatcher for LetStatementMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        matches!(statement, Statement::Let(let_statement) if {
            let_statement.mutable == self.mutable
                && let_statement.identifier.name == self.identifier
                && self.ttype.matches(&let_statement.ttype)
                && self.expression.matches(&let_statement.expression)
        })
    }
}

pub struct FunctionDeclarationMatcher {
    identifier: String,
    parameters: Vec<Box<dyn ParameterMatcher>>,
    return_type: Box<dyn TypeMatcher>,
}

impl FunctionDeclarationMatcher {
    pub fn new(
        identifier: String,
        parameters: Vec<Box<dyn ParameterMatcher>>,
        return_type: Box<dyn TypeMatcher>,
    ) -> Box<FunctionDeclarationMatcher> {
        Box::new(FunctionDeclarationMatcher {
            identifier,
            parameters,
            return_type,
        })
    }
}

impl StatementMatcher for FunctionDeclarationMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        matches!(statement, Statement::FunctionDeclaration(function_declaration) if {
            function_declaration.identifier.name == self.identifier
                && self.return_type.matches(&function_declaration.return_type)
                && function_declaration.parameters.len() == self.parameters.len()
                && self.parameters.iter().zip(function_declaration.parameters.iter()).all(|(m, p)| m.matches(p))
        })
    }
}

pub struct IdentifierMatcher {
    identifier: String,
}

impl IdentifierMatcher {
    pub fn new(identifier: String) -> Box<IdentifierMatcher> {
        Box::new(IdentifierMatcher { identifier })
    }
}

impl ExpressionMatcher for IdentifierMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Identifier(i) if i.name == self.identifier)
    }
}

pub struct AnyIdentifierMatcher {
    _private: (),
}

impl AnyIdentifierMatcher {
    pub fn new() -> Box<AnyIdentifierMatcher> {
        Box::new(AnyIdentifierMatcher { _private: () })
    }
}

impl ExpressionMatcher for AnyIdentifierMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Identifier(_))
    }
}

pub struct IntegerLiteralMatcher {
    identifier: String,
}

impl IntegerLiteralMatcher {
    pub fn new(identifier: String) -> Box<IntegerLiteralMatcher> {
        Box::new(IntegerLiteralMatcher { identifier })
    }
}

impl ExpressionMatcher for IntegerLiteralMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::IntegerLiteral(i) if i.text == self.identifier)
    }
}

pub struct AnyIntegerLiteralMatcher {
    _private: (),
}

impl AnyIntegerLiteralMatcher {
    pub fn new() -> Box<AnyIntegerLiteralMatcher> {
        Box::new(AnyIntegerLiteralMatcher { _private: () })
    }
}

impl ExpressionMatcher for AnyIntegerLiteralMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::IntegerLiteral(_))
    }
}

pub struct FloatLiteralMatcher {
    identifier: String,
}

impl FloatLiteralMatcher {
    pub fn new(identifier: String) -> Box<FloatLiteralMatcher> {
        Box::new(FloatLiteralMatcher { identifier })
    }
}

impl ExpressionMatcher for FloatLiteralMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::FloatLiteral(i) if i.text == self.identifier)
    }
}

pub struct AnyFloatLiteralMatcher {
    _private: (),
}

impl AnyFloatLiteralMatcher {
    pub fn new() -> Box<AnyFloatLiteralMatcher> {
        Box::new(AnyFloatLiteralMatcher { _private: () })
    }
}

impl ExpressionMatcher for AnyFloatLiteralMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::FloatLiteral(_))
    }
}

pub struct BinaryExpressionMatcher {
    left: Box<dyn ExpressionMatcher>,
    right: Box<dyn ExpressionMatcher>,
    operator: BinaryOperator,
}

impl BinaryExpressionMatcher {
    pub fn new(
        left: Box<dyn ExpressionMatcher>,
        operator: BinaryOperator,
        right: Box<dyn ExpressionMatcher>,
    ) -> Box<BinaryExpressionMatcher> {
        Box::new(BinaryExpressionMatcher {
            left,
            operator,
            right,
        })
    }
}

impl ExpressionMatcher for BinaryExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::BinaryExpression(binary_exp) if {
            binary_exp.operator == self.operator
                && self.left.matches(&binary_exp.left)
                && self.right.matches(&binary_exp.right)
        })
    }
}

pub struct AnyBinaryExpressionMatcher {}

impl AnyBinaryExpressionMatcher {
    pub fn new() -> Box<AnyBinaryExpressionMatcher> {
        Box::new(AnyBinaryExpressionMatcher {})
    }
}

impl ExpressionMatcher for AnyBinaryExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::BinaryExpression(_))
    }
}

#[macro_export]
macro_rules! match_integer_literal {
    ($integer_literal:literal) => {
        IntegerLiteralMatcher::new($integer_literal.to_string())
    };
    () => {
        AnyIntegerLiteralMatcher::new()
    };
}

#[macro_export]
macro_rules! match_float_literal {
    ($float_literal:literal) => {
        FloatLiteralMatcher::new($float_literal.to_string())
    };
    () => {
        AnyFloatLiteralMatcher::new()
    };
}

#[macro_export]
macro_rules! match_identifier {
    ($identifier:literal) => {
        IdentifierMatcher::new($identifier.to_string())
    };
    () => {
        AnyIdentifierMatcher::new()
    };
}

#[macro_export]
macro_rules! match_binary_expression {
    ($left:expr, $operator:expr, $right:expr) => {
        BinaryExpressionMatcher::new($left, $operator, $right)
    };
    () => {
        AnyBinaryExpressionMatcher::new()
    };
}

#[macro_export]
macro_rules! match_any_expression {
    () => {
        AnyMatcher::new()
    };
}

#[macro_export]
macro_rules! match_any_type {
    () => {
        AnyTypeMatcher::new()
    };
}

#[macro_export]
macro_rules! match_let_statement {
    ($identifier:literal, $ttype:expr, $expression:expr) => {
        LetStatementMatcher::new($identifier.to_string(), $ttype, false, $expression)
    };
}

#[macro_export]
macro_rules! match_mutable_let_statement {
    ($identifier:literal, $ttype:expr, $expression:expr) => {
        LetStatementMatcher::new($identifier.to_string(), $ttype, true, $expression)
    };
}

#[macro_export]
macro_rules! match_function_declaration {
    ($identifier:literal, $params:expr, $ttype:expr) => {
        FunctionDeclarationMatcher::new($identifier.to_string(), $params, $ttype)
    };
    ($identifier:literal, $ttype:expr) => {
        FunctionDeclarationMatcher::new($identifier.to_string(), vec![], $ttype)
    };
}

#[macro_export]
macro_rules! match_type {
    ($name:literal) => {
        NamedTypeMatcher::new($name.to_string())
    };
    () => {
        AnyMatcher::new()
    };
}

#[macro_export]
macro_rules! match_parameter {
    ($identifier:literal, $ttype:literal) => {
        NamedParameterMatcher::new($identifier.to_string(), match_type!($ttype))
    };
    () => {
        AnyMatcher::new()
    };
}
