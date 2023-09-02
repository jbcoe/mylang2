#![macro_use]

use crate::ast::{BinaryOperator, Expression, Statement};

pub trait ExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool;
}

pub trait StatementMatcher {
    fn matches(&self, statement: &Statement) -> bool;
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

pub struct AnyIdentifierMatcher {}

impl AnyIdentifierMatcher {
    pub fn new() -> Box<AnyIdentifierMatcher> {
        Box::new(AnyIdentifierMatcher {})
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
pub struct AnyIntegerLiteralMatcher {}

impl AnyIntegerLiteralMatcher {
    pub fn new() -> Box<AnyIntegerLiteralMatcher> {
        Box::new(AnyIntegerLiteralMatcher {})
    }
}

impl ExpressionMatcher for AnyIntegerLiteralMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::IntegerLiteral(_))
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
macro_rules! match_identifier {
    ($identifier:literal) => {
        IdentifierMatcher::new($identifier.to_string())
    };
    () => {
        Box::new(AnyIdentifierMatcher {})
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
