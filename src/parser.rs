#![allow(irrefutable_let_patterns)]

use crate::{
    ast::Program,
    ast::{
        self, BinaryExpression, Expression, Indentifier, IntegerLiteral, LetStatement, Statement,
    },
    token::{Kind, Token},
};

#[derive(Debug)]
pub struct ParserError {
    pub message: String,
}

pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    position: usize,
    read_position: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Parser<'a> {
        let mut parser = Parser {
            tokens,
            position: 0,
            read_position: 0,
        };
        assert!(!parser.tokens.is_empty());
        assert!(parser.tokens.last().unwrap().kind() == Kind::EndOfFile);
        parser.step();
        parser
    }

    // Returns the current token.
    fn token(&self) -> &'a Token<'a> {
        &self.tokens[self.position]
    }

    // Returns true if the next token is a newline.
    fn peek_newline(&self) -> bool {
        if self.read_position >= self.tokens.len() {
            return false;
        }
        return self.tokens[self.read_position].kind() == Kind::Whitespace
            && self.tokens[self.read_position].text().starts_with('\n');
    }

    // Advances the parser.
    fn step(&mut self) {
        if self.read_position >= self.tokens.len() {
            return;
        }
        self.position = self.read_position;
        self.read_position += 1;
        // Ignore whitespace.
        while self.token().kind() == Kind::Whitespace && self.read_position < self.tokens.len() {
            self.position = self.read_position;
            self.read_position += 1;
        }
    }

    // Resets the parser to a previous position.
    fn reset(&mut self, position: usize) {
        self.position = position;
        self.read_position = position + 1;
    }

    fn try_parse_let_stmt(&mut self) -> Result<Statement<'a>, String> {
        assert!(self.token().kind() == Kind::Let);
        let start = self.position;
        self.step(); // Consume the "let" token.
        let identifier = self.token();
        if identifier.kind() != Kind::Identifier {
            self.reset(start);
            return Err(format!("Expected identifier, got {:?}", identifier));
        }

        let identifier = Indentifier {
            name: identifier.text(),
        };
        self.step(); // Consume the identifier.

        let colon = self.token();
        if colon.kind() != Kind::Colon {
            self.reset(start);
            return Err(format!("Expected colon, got {:?}", colon));
        }
        self.step(); // Consume the colon.

        let ttype = self.token();
        let typename = match ttype.kind() {
            Kind::Int32 => "int32",
            _ => {
                self.reset(start);
                return Err(format!("Expected type, got {:?}", colon));
            }
        };
        self.step(); // Consume the typename.

        let equals = self.token();
        if equals.kind() != Kind::EqualSign {
            self.reset(start);
            return Err(format!("Expected equals, got {:?}", equals));
        }
        self.step(); // Consume the equals symbol.

        let literal = self.token();
        if literal.kind() != Kind::IntegerLiteral {
            self.reset(start);
            return Err(format!("Expected integer literal, got {:?}", literal));
        }

        // Require a newline for the end of the statement.
        if !self.peek_newline() {
            self.reset(start);
            return Err("Missing newline after let statement".to_string());
        }
        self.step(); // Consume the literal and whitespace.

        let expression = crate::ast::Expression::IntegerLiteral(IntegerLiteral {
            text: literal.text(),
        });
        Ok(ast::Statement::Let(LetStatement {
            identifier,
            ttype: crate::ast::Type { name: typename },
            expression: Box::new(expression),
        }))
    }

    fn try_parse_binary_expression(&mut self) -> Result<Statement<'a>, String> {
        let start = self.position;
        let left_token = self.token();
        let left = match left_token.kind() {
            Kind::Identifier => {
                let id = Indentifier {
                    name: left_token.text(),
                };
                Box::new(Expression::Identifier(id))
            }
            Kind::IntegerLiteral => {
                let literal = IntegerLiteral {
                    text: left_token.text(),
                };
                Box::new(Expression::IntegerLiteral(literal))
            }
            _ => {
                self.reset(start);
                return Err(format!("Expected identifier, got {:?}", left_token));
            }
        };
        self.step(); // Consume the identifier.

        let op_token = self.token();
        let operator = match op_token.kind() {
            Kind::Plus => ast::BinaryOperator::Plus,
            Kind::Minus => ast::BinaryOperator::Minus,
            Kind::Star => ast::BinaryOperator::Star,
            Kind::Divide => ast::BinaryOperator::Divide,
            _ => {
                self.reset(start);
                return Err(format!("Expected '+', got {:?}", op_token));
            }
        };
        self.step(); // Consume the op symbol.

        let right_token = self.token();
        let right = match right_token.kind() {
            Kind::Identifier => {
                let id = Indentifier {
                    name: right_token.text(),
                };
                Box::new(Expression::Identifier(id))
            }
            Kind::IntegerLiteral => {
                let literal = IntegerLiteral {
                    text: right_token.text(),
                };
                Box::new(Expression::IntegerLiteral(literal))
            }
            _ => {
                self.reset(start);
                return Err(format!("Expected identifier, got {:?}", right_token));
            }
        };

        // Require a newline for the end of the statement.
        if !self.peek_newline() {
            self.reset(start);
            return Err("Missing newline after binary expression".to_string());
        }
        self.step(); // Consume the identifier and newline.

        let expression = Expression::BinaryExpression(BinaryExpression {
            operator,
            left,
            right,
        });
        Ok(ast::Statement::Expression(expression))
    }

    // Reads the next statement.
    fn read_statement(&mut self) -> Result<Statement<'a>, String> {
        let token = self.token();
        match token.kind() {
            Kind::Let => self.try_parse_let_stmt(),
            Kind::Identifier => self.try_parse_binary_expression(),
            Kind::IntegerLiteral => self.try_parse_binary_expression(),
            _ => Err(format!("Failed to parse token {:?}", token)),
        }
    }

    // Reads the next statement and advances the parser.
    //
    // Returns an error if the statement cannot be parsed.
    // The parser is not advanced if an error is returned.
    fn next_stmt(&mut self) -> Result<Statement<'a>, String> {
        let stmt = self.read_statement();
        if stmt.is_ok() {
            self.step();
        }
        stmt
    }

    // Parses a program from tokens.
    //
    // Returns an error if the program cannot be parsed.
    pub fn parse_program(tokens: &'a [Token]) -> Result<Program<'a>, ParserError> {
        let mut parser = Parser::new(tokens);
        let mut statements = vec![];
        while parser.token().kind() != Kind::EndOfFile {
            match parser.next_stmt() {
                Ok(stmt) => statements.push(stmt),
                Err(message) => return Err(ParserError { message }),
            }
            parser.step();
        }
        Ok(Program { statements })
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast, lexer::Lexer, matcher::*, parser::Parser, *};

    #[test]
    fn empty_file_can_be_parsed() {
        let input = "";
        let tokens = Lexer::tokenize(input);
        let program = Parser::parse_program(&tokens);
        assert!(program.is_ok());
        assert!(program.unwrap().statements.is_empty());
    }

    #[test]
    fn parse_let_statement() {
        let input = "let x: int32 = 5\n";
        let tokens = Lexer::tokenize(input);
        let program = Parser::parse_program(&tokens);
        match program {
            Ok(program) => {
                assert!(program.statements.len() == 1);
                if let ast::Statement::Let(let_stmt) = &program.statements[0] {
                    assert_eq!(let_stmt.identifier.name, "x");
                    assert_eq!(let_stmt.ttype.name, "int32");
                    if let ast::Expression::IntegerLiteral(value) = &*let_stmt.expression {
                        assert_eq!(value.text, "5");
                    } else {
                        panic!("Expected integer literal");
                    }
                } else {
                    panic!("Expected let statement");
                }
            }
            Err(err) => panic!("Failed to parse program: {}", err.message),
        }
    }

    #[test]
    fn fail_to_parse_let_statement_with_no_trailing_newline() {
        let input = "let x: int32 = 5";
        let tokens = Lexer::tokenize(input);
        let program = Parser::parse_program(&tokens);
        match program {
            Ok(_) => {
                panic!("Expected parse error");
            }
            Err(err) => {
                assert!(err.message == "Missing newline after let statement");
            }
        }
    }

    macro_rules! parse_expression_test {
        (name:$name:ident, input:$input:expr, matcher:$matcher:expr) => {
            #[test]
            fn $name() {
                let tokens = Lexer::tokenize($input);
                match Parser::parse_program(&tokens) {
                    Ok(program) => {
                        if let ast::Statement::Expression(expr) = &program.statements[0] {
                            assert!($matcher.matches(expr));
                        } else {
                            panic!("Expected an expression statement");
                        }
                    }
                    Err(err) => panic!("Failed to parse program: {}", err.message),
                }
            }
        };
    }

    #[test]
    fn test_matcher() {
        let input = "x + y\n";
        let tokens = Lexer::tokenize(input);
        match Parser::parse_program(&tokens) {
            Ok(program) => {
                let matcher = match_binary_expression!();
                if let ast::Statement::Expression(expr) = &program.statements[0] {
                    assert!(matcher.matches(expr));
                } else {
                    panic!("Expected an expression statement");
                }
            }
            Err(err) => panic!("Failed to parse program: {}", err.message),
        }
    }

    parse_expression_test!(name:parse_binary_plus_expression_with_identifiers,
                 input:"x + y\n",
                 matcher:match_binary_expression!(
                    match_identifier!("x"),
                    ast::BinaryOperator::Plus,
                    match_identifier!("y")));

    parse_expression_test!(name:parse_binary_plus_expression_with_integer_literals,
                 input:"2 + 4\n",
                 matcher:match_binary_expression!(
                    match_integer_literal!("2"),
                    ast::BinaryOperator::Plus,
                    match_integer_literal!("4")));

    parse_expression_test!(name:parse_binary_minus_expression,
        input:"2 - 4\n",
        matcher:match_binary_expression!(
            match_any_expression!(),
            ast::BinaryOperator::Minus,
            match_any_expression!()));

    parse_expression_test!(name:parse_binary_star_expression,
                input:"2 * 4\n",
                matcher:match_binary_expression!(
                    match_any_expression!(),
                    ast::BinaryOperator::Star,
                    match_any_expression!()));

    parse_expression_test!(name:parse_binary_divide_expression,
                        input:"2 / 4\n",
                        matcher:match_binary_expression!(
                            match_any_expression!(),
                            ast::BinaryOperator::Divide,
                            match_any_expression!()));
}
