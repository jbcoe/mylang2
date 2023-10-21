#![allow(irrefutable_let_patterns)]

use crate::{
    ast::Program,
    ast::{
        self, BinaryExpression, Expression, Identifier, IntegerLiteral, LetStatement, Statement,
        Type,
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
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Parser<'a> {
        let parser = Parser {
            tokens,
            position: 0,
        };
        assert!(!parser.tokens.is_empty());
        assert!(parser.tokens.last().unwrap().kind() == Kind::EndOfFile);
        parser
    }

    // Returns the current token.
    fn token(&self) -> &'a Token<'a> {
        &self.tokens[self.position]
    }

    // Advances the parser.
    fn step(&mut self) {
        if self.position >= self.tokens.len() {
            return;
        }
        self.position += 1;
        // Ignore whitespace.
        while self.token().kind() == Kind::Whitespace && self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    // Resets the parser to a previous position.
    fn reset(&mut self, position: usize) {
        self.position = position;
    }

    fn consume(&mut self, kind: Kind, start: usize) -> Result<(), String> {
        let token = self.token();
        if token.kind() == kind {
            self.step();
            Ok(())
        } else {
            self.reset(start);
            Err(format!("Expected {:?}, got {:?}", kind, token))
        }
    }

    fn consume_identifier_name(&mut self, start: usize) -> Result<&'a str, String> {
        let token = self.token();
        if token.kind() == Kind::Identifier {
            self.step();
            Ok(token.text())
        } else {
            self.reset(start);
            Err(format!("Expected identifier, got {:?}", token))
        }
    }

    fn try_parse_let_stmt(&mut self) -> Result<Statement<'a>, String> {
        let start = self.position;
        self.consume(Kind::Let, start)?;

        // See if we have a `mut` keyword
        let mut_token = self.token();
        let mutable = match mut_token.kind() {
            Kind::Mut => {
                self.step(); // Consume the "mut" token.
                true
            }
            _ => false,
        };

        let name = self.consume_identifier_name(start)?;
        let identifier = Identifier { name };
        self.consume(Kind::Colon, start)?;

        let type_name = self.consume_identifier_name(start)?;
        let ttype = ast::Type { name: type_name };

        self.consume(Kind::EqualSign, start)?;

        let expression_token = self.token();
        let expression = match expression_token.kind() {
            Kind::IntegerLiteral => crate::ast::Expression::IntegerLiteral(IntegerLiteral {
                text: expression_token.text(),
            }),
            Kind::Identifier => crate::ast::Expression::Identifier(Identifier {
                name: expression_token.text(),
            }),
            _ => {
                self.reset(start);
                return Err(format!(
                    "Expected integer literal or identifier, got {:?}",
                    expression_token
                ));
            }
        };
        self.step(); // Consume the value.

        self.consume(Kind::Semicolon, start)?;

        Ok(ast::Statement::Let(LetStatement {
            identifier,
            mutable,
            ttype,
            expression: Box::new(expression),
        }))
    }

    fn try_parse_binary_expression(&mut self) -> Result<Statement<'a>, String> {
        let start = self.position;
        let left_token = self.token();
        let left = match left_token.kind() {
            Kind::Identifier => {
                let id = Identifier {
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
        self.step(); // Consume the identifier or literal.

        let op_token = self.token();
        let operator = match op_token.kind() {
            Kind::Plus => ast::BinaryOperator::Plus,
            Kind::Minus => ast::BinaryOperator::Minus,
            Kind::Star => ast::BinaryOperator::Star,
            Kind::Divide => ast::BinaryOperator::Divide,
            _ => {
                self.reset(start);
                return Err(format!("Expected a binary op, got {:?}", op_token));
            }
        };
        self.step(); // Consume the op symbol.

        let right_token = self.token();
        let right = match right_token.kind() {
            Kind::Identifier => {
                let id = Identifier {
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
        self.step(); // Consume the identifier or literal.
        self.consume(Kind::Semicolon, start)?;

        let expression = Expression::BinaryExpression(BinaryExpression {
            operator,
            left,
            right,
        });
        Ok(ast::Statement::Expression(expression))
    }

    fn try_parse_function(&mut self) -> Result<Statement<'a>, String> {
        let start = self.position;
        self.consume(Kind::Fn, start)?;

        let name = self.consume_identifier_name(start)?;
        let identifier = Identifier { name };

        self.consume(Kind::LeftParenthesis, start)?;

        // Parse the parameters.
        let mut parameters = vec![];
        while self.token().kind() != Kind::RightParenthesis {
            let parameter_token = self.token();
            if let Kind::Identifier = parameter_token.kind() {
                let name = parameter_token.text();
                self.step(); // Consume the identifier.
                self.consume(Kind::Colon, start)?;

                let type_token = self.token();
                if type_token.kind() != Kind::Identifier {
                    self.reset(start);
                    return Err(format!("Expected a type name, got {:?}", type_token));
                }
                let type_name = type_token.text();
                self.step(); // Consume the type name.
                parameters.push(ast::Parameter {
                    identifier: ast::Identifier { name },
                    ttype: ast::Type { name: type_name },
                });
                if let Kind::Comma = self.token().kind() {
                    self.step(); // Consume the comma.
                };
            } else {
                self.reset(start);
                return Err(format!(
                    "Expected identifier or ')', got {:?}",
                    parameter_token
                ));
            };
            if self.token().kind() == Kind::Comma {
                self.step(); // Consume the ',' token.
            }
        }
        self.step(); // Consume the ')' token.
        self.consume(Kind::Arrow, start)?;

        let return_type = match self.token().kind() {
            Kind::Identifier => Type {
                name: self.token().text(),
            },
            _ => {
                self.reset(start);
                return Err(format!("Expected type identifier, got {:?}", self.token()));
            }
        };
        self.step(); // Consume the return type.
        self.consume(Kind::Semicolon, start)?;

        return Ok(ast::Statement::FunctionDeclaration(
            ast::FunctionDeclaration {
                identifier,
                parameters,
                return_type,
            },
        ));
    }

    // Reads the next statement.
    fn read_statement(&mut self) -> Result<Statement<'a>, String> {
        let token = self.token();
        match token.kind() {
            Kind::Let => self.try_parse_let_stmt(),
            Kind::Identifier => self.try_parse_binary_expression(),
            Kind::IntegerLiteral => self.try_parse_binary_expression(),
            Kind::Fn => self.try_parse_function(),
            _ => Err(format!("Failed to parse token {:?}", token)),
        }
    }

    // Parses a program from tokens.
    //
    // Returns an error if the program cannot be parsed.
    pub fn parse_program(tokens: &'a [Token]) -> Result<Program<'a>, ParserError> {
        let mut parser = Parser::new(tokens);
        let mut statements = vec![];
        while parser.token().kind() != Kind::EndOfFile {
            if parser.token().kind() == Kind::Comment {
                parser.step();
                continue;
            }
            match parser.read_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(message) => return Err(ParserError { message }),
            }
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
    fn fail_to_parse_let_statement_with_no_trailing_semicolon() {
        let input = "let x: int32 = 5";
        let tokens = Lexer::tokenize(input);
        let program = Parser::parse_program(&tokens);
        match program {
            Ok(_) => {
                panic!("Expected parse error");
            }
            Err(err) => {
                assert!(err
                    .message
                    .eq("Expected Semicolon, got Token { text: \"<EOF>\", offset: 15, kind: EndOfFile }"));
            }
        }
    }
    #[test]
    fn multiple_statements_can_be_parsed() {
        let input = "fn max() -> int32; fn min() -> int32; fn mean() -> float32;";
        let tokens = Lexer::tokenize(input);
        let program = Parser::parse_program(&tokens);
        match program {
            Ok(_) => {}
            Err(_) => {
                panic!("Failed to parse program");
            }
        }
    }

    #[test]
    fn test_matcher() {
        let input = "x + y;";
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

    parse_expression_test!(name:parse_binary_plus_expression_with_identifiers,
                 input:"x + y;",
                 matcher:match_binary_expression!(
                    match_identifier!("x"),
                    ast::BinaryOperator::Plus,
                    match_identifier!("y")));

    parse_expression_test!(name:parse_binary_plus_expression_with_integer_literals,
                 input:"2 + 4;",
                 matcher:match_binary_expression!(
                    match_integer_literal!("2"),
                    ast::BinaryOperator::Plus,
                    match_integer_literal!("4")));

    parse_expression_test!(name:parse_binary_minus_expression,
        input:"2 - 4;",
        matcher:match_binary_expression!(
            match_any_expression!(),
            ast::BinaryOperator::Minus,
            match_any_expression!()));

    parse_expression_test!(name:parse_binary_star_expression,
                input:"2 * 4;",
                matcher:match_binary_expression!(
                    match_any_expression!(),
                    ast::BinaryOperator::Star,
                    match_any_expression!()));

    parse_expression_test!(name:parse_binary_divide_expression,
                        input:"2 / 4;",
                        matcher:match_binary_expression!(
                            match_any_expression!(),
                            ast::BinaryOperator::Divide,
                            match_any_expression!()));

    macro_rules! parse_statement_test {
        (name:$name:ident, input:$input:expr, matcher:$matcher:expr) => {
            #[test]
            fn $name() {
                let tokens = Lexer::tokenize($input);
                match Parser::parse_program(&tokens) {
                    Ok(program) => assert!($matcher.matches(&program.statements[0])),
                    Err(err) => panic!("Failed to parse program: {}", err.message),
                }
            }
        };
        (name:$name:ident, input:$input:expr, matchers:$matchers:expr) => {
            #[test]
            fn $name() {
                let tokens = Lexer::tokenize($input);
                match Parser::parse_program(&tokens) {
                    Ok(program) => {
                        for (statement, matcher) in program.statements.iter().zip($matchers.iter())
                        {
                            assert!(matcher.matches(statement));
                        }
                    }
                    Err(err) => panic!("Failed to parse program: {}", err.message),
                }
            }
        };
    }

    parse_statement_test! {
        name:parse_let_statement_with_integer_literal,
        input:"let x: int32 = 5;",
        matcher:match_let_statement!(
            "x",
            match_type!(),
            match_integer_literal!("5"))
    }

    parse_statement_test! {
        name:parse_let_statement_with_identifier,
        input:"let x: int32 = y; let x: int32 = y; let x: int32 = y;",
        matcher:match_let_statement!(
            "x",
            match_type!(),
            match_identifier!("y"))
    }

    parse_statement_test! {
        name:parse_mutable_let_statement,
        input:"let mut x: int32 = y;",
        matcher:match_mutable_let_statement!(
            "x",
            match_type!(),
            match_any_expression!())
    }

    parse_statement_test! {
        name:parse_function,
        input:"fn max(x:int32, y:int32) -> int32;",
        matcher:match_function_declaration!(
            "max",
            vec![match_parameter!("x", "int32"), match_parameter!("y", "int32")],
            match_type!("int32"))
    }

    parse_statement_test! {
        name:parse_function_with_no_parameters,
        input:"fn max() -> int32;",
        matcher:match_function_declaration!(
            "max",
            vec![],
            match_type!("int32"))
    }

    parse_statement_test! {
        name: parse_functions,
        input:"fn max() -> int32;
               fn min() -> int32; 
               fn mean() -> float32;",
        matchers: [
            match_function_declaration!(
                "max",
                vec![],
                match_type!("int32")),
            match_function_declaration!(
                "min",
                vec![],
                match_type!("int32")),
            match_function_declaration!(
                "mean",
                vec![],
                match_type!("float32")),
            ]
    }
}
