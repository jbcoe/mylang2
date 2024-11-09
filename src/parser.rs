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

    fn maybe_consume(&mut self, kind: Kind) {
        let token = self.token();
        if token.kind() == kind {
            self.step();
        }
    }

    fn parse_expression(&mut self) -> Result<Expression<'a>, String> {
        let start: usize = self.position;
        if let Ok(expression) = self.parse_function_call(start) {
            return Ok(expression);
        }
        if let Ok(expression) = self.parse_binary_expression(start) {
            return Ok(expression);
        }
        if let Ok(expression) = self.parse_identifier_expression(start) {
            return Ok(expression);
        }
        self.reset(start);
        Err("Failed to parse expression".to_string())
    }

    fn parse_expression_statement(&mut self) -> Result<Statement<'a>, String> {
        let start = self.position;
        if let Ok(expression) = self.parse_expression() {
            self.consume(Kind::Semicolon, start)?;
            return Ok(ast::Statement::Expression(expression));
        }
        self.reset(start);
        Err("Failed to parse expression statement".to_string())
    }

    fn parse_function_call(&mut self, start: usize) -> Result<Expression<'a>, String> {
        let token = self.token();
        if token.kind() != Kind::Identifier {
            self.reset(start);
            return Err(format!("Expected identifier, got {:?}", token));
        }
        let identifier = Identifier { name: token.text() };
        self.step(); // Consume the identifier.
        self.consume(Kind::LeftParenthesis, start)?;
        let mut arguments = vec![];
        while self.token().kind() != Kind::RightParenthesis {
            let argument = self.parse_identifier_expression(start)?;
            arguments.push(argument);
            self.maybe_consume(Kind::Comma);
        }
        self.consume(Kind::RightParenthesis, start)?; // Consume the ')'.
        Ok(Expression::FunctionCall(ast::FunctionCall {
            identifier,
            arguments,
        }))
    }

    fn parse_identifier_expression(&mut self, start: usize) -> Result<Expression<'a>, String> {
        let token = self.token();
        match token.kind() {
            Kind::Identifier => {
                let id = Identifier { name: token.text() };
                self.step(); // Consume the identifier.
                Ok(Expression::Identifier(id))
            }
            Kind::IntegerLiteral => {
                let literal = IntegerLiteral { text: token.text() };
                self.step(); // Consume the integer literal.
                Ok(Expression::IntegerLiteral(literal))
            }
            Kind::FloatLiteral => {
                let literal = ast::FloatLiteral { text: token.text() };
                self.step(); // Consume the float literal.
                Ok(Expression::FloatLiteral(literal))
            }
            Kind::True => {
                let literal = ast::BooleanLiteral { value: true };
                self.step(); // Consume the boolean literal.
                Ok(Expression::BooleanLiteral(literal))
            }
            Kind::False => {
                let literal = ast::BooleanLiteral { value: false };
                self.step(); // Consume the boolean literal.
                Ok(Expression::BooleanLiteral(literal))
            }
            //TODO:DEV ADD StringLiteral
            _ => {
                self.reset(start);
                Err(format!(
                    "Expected identifier or integer literal, got {:?}",
                    token
                ))
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Statement<'a>, String> {
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
        let identifier = Identifier {
            name: self.consume_identifier_name(start)?,
        };
        self.consume(Kind::Colon, start)?;
        let ttype = ast::Type {
            name: self.consume_identifier_name(start)?,
        };
        self.consume(Kind::Assign, start)?;
        let expression = Box::new(self.parse_expression()?);
        self.consume(Kind::Semicolon, start)?;

        Ok(ast::Statement::Let(LetStatement {
            identifier,
            mutable,
            ttype,
            expression,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement<'a>, String> {
        let start = self.position;
        self.consume(Kind::Return, start)?;
        let expression = Box::new(self.parse_expression()?);
        self.consume(Kind::Semicolon, start)?;
        Ok(ast::Statement::Return(ast::ReturnStatement { expression }))
    }

    fn parse_binary_expression(&mut self, start: usize) -> Result<Expression<'a>, String> {
        let left = Box::new(self.parse_identifier_expression(start)?);

        let op_token = self.token();
        let operator = match op_token.kind() {
            Kind::Or => ast::BinaryOperator::Or,
            Kind::And => ast::BinaryOperator::And,
            Kind::XOr => ast::BinaryOperator::XOr,
            Kind::Plus => ast::BinaryOperator::Plus,
            Kind::Minus => ast::BinaryOperator::Minus,
            Kind::Star => ast::BinaryOperator::Star,
            Kind::Divide => ast::BinaryOperator::Divide,
            Kind::Less => ast::BinaryOperator::Less,
            Kind::LessOrEqual => ast::BinaryOperator::LessOrEqual,
            Kind::Greater => ast::BinaryOperator::Greater,
            Kind::GreaterOrEqual => ast::BinaryOperator::GreaterOrEqual,
            Kind::Equal => ast::BinaryOperator::Equal,
            Kind::NotEqual => ast::BinaryOperator::NotEqual,
            _ => {
                self.reset(start);
                return Err(format!("Expected a binary op, got {:?}", op_token));
            }
        };
        self.step(); // Consume the op symbol.

        let right = Box::new(self.parse_identifier_expression(start)?);

        Ok(Expression::BinaryExpression(BinaryExpression {
            operator,
            left,
            right,
        }))
    }

    fn parse_function_definition(&mut self) -> Result<Statement<'a>, String> {
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
                let type_name = self.consume_identifier_name(start)?;
                parameters.push(ast::Parameter {
                    identifier: ast::Identifier { name },
                    ttype: ast::Type { name: type_name },
                });
            } else {
                self.reset(start);
                return Err(format!(
                    "Expected identifier or ')', got {:?}",
                    parameter_token
                ));
            };
            self.maybe_consume(Kind::Comma);
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
    fn parse_statement(&mut self) -> Result<Statement<'a>, String> {
        let token = self.token();
        match token.kind() {
            Kind::If => self.parse_if_statement(),
            Kind::Return => self.parse_return_statement(),
            Kind::Let => self.parse_let_stmt(),
            Kind::Fn => self.parse_function_definition(),
            _ => match self.parse_expression_statement() {
                Ok(stmt) => Ok(stmt),
                Err(_) => Err(format!("Failed to parse statement {:?}", token)),
            },
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement<'a>, String> {
        let start = self.position;
        self.consume(Kind::If, start)?;
        let condition = self.parse_expression()?;
        self.consume(Kind::LeftBrace, start)?;
        let mut body = vec![];
        while self.token().kind() != Kind::RightBrace {
            body.push(self.parse_statement()?);
        }
        self.consume(Kind::RightBrace, start)?;
        Ok(ast::Statement::If(ast::IfStatement {
            condition: Box::new(condition),
            body,
        }))
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
            match parser.parse_statement() {
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
        ($name:ident, $input:expr, $($m:expr),+) => {
            #[test]
            fn $name() {
                let tokens = Lexer::tokenize($input);
                let matchers = vec![$($m),+];
                match Parser::parse_program(&tokens) {
                    Ok(program) =>{
                        for (statement, matcher) in program.statements.iter().zip(matchers.iter())
                        {
                            if let ast::Statement::Expression(expr) = statement {
                                assert!(matcher.matches(expr),
                                        "Matcher failed to match expression {:?}", expr);
                            } else {
                                panic!("Expected an expression statement");
                            }
                        }
                    },
                    Err(err) => panic!("Failed to parse program: {}", err.message),
                }
            }
        };
    }

    parse_expression_test!(
        parse_binary_plus_expression_with_identifiers,
        "x + y;",
        match_binary_expression!(
            match_identifier!("x"),
            ast::BinaryOperator::Plus,
            match_identifier!("y")
        )
    );

    parse_expression_test!(
        parse_binary_plus_expression_with_integer_literals,
        "2 + 4;",
        match_binary_expression!(
            match_integer_literal!("2"),
            ast::BinaryOperator::Plus,
            match_integer_literal!("4")
        )
    );

    parse_expression_test!(
        parse_binary_minus_expression,
        "2 - 4;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::Minus, match_any!())
    );

    parse_expression_test!(
        parse_binary_star_expression,
        "2 * 4;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::Star, match_any!())
    );

    parse_expression_test!(
        parse_binary_divide_expression,
        "2 / 4;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::Divide, match_any!())
    );

    parse_expression_test!(
        parse_less_than_expression,
        "2 < 4;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::Less, match_any!())
    );

    parse_expression_test!(
        parse_less_than_or_equal_expression,
        "2 <= 4;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::LessOrEqual, match_any!())
    );

    parse_expression_test!(
        parse_greater_than_expression,
        "2 > 4;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::Greater, match_any!())
    );

    parse_expression_test!(
        parse_greater_than_or_equal_expression,
        "2 >= 4;",
        match_binary_expression!(
            match_any!(),
            ast::BinaryOperator::GreaterOrEqual,
            match_any!()
        )
    );

    parse_expression_test!(
        parse_equal_expression,
        "2 == 4;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::Equal, match_any!())
    );

    parse_expression_test!(
        parse_not_equal_expression,
        "2 != 4;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::NotEqual, match_any!())
    );

    parse_expression_test!(
        parse_or_expression,
        "x or y;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::Or, match_any!())
    );

    parse_expression_test!(
        parse_and_expression,
        "x and false;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::And, match_any!())
    );

    parse_expression_test!(
        parse_xor_expression,
        "true xor y;",
        match_binary_expression!(match_any!(), ast::BinaryOperator::XOr, match_any!())
    );

    parse_expression_test!(
        parse_function_call_expression,
        "foo();",
        match_function_call!("foo")
    );

    parse_expression_test!(
        parse_function_call_expression_with_arguments,
        "foo(x, y, 5);",
        match_function_call!(
            "foo",
            vec![
                match_identifier!("x"),
                match_identifier!("y"),
                match_integer_literal!("5")
            ]
        )
    );

    macro_rules! parse_statement_test {
        ($name:ident, $input:expr, $($m:expr),+) => {
            #[test]
            fn $name() {
                let tokens = Lexer::tokenize($input);
                let matchers = vec![$($m),+];
                match Parser::parse_program(&tokens) {
                    Ok(program) => {
                        for (statement, matcher) in program.statements.iter().zip(matchers.iter())
                        {
                            assert!(StatementMatcher::matches(&**matcher, statement),
                                    "Matcher failed to match expression {:?}", statement);
                        }
                    }
                    Err(err) => panic!("Failed to parse program: {}", err.message),
                }
            }
        };
    }

    parse_statement_test! {
        parse_let_statement_with_integer_literal,
        "let x: int32 = 5;",
        match_let_statement!(
            "x",
            match_type!(),
            match_integer_literal!("5"))
    }

    parse_statement_test! {
        parse_let_statement_with_floating_point_literal,
        "let x: float32 = 3.14159;",
        match_let_statement!(
            "x",
            match_type!("float32"),
            match_float_literal!("3.14159"))
    }

    parse_statement_test! {
        parse_let_statement_with_boolean_literal_true,
        "let x: bool = true;",
        match_let_statement!(
            "x",
            match_type!(),
            match_boolean_literal!(true))
    }

    parse_statement_test! {
        parse_let_statement_with_boolean_literal_false,
        "let x: bool = false;",
        match_let_statement!(
            "x",
            match_type!("bool"),
            match_boolean_literal!())
    }

    parse_statement_test! {
        parse_let_statement_with_identifier,
        "let x: int32 = y;",
        match_let_statement!(
            "x",
            match_type!(),
            match_identifier!("y"))
    }

    parse_statement_test! {
        parse_mutable_let_statement,
        "let mut x: int32 = y;",
        match_mutable_let_statement!(
            "x",
            match_type!(),
            match_any!())
    }

    parse_statement_test! {
        parse_let_statement_with_expression,
        "let x: int32 = 2 + 7;",
        match_let_statement!(
            "x",
            match_type!(),
            match_binary_expression!())
    }

    parse_statement_test! {
        parse_function_definition,
        "fn max(x:int32, y:int32) -> int32;",
        match_function_declaration!(
            "max",
            vec![match_parameter!("x", "int32"), match_parameter!("y", "int32")],
            match_type!("int32"))
    }

    parse_statement_test! {
        parse_function_definition_with_no_parameters,
        "fn max() -> int32;",
        match_function_declaration!(
            "max",
            match_type!("int32"))
    }

    parse_statement_test! {
        parse_function_definitions,
        "fn max() -> int32;
        fn min() -> int32;
        fn mean() -> float32;",
        match_function_declaration!(
            "max",
            match_type!("int32")),
        match_function_declaration!(
            "min",
            match_type!("int32")),
        match_function_declaration!(
            "mean",
            match_type!("float32"))
    }

    parse_statement_test! {
        parse_return_statement,
        "return 5.0;",
        match_return_statement!(
            match_float_literal!("5.0"))
    }

    parse_statement_test! {
        parse_return_integer_expression,
        "return 2 + 2;",
        match_return_statement!(
            match_binary_expression!(
                match_integer_literal!(2),
                ast::BinaryOperator::Plus,
                match_integer_literal!(2)))
    }

    parse_statement_test! {
        parse_return_boolean_expression,
        "return true xor false;",
        match_return_statement!(match_any!())
    }

    parse_statement_test! {
        parse_if_statement,
        "if x < 0 { foo(x); }",
        match_if_statement!(
            match_binary_expression!(
                match_identifier!("x"),
                ast::BinaryOperator::Less,
                match_integer_literal!("0")),
            vec![
                match_expression_statement!(match_function_call!("foo", vec![match_identifier!("x")]))
            ]
        )
    }
}
