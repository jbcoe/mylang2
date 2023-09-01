#![allow(irrefutable_let_patterns)]

use crate::{
    ast::Program,
    ast::{self, Indentifier, LetStatement, Statement},
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
    pub fn new(tokens: &'a [Token]) -> Parser<'a> {
        let mut parser = Parser {
            tokens,
            position: 0,
            read_position: 0,
        };
        assert!(!parser.tokens.is_empty());
        assert!(parser.tokens.last().unwrap().kind() == Kind::EndOfFile);
        parser.advance();
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
    fn advance(&mut self) {
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

    fn try_parse_let_stmt(&mut self) -> Result<Statement, String> {
        assert!(self.token().kind() == Kind::Let);
        let start = self.position;
        self.advance(); // Consume the "let" token.
        let identifier = self.token();
        if identifier.kind() != Kind::Identifier {
            self.reset(start);
            return Err(format!("Expected identifier, got {:?}", identifier));
        }

        let identifier = Indentifier {
            name: identifier.text().to_string(),
        };
        self.advance(); // Consume the identifier.

        let colon = self.token();
        if colon.kind() != Kind::Colon {
            self.reset(start);
            return Err(format!("Expected colon, got {:?}", colon));
        }
        self.advance(); // Consume the colon.

        let ttype = self.token();
        let typename = match ttype.kind() {
            Kind::Int32 => "int32",
            _ => {
                self.reset(start);
                return Err(format!("Expected type, got {:?}", colon));
            }
        };
        self.advance(); // Consume the typename.

        let equals = self.token();
        if equals.kind() != Kind::EqualSign {
            self.reset(start);
            return Err(format!("Expected equals, got {:?}", equals));
        }
        self.advance(); // Consume the equals symbol.

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
        self.advance(); // Consume the literal and whitespace.

        let expression = crate::ast::Expression::IntegerLiteral(literal.text().to_string());
        Ok(ast::Statement::Let(LetStatement {
            identifier,
            ttype: crate::ast::Type {
                name: typename.to_string(),
            },
            expression: Box::new(expression),
        }))
    }

    // Reads the next statement and advances the parser.
    //
    // Returns an error if the statement cannot be parsed.
    // The parser is not advanced if an error is returned.
    fn next_stmt(&mut self) -> Result<Statement, String> {
        let token = self.token();
        match token.kind() {
            Kind::Let => self.try_parse_let_stmt(),
            _ => Err(format!("Failed to parse token {:?}", token)),
        }
    }

    // Parses a program consuming the parser.
    //
    // Returns an error if the program cannot be parsed.
    pub fn parse_program(mut self) -> Result<Program, ParserError> {
        let mut statements = vec![];
        while self.position < self.tokens.len() && self.token().kind() != Kind::EndOfFile {
            match self.next_stmt() {
                Ok(stmt) => statements.push(stmt),
                Err(message) => return Err(ParserError { message }),
            }
            self.advance();
        }
        Ok(Program { statements })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{self, Statement};

    fn tokenize(input: &'static str) -> Vec<crate::token::Token<'static>> {
        let mut tokens = vec![];
        let mut lexer = crate::lexer::Lexer::new(input);
        let mut token = lexer.next_token();
        while token.kind() != crate::token::Kind::EndOfFile {
            tokens.push(token);
            token = lexer.next_token();
        }
        tokens.push(token);
        tokens
    }

    #[test]
    fn empty_file_can_be_parsed() {
        let input = "";
        let tokens = tokenize(input);
        let parser = super::Parser::new(&tokens);
        let program = parser.parse_program();
        assert!(program.is_ok());
        assert!(program.unwrap().statements.is_empty());
    }

    #[test]
    fn parse_let_statement() {
        let input = "let x: int32 = 5\n";
        let tokens = tokenize(input);
        let parser = super::Parser::new(&tokens);
        match parser.parse_program() {
            Ok(program) => {
                assert!(program.statements.len() == 1);
                if let Statement::Let(let_stmt) = &program.statements[0] {
                    assert_eq!(let_stmt.identifier.name, "x");
                    assert_eq!(let_stmt.ttype.name, "int32");
                    if let ast::Expression::IntegerLiteral(value) = &*let_stmt.expression {
                        assert_eq!(value, "5");
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
        let tokens = tokenize(input);
        let parser = super::Parser::new(&tokens);
        match parser.parse_program() {
            Ok(_) => {
                panic!("Expected parse error statement");
            }
            Err(err) => {
                assert!(err.message == "Missing newline after let statement");
            }
        }
    }
}
