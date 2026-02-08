use crate::ast::{BinaryOperator, Expression, Program, Statement};
use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Clone)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token<'a>,
    peeked: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let current = lexer.next_token();
        Self {
            lexer,
            current,
            peeked: None,
        }
    }

    pub fn parse_program(mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();
        while !matches!(self.current, Token::EOF) {
            if self.consume_newlines() {
                continue;
            }
            statements.push(self.parse_statement()?);
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if matches!(self.current, Token::Def) {
            return self.parse_function_def();
        }
        if matches!(self.current, Token::Identifier(_)) && matches!(self.peek(), Token::Equal) {
            return self.parse_assignment();
        }
        let expr = self.parse_expression()?;
        self.expect_newline()?;
        Ok(Statement::Expr(expr))
    }

    fn parse_function_def(&mut self) -> Result<Statement, ParseError> {
        self.expect_def()?;
        let name = self.expect_identifier()?;
        self.expect_lparen()?;
        self.expect_rparen()?;
        self.expect_colon()?;
        self.expect_newline()?;
        self.expect_indent()?;

        let mut body = Vec::new();
        while !matches!(self.current, Token::Dedent | Token::EOF) {
            if self.consume_newlines() {
                continue;
            }
            body.push(self.parse_statement()?);
        }
        self.expect_dedent()?;

        Ok(Statement::FunctionDef { name, body })
    }

    fn parse_assignment(&mut self) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;
        self.expect_equal()?;
        let value = self.parse_expression()?;
        self.expect_newline()?;
        Ok(Statement::Assign { name, value })
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_call()?;
        loop {
            if matches!(self.current, Token::Plus) {
                self.advance();
                let right = self.parse_call()?;
                expr = Expression::BinaryOp {
                    left: Box::new(expr),
                    op: BinaryOperator::Add,
                    right: Box::new(right),
                };
            } else if matches!(self.current, Token::Minus) {
                self.advance();
                let right = self.parse_call()?;
                expr = Expression::BinaryOp {
                    left: Box::new(expr),
                    op: BinaryOperator::Sub,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_call(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_primary()?;
        while matches!(self.current, Token::LParen) {
            self.advance();
            let mut args = Vec::new();
            if !matches!(self.current, Token::RParen) {
                args.push(self.parse_expression()?);
            }
            self.expect_rparen()?;
            expr = Expression::Call {
                callee: Box::new(expr),
                args,
            };
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match &self.current {
            Token::Integer(value) => {
                let value = *value;
                self.advance();
                Ok(Expression::Integer(value))
            }
            Token::Identifier(name) => {
                let name = name.to_string();
                self.advance();
                Ok(Expression::Identifier(name))
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect_rparen()?;
                Ok(expr)
            }
            _ => Err(self.error("expression")),
        }
    }

    fn consume_newlines(&mut self) -> bool {
        let mut consumed = false;
        while matches!(self.current, Token::Newline) {
            consumed = true;
            self.advance();
        }
        consumed
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        if let Token::Identifier(name) = &self.current {
            let name = name.to_string();
            self.advance();
            Ok(name)
        } else {
            Err(self.error("identifier"))
        }
    }

    fn expect_def(&mut self) -> Result<(), ParseError> {
        if matches!(self.current, Token::Def) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("def"))
        }
    }

    fn expect_equal(&mut self) -> Result<(), ParseError> {
        if matches!(self.current, Token::Equal) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("="))
        }
    }

    fn expect_lparen(&mut self) -> Result<(), ParseError> {
        if matches!(self.current, Token::LParen) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("("))
        }
    }

    fn expect_rparen(&mut self) -> Result<(), ParseError> {
        if matches!(self.current, Token::RParen) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(")"))
        }
    }

    fn expect_colon(&mut self) -> Result<(), ParseError> {
        if matches!(self.current, Token::Colon) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(":"))
        }
    }

    fn expect_newline(&mut self) -> Result<(), ParseError> {
        if matches!(self.current, Token::Newline) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("newline"))
        }
    }

    fn expect_indent(&mut self) -> Result<(), ParseError> {
        if matches!(self.current, Token::Indent) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("indent"))
        }
    }

    fn expect_dedent(&mut self) -> Result<(), ParseError> {
        if matches!(self.current, Token::Dedent) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("dedent"))
        }
    }

    fn advance(&mut self) -> Token<'a> {
        let next = self.next_token();
        std::mem::replace(&mut self.current, next)
    }

    fn next_token(&mut self) -> Token<'a> {
        if let Some(token) = self.peeked.take() {
            token
        } else {
            self.lexer.next_token()
        }
    }

    fn peek(&mut self) -> &Token<'a> {
        if self.peeked.is_none() {
            self.peeked = Some(self.lexer.next_token());
        }
        self.peeked.as_ref().expect("peeked token missing")
    }

    fn error(&self, expected: &str) -> ParseError {
        ParseError::new(format!("Expected {expected}, got {:?}", self.current))
    }
}

pub fn parse(input: &str) -> Result<Program, ParseError> {
    Parser::new(input).parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn parses_simple_program() {
        let input = indoc! {"
            def fn():
                n = 4 + 4
                print(n)
            fn()
        "};
        let program = parse(input).expect("parse failed");

        let expected = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "fn".to_string(),
                    body: vec![
                        Statement::Assign {
                            name: "n".to_string(),
                            value: Expression::BinaryOp {
                                left: Box::new(Expression::Integer(4)),
                                op: BinaryOperator::Add,
                                right: Box::new(Expression::Integer(4)),
                            },
                        },
                        Statement::Expr(Expression::Call {
                            callee: Box::new(Expression::Identifier("print".to_string())),
                            args: vec![Expression::Identifier("n".to_string())],
                        }),
                    ],
                },
                Statement::Expr(Expression::Call {
                    callee: Box::new(Expression::Identifier("fn".to_string())),
                    args: vec![],
                }),
            ],
        };

        assert_eq!(program, expected);
    }
}
