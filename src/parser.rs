use thiserror::Error;

use crate::ast::{BinaryOperator, Expression, Program, Statement};
use crate::token::{Span, Token, TokenKind};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParseError {
    #[error("Cannot create parser from an empty token stream")]
    EmptyTokenStream,
    #[error("Expected {expected}, got {found} at byte range {start}..{end}")]
    UnexpectedToken {
        expected: &'static str,
        found: String,
        start: usize,
        end: usize,
    },
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> ParseResult<Self> {
        if tokens.is_empty() {
            return Err(ParseError::EmptyTokenStream);
        }
        Ok(Self { tokens, pos: 0 })
    }

    pub fn parse_program(mut self) -> ParseResult<Program> {
        let mut statements = Vec::new();
        while !matches!(self.current_kind(), TokenKind::EOF) {
            if self.consume_newlines() {
                continue;
            }
            statements.push(self.parse_statement()?);
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        if matches!(self.current_kind(), TokenKind::Def) {
            return self.parse_function_def();
        }
        if matches!(self.current_kind(), TokenKind::If) {
            return self.parse_if();
        }
        if matches!(self.current_kind(), TokenKind::While) {
            return self.parse_while();
        }
        if matches!(self.current_kind(), TokenKind::Return) {
            return self.parse_return();
        }
        if matches!(self.current_kind(), TokenKind::Pass) {
            return self.parse_pass();
        }
        if matches!(self.current_kind(), TokenKind::Identifier(_))
            && matches!(self.peek_kind(), TokenKind::Equal)
        {
            return self.parse_assignment();
        }
        let expr = self.parse_expression()?;
        self.expect_token(TokenKind::Newline, "newline")?;
        Ok(Statement::Expr(expr))
    }

    fn parse_function_def(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Def, "def")?;
        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::LParen, "(")?;
        self.expect_token(TokenKind::RParen, ")")?;
        self.expect_token(TokenKind::Colon, ":")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        let body = self.parse_indented_block()?;

        Ok(Statement::FunctionDef { name, body })
    }

    fn parse_assignment(&mut self) -> ParseResult<Statement> {
        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::Equal, "=")?;
        let value = self.parse_expression()?;
        self.expect_token(TokenKind::Newline, "newline")?;
        Ok(Statement::Assign { name, value })
    }

    fn parse_if(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::If, "if")?;
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::Colon, ":")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        let then_body = self.parse_indented_block()?;

        let mut else_body = Vec::new();
        if self.try_consume(TokenKind::Else) {
            self.expect_token(TokenKind::Colon, ":")?;
            self.expect_token(TokenKind::Newline, "newline")?;
            else_body = self.parse_indented_block()?;
        }

        Ok(Statement::If {
            condition,
            then_body,
            else_body,
        })
    }

    fn parse_while(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::While, "while")?;
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::Colon, ":")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        let body = self.parse_indented_block()?;

        Ok(Statement::While { condition, body })
    }

    fn parse_return(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Return, "return")?;
        if self.try_consume(TokenKind::Newline) {
            return Ok(Statement::Return(None));
        }
        let value = self.parse_expression()?;
        self.expect_token(TokenKind::Newline, "newline")?;
        Ok(Statement::Return(Some(value)))
    }

    fn parse_pass(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Pass, "pass")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        Ok(Statement::Pass)
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        // Entry point for expressions: parse the lowest-precedence level.
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> ParseResult<Expression> {
        // Comparison level (`<`), with additive expressions as operands.
        let mut expr = self.parse_additive()?;
        while self.try_consume(TokenKind::Less) {
            let right = self.parse_additive()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                op: BinaryOperator::LessThan,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_additive(&mut self) -> ParseResult<Expression> {
        // Additive level (`+`/`-`), with call/primary expressions as operands.
        let mut expr = self.parse_call()?;
        loop {
            if self.try_consume(TokenKind::Plus) {
                let right = self.parse_call()?;
                expr = Expression::BinaryOp {
                    left: Box::new(expr),
                    op: BinaryOperator::Add,
                    right: Box::new(right),
                };
            } else if self.try_consume(TokenKind::Minus) {
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

    fn parse_call(&mut self) -> ParseResult<Expression> {
        // Postfix call level: parse `callee(...)` chains left-to-right.
        let mut expr = self.parse_primary()?;
        while self.try_consume(TokenKind::LParen) {
            let mut args = Vec::new();
            if !matches!(self.current_kind(), TokenKind::RParen) {
                args.push(self.parse_expression()?);
            }
            self.expect_token(TokenKind::RParen, ")")?;
            expr = Expression::Call {
                callee: Box::new(expr),
                args,
            };
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> ParseResult<Expression> {
        // Primary atoms: literals, identifiers, and parenthesized expressions.
        let expression = match self.current_kind() {
            TokenKind::Integer(value) => Expression::Integer(value),
            TokenKind::True => Expression::Boolean(true),
            TokenKind::False => Expression::Boolean(false),
            TokenKind::String(value) => Expression::String(value.to_string()),
            TokenKind::Identifier(name) => Expression::Identifier(name.to_string()),
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect_token(TokenKind::RParen, ")")?;
                return Ok(expr);
            }
            _ => return Err(self.error("expression")),
        };
        self.advance();
        Ok(expression)
    }

    fn consume_newlines(&mut self) -> bool {
        let mut consumed = false;
        while self.try_consume(TokenKind::Newline) {
            consumed = true;
        }
        consumed
    }

    fn parse_indented_block(&mut self) -> ParseResult<Vec<Statement>> {
        self.expect_token(TokenKind::Indent, "indent")?;

        let mut body = Vec::new();
        while !matches!(self.current_kind(), TokenKind::Dedent | TokenKind::EOF) {
            if self.consume_newlines() {
                continue;
            }
            body.push(self.parse_statement()?);
        }

        self.expect_token(TokenKind::Dedent, "dedent")?;
        Ok(body)
    }
    fn expect_identifier(&mut self) -> ParseResult<String> {
        if let TokenKind::Identifier(name) = self.current_kind() {
            self.advance();
            Ok(name.to_string())
        } else {
            Err(self.error("identifier"))
        }
    }

    fn expect_token(
        &mut self,
        expected_kind: TokenKind<'static>,
        expected_name: &'static str,
    ) -> ParseResult<()> {
        if self.current_kind() == expected_kind {
            self.advance();
            Ok(())
        } else {
            Err(self.error(expected_name))
        }
    }

    fn try_consume(&mut self, expected_kind: TokenKind<'static>) -> bool {
        if self.current_kind() == expected_kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) {
        if self.pos + 1 < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn current_kind(&self) -> TokenKind<'a> {
        self.tokens[self.pos].kind
    }

    fn current_span(&self) -> Span {
        self.tokens[self.pos].span
    }

    fn peek_kind(&self) -> TokenKind<'a> {
        self.tokens
            .get(self.pos + 1)
            .map(|token| token.kind)
            .unwrap_or(TokenKind::EOF)
    }

    fn error(&self, expected: &'static str) -> ParseError {
        let span = self.current_span();
        ParseError::UnexpectedToken {
            expected,
            found: format!("{:?}", self.current_kind()),
            start: span.start,
            end: span.end,
        }
    }
}

pub fn parse_tokens<'a>(tokens: Vec<Token<'a>>) -> ParseResult<Program> {
    Parser::new(tokens)?.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Span;

    fn tok<'a>(kind: TokenKind<'a>) -> Token<'a> {
        Token::new(kind, Span::default())
    }

    #[test]
    fn parses_simple_program() {
        let tokens = vec![
            tok(TokenKind::Def),
            tok(TokenKind::Identifier("fn")),
            tok(TokenKind::LParen),
            tok(TokenKind::RParen),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Identifier("n")),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(4)),
            tok(TokenKind::Plus),
            tok(TokenKind::Integer(4)),
            tok(TokenKind::Newline),
            tok(TokenKind::Identifier("print")),
            tok(TokenKind::LParen),
            tok(TokenKind::Identifier("n")),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::Identifier("fn")),
            tok(TokenKind::LParen),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];
        let program = parse_tokens(tokens).expect("parse failed");

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

    #[test]
    fn errors_on_empty_token_stream() {
        let err = parse_tokens(vec![]).expect_err("expected empty token stream failure");
        assert_eq!(err, ParseError::EmptyTokenStream);
    }

    #[test]
    fn errors_when_newline_is_missing_after_statement() {
        let tokens = vec![
            tok(TokenKind::Identifier("x")),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::EOF),
        ];

        let err = parse_tokens(tokens).expect_err("expected parse failure");
        assert_eq!(
            err,
            ParseError::UnexpectedToken {
                expected: "newline",
                found: "EOF".to_string(),
                start: 0,
                end: 0,
            }
        );
    }

    #[test]
    fn parses_if_else_while_and_return() {
        let tokens = vec![
            tok(TokenKind::If),
            tok(TokenKind::True),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Pass),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::Else),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Return),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::While),
            tok(TokenKind::False),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Identifier("x")),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        let expected = Program {
            statements: vec![
                Statement::If {
                    condition: Expression::Boolean(true),
                    then_body: vec![Statement::Pass],
                    else_body: vec![Statement::Return(None)],
                },
                Statement::While {
                    condition: Expression::Boolean(false),
                    body: vec![Statement::Assign {
                        name: "x".to_string(),
                        value: Expression::Integer(1),
                    }],
                },
            ],
        };

        assert_eq!(program, expected);
    }

    #[test]
    fn skips_leading_blank_lines() {
        let tokens = vec![
            tok(TokenKind::Newline),
            tok(TokenKind::Newline),
            tok(TokenKind::Pass),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Pass],
            }
        );
    }
}
