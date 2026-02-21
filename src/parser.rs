use thiserror::Error;

pub mod ast;

use self::ast::{AssignTarget, BinaryOperator, Expression, Program, Statement};
use crate::lexer::{Span, Token, TokenKind};

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

    /// Parse a full token stream into a `Program`.
    /// Example:
    /// ```text
    /// x = 1
    /// print(x)
    /// ```
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

    /// Parse one statement at the current token position.
    /// Examples:
    /// - `x = 1\n`
    /// - `if True:\n    pass\n`
    /// - `print(x)\n`
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.current_kind() {
            TokenKind::Class => self.parse_class_def(),
            TokenKind::Def => self.parse_function_def(),
            TokenKind::If => self.parse_if(),
            TokenKind::Try => self.parse_try(),
            TokenKind::While => self.parse_while(),
            TokenKind::For => self.parse_for(),
            TokenKind::Raise => self.parse_raise(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Pass => self.parse_pass(),
            TokenKind::Identifier(_) => self.parse_identifier_led_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    /// Parse a function definition statement.
    /// Example:
    /// ```text
    /// def sum2(a, b):
    ///     return a + b
    /// ```
    fn parse_function_def(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Def, "def")?;
        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::LParen, "(")?;
        let params = self.parse_identifier_list(TokenKind::RParen)?;
        self.expect_token(TokenKind::RParen, ")")?;
        self.expect_token(TokenKind::Colon, ":")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        let body = self.parse_indented_block()?;

        Ok(Statement::FunctionDef { name, params, body })
    }

    /// Parse a class definition statement.
    /// Example:
    /// ```text
    /// class Greeter:
    ///     def hello(self):
    ///         return 7
    /// ```
    fn parse_class_def(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Class, "class")?;
        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::Colon, ":")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        let body = self.parse_indented_block()?;

        Ok(Statement::ClassDef { name, body })
    }

    /// Parse statements that begin with an identifier.
    /// Examples:
    /// - `x = 1\n`
    /// - `values[1] = 7\n`
    /// - `self.x = 3\n`
    /// - `print(x)\n`
    fn parse_identifier_led_statement(&mut self) -> ParseResult<Statement> {
        let expr = self.parse_expression()?;
        if !matches!(self.current_kind(), TokenKind::Equal) {
            self.expect_statement_terminator()?;
            return Ok(Statement::Expr(expr));
        }

        let target = self.assignment_target(expr)?;
        self.expect_token(TokenKind::Equal, "=")?;
        let value = self.parse_expression()?;
        self.expect_statement_terminator()?;
        Ok(Statement::Assign { target, value })
    }

    fn assignment_target(&self, expr: Expression) -> ParseResult<AssignTarget> {
        match expr {
            Expression::Identifier(name) => Ok(AssignTarget::Name(name)),
            Expression::Index { object, index } => match *object {
                Expression::Identifier(name) => Ok(AssignTarget::Index {
                    name,
                    index: *index,
                }),
                _ => Err(self.error("assignment target")),
            },
            Expression::Attribute { object, name } => Ok(AssignTarget::Attribute {
                object: *object,
                name,
            }),
            _ => Err(self.error("assignment target")),
        }
    }

    /// Parse an expression statement followed by newline.
    /// Examples: `print(x)\n`, `f(1, 2)\n`, `values[0]\n`.
    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expr = self.parse_expression()?;
        self.expect_statement_terminator()?;
        Ok(Statement::Expr(expr))
    }

    /// Parse an `if` statement with optional `else` block.
    /// Example:
    /// ```text
    /// if n < 1:
    ///     pass
    /// else:
    ///     return 1
    /// ```
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

    /// Parse a `while` loop statement.
    /// Example:
    /// ```text
    /// while n < 10:
    ///     n = n + 1
    /// ```
    fn parse_while(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::While, "while")?;
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::Colon, ":")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        let body = self.parse_indented_block()?;

        Ok(Statement::While { condition, body })
    }

    /// Parse a `for` loop statement.
    /// Example:
    /// ```text
    /// for x in values:
    ///     print(x)
    /// ```
    fn parse_for(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::For, "for")?;
        let target = self.expect_identifier()?;
        self.expect_token(TokenKind::In, "in")?;
        let iterable = self.parse_expression()?;
        self.expect_token(TokenKind::Colon, ":")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        let body = self.parse_indented_block()?;
        Ok(Statement::For {
            target,
            iterable,
            body,
        })
    }

    /// Parse a `try` statement with catch-all `except` and/or `finally` blocks.
    fn parse_try(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Try, "try")?;
        self.expect_token(TokenKind::Colon, ":")?;
        self.expect_token(TokenKind::Newline, "newline")?;
        let body = self.parse_indented_block()?;

        let mut except_body = None;
        if self.try_consume(TokenKind::Except) {
            self.expect_token(TokenKind::Colon, ":")?;
            self.expect_token(TokenKind::Newline, "newline")?;
            except_body = Some(self.parse_indented_block()?);
        }

        let mut finally_body = None;
        if self.try_consume(TokenKind::Finally) {
            self.expect_token(TokenKind::Colon, ":")?;
            self.expect_token(TokenKind::Newline, "newline")?;
            finally_body = Some(self.parse_indented_block()?);
        }

        if except_body.is_none() && finally_body.is_none() {
            return Err(self.error("except or finally"));
        }

        Ok(Statement::Try {
            body,
            except_body,
            finally_body,
        })
    }

    /// Parse a `raise` statement with a required expression argument.
    fn parse_raise(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Raise, "raise")?;
        let value = self.parse_expression()?;
        self.expect_statement_terminator()?;
        Ok(Statement::Raise(value))
    }

    /// Parse a `return` statement.
    /// Examples: `return\n`, `return a + b\n`.
    fn parse_return(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Return, "return")?;
        if matches!(
            self.current_kind(),
            TokenKind::Newline | TokenKind::Dedent | TokenKind::EOF
        ) {
            self.expect_statement_terminator()?;
            return Ok(Statement::Return(None));
        }
        let value = self.parse_expression()?;
        self.expect_statement_terminator()?;
        Ok(Statement::Return(Some(value)))
    }

    /// Parse a `pass` statement.
    /// Example: `pass\n`.
    fn parse_pass(&mut self) -> ParseResult<Statement> {
        self.expect_token(TokenKind::Pass, "pass")?;
        self.expect_statement_terminator()?;
        Ok(Statement::Pass)
    }

    /// Parse an expression with full precedence handling.
    /// Example: `1 + 2 < 4 - 1`.
    fn parse_expression(&mut self) -> ParseResult<Expression> {
        self.parse_comparison()
    }

    /// Parse comparison expressions (`<`) over additive expressions.
    /// Example: `a + 1 < b - 2`.
    fn parse_comparison(&mut self) -> ParseResult<Expression> {
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

    /// Parse additive expressions (`+` and `-`) over postfix expressions.
    /// Example: `a + b - c`.
    fn parse_additive(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_postfix()?;
        loop {
            if self.try_consume(TokenKind::Plus) {
                let right = self.parse_postfix()?;
                expr = Expression::BinaryOp {
                    left: Box::new(expr),
                    op: BinaryOperator::Add,
                    right: Box::new(right),
                };
            } else if self.try_consume(TokenKind::Minus) {
                let right = self.parse_postfix()?;
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

    /// Parse postfix chains of calls, indexing, and attribute access.
    /// Example: `obj.method(1)[0]`.
    fn parse_postfix(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.try_consume(TokenKind::LParen) {
                let args = self.parse_expression_list(TokenKind::RParen)?;
                self.expect_token(TokenKind::RParen, ")")?;
                expr = Expression::Call {
                    callee: Box::new(expr),
                    args,
                };
            } else if self.try_consume(TokenKind::LBracket) {
                let index = self.parse_expression()?;
                self.expect_token(TokenKind::RBracket, "]")?;
                expr = Expression::Index {
                    object: Box::new(expr),
                    index: Box::new(index),
                };
            } else if self.try_consume(TokenKind::Dot) {
                let name = self.expect_identifier()?;
                expr = Expression::Attribute {
                    object: Box::new(expr),
                    name,
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Parse a comma-separated identifier list until `terminator`.
    /// Example: `(a, b, c)` in function parameters.
    fn parse_identifier_list(
        &mut self,
        terminator: TokenKind<'static>,
    ) -> ParseResult<Vec<String>> {
        let mut identifiers = Vec::new();
        if self.current_kind() == terminator {
            return Ok(identifiers);
        }
        loop {
            identifiers.push(self.expect_identifier()?);
            if self.try_consume(TokenKind::Comma) {
                if self.current_kind() == terminator {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(identifiers)
    }

    /// Parse a comma-separated expression list until `terminator`.
    /// Example: `(1, x + 2)` or `[1, 2, 3]`.
    fn parse_expression_list(
        &mut self,
        terminator: TokenKind<'static>,
    ) -> ParseResult<Vec<Expression>> {
        let mut expressions = Vec::new();
        if self.current_kind() == terminator {
            return Ok(expressions);
        }
        loop {
            expressions.push(self.parse_expression()?);
            if self.try_consume(TokenKind::Comma) {
                if self.current_kind() == terminator {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(expressions)
    }

    /// Parse dict items in `{key: value}` form until `}`.
    /// Example: `{"a": 1, "b": 2}`.
    fn parse_dict_entries(&mut self) -> ParseResult<Vec<(Expression, Expression)>> {
        let mut entries = Vec::new();
        if self.current_kind() == TokenKind::RBrace {
            return Ok(entries);
        }
        loop {
            let key = self.parse_expression()?;
            self.expect_token(TokenKind::Colon, ":")?;
            let value = self.parse_expression()?;
            entries.push((key, value));
            if self.try_consume(TokenKind::Comma) {
                if self.current_kind() == TokenKind::RBrace {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(entries)
    }

    /// Parse primary expressions (literals, identifiers, grouped, list/dict literals).
    /// Examples: `42`, `name`, `(a + b)`, `[1, 2]`, `{"a": 1}`.
    fn parse_primary(&mut self) -> ParseResult<Expression> {
        let expression = match self.current_kind() {
            TokenKind::Integer(value) => Expression::Integer(value),
            TokenKind::True => Expression::Boolean(true),
            TokenKind::False => Expression::Boolean(false),
            TokenKind::String(value) => Expression::String(value.to_string()),
            TokenKind::Identifier(name) => Expression::Identifier(name.to_string()),
            TokenKind::LBracket => {
                self.advance();
                let elements = self.parse_expression_list(TokenKind::RBracket)?;
                self.expect_token(TokenKind::RBracket, "]")?;
                return Ok(Expression::List(elements));
            }
            TokenKind::LBrace => {
                self.advance();
                let entries = self.parse_dict_entries()?;
                self.expect_token(TokenKind::RBrace, "}")?;
                return Ok(Expression::Dict(entries));
            }
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

    fn expect_statement_terminator(&mut self) -> ParseResult<()> {
        match self.current_kind() {
            TokenKind::Newline => {
                self.advance();
                Ok(())
            }
            TokenKind::Dedent | TokenKind::EOF => Ok(()),
            _ => Err(self.error("newline")),
        }
    }

    /// Parse an indented statement block between `Indent` and `Dedent`.
    /// Example body of `if`, `while`, or `def`.
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

/// Parse a full token vector into a `Program`.
/// Example: tokens for `x = 1\nprint(x)\n`.
pub fn parse_tokens<'a>(tokens: Vec<Token<'a>>) -> ParseResult<Program> {
    Parser::new(tokens)?.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Span;

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
                    params: vec![],
                    body: vec![
                        Statement::Assign {
                            target: AssignTarget::Name("n".to_string()),
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
    fn errors_when_statement_separator_is_missing() {
        let tokens = vec![
            tok(TokenKind::Identifier("x")),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Identifier("y")),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(2)),
            tok(TokenKind::EOF),
        ];

        let err = parse_tokens(tokens).expect_err("expected parse failure");
        assert_eq!(
            err,
            ParseError::UnexpectedToken {
                expected: "newline",
                found: "Identifier(\"y\")".to_string(),
                start: 0,
                end: 0,
            }
        );
    }

    #[test]
    fn parses_last_top_level_statement_without_trailing_newline() {
        let tokens = vec![
            tok(TokenKind::Identifier("x")),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Newline),
            tok(TokenKind::Identifier("print")),
            tok(TokenKind::LParen),
            tok(TokenKind::Identifier("x")),
            tok(TokenKind::RParen),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![
                    Statement::Assign {
                        target: AssignTarget::Name("x".to_string()),
                        value: Expression::Integer(1),
                    },
                    Statement::Expr(Expression::Call {
                        callee: Box::new(Expression::Identifier("print".to_string())),
                        args: vec![Expression::Identifier("x".to_string())],
                    }),
                ],
            }
        );
    }

    #[test]
    fn parses_last_block_statement_without_trailing_newline() {
        let tokens = vec![
            tok(TokenKind::If),
            tok(TokenKind::True),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Identifier("x")),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Dedent),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::If {
                    condition: Expression::Boolean(true),
                    then_body: vec![Statement::Assign {
                        target: AssignTarget::Name("x".to_string()),
                        value: Expression::Integer(1),
                    }],
                    else_body: vec![],
                }],
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
                        target: AssignTarget::Name("x".to_string()),
                        value: Expression::Integer(1),
                    }],
                },
            ],
        };

        assert_eq!(program, expected);
    }

    #[test]
    fn parses_for_loop() {
        let tokens = vec![
            tok(TokenKind::For),
            tok(TokenKind::Identifier("x")),
            tok(TokenKind::In),
            tok(TokenKind::Identifier("values")),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Identifier("print")),
            tok(TokenKind::LParen),
            tok(TokenKind::Identifier("x")),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::For {
                    target: "x".to_string(),
                    iterable: Expression::Identifier("values".to_string()),
                    body: vec![Statement::Expr(Expression::Call {
                        callee: Box::new(Expression::Identifier("print".to_string())),
                        args: vec![Expression::Identifier("x".to_string())],
                    })],
                }],
            }
        );
    }

    #[test]
    fn parses_raise_statement() {
        let tokens = vec![
            tok(TokenKind::Raise),
            tok(TokenKind::String("boom")),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse failed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Raise(Expression::String("boom".to_string()))],
            }
        );
    }

    #[test]
    fn parses_try_except_finally() {
        let tokens = vec![
            tok(TokenKind::Try),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Raise),
            tok(TokenKind::String("boom")),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::Except),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Pass),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::Finally),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Pass),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse failed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Try {
                    body: vec![Statement::Raise(Expression::String("boom".to_string()))],
                    except_body: Some(vec![Statement::Pass]),
                    finally_body: Some(vec![Statement::Pass]),
                }],
            }
        );
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

    #[test]
    fn parses_function_and_call_with_multiple_arguments() {
        let tokens = vec![
            tok(TokenKind::Def),
            tok(TokenKind::Identifier("sum2")),
            tok(TokenKind::LParen),
            tok(TokenKind::Identifier("a")),
            tok(TokenKind::Comma),
            tok(TokenKind::Identifier("b")),
            tok(TokenKind::RParen),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Return),
            tok(TokenKind::Identifier("a")),
            tok(TokenKind::Plus),
            tok(TokenKind::Identifier("b")),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::Identifier("sum2")),
            tok(TokenKind::LParen),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Comma),
            tok(TokenKind::Integer(2)),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![
                    Statement::FunctionDef {
                        name: "sum2".to_string(),
                        params: vec!["a".to_string(), "b".to_string()],
                        body: vec![Statement::Return(Some(Expression::BinaryOp {
                            left: Box::new(Expression::Identifier("a".to_string())),
                            op: BinaryOperator::Add,
                            right: Box::new(Expression::Identifier("b".to_string())),
                        }))],
                    },
                    Statement::Expr(Expression::Call {
                        callee: Box::new(Expression::Identifier("sum2".to_string())),
                        args: vec![Expression::Integer(1), Expression::Integer(2)],
                    }),
                ],
            }
        );
    }

    #[test]
    fn parses_trailing_commas_in_params_and_call_arguments() {
        let tokens = vec![
            tok(TokenKind::Def),
            tok(TokenKind::Identifier("sum2")),
            tok(TokenKind::LParen),
            tok(TokenKind::Identifier("a")),
            tok(TokenKind::Comma),
            tok(TokenKind::Identifier("b")),
            tok(TokenKind::Comma),
            tok(TokenKind::RParen),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Return),
            tok(TokenKind::Identifier("a")),
            tok(TokenKind::Plus),
            tok(TokenKind::Identifier("b")),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::Identifier("sum2")),
            tok(TokenKind::LParen),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Comma),
            tok(TokenKind::Integer(2)),
            tok(TokenKind::Comma),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![
                    Statement::FunctionDef {
                        name: "sum2".to_string(),
                        params: vec!["a".to_string(), "b".to_string()],
                        body: vec![Statement::Return(Some(Expression::BinaryOp {
                            left: Box::new(Expression::Identifier("a".to_string())),
                            op: BinaryOperator::Add,
                            right: Box::new(Expression::Identifier("b".to_string())),
                        }))],
                    },
                    Statement::Expr(Expression::Call {
                        callee: Box::new(Expression::Identifier("sum2".to_string())),
                        args: vec![Expression::Integer(1), Expression::Integer(2)],
                    }),
                ],
            }
        );
    }

    #[test]
    fn parses_list_literal() {
        let tokens = vec![
            tok(TokenKind::Identifier("print")),
            tok(TokenKind::LParen),
            tok(TokenKind::LBracket),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Comma),
            tok(TokenKind::Integer(2)),
            tok(TokenKind::RBracket),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expr(Expression::Call {
                    callee: Box::new(Expression::Identifier("print".to_string())),
                    args: vec![Expression::List(vec![
                        Expression::Integer(1),
                        Expression::Integer(2),
                    ])],
                })],
            }
        );
    }

    #[test]
    fn parses_list_literal_with_trailing_comma() {
        let tokens = vec![
            tok(TokenKind::Identifier("print")),
            tok(TokenKind::LParen),
            tok(TokenKind::LBracket),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Comma),
            tok(TokenKind::Integer(2)),
            tok(TokenKind::Comma),
            tok(TokenKind::RBracket),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expr(Expression::Call {
                    callee: Box::new(Expression::Identifier("print".to_string())),
                    args: vec![Expression::List(vec![
                        Expression::Integer(1),
                        Expression::Integer(2),
                    ])],
                })],
            }
        );
    }

    #[test]
    fn parses_dict_literal_with_trailing_comma() {
        let tokens = vec![
            tok(TokenKind::Identifier("print")),
            tok(TokenKind::LParen),
            tok(TokenKind::LBrace),
            tok(TokenKind::String("a")),
            tok(TokenKind::Colon),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Comma),
            tok(TokenKind::String("b")),
            tok(TokenKind::Colon),
            tok(TokenKind::Integer(2)),
            tok(TokenKind::Comma),
            tok(TokenKind::RBrace),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expr(Expression::Call {
                    callee: Box::new(Expression::Identifier("print".to_string())),
                    args: vec![Expression::Dict(vec![
                        (Expression::String("a".to_string()), Expression::Integer(1)),
                        (Expression::String("b".to_string()), Expression::Integer(2)),
                    ])],
                })],
            }
        );
    }

    #[test]
    fn parses_list_index_and_assignment() {
        let tokens = vec![
            tok(TokenKind::Identifier("values")),
            tok(TokenKind::LBracket),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::RBracket),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(7)),
            tok(TokenKind::Newline),
            tok(TokenKind::Identifier("print")),
            tok(TokenKind::LParen),
            tok(TokenKind::Identifier("values")),
            tok(TokenKind::LBracket),
            tok(TokenKind::Integer(0)),
            tok(TokenKind::RBracket),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![
                    Statement::Assign {
                        target: AssignTarget::Index {
                            name: "values".to_string(),
                            index: Expression::Integer(1),
                        },
                        value: Expression::Integer(7),
                    },
                    Statement::Expr(Expression::Call {
                        callee: Box::new(Expression::Identifier("print".to_string())),
                        args: vec![Expression::Index {
                            object: Box::new(Expression::Identifier("values".to_string())),
                            index: Box::new(Expression::Integer(0)),
                        }],
                    }),
                ],
            }
        );
    }

    #[test]
    fn parses_method_call_expression_statement() {
        let tokens = vec![
            tok(TokenKind::Identifier("values")),
            tok(TokenKind::Dot),
            tok(TokenKind::Identifier("append")),
            tok(TokenKind::LParen),
            tok(TokenKind::Integer(3)),
            tok(TokenKind::RParen),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expr(Expression::Call {
                    callee: Box::new(Expression::Attribute {
                        object: Box::new(Expression::Identifier("values".to_string())),
                        name: "append".to_string(),
                    }),
                    args: vec![Expression::Integer(3)],
                })],
            }
        );
    }

    #[test]
    fn parses_attribute_assignment() {
        let tokens = vec![
            tok(TokenKind::Identifier("self")),
            tok(TokenKind::Dot),
            tok(TokenKind::Identifier("value")),
            tok(TokenKind::Equal),
            tok(TokenKind::Integer(1)),
            tok(TokenKind::Newline),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Assign {
                    target: AssignTarget::Attribute {
                        object: Expression::Identifier("self".to_string()),
                        name: "value".to_string(),
                    },
                    value: Expression::Integer(1),
                }],
            }
        );
    }

    #[test]
    fn parses_class_with_method() {
        let tokens = vec![
            tok(TokenKind::Class),
            tok(TokenKind::Identifier("Greeter")),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Def),
            tok(TokenKind::Identifier("hello")),
            tok(TokenKind::LParen),
            tok(TokenKind::Identifier("self")),
            tok(TokenKind::RParen),
            tok(TokenKind::Colon),
            tok(TokenKind::Newline),
            tok(TokenKind::Indent),
            tok(TokenKind::Return),
            tok(TokenKind::Integer(7)),
            tok(TokenKind::Newline),
            tok(TokenKind::Dedent),
            tok(TokenKind::Dedent),
            tok(TokenKind::EOF),
        ];

        let program = parse_tokens(tokens).expect("parse should succeed");
        assert_eq!(
            program,
            Program {
                statements: vec![Statement::ClassDef {
                    name: "Greeter".to_string(),
                    body: vec![Statement::FunctionDef {
                        name: "hello".to_string(),
                        params: vec!["self".to_string()],
                        body: vec![Statement::Return(Some(Expression::Integer(7)))],
                    }],
                }],
            }
        );
    }
}
