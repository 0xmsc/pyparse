use anyhow::Result;

use crate::ast::{BinaryOperator, Expression, Program, Statement};
use crate::token::{Span, Token, TokenKind};

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    pos: usize,
    current: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(mut tokens: Vec<Token<'a>>) -> Self {
        if tokens.is_empty() {
            tokens.push(Token::new(TokenKind::EOF, Span::default()));
        }
        let current = tokens[0].clone();
        Self {
            tokens,
            pos: 0,
            current,
        }
    }

    pub fn parse_program(mut self) -> Result<Program> {
        let mut statements = Vec::new();
        while !matches!(self.current.kind, TokenKind::EOF) {
            if self.consume_newlines() {
                continue;
            }
            statements.push(self.parse_statement()?);
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        if matches!(self.current.kind, TokenKind::Def) {
            return self.parse_function_def();
        }
        if matches!(self.current.kind, TokenKind::If) {
            return self.parse_if();
        }
        if matches!(self.current.kind, TokenKind::While) {
            return self.parse_while();
        }
        if matches!(self.current.kind, TokenKind::Return) {
            return self.parse_return();
        }
        if matches!(self.current.kind, TokenKind::Pass) {
            return self.parse_pass();
        }
        if matches!(self.current.kind, TokenKind::Identifier(_))
            && matches!(self.peek_kind(), TokenKind::Equal)
        {
            return self.parse_assignment();
        }
        let expr = self.parse_expression()?;
        self.expect_newline()?;
        Ok(Statement::Expr(expr))
    }

    fn parse_function_def(&mut self) -> Result<Statement> {
        self.expect_def()?;
        let name = self.expect_identifier()?;
        self.expect_lparen()?;
        self.expect_rparen()?;
        self.expect_colon()?;
        self.expect_newline()?;
        self.expect_indent()?;

        let mut body = Vec::new();
        while !matches!(self.current.kind, TokenKind::Dedent | TokenKind::EOF) {
            if self.consume_newlines() {
                continue;
            }
            body.push(self.parse_statement()?);
        }
        self.expect_dedent()?;

        Ok(Statement::FunctionDef { name, body })
    }

    fn parse_assignment(&mut self) -> Result<Statement> {
        let name = self.expect_identifier()?;
        self.expect_equal()?;
        let value = self.parse_expression()?;
        self.expect_newline()?;
        Ok(Statement::Assign { name, value })
    }

    fn parse_if(&mut self) -> Result<Statement> {
        self.expect_if()?;
        let condition = self.parse_expression()?;
        self.expect_colon()?;
        self.expect_newline()?;
        self.expect_indent()?;

        let mut then_body = Vec::new();
        while !matches!(self.current.kind, TokenKind::Dedent | TokenKind::EOF) {
            if self.consume_newlines() {
                continue;
            }
            then_body.push(self.parse_statement()?);
        }
        self.expect_dedent()?;

        let mut else_body = Vec::new();
        if matches!(self.current.kind, TokenKind::Else) {
            self.expect_else()?;
            self.expect_colon()?;
            self.expect_newline()?;
            self.expect_indent()?;
            while !matches!(self.current.kind, TokenKind::Dedent | TokenKind::EOF) {
                if self.consume_newlines() {
                    continue;
                }
                else_body.push(self.parse_statement()?);
            }
            self.expect_dedent()?;
        }

        Ok(Statement::If {
            condition,
            then_body,
            else_body,
        })
    }

    fn parse_while(&mut self) -> Result<Statement> {
        self.expect_while()?;
        let condition = self.parse_expression()?;
        self.expect_colon()?;
        self.expect_newline()?;
        self.expect_indent()?;

        let mut body = Vec::new();
        while !matches!(self.current.kind, TokenKind::Dedent | TokenKind::EOF) {
            if self.consume_newlines() {
                continue;
            }
            body.push(self.parse_statement()?);
        }
        self.expect_dedent()?;

        Ok(Statement::While { condition, body })
    }

    fn parse_return(&mut self) -> Result<Statement> {
        self.expect_return()?;
        if matches!(self.current.kind, TokenKind::Newline) {
            self.advance();
            return Ok(Statement::Return(None));
        }
        let value = self.parse_expression()?;
        self.expect_newline()?;
        Ok(Statement::Return(Some(value)))
    }

    fn parse_pass(&mut self) -> Result<Statement> {
        self.expect_pass()?;
        self.expect_newline()?;
        Ok(Statement::Pass)
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<Expression> {
        let mut expr = self.parse_additive()?;
        while matches!(self.current.kind, TokenKind::Less) {
            self.advance();
            let right = self.parse_additive()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                op: BinaryOperator::LessThan,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<Expression> {
        let mut expr = self.parse_call()?;
        loop {
            if matches!(self.current.kind, TokenKind::Plus) {
                self.advance();
                let right = self.parse_call()?;
                expr = Expression::BinaryOp {
                    left: Box::new(expr),
                    op: BinaryOperator::Add,
                    right: Box::new(right),
                };
            } else if matches!(self.current.kind, TokenKind::Minus) {
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

    fn parse_call(&mut self) -> Result<Expression> {
        let mut expr = self.parse_primary()?;
        while matches!(self.current.kind, TokenKind::LParen) {
            self.advance();
            let mut args = Vec::new();
            if !matches!(self.current.kind, TokenKind::RParen) {
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

    fn parse_primary(&mut self) -> Result<Expression> {
        match &self.current.kind {
            TokenKind::Integer(value) => {
                let value = *value;
                self.advance();
                Ok(Expression::Integer(value))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expression::Boolean(true))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expression::Boolean(false))
            }
            TokenKind::String(value) => {
                let value = value.to_string();
                self.advance();
                Ok(Expression::String(value))
            }
            TokenKind::Identifier(name) => {
                let name = name.to_string();
                self.advance();
                Ok(Expression::Identifier(name))
            }
            TokenKind::LParen => {
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
        while matches!(self.current.kind, TokenKind::Newline) {
            consumed = true;
            self.advance();
        }
        consumed
    }

    fn expect_identifier(&mut self) -> Result<String> {
        if let TokenKind::Identifier(name) = &self.current.kind {
            let name = name.to_string();
            self.advance();
            Ok(name)
        } else {
            Err(self.error("identifier"))
        }
    }

    fn expect_def(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Def) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("def"))
        }
    }

    fn expect_if(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::If) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("if"))
        }
    }

    fn expect_while(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::While) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("while"))
        }
    }

    fn expect_else(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Else) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("else"))
        }
    }

    fn expect_return(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Return) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("return"))
        }
    }

    fn expect_pass(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Pass) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("pass"))
        }
    }

    fn expect_equal(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Equal) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("="))
        }
    }

    fn expect_lparen(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::LParen) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("("))
        }
    }

    fn expect_rparen(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::RParen) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(")"))
        }
    }

    fn expect_colon(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Colon) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(":"))
        }
    }

    fn expect_newline(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Newline) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("newline"))
        }
    }

    fn expect_indent(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Indent) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("indent"))
        }
    }

    fn expect_dedent(&mut self) -> Result<()> {
        if matches!(self.current.kind, TokenKind::Dedent) {
            self.advance();
            Ok(())
        } else {
            Err(self.error("dedent"))
        }
    }

    fn advance(&mut self) -> Token<'a> {
        let next = self
            .tokens
            .get(self.pos + 1)
            .cloned()
            .unwrap_or(Token::new(TokenKind::EOF, Span::default()));
        self.pos = self.pos.saturating_add(1);
        std::mem::replace(&mut self.current, next)
    }

    fn peek_kind(&self) -> TokenKind<'a> {
        self.tokens
            .get(self.pos + 1)
            .map(|token| token.kind)
            .unwrap_or(TokenKind::EOF)
    }

    fn error(&self, expected: &str) -> anyhow::Error {
        let span = self.current.span();
        anyhow::anyhow!(
            "Expected {expected}, got {:?} at line {}, column {}",
            self.current.kind(),
            span.line,
            span.column
        )
    }
}

pub fn parse_tokens<'a>(tokens: Vec<Token<'a>>) -> Result<Program> {
    Parser::new(tokens).parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_simple_program() {
        fn tok<'a>(kind: TokenKind<'a>) -> Token<'a> {
            Token::new(kind, Span::default())
        }

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
}
