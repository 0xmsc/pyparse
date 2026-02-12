use std::{
    collections::VecDeque,
    iter::Peekable,
    str::{CharIndices, Chars},
};

use anyhow::{Result, anyhow, bail, ensure};

use crate::token::{Span, Token, TokenKind};

enum LexerState {
    BeginLine,
    EndOfFile,
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    state: LexerState,
    indent_stack: Vec<usize>,
    pending_tokens: VecDeque<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            state: LexerState::BeginLine,
            indent_stack: vec![0],
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        loop {
            match self.state {
                LexerState::EndOfFile => {
                    return Ok(Token::new(
                        TokenKind::EOF,
                        Span {
                            start: self.pos,
                            end: self.pos,
                        },
                    ));
                }
                LexerState::BeginLine => {
                    // Count intendation
                    let indentation = self.consume_while(|c| c == ' ');

                    ensure!(
                        self.peek_char() != Some('\t'),
                        "Tabs are not allowed for indentation"
                    );

                    if self.peek_char() == Some('\n') {
                        // Fully blank line. Consume & go to next.
                        self.consume_char();
                    }

                    let indent_level = indentation.len();
                    if indent_level > self.current_indent() {
                        self.indent_stack.push(indent_level);
                        return Ok(Token::new(
                            TokenKind::Indent,
                            Span {
                                start: self.pos - indent_level,
                                end: self.pos,
                            },
                        ));
                    } else if indent_level < self.current_indent() {
                        // Pop until we find the indent level we want to return to.
                        // If we can't find it, it's an error.
                        while self.current_indent() > indent_level {
                            self.indent_stack.pop();
                            self.pending_tokens.push_back(Token::new(
                                TokenKind::Dedent,
                                Span {
                                    start: self.pos - indent_level,
                                    end: self.pos,
                                },
                            ));
                        }
                        ensure!(
                            self.current_indent() == indent_level,
                            "Inconsistent indentation: expected indent level {}, got {}",
                            self.current_indent(),
                            indent_level
                        );
                    } else {
                    }
                }
            }
        }
    }

    pub fn consume_while<T: Fn(char) -> bool>(&mut self, pred: T) -> &'a str {
        let start = self.pos;
        let end = self.pos;
        while let Some(c) = self.peek_char()
            && pred(c)
        {
            self.pos += c.len_utf8();
        }
        &self.input[start..end]
    }

    fn get_char(&self, pos: usize) -> Option<char> {
        self.input[pos..].chars().next()
    }

    fn peek_char(&self) -> Option<char> {
        self.get_char(self.pos)
    }

    fn consume_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn current_indent(&self) -> usize {
        // Since we must always return to a previous indent level, the stack can never be empty.
        *self.indent_stack.last().unwrap()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) => Some(Ok(token)),
            Err(e) => Some(Err(e)),
        }
    }
}

pub fn tokenize<'a>(input: &'a str) -> Result<Vec<Token<'a>>> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token()?;
        let is_eof = matches!(token.kind, TokenKind::EOF);
        tokens.push(token);
        if is_eof {
            break;
        }
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_simple_program() {
        let input = indoc! {"
            def fn():
                n = 4 + 4
                print(n)
            fn()
        "};
        let actual_tokens = tokenize(input).expect("tokenize should succeed");
        let expected_tokens = vec![
            TokenKind::Def,
            TokenKind::Identifier("fn"),
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Colon,
            TokenKind::Newline,
            TokenKind::Indent,
            TokenKind::Identifier("n"),
            TokenKind::Equal,
            TokenKind::Integer(4),
            TokenKind::Plus,
            TokenKind::Integer(4),
            TokenKind::Newline,
            TokenKind::Identifier("print"),
            TokenKind::LParen,
            TokenKind::Identifier("n"),
            TokenKind::RParen,
            TokenKind::Newline,
            TokenKind::Dedent,
            TokenKind::Identifier("fn"),
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Newline,
            TokenKind::EOF,
        ];

        let actual_kinds = actual_tokens
            .into_iter()
            .map(|token| token.kind)
            .collect::<Vec<_>>();
        assert_eq!(actual_kinds, expected_tokens);
    }

    #[test]
    fn errors_on_invalid_character() {
        let err = tokenize("x = 1 @ 2\n").expect_err("expected lexing failure");
        assert!(err.to_string().contains("Unexpected character '@'"));
    }

    #[test]
    fn errors_on_integer_overflow() {
        let err = tokenize("n = 99999999999999999999999999\n").expect_err("expected overflow");
        assert!(err.to_string().contains("Invalid integer literal"));
    }
}
