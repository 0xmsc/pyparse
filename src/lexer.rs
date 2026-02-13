use anyhow::{Result, anyhow, bail};

use crate::token::{Span, Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerState {
    LineBegin,
    TokenStart,
    EmitPending,
    EmitEof,
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    indent_stack: Vec<usize>,
    pending_tokens: Vec<Token<'a>>,
    state: LexerState,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            indent_stack: vec![0],
            pending_tokens: Vec::new(),
            state: LexerState::LineBegin,
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        loop {
            match self.state {
                LexerState::LineBegin => {
                    let indent_level = self.count_indentation()?;
                    let current_indent = *self.indent_stack.last().unwrap();
                    let index = self.current_index();
                    let span = Span {
                        start: index,
                        end: index,
                    };

                    if indent_level > current_indent {
                        self.indent_stack.push(indent_level);
                        self.state = LexerState::TokenStart;
                        return Ok(Token::new(TokenKind::Indent, span));
                    }

                    if indent_level < current_indent {
                        while let Some(&top) = self.indent_stack.last() {
                            if top > indent_level {
                                self.indent_stack.pop();
                                self.pending_tokens.push(Token::new(TokenKind::Dedent, span));
                            } else {
                                break;
                            }
                        }
                        if *self.indent_stack.last().unwrap() != indent_level {
                            let (line, column) = self.line_column_for_index(index);
                            bail!(
                                "Invalid dedent to {} spaces at line {}, column {}",
                                indent_level,
                                line,
                                column
                            );
                        }
                        self.state = LexerState::EmitPending;
                        continue;
                    }

                    self.state = LexerState::TokenStart;
                }
                LexerState::TokenStart => {
                    self.skip_whitespace();

                    let start_idx = self.current_index();
                    let Some(ch) = self.peek_char() else {
                        while self.indent_stack.len() > 1 {
                            self.indent_stack.pop();
                            let index = self.current_index();
                            let span = Span {
                                start: index,
                                end: index,
                            };
                            self.pending_tokens.push(Token::new(TokenKind::Dedent, span));
                        }
                        self.state = if self.pending_tokens.is_empty() {
                            LexerState::EmitEof
                        } else {
                            LexerState::EmitPending
                        };
                        continue;
                    };

                    let token = match ch {
                        '\n' => {
                            self.advance_char();
                            self.state = LexerState::LineBegin;
                            Token::new(
                                TokenKind::Newline,
                                Span {
                                    start: start_idx,
                                    end: start_idx + 1,
                                },
                            )
                        }
                        '=' => {
                            self.advance_char();
                            Token::new(
                                TokenKind::Equal,
                                Span {
                                    start: start_idx,
                                    end: start_idx + 1,
                                },
                            )
                        }
                        '+' => {
                            self.advance_char();
                            Token::new(
                                TokenKind::Plus,
                                Span {
                                    start: start_idx,
                                    end: start_idx + 1,
                                },
                            )
                        }
                        '-' => {
                            self.advance_char();
                            Token::new(
                                TokenKind::Minus,
                                Span {
                                    start: start_idx,
                                    end: start_idx + 1,
                                },
                            )
                        }
                        '<' => {
                            self.advance_char();
                            Token::new(
                                TokenKind::Less,
                                Span {
                                    start: start_idx,
                                    end: start_idx + 1,
                                },
                            )
                        }
                        ':' => {
                            self.advance_char();
                            Token::new(
                                TokenKind::Colon,
                                Span {
                                    start: start_idx,
                                    end: start_idx + 1,
                                },
                            )
                        }
                        '(' => {
                            self.advance_char();
                            Token::new(
                                TokenKind::LParen,
                                Span {
                                    start: start_idx,
                                    end: start_idx + 1,
                                },
                            )
                        }
                        ')' => {
                            self.advance_char();
                            Token::new(
                                TokenKind::RParen,
                                Span {
                                    start: start_idx,
                                    end: start_idx + 1,
                                },
                            )
                        }
                        '"' => self.read_string(start_idx)?,
                        c if c.is_alphabetic() || c == '_' => self.read_identifier(start_idx),
                        c if c.is_ascii_digit() => self.read_integer(start_idx)?,
                        _ => {
                            let (line, column) = self.line_column_for_index(start_idx);
                            return Err(anyhow!(
                                "Unexpected character '{}' at line {}, column {}",
                                ch,
                                line,
                                column
                            ));
                        }
                    };
                    return Ok(token);
                }
                LexerState::EmitPending => {
                    if let Some(token) = self.pending_tokens.pop() {
                        return Ok(token);
                    }
                    self.state = if self.peek_char().is_none() {
                        LexerState::EmitEof
                    } else {
                        LexerState::TokenStart
                    };
                }
                LexerState::EmitEof => {
                    let index = self.current_index();
                    return Ok(Token::new(
                        TokenKind::EOF,
                        Span {
                            start: index,
                            end: index,
                        },
                    ));
                }
            }
        }
    }

    fn count_indentation(&mut self) -> Result<usize> {
        let mut lookahead = self.pos;
        while let Some(c) = self.char_at(lookahead) {
            match c {
                ' ' => lookahead += 1,
                '\t' => {
                    let (line, column) = self.line_column_for_index(lookahead);
                    bail!(
                        "Tabs are not supported for indentation at line {}, column {}",
                        line,
                        column
                    );
                }
                '\n' => {
                    return Ok(*self.indent_stack.last().unwrap());
                }
                _ => break,
            }
        }

        let indent = lookahead - self.pos;
        self.pos = lookahead;
        Ok(indent)
    }

    fn skip_whitespace(&mut self) {
        self.consume_until(|c| c != ' ');
    }

    fn read_identifier(&mut self, start: usize) -> Token<'a> {
        self.consume_until(|c| !(c.is_alphanumeric() || c == '_'));
        let end_idx = self.current_index();

        let ident = &self.input[start..end_idx];
        let kind = match ident {
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "def" => TokenKind::Def,
            "return" => TokenKind::Return,
            "pass" => TokenKind::Pass,
            "True" => TokenKind::True,
            "False" => TokenKind::False,
            _ => TokenKind::Identifier(ident),
        };
        Token::new(
            kind,
            Span {
                start,
                end: end_idx,
            },
        )
    }

    fn read_integer(&mut self, start: usize) -> Result<Token<'a>> {
        self.consume_until(|c| !c.is_ascii_digit());
        let end_idx = self.current_index();

        let num_str = &self.input[start..end_idx];
        let (line, column) = self.line_column_for_index(start);
        let num = num_str.parse::<i64>().map_err(|_| {
            anyhow!("Invalid integer literal '{num_str}' at line {line}, column {column}")
        })?;
        Ok(Token::new(
            TokenKind::Integer(num),
            Span {
                start,
                end: end_idx,
            },
        ))
    }

    fn read_string(&mut self, start: usize) -> Result<Token<'a>> {
        self.advance_char(); // opening quote
        let content_start = self.current_index();
        let (line, column) = self.line_column_for_index(start);

        self.consume_until(|c| c == '"' || c == '\n');

        match self.peek_char() {
            Some('"') => {
                let content_end = self.current_index();
                self.advance_char(); // closing quote
                Ok(Token::new(
                    TokenKind::String(&self.input[content_start..content_end]),
                    Span {
                        start,
                        end: self.current_index(),
                    },
                ))
            }
            Some('\n') | None => {
                bail!("Unterminated string literal at line {line}, column {column}")
            }
            _ => unreachable!(),
        }
    }

    fn consume_until<P>(&mut self, stop_predicate: P) -> usize
    where
        P: Fn(char) -> bool,
    {
        while let Some(c) = self.peek_char() {
            if stop_predicate(c) {
                break;
            }
            self.advance_char();
        }
        self.pos
    }

    fn char_at(&self, index: usize) -> Option<char> {
        if index >= self.input.len() {
            None
        } else {
            self.input[index..].chars().next()
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.char_at(self.pos)
    }

    fn advance_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn current_index(&self) -> usize {
        self.pos
    }

    fn line_column_for_index(&self, index: usize) -> (usize, usize) {
        let clamped_index = index.min(self.input.len());
        let mut line = 1;
        let mut column = 0;

        for c in self.input[..clamped_index].chars() {
            if c == '\n' {
                line += 1;
                column = 0;
            } else {
                column += 1;
            }
        }

        (line, column)
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
