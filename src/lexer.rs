use std::{iter::Peekable, str::CharIndices};

use anyhow::{Result, anyhow, bail};

use crate::token::{Span, Token, TokenKind};

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    indent_stack: Vec<usize>,
    pending_tokens: Vec<Token<'a>>,
    at_line_start: bool,
    eof_reached: bool,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.char_indices().peekable(),
            indent_stack: vec![0],
            pending_tokens: Vec::new(),
            at_line_start: true,
            eof_reached: false,
            line: 1,
            column: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        if let Some(token) = self.pending_tokens.pop() {
            return Ok(token);
        }

        if self.eof_reached {
            let index = self.current_index();
            return Ok(Token::new(
                TokenKind::EOF,
                Span {
                    start: index,
                    end: index,
                    line: self.line,
                    column: self.column,
                },
            ));
        }

        if self.at_line_start {
            self.at_line_start = false;
            let indent_level = self.count_indentation()?;
            let current_indent = *self.indent_stack.last().unwrap();
            let index = self.current_index();
            let span = Span {
                start: index,
                end: index,
                line: self.line,
                column: self.column,
            };

            if indent_level > current_indent {
                self.indent_stack.push(indent_level);
                return Ok(Token::new(TokenKind::Indent, span));
            } else if indent_level < current_indent {
                while let Some(&top) = self.indent_stack.last() {
                    if top > indent_level {
                        self.indent_stack.pop();
                        self.pending_tokens
                            .push(Token::new(TokenKind::Dedent, span));
                    } else {
                        break;
                    }
                }
                if *self.indent_stack.last().unwrap() != indent_level {
                    bail!(
                        "Invalid dedent to {} spaces at line {}, column {}",
                        indent_level,
                        self.line,
                        self.column
                    );
                }
                // Determine if we need to return a token now (Dedent)
                if !self.pending_tokens.is_empty() {
                    let token = self.pending_tokens.pop().unwrap();
                    return Ok(token);
                }
            }
        }

        self.skip_whitespace();

        let (start_idx, ch) = match self.chars.peek() {
            Some(&(idx, c)) => (idx, c),
            None => {
                self.eof_reached = true;
                // Handle remaining dedents at EOF
                while self.indent_stack.len() > 1 {
                    self.indent_stack.pop();
                    let index = self.current_index();
                    let span = Span {
                        start: index,
                        end: index,
                        line: self.line,
                        column: self.column,
                    };
                    self.pending_tokens
                        .push(Token::new(TokenKind::Dedent, span));
                }
                if !self.pending_tokens.is_empty() {
                    return Ok(self.pending_tokens.pop().unwrap());
                }
                let index = self.current_index();
                return Ok(Token::new(
                    TokenKind::EOF,
                    Span {
                        start: index,
                        end: index,
                        line: self.line,
                        column: self.column,
                    },
                ));
            }
        };

        let start_line = self.line;
        let start_column = self.column;
        match ch {
            '\n' => Ok({
                self.advance_char();
                self.at_line_start = true;
                Token::new(
                    TokenKind::Newline,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                        line: start_line,
                        column: start_column,
                    },
                )
            }),
            '=' => Ok({
                self.advance_char();
                Token::new(
                    TokenKind::Equal,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                        line: start_line,
                        column: start_column,
                    },
                )
            }),
            '+' => Ok({
                self.advance_char();
                Token::new(
                    TokenKind::Plus,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                        line: start_line,
                        column: start_column,
                    },
                )
            }),
            '-' => Ok({
                self.advance_char();
                Token::new(
                    TokenKind::Minus,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                        line: start_line,
                        column: start_column,
                    },
                )
            }),
            '<' => Ok({
                self.advance_char();
                Token::new(
                    TokenKind::Less,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                        line: start_line,
                        column: start_column,
                    },
                )
            }),
            ':' => Ok({
                self.advance_char();
                Token::new(
                    TokenKind::Colon,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                        line: start_line,
                        column: start_column,
                    },
                )
            }),
            '(' => Ok({
                self.advance_char();
                Token::new(
                    TokenKind::LParen,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                        line: start_line,
                        column: start_column,
                    },
                )
            }),
            ')' => Ok({
                self.advance_char();
                Token::new(
                    TokenKind::RParen,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                        line: start_line,
                        column: start_column,
                    },
                )
            }),
            '"' => self.read_string(start_idx, start_line, start_column),
            c if c.is_alphabetic() || c == '_' => {
                Ok(self.read_identifier(start_idx, start_line, start_column))
            }
            c if c.is_ascii_digit() => self.read_integer(start_idx, start_line, start_column),
            _ => Err(anyhow!(
                "Unexpected character '{}' at line {}, column {}",
                ch,
                start_line,
                start_column
            )),
        }
    }

    fn count_indentation(&mut self) -> Result<usize> {
        let mut count = 0;

        // Use clone to look ahead for empty lines check
        let mut temp_chars = self.chars.clone();
        let mut is_empty_line = false;

        while let Some(&(_, c)) = temp_chars.peek() {
            if c == ' ' {
                temp_chars.next();
            } else if c == '\t' {
                bail!(
                    "Tabs are not supported for indentation at line {}, column {}",
                    self.line,
                    self.column
                );
            } else if c == '\n' {
                is_empty_line = true;
                break;
            } else {
                break;
            }
        }

        if is_empty_line {
            // Return current indentation to avoid generating Indent/Dedent tokens
            return Ok(*self.indent_stack.last().unwrap());
        }

        while let Some(&(_, c)) = self.chars.peek() {
            if c == ' ' {
                self.advance_char();
                count += 1;
            } else {
                break;
            }
        }

        Ok(count)
    }

    fn skip_whitespace(&mut self) {
        while let Some(&(_, c)) = self.chars.peek() {
            if c == ' ' {
                self.advance_char();
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self, start: usize, line: usize, column: usize) -> Token<'a> {
        self.advance_char(); // Consume first char
        while let Some(&(_, c)) = self.chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance_char();
            } else {
                break;
            }
        }

        let end_idx = match self.chars.peek() {
            Some(&(idx, _)) => idx,
            None => self.input.len(),
        };

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
                line,
                column,
            },
        )
    }

    fn read_integer(&mut self, start: usize, line: usize, column: usize) -> Result<Token<'a>> {
        self.advance_char(); // Consume first digit
        while let Some(&(_, c)) = self.chars.peek() {
            if c.is_ascii_digit() {
                self.advance_char();
            } else {
                break;
            }
        }

        let end_idx = match self.chars.peek() {
            Some(&(idx, _)) => idx,
            None => self.input.len(),
        };

        let num_str = &self.input[start..end_idx];
        let num = num_str.parse::<i64>().map_err(|_| {
            anyhow!("Invalid integer literal '{num_str}' at line {line}, column {column}")
        })?;
        Ok(Token::new(
            TokenKind::Integer(num),
            Span {
                start,
                end: end_idx,
                line,
                column,
            },
        ))
    }

    fn read_string(&mut self, start: usize, line: usize, column: usize) -> Result<Token<'a>> {
        self.advance_char(); // Consume opening quote
        let content_start = (start + 1).min(self.input.len());
        while let Some(&(idx, c)) = self.chars.peek() {
            if c == '"' {
                let content_end = idx;
                self.advance_char(); // Consume closing quote
                return Ok(Token::new(
                    TokenKind::String(&self.input[content_start..content_end]),
                    Span {
                        start,
                        end: idx + 1,
                        line,
                        column,
                    },
                ));
            }
            if c == '\n' {
                bail!("Unterminated string literal at line {line}, column {column}");
            }
            self.advance_char();
        }
        bail!("Unterminated string literal at line {line}, column {column}");
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

impl<'a> Lexer<'a> {
    fn advance_char(&mut self) -> Option<(usize, char)> {
        let next = self.chars.next();
        if let Some((_, c)) = next {
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }
        next
    }

    fn current_index(&mut self) -> usize {
        self.chars
            .peek()
            .map(|(idx, _)| *idx)
            .unwrap_or(self.input.len())
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
