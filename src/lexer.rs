use thiserror::Error;

use crate::token::{Span, Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerState {
    LineBegin,
    TokenStart,
}

enum StepOutcome<'a> {
    Emit(Token<'a>),
    Continue,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum LexError {
    #[error("Invalid dedent to {indent_level} spaces at position {position}")]
    InvalidDedent {
        indent_level: usize,
        position: usize,
    },
    #[error("Unexpected character '{character}' at position {position}")]
    UnexpectedCharacter { character: char, position: usize },
    #[error("Tabs are not supported for indentation at position {position}")]
    TabIndentation { position: usize },
    #[error("Invalid integer literal '{literal}' at position {position}")]
    InvalidIntegerLiteral { literal: String, position: usize },
    #[error("Unterminated string literal at position {position}")]
    UnterminatedString { position: usize },
    #[error("Lexer invariant violated: {message}")]
    InvariantViolation { message: &'static str },
}

pub type LexResult<T> = Result<T, LexError>;

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

    pub fn next_token(&mut self) -> LexResult<Token<'a>> {
        loop {
            if let Some(token) = self.pending_tokens.pop() {
                return Ok(token);
            }

            match self.step_state()? {
                StepOutcome::Emit(token) => return Ok(token),
                StepOutcome::Continue => continue,
            }
        }
    }

    fn step_state(&mut self) -> LexResult<StepOutcome<'a>> {
        match self.state {
            LexerState::LineBegin => {
                // Compute indentation delta and produce Indent/Dedent tokens as needed.
                let indent_level = self.count_indentation()?;
                let current_indent = self.current_indent()?;
                let index = self.current_index();
                let span = Span {
                    start: index,
                    end: index,
                };

                if indent_level > current_indent {
                    self.indent_stack.push(indent_level);
                    self.state = LexerState::TokenStart;
                    return Ok(StepOutcome::Emit(Token::new(TokenKind::Indent, span)));
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
                    if self.current_indent()? != indent_level {
                        return Err(LexError::InvalidDedent {
                            indent_level,
                            position: index,
                        });
                    }
                    self.state = LexerState::TokenStart;
                    return Ok(StepOutcome::Continue);
                }

                self.state = LexerState::TokenStart;
                Ok(StepOutcome::Continue)
            }
            LexerState::TokenStart => {
                self.skip_whitespace();

                if self.peek_char().is_none() {
                    return self.handle_eof();
                }

                Ok(StepOutcome::Emit(self.read_token_from_current_position()?))
            }
        }
    }

    fn handle_eof(&mut self) -> LexResult<StepOutcome<'a>> {
        // At physical EOF, we must emit all pending Dedent tokens before EOF.
        self.flush_eof_dedents();
        if !self.pending_tokens.is_empty() {
            return Ok(StepOutcome::Continue);
        }

        let index = self.current_index();
        Ok(StepOutcome::Emit(Token::new(
            TokenKind::EOF,
            Span {
                start: index,
                end: index,
            },
        )))
    }

    fn count_indentation(&mut self) -> LexResult<usize> {
        let indentation = self.consume_while(|c| c == ' ');
        match self.peek_char() {
            Some('\t') => {
                return Err(LexError::TabIndentation {
                    position: self.current_index(),
                });
            }
            Some('\n') => {
                // Blank lines do not change indentation depth.
                return self.current_indent();
            }
            _ => {}
        }
        Ok(indentation)
    }

    fn skip_whitespace(&mut self) {
        self.consume_while(|c| c == ' ');
    }

    fn read_token_from_current_position(&mut self) -> LexResult<Token<'a>> {
        let start_idx = self.current_index();
        let ch = self
            .peek_char()
            .ok_or(LexError::InvariantViolation {
                message: "read_token_from_current_position called at EOF",
            })?;

        let token = match ch {
            '\n' => {
                self.consume_char();
                self.state = LexerState::LineBegin;
                Token::new(
                    TokenKind::Newline,
                    Span {
                        start: start_idx,
                        end: start_idx + 1,
                    },
                )
            }
            _ => {
                if let Some(token) = self.try_consume_single_char_token(ch, start_idx) {
                    token
                } else {
                    match ch {
                        '"' => self.read_string(start_idx)?,
                        c if c.is_alphabetic() || c == '_' => self.read_identifier(start_idx),
                        c if c.is_ascii_digit() => self.read_integer(start_idx)?,
                        _ => {
                            return Err(LexError::UnexpectedCharacter {
                                character: ch,
                                position: start_idx,
                            });
                        }
                    }
                }
            }
        };

        Ok(token)
    }

    fn try_consume_single_char_token(&mut self, ch: char, start: usize) -> Option<Token<'a>> {
        let kind = match ch {
            '=' => TokenKind::Equal,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '<' => TokenKind::Less,
            ':' => TokenKind::Colon,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            _ => return None,
        };

        self.consume_char();
        Some(Token::new(
            kind,
            Span {
                start,
                end: start + 1,
            },
        ))
    }

    fn read_identifier(&mut self, start: usize) -> Token<'a> {
        self.consume_while(|c| c.is_alphanumeric() || c == '_');
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

    fn read_integer(&mut self, start: usize) -> LexResult<Token<'a>> {
        self.consume_while(|c| c.is_ascii_digit());
        let end_idx = self.current_index();

        let num_str = &self.input[start..end_idx];
        let num = num_str.parse::<i64>().map_err(|_| LexError::InvalidIntegerLiteral {
            literal: num_str.to_string(),
            position: start,
        })?;
        Ok(Token::new(
            TokenKind::Integer(num),
            Span {
                start,
                end: end_idx,
            },
        ))
    }

    fn read_string(&mut self, start: usize) -> LexResult<Token<'a>> {
        self.consume_char(); // opening quote
        let content_start = self.current_index();

        self.consume_while(|c| c != '"' && c != '\n');

        match self.peek_char() {
            Some('"') => {
                let content_end = self.current_index();
                self.consume_char(); // closing quote
                Ok(Token::new(
                    TokenKind::String(&self.input[content_start..content_end]),
                    Span {
                        start,
                        end: self.current_index(),
                    },
                ))
            }
            Some('\n') | None => {
                Err(LexError::UnterminatedString { position: start })
            }
            _ => unreachable!(),
        }
    }

    fn consume_while<P>(&mut self, keep_predicate: P) -> usize
    where
        P: Fn(char) -> bool,
    {
        let start = self.pos;
        while let Some(c) = self.peek_char() {
            if !keep_predicate(c) {
                break;
            }
            self.consume_char();
        }
        self.pos - start
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

    fn consume_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn current_index(&self) -> usize {
        self.pos
    }

    fn current_indent(&self) -> LexResult<usize> {
        self.indent_stack
            .last()
            .copied()
            .ok_or(LexError::InvariantViolation {
                message: "indent stack is empty",
            })
    }

    fn flush_eof_dedents(&mut self) {
        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            let index = self.current_index();
            let span = Span {
                start: index,
                end: index,
            };
            self.pending_tokens.push(Token::new(TokenKind::Dedent, span));
        }
    }

}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) => Some(Ok(token)),
            Err(e) => Some(Err(e)),
        }
    }
}

pub fn tokenize<'a>(input: &'a str) -> LexResult<Vec<Token<'a>>> {
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
        let err = tokenize(indoc! {"
            x = 1 @ 2
        "})
        .expect_err("expected lexing failure");
        assert!(err.to_string().contains("Unexpected character '@'"));
    }

    #[test]
    fn errors_on_integer_overflow() {
        let err = tokenize(indoc! {"
            n = 99999999999999999999999999
        "})
        .expect_err("expected overflow");
        assert!(err.to_string().contains("Invalid integer literal"));
    }

    #[test]
    fn errors_on_tab_indentation() {
        let err = tokenize(indoc!("\tx = 1\n")).expect_err("expected tab indentation failure");
        assert_eq!(err, LexError::TabIndentation { position: 0 });
    }

    #[test]
    fn errors_on_inconsistent_dedent() {
        let input = indoc! {"
            if True:
                x = 1
              y = 2
        "};
        let err = tokenize(input).expect_err("expected inconsistent dedent failure");
        assert_eq!(
            err,
            LexError::InvalidDedent {
                indent_level: 2,
                position: 21
            }
        );
    }

    #[test]
    fn blank_line_does_not_change_indentation() {
        let input = indoc! {"
            if True:
                x = 1

                y = 2
        "};
        let tokens = tokenize(input).expect("tokenize should succeed");
        let kinds = tokens.into_iter().map(|token| token.kind).collect::<Vec<_>>();

        let expected = vec![
            TokenKind::If,
            TokenKind::True,
            TokenKind::Colon,
            TokenKind::Newline,
            TokenKind::Indent,
            TokenKind::Identifier("x"),
            TokenKind::Equal,
            TokenKind::Integer(1),
            TokenKind::Newline,
            TokenKind::Newline,
            TokenKind::Identifier("y"),
            TokenKind::Equal,
            TokenKind::Integer(2),
            TokenKind::Newline,
            TokenKind::Dedent,
            TokenKind::EOF,
        ];

        assert_eq!(kinds, expected);
    }

    #[test]
    fn emits_dedent_before_eof() {
        let input = indoc!("if True:\n    x = 1");
        let tokens = tokenize(input).expect("tokenize should succeed");
        let kinds = tokens.into_iter().map(|token| token.kind).collect::<Vec<_>>();

        let expected = vec![
            TokenKind::If,
            TokenKind::True,
            TokenKind::Colon,
            TokenKind::Newline,
            TokenKind::Indent,
            TokenKind::Identifier("x"),
            TokenKind::Equal,
            TokenKind::Integer(1),
            TokenKind::Dedent,
            TokenKind::EOF,
        ];

        assert_eq!(kinds, expected);
    }

    #[test]
    fn errors_on_unterminated_string() {
        let err = tokenize(indoc! {"
            x = \"abc
        "})
        .expect_err("expected unterminated string failure");
        assert_eq!(err, LexError::UnterminatedString { position: 4 });
    }
}
