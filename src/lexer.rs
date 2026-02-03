use std::{iter::Peekable, str::CharIndices};

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Identifier(&'a str),
    Integer(i64),

    // Keywords
    If,
    Else,
    Def,
    Return,
    Pass,

    // Operators
    Equal, // =
    Plus,  // +
    Minus, // -

    // Delimiters
    Colon,  // :
    LParen, // (
    RParen, // )

    // Structural
    Newline,
    Indent,
    Dedent,
    EOF,
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    indent_stack: Vec<usize>,
    pending_tokens: Vec<Token<'a>>,
    at_line_start: bool,
    eof_reached: bool,
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
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        if let Some(token) = self.pending_tokens.pop() {
            return token;
        }

        if self.eof_reached {
            return Token::EOF;
        }

        if self.at_line_start {
            self.at_line_start = false;
            let indent_level = self.count_indentation();
            let current_indent = *self.indent_stack.last().unwrap();

            if indent_level > current_indent {
                self.indent_stack.push(indent_level);
                return Token::Indent;
            } else if indent_level < current_indent {
                while let Some(&top) = self.indent_stack.last() {
                    if top > indent_level {
                        self.indent_stack.pop();
                        self.pending_tokens.push(Token::Dedent);
                    } else {
                        break;
                    }
                }
                // Determine if we need to return a token now (Dedent)
                if !self.pending_tokens.is_empty() {
                    let token = self.pending_tokens.pop().unwrap();
                    return token;
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
                    self.pending_tokens.push(Token::Dedent);
                }
                if !self.pending_tokens.is_empty() {
                    return self.pending_tokens.pop().unwrap();
                }
                return Token::EOF;
            }
        };

        match ch {
            '\n' => {
                self.chars.next();
                self.at_line_start = true;
                Token::Newline
            }
            '=' => {
                self.chars.next();
                Token::Equal
            }
            '+' => {
                self.chars.next();
                Token::Plus
            }
            '-' => {
                self.chars.next();
                Token::Minus
            }
            ':' => {
                self.chars.next();
                Token::Colon
            }
            '(' => {
                self.chars.next();
                Token::LParen
            }
            ')' => {
                self.chars.next();
                Token::RParen
            }
            c if c.is_alphabetic() || c == '_' => self.read_identifier(start_idx),
            c if c.is_digit(10) => self.read_integer(start_idx),
            _ => {
                // Unexpected char, skip
                self.chars.next();
                self.next_token()
            }
        }
    }

    fn count_indentation(&mut self) -> usize {
        let mut count = 0;

        // Use clone to look ahead for empty lines check
        let mut temp_chars = self.chars.clone();
        let mut is_empty_line = false;

        while let Some(&(_, c)) = temp_chars.peek() {
            if c == ' ' {
                temp_chars.next();
            } else if c == '\n' {
                is_empty_line = true;
                break;
            } else {
                break;
            }
        }

        if is_empty_line {
            // Return current indentation to avoid generating Indent/Dedent tokens
            return *self.indent_stack.last().unwrap();
        }

        while let Some(&(_, c)) = self.chars.peek() {
            if c == ' ' {
                self.chars.next();
                count += 1;
            } else {
                break;
            }
        }

        count
    }

    fn skip_whitespace(&mut self) {
        while let Some(&(_, c)) = self.chars.peek() {
            if c == ' ' {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self, start: usize) -> Token<'a> {
        self.chars.next(); // Consume first char
        while let Some(&(_, c)) = self.chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.chars.next();
            } else {
                break;
            }
        }

        let end_idx = match self.chars.peek() {
            Some(&(idx, _)) => idx,
            None => self.input.len(),
        };

        let ident = &self.input[start..end_idx];
        match ident {
            "if" => Token::If,
            "else" => Token::Else,
            "def" => Token::Def,
            "return" => Token::Return,
            "pass" => Token::Pass,
            _ => Token::Identifier(ident),
        }
    }

    fn read_integer(&mut self, start: usize) -> Token<'a> {
        self.chars.next(); // Consume first digit
        while let Some(&(_, c)) = self.chars.peek() {
            if c.is_digit(10) {
                self.chars.next();
            } else {
                break;
            }
        }

        let end_idx = match self.chars.peek() {
            Some(&(idx, _)) => idx,
            None => self.input.len(),
        };

        let num_str = &self.input[start..end_idx];
        let num = num_str.parse::<i64>().unwrap_or(0);
        Token::Integer(num)
    }
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
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Def);
        assert_eq!(lexer.next_token(), Token::Identifier("fn"));
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::Colon);
        assert_eq!(lexer.next_token(), Token::Newline);

        assert_eq!(lexer.next_token(), Token::Indent);

        assert_eq!(lexer.next_token(), Token::Identifier("n"));
        assert_eq!(lexer.next_token(), Token::Equal);
        assert_eq!(lexer.next_token(), Token::Integer(4));
        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Integer(4));
        assert_eq!(lexer.next_token(), Token::Newline);

        assert_eq!(lexer.next_token(), Token::Identifier("print"));
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Identifier("n"));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::Newline);

        assert_eq!(lexer.next_token(), Token::Dedent);

        assert_eq!(lexer.next_token(), Token::Identifier("fn"));
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::Newline);

        assert_eq!(lexer.next_token(), Token::EOF);
    }
}
