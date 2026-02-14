use thiserror::Error;

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
