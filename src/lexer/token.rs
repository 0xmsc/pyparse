#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'a> {
    Identifier(&'a str),
    Integer(i64),
    String(&'a str),
    True,
    False,

    // Keywords
    If,
    Else,
    While,
    Def,
    Return,
    Pass,

    // Operators
    Equal, // =
    Plus,  // +
    Minus, // -
    Less,  // <

    // Delimiters
    Colon,    // :
    Comma,    // ,
    Dot,      // .
    LParen,   // (
    RParen,   // )
    LBracket, // [
    RBracket, // ]

    // Structural
    Newline,
    Indent,
    Dedent,
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}
