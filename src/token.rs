#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
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
    Colon,  // :
    LParen, // (
    RParen, // )

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

    pub fn kind(&self) -> &TokenKind<'a> {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
