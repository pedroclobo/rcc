#[derive(Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str) -> Self {
        Self { kind, lexeme }
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}(\"{}\")", self.kind, self.lexeme)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Identifier,
    LParen,
    RParen,
    Void,
    Int,
    LBrace,
    RBrace,
    Return,
    Constant,
    Semicolon,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Increment,
    Decrement,
    Tilde,
    Ampersand,
    Pipe,
    Caret,
    LShift,
    RShift,
    Bang,
    And,
    Or,
    EqEq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
}
