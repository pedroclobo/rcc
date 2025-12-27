use crate::parser::Span;

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub span: Span,
}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.lexeme == other.lexeme
    }
}

impl Eq for Token<'_> {}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str, span: Span) -> Self {
        Self { kind, lexeme, span }
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
    PlusEq,
    MinusEq,
    MulEq,
    DivEq,
    ModEq,
    AmpersandEq,
    PipeEq,
    CaretEq,
    LShiftEq,
    RShiftEq,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Identifier => "identifier",
                TokenKind::LParen => "'('",
                TokenKind::RParen => "')'",
                TokenKind::Void => "'void'",
                TokenKind::Int => "'int'",
                TokenKind::LBrace => "'{'",
                TokenKind::RBrace => "'}'",
                TokenKind::Return => "'return'",
                TokenKind::Constant => "constant",
                TokenKind::Semicolon => "';'",
                TokenKind::Plus => "'+'",
                TokenKind::Minus => "'-'",
                TokenKind::Mul => "'*'",
                TokenKind::Div => "'/'",
                TokenKind::Mod => "'%'",
                TokenKind::Increment => "'++'",
                TokenKind::Decrement => "'--'",
                TokenKind::Tilde => "'~'",
                TokenKind::Ampersand => "'&'",
                TokenKind::Pipe => "'|'",
                TokenKind::Caret => "'^'",
                TokenKind::LShift => "'<<'",
                TokenKind::RShift => "'>>'",
                TokenKind::Bang => "'!'",
                TokenKind::And => "'&&'",
                TokenKind::Or => "'||'",
                TokenKind::EqEq => "'=='",
                TokenKind::Neq => "'!='",
                TokenKind::Lt => "'<'",
                TokenKind::Gt => "'>'",
                TokenKind::Le => "'<='",
                TokenKind::Ge => "'>='",
                TokenKind::Eq => "'='",
                TokenKind::PlusEq => "'+='",
                TokenKind::MinusEq => "'-='",
                TokenKind::MulEq => "'*='",
                TokenKind::DivEq => "'/='",
                TokenKind::ModEq => "'%='",
                TokenKind::AmpersandEq => "'&='",
                TokenKind::PipeEq => "'|='",
                TokenKind::CaretEq => "'^='",
                TokenKind::LShiftEq => "'<<='",
                TokenKind::RShiftEq => "'>>='",
            }
        )
    }
}
