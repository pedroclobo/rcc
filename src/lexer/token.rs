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
    PlusPlus,
    MinusMinus,
    If,
    Else,
    QMark,
    Colon,
    Goto,
    Do,
    While,
    For,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Comma,
    Static,
    Extern,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;

        write!(
            f,
            "{}",
            match self {
                Identifier => "identifier",
                LParen => "'('",
                RParen => "')'",
                Void => "'void'",
                Int => "'int'",
                LBrace => "'{'",
                RBrace => "'}'",
                Return => "'return'",
                Constant => "constant",
                Semicolon => "';'",
                Plus => "'+'",
                Minus => "'-'",
                Mul => "'*'",
                Div => "'/'",
                Mod => "'%'",
                Tilde => "'~'",
                Ampersand => "'&'",
                Pipe => "'|'",
                Caret => "'^'",
                LShift => "'<<'",
                RShift => "'>>'",
                Bang => "'!'",
                And => "'&&'",
                Or => "'||'",
                EqEq => "'=='",
                Neq => "'!='",
                Lt => "'<'",
                Gt => "'>'",
                Le => "'<='",
                Ge => "'>='",
                Eq => "'='",
                PlusEq => "'+='",
                MinusEq => "'-='",
                MulEq => "'*='",
                DivEq => "'/='",
                ModEq => "'%='",
                AmpersandEq => "'&='",
                PipeEq => "'|='",
                CaretEq => "'^='",
                LShiftEq => "'<<='",
                RShiftEq => "'>>='",
                PlusPlus => "'++'",
                MinusMinus => "'--'",
                If => "'if'",
                Else => "'else'",
                QMark => "'?'",
                Colon => "':'",
                Goto => "'goto'",
                Do => "'do'",
                While => "'while'",
                For => "'for'",
                Break => "'break'",
                Continue => "'continue'",
                Switch => "'switch'",
                Case => "'case'",
                Default => "'default'",
                Comma => "','",
                Static => "'static'",
                Extern => "'extern'",
            }
        )
    }
}
