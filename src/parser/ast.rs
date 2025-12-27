use miette::SourceSpan;

use super::ParserError;
use crate::lexer::{Token, TokenKind};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Into<SourceSpan> for Span {
    fn into(self) -> SourceSpan {
        SourceSpan::new(self.start.into(), self.end - self.start)
    }
}

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<FunctionDefinition<'a>>,
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub body: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

impl PartialEq for BlockItem {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BlockItem::Decl(a), BlockItem::Decl(b)) => a == b,
            (BlockItem::Stmt(a), BlockItem::Stmt(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for BlockItem {}

#[derive(Debug)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

impl PartialEq for Decl {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Decl {}

#[derive(Debug, PartialEq, Eq)]
pub struct DeclKind {
    pub name: String,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl PartialEq for Stmt {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Stmt {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StmtKind {
    Return(Expr),
    Expr(Option<Expr>),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    BNot,
    Neg,
    Not,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BAnd,
    BOr,
    Xor,
    LShift,
    RShift,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

impl TryFrom<&Token<'_>> for BinaryOperator {
    type Error = ParserError;

    fn try_from(tok: &Token<'_>) -> Result<Self, Self::Error> {
        match tok.kind {
            TokenKind::Plus => Ok(BinaryOperator::Add),
            TokenKind::Minus => Ok(BinaryOperator::Sub),
            TokenKind::Mul => Ok(BinaryOperator::Mul),
            TokenKind::Div => Ok(BinaryOperator::Div),
            TokenKind::Mod => Ok(BinaryOperator::Mod),
            TokenKind::Ampersand => Ok(BinaryOperator::BAnd),
            TokenKind::Pipe => Ok(BinaryOperator::BOr),
            TokenKind::Caret => Ok(BinaryOperator::Xor),
            TokenKind::LShift => Ok(BinaryOperator::LShift),
            TokenKind::RShift => Ok(BinaryOperator::RShift),
            TokenKind::And => Ok(BinaryOperator::And),
            TokenKind::Or => Ok(BinaryOperator::Or),
            TokenKind::EqEq => Ok(BinaryOperator::Eq),
            TokenKind::Neq => Ok(BinaryOperator::Neq),
            TokenKind::Lt => Ok(BinaryOperator::Lt),
            TokenKind::Gt => Ok(BinaryOperator::Gt),
            TokenKind::Le => Ok(BinaryOperator::Le),
            TokenKind::Ge => Ok(BinaryOperator::Ge),
            _ => Err(ParserError::InvalidBinaryOperator {
                op: tok.kind,
                span: tok.span.into(),
            }),
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Mod => write!(f, "%"),
            BinaryOperator::BAnd => write!(f, "&"),
            BinaryOperator::BOr => write!(f, "|"),
            BinaryOperator::Xor => write!(f, "^"),
            BinaryOperator::LShift => write!(f, "<<"),
            BinaryOperator::RShift => write!(f, ">>"),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::Eq => write!(f, "=="),
            BinaryOperator::Neq => write!(f, "!="),
            BinaryOperator::Lt => write!(f, "<"),
            BinaryOperator::Gt => write!(f, ">"),
            BinaryOperator::Le => write!(f, "<="),
            BinaryOperator::Ge => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Expr {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExprKind {
    Constant(i32),
    Var(String),
    Unary(UnaryOperator, Box<Expr>),
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
}
