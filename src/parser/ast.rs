use std::collections::VecDeque;

use miette::SourceSpan;

use super::ParserError;
use crate::lexer::{Token, TokenKind};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Span> for SourceSpan {
    fn from(val: Span) -> Self {
        SourceSpan::new(val.start.into(), val.end - val.start)
    }
}

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: VecDeque<BlockItem>,
    pub span: Span,
}

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}

impl Eq for Block {}

impl IntoIterator for Block {
    type Item = BlockItem;
    type IntoIter = std::collections::vec_deque::IntoIter<BlockItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> IntoIterator for &'a Block {
    type Item = &'a BlockItem;
    type IntoIter = std::collections::vec_deque::Iter<'a, BlockItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<'a> IntoIterator for &'a mut Block {
    type Item = &'a mut BlockItem;
    type IntoIter = std::collections::vec_deque::IterMut<'a, BlockItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter_mut()
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
    pub storage: Option<StorageClass>,
}

impl PartialEq for Decl {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Decl {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DeclKind {
    VarDecl {
        name: String,
        initializer: Option<Expr>,
    },
    FunDecl {
        name: String,
        params: Vec<Param>,
        body: Option<Block>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int,
    FunType { ret: Box<Type>, params: Vec<Type> },
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::FunType { ret, params } => {
                write!(
                    f,
                    "({})",
                    params
                        .iter()
                        .map(|ty| ty.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
                write!(f, " -> {}", ret)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

impl PartialEq for Param {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

impl Eq for Param {}

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
    If {
        cond: Expr,
        then: Box<Stmt>,
        r#else: Option<Box<Stmt>>,
    },
    Labeled {
        label: Label,
        stmt: Box<Stmt>,
    },
    Goto(Label),
    Block(Block),
    Break,
    Continue,
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
    DoWhile {
        body: Box<Stmt>,
        cond: Expr,
    },
    For {
        init: ForInit,
        cond: Option<Expr>,
        post: Option<Expr>,
        body: Box<Stmt>,
    },
    Switch {
        expr: Expr,
        body: Box<Stmt>,
    },
    Case {
        expr: Expr,
        body: Box<Stmt>,
    },
    Default {
        body: Box<Stmt>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ForInit {
    Decl(Decl),
    Expr(Option<Expr>),
}

#[derive(Debug, Clone)]
pub struct Label {
    pub name: String,
    pub span: Span,
}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Label {}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    BNot,
    Neg,
    Not,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

impl TryFrom<&Token<'_>> for UnaryOperator {
    type Error = ParserError;

    fn try_from(tok: &Token<'_>) -> Result<Self, Self::Error> {
        use TokenKind::*;

        match tok.kind {
            Minus => Ok(UnaryOperator::Neg),
            Tilde => Ok(UnaryOperator::BNot),
            Bang => Ok(UnaryOperator::Not),
            MinusMinus => Ok(UnaryOperator::PreDec),
            PlusPlus => Ok(UnaryOperator::PreInc),
            _ => Err(ParserError::InvalidUnaryOperator {
                op: tok.kind,
                span: tok.span.into(),
            }),
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::BNot => write!(f, "~"),
            UnaryOperator::Neg => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
            UnaryOperator::PreInc => write!(f, "++"),
            UnaryOperator::PreDec => write!(f, "--"),
            UnaryOperator::PostInc => write!(f, "++"),
            UnaryOperator::PostDec => write!(f, "--"),
        }
    }
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
        use TokenKind::*;

        match tok.kind {
            Plus | PlusEq => Ok(BinaryOperator::Add),
            Minus | MinusEq => Ok(BinaryOperator::Sub),
            Mul | MulEq => Ok(BinaryOperator::Mul),
            Div | DivEq => Ok(BinaryOperator::Div),
            Mod | ModEq => Ok(BinaryOperator::Mod),
            Ampersand | AmpersandEq => Ok(BinaryOperator::BAnd),
            Pipe | PipeEq => Ok(BinaryOperator::BOr),
            Caret | CaretEq => Ok(BinaryOperator::Xor),
            LShift | LShiftEq => Ok(BinaryOperator::LShift),
            RShift | RShiftEq => Ok(BinaryOperator::RShift),
            And => Ok(BinaryOperator::And),
            Or => Ok(BinaryOperator::Or),
            EqEq => Ok(BinaryOperator::Eq),
            Neq => Ok(BinaryOperator::Neq),
            Lt => Ok(BinaryOperator::Lt),
            Gt => Ok(BinaryOperator::Gt),
            Le => Ok(BinaryOperator::Le),
            Ge => Ok(BinaryOperator::Ge),
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
    Conditional {
        cond: Box<Expr>,
        then: Box<Expr>,
        r#else: Box<Expr>,
    },
    FunctionCall {
        identifier: Box<Expr>,
        arguments: Vec<Expr>,
    },
}
