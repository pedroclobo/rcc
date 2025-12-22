use crate::{lexer::TokenKind, parser::ParserError};

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<FunctionDefinition<'a>>,
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub body: Statement,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Return(Expression),
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

impl TryFrom<TokenKind> for BinaryOperator {
    type Error = ParserError<'static>;

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        match kind {
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
            _ => Err(ParserError::InvalidBinaryOperator(kind)),
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}
