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
    Tilde,
    Minus,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    LShift,
    RShift,
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
            TokenKind::Ampersand => Ok(BinaryOperator::And),
            TokenKind::Pipe => Ok(BinaryOperator::Or),
            TokenKind::Caret => Ok(BinaryOperator::Xor),
            TokenKind::LShift => Ok(BinaryOperator::LShift),
            TokenKind::RShift => Ok(BinaryOperator::RShift),
            _ => Err(ParserError::InvalidBinaryOperator(kind)),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}

pub trait AstVisitor<'a> {
    type Error;

    fn visit_program(&mut self, program: Program<'a>) -> Result<(), Self::Error>;
    fn visit_function_definition(
        &mut self,
        function_definition: FunctionDefinition<'a>,
    ) -> Result<(), Self::Error>;
    fn visit_statement(&mut self, statement: Statement) -> Result<(), Self::Error>;
    fn visit_expression(&mut self, expression: Expression) -> Result<(), Self::Error>;
}
