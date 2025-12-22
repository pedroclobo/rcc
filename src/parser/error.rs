use crate::lexer::{LexerError, TokenKind};

use std::{error::Error, num::ParseIntError};

#[derive(Debug)]
pub enum ParserError<'a> {
    NoMoreTokens,
    Expected(TokenKind, TokenKind),
    ExpectedAny(&'a [TokenKind], TokenKind),
    LexerError(LexerError<'a>),
    ParseIntError(ParseIntError),
    InvalidUnaryOperator(TokenKind),
    InvalidBinaryOperator(TokenKind),
}

impl std::fmt::Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::NoMoreTokens => write!(f, "All tokens were exhausted"),
            ParserError::Expected(expected, got) => {
                write!(f, "Expected {:?}, got {:?}", expected, got)
            }
            ParserError::LexerError(e) => e.fmt(f),
            ParserError::ParseIntError(e) => e.fmt(f),
            ParserError::ExpectedAny(expected, got) => {
                write!(f, "Expected any of {:?}, got {:?}", expected, got)
            }
            ParserError::InvalidUnaryOperator(op) => write!(f, "Invalid unary operator: {:?}", op),
            ParserError::InvalidBinaryOperator(op) => {
                write!(f, "Invalid binary operator: {:?}", op)
            }
        }
    }
}

impl Error for ParserError<'_> {}

impl<'a> From<LexerError<'a>> for ParserError<'a> {
    fn from(e: LexerError<'a>) -> Self {
        Self::LexerError(e)
    }
}

impl<'a> From<ParseIntError> for ParserError<'a> {
    fn from(e: ParseIntError) -> Self {
        Self::ParseIntError(e)
    }
}
