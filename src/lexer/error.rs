use std::error::Error;

#[derive(Debug, Clone, Copy)]
pub enum LexerError<'a> {
    InvalidChar(char),
    InvalidConstant(&'a str),
    InvalidSequence(&'a str),
}

impl std::fmt::Display for LexerError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::InvalidChar(c) => write!(f, "Invalid char: {}", c),
            LexerError::InvalidConstant(c) => write!(f, "Invalid constant literal: {}", c),
            LexerError::InvalidSequence(s) => write!(f, "Invalid character sequence: {}", s),
        }
    }
}

impl Error for LexerError<'_> {}
