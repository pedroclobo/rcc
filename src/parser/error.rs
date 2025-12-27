use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::lexer::{LexerError, TokenKind};

use std::num::ParseIntError;

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ParserError {
    #[error(transparent)]
    LexerError(#[from] LexerError),

    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),

    #[diagnostic(code(parser::no_more_tokens))]
    #[error("Unexpected EOF")]
    NoMoreTokens {
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(parser::expected))]
    #[error("Expected {}, got {}", expected, got)]
    Expected {
        expected: TokenKind,
        got: TokenKind,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(parser::expected_expression))]
    #[error("Expected expression, got {}", got)]
    ExpectedExpression {
        got: TokenKind,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(parser::invalid_unary_operator))]
    #[error("Invalid unary operator: {}", op)]
    InvalidUnaryOperator {
        op: TokenKind,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(parser::invalid_binary_operator))]
    #[error("Invalid binary operator: {}", op)]
    InvalidBinaryOperator {
        op: TokenKind,
        #[label]
        span: SourceSpan,
    },
}
