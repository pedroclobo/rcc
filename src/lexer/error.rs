use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum LexerError {
    #[diagnostic(code(lexer::invalid_char))]
    #[error("Invalid char '{char}'")]
    InvalidChar {
        char: char,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(lexer::invalid_integer_literal))]
    #[error("Invalid integer literal '{constant}'")]
    InvalidIntegerLiteral {
        constant: String,
        #[label]
        span: SourceSpan,
    },
}
