use miette::Diagnostic;
use thiserror::Error;

use crate::parser;

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum TackyError {
    #[error("No matching unary operator for '{op}'")]
    UnsupportedUnaryOperator { op: parser::UnaryOperator },

    #[error("No matching binary operator for '{op}'")]
    UnsupportedBinaryOperator { op: parser::BinaryOperator },
}
