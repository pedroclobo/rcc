use miette::Diagnostic;
use thiserror::Error;

use crate::parser;

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum TackyError {
    #[error("No matching binary operator for '{op}'")]
    UnsupportedBinaryOperator { op: parser::BinaryOperator },
}
