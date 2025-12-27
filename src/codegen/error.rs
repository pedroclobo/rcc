use miette::Diagnostic;
use thiserror::Error;

use crate::tacky;

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum X86EmitterError {
    #[diagnostic(code(rcc::codegen::no_program))]
    #[error("No program provided")]
    NoProgram,

    #[diagnostic(code(rcc::codegen::unsupported_binary_operator))]
    #[error("No matching binary operator for {op}")]
    UnsupportedBinaryOperator { op: tacky::BinaryOperator },

    #[diagnostic(code(rcc::codegen::no_matching_condition_code))]
    #[error("No matching condition code for {op}")]
    NoMatchingConditionCode { op: tacky::BinaryOperator },
}
