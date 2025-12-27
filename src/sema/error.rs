use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::parser;

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum SemaError {
    #[diagnostic(code(sema::undeclared_variable))]
    #[error("Variable '{var}' not declared")]
    UndeclaredVariable {
        var: String,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::invalid_assignment_target))]
    #[error("Invalid assignment target")]
    InvalidAssignmentTarget {
        expr: parser::Expr,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::duplicate_declaration))]
    #[error("Duplicate declaration of variable '{var}'")]
    DuplicateDeclaration {
        var: String,
        #[label]
        span: SourceSpan,
    },
}
