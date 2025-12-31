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

    #[diagnostic(code(sema::duplicate_variable_declaration))]
    #[error("Duplicate declaration of variable '{var}'")]
    DuplicateVariableDeclaration {
        var: String,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::undeclared_label))]
    #[error("Label '{label}' not declared")]
    UndeclaredLabel {
        label: String,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::duplicate_label_declaration))]
    #[error("Duplicate declaration of label '{label}'")]
    DuplicateLabelDeclaration {
        label: String,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::break_outside_loop))]
    #[error("'break' statement outside of loop")]
    BreakOutsideLoop {
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::continue_outside_loop))]
    #[error("'continue' statement outside of loop")]
    ContinueOutsideLoop {
        #[label]
        span: SourceSpan,
    },
}
