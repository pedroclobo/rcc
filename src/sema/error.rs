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

    #[diagnostic(code(sema::break_outside_loop_or_switch))]
    #[error("'break' statement outside of loop or switch")]
    BreakOutsideLoopOrSwitch {
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::continue_outside_loop))]
    #[error("'continue' statement outside of loop")]
    ContinueOutsideLoop {
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::case_outside_switch))]
    #[error("'case' statement outside of switch")]
    CaseOutsideSwitch {
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::default_outside_switch))]
    #[error("'default' statement outside of switch")]
    DefaultOutsideSwitch {
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::duplicate_case))]
    #[error("Duplicate case value '{value}'")]
    DuplicateCase {
        value: String,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::invalid_case_expression))]
    #[error("Invalid case expression")]
    InvalidCaseExpression {
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::invalid_function_declaration))]
    #[error("Function '{func}' is defined outside of global context")]
    InvalidFunctionDeclaration {
        func: String,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::invalid_function_definition))]
    #[error("Function definitions aren't allowed in 'for' loop headers")]
    InvalidFunctionDefinition {
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::invalid_redeclaration))]
    #[error("Conflicting types for '{name}'")]
    InvalidRedeclaration {
        name: String,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::assignement_type_mismatch))]
    #[error("Invalid assignment of type '{rhs_ty}' to variable of type '{lhs_ty}'")]
    AssignmentTypeMismatch {
        lhs_ty: parser::Type,
        rhs_ty: parser::Type,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::unary_expression_type_mismatch))]
    #[error("Invalid unary expression with operand of type '{ty}'")]
    UnaryExpressionTypeMismatch {
        ty: parser::Type,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::binary_expression_type_mismatch))]
    #[error("Invalid binary expression with operands of type '{lhs_ty}' and '{rhs_ty}'")]
    BinaryExpressionTypeMismatch {
        lhs_ty: parser::Type,
        rhs_ty: parser::Type,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::switch_statement_type_mismatch))]
    #[error("Invalid switch statement with operand of type '{ty}'")]
    SwitchStatementTypeMismatch {
        ty: parser::Type,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::invalid_function_callable))]
    #[error("Invalid function call to '{name}'")]
    InvalidFunctionCallable {
        name: String,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::function_argument_count_mismatch))]
    #[error("Function '{name}' expects {expected} arguments, but {actual} were provided")]
    FunctionArgumentCountMismatch {
        name: String,
        expected: usize,
        actual: usize,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::function_argument_type_mismatch))]
    #[error(
        "Function '{name}' expects parameter with type '{expected_type}', but '{actual_type}' was provided"
    )]
    FunctionArgumentTypeMismatch {
        name: String,
        expected_type: parser::Type,
        actual_type: parser::Type,
        #[label]
        span: SourceSpan,
    },

    #[diagnostic(code(sema::invalid_redefinition))]
    #[error("Redefinition of '{name}' as different kind of symbol")]
    InvalidRedefinition {
        name: String,
        #[label]
        span: SourceSpan,
    },
}
