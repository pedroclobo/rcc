use crate::parser;

#[derive(Debug)]
pub enum TackyError {
    UnsupportedBinaryOperator(parser::BinaryOperator),
}

impl std::fmt::Display for TackyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TackyError::UnsupportedBinaryOperator(op) => {
                write!(f, "No matching binary operator for {}", op)
            }
        }
    }
}
