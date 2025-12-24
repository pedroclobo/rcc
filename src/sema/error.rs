#[derive(Debug)]
pub enum SemaError {
    UndeclaredVariable(String),
    DuplicateDeclaration(String),
}

impl std::fmt::Display for SemaError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SemaError::UndeclaredVariable(name) => write!(f, "Variable '{}' not declared", name),
            SemaError::DuplicateDeclaration(name) => {
                write!(f, "Duplicate declaration of variable '{}'", name)
            }
        }
    }
}

impl std::error::Error for SemaError {}
