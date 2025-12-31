mod error;
mod loop_labeler;
mod variable_resolver;

pub use error::SemaError;
use loop_labeler::LoopLabeler;
use variable_resolver::VariableResolver;

use crate::parser;

pub struct Sema {}

impl Sema {
    pub fn new() -> Self {
        Sema {}
    }

    pub fn run(&mut self, program: &mut parser::Program) -> Result<(), SemaError> {
        let mut var_resolver = VariableResolver::new();
        let mut loop_labeler = LoopLabeler::new();

        var_resolver.run(program)?;
        loop_labeler.run(program)?;

        Ok(())
    }
}

impl Default for Sema {
    fn default() -> Self {
        Sema::new()
    }
}
