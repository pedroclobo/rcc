mod context_checker;
mod error;
mod instruction_collector;
mod label_resolver;
mod variable_resolver;

use context_checker::ContextChecker;
pub use error::SemaError;
pub use instruction_collector::InstructionCollector;
use label_resolver::LabelResolver;
use variable_resolver::VariableResolver;

use crate::parser;

pub struct Sema {}

impl Sema {
    pub fn new() -> Self {
        Sema {}
    }

    pub fn run(&mut self, program: &mut parser::Program) -> Result<(), SemaError> {
        let mut var_resolver = VariableResolver::new();
        let mut label_resolver = LabelResolver::new();
        let mut context_checker = ContextChecker::new();

        var_resolver.run(program)?;
        label_resolver.run(program)?;
        context_checker.run(program)?;

        Ok(())
    }
}

impl Default for Sema {
    fn default() -> Self {
        Sema::new()
    }
}
