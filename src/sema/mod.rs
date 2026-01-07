mod context_checker;
mod error;
mod instruction_collector;
mod label_resolver;
mod type_checker;
mod variable_resolver;

use context_checker::ContextChecker;
pub use error::SemaError;
pub use instruction_collector::InstructionCollector;
use label_resolver::LabelResolver;
pub use type_checker::{InitValue, Symbol, SymbolTable, TypeChecker};
pub use variable_resolver::VariableResolver;

use crate::parser;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Linkage {
    // No linkage
    None,
    // Internal linkage (static)
    Internal,
    // External linkage
    External,
}

pub struct Sema {
    pub symtab: SymbolTable,
}

impl Sema {
    pub fn new() -> Self {
        Sema {
            symtab: SymbolTable::new(),
        }
    }

    pub fn run(&mut self, program: &mut parser::Program) -> Result<(), SemaError> {
        let mut var_resolver = VariableResolver::new();
        let mut label_resolver = LabelResolver::new();
        let mut type_checker = TypeChecker::new(&mut self.symtab);
        let mut context_checker = ContextChecker::new();

        var_resolver.run(program)?;
        label_resolver.run(program)?;
        type_checker.run(program)?;
        context_checker.run(program)?;

        Ok(())
    }
}

impl Default for Sema {
    fn default() -> Self {
        Sema::new()
    }
}
