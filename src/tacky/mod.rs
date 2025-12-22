mod domain;
mod error;
mod tacky_emitter;

pub use domain::{BinaryOperator, FunctionDefinition, Instruction, Program, UnaryOperator, Value};
pub use error::TackyError;
pub use tacky_emitter::TackyEmitter;
