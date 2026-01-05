use super::{FunctionDefinition, Operand};
use std::collections::HashMap;

pub struct PseudoRegisterReplacer {
    offset: i32,
    operands: HashMap<String, Operand>,
}

impl Default for PseudoRegisterReplacer {
    fn default() -> Self {
        Self::new()
    }
}

impl PseudoRegisterReplacer {
    pub fn new() -> Self {
        Self {
            offset: 0,
            operands: HashMap::new(),
        }
    }

    pub fn run(&mut self, function: &mut FunctionDefinition) {
        self.collect_pseudo_registers(function);
        self.replace_pseudo_registers(function);
    }

    pub fn get_offset(&self) -> i32 {
        // Make sure the stack pointer is 16-byte aligned
        self.offset & -16
    }

    fn add_pseudo_register(&mut self, id: &str) {
        self.offset -= 4;
        self.operands.insert(
            id.to_string(),
            Operand::Stack {
                size: 4,
                offset: self.offset,
            },
        );
    }

    fn collect_pseudo_registers(&mut self, function: &mut FunctionDefinition) {
        for instruction in &function.body {
            for operand in instruction.operands() {
                if let Operand::PseudoReg(id) = operand
                    && !self.operands.contains_key(id)
                {
                    self.add_pseudo_register(id);
                }
            }
        }
    }

    fn replace_pseudo_registers(&mut self, function: &mut FunctionDefinition) {
        for instruction in &mut function.body {
            for operand in instruction.operands_mut() {
                if let Operand::PseudoReg(id) = operand {
                    *operand = self.operands.get(id).expect("Operand not found").clone();
                }
            }
        }
    }
}
