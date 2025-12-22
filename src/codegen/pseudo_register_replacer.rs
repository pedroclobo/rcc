use super::{Operand, Program};
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

    pub fn run(&mut self, program: &mut Program) {
        self.collect_pseudo_registers(program);
        self.replace_pseudo_registers(program);
    }

    pub fn get_offset(&self) -> i32 {
        self.offset
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

    fn collect_pseudo_registers(&mut self, program: &mut Program) {
        for function in &program.functions {
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
    }

    fn replace_pseudo_registers(&mut self, program: &mut Program) {
        for function in &mut program.functions {
            for instruction in &mut function.body {
                for operand in instruction.operands_mut() {
                    if let Operand::PseudoReg(id) = operand {
                        *operand = self.operands.get(id).expect("Operand not found").clone();
                    }
                }
            }
        }
    }
}
