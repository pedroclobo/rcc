use std::collections::HashMap;

use crate::codegen::FunctionDefinition;

use super::{BinaryOperator, Instruction, Operand, Program, PseudoRegisterReplacer, Register};

pub struct InstructionFixer {
    instructions: HashMap<String, Vec<Instruction>>,
}

impl Default for InstructionFixer {
    fn default() -> Self {
        Self::new()
    }
}

impl InstructionFixer {
    pub fn new() -> Self {
        Self {
            instructions: HashMap::new(),
        }
    }

    pub fn run(&mut self, program: &mut Program<'_>) {
        for function in &mut program.functions {
            let mut replacer = PseudoRegisterReplacer::new();
            replacer.run(function);
            self.collect_instructions(function, -replacer.get_offset());
            self.assign_instructions(function);
        }
    }

    fn collect_instructions(&mut self, function: &FunctionDefinition<'_>, offset: i32) {
        self.instructions
            .insert(function.name.to_string(), Vec::new());
        let instructions = self
            .instructions
            .get_mut(function.name)
            .expect("Function not found");

        // Preamble
        instructions.extend(vec![
            Instruction::Push(Operand::Reg(Register::Rbp)),
            Instruction::Mov(Operand::Reg(Register::Rsp), Operand::Reg(Register::Rbp)),
            Instruction::Binary(
                BinaryOperator::Sub,
                Operand::Imm(offset),
                Operand::Reg(Register::Rsp),
            ),
        ]);

        for instruction in &function.body {
            instructions.extend(match instruction {
                Instruction::Mov(
                    Operand::Stack {
                        size: src_size,
                        offset: src_offset,
                    },
                    Operand::Stack {
                        size: dst_size,
                        offset: dst_offset,
                    },
                ) => {
                    vec![
                        Instruction::Mov(
                            Operand::Stack {
                                size: *src_size,
                                offset: *src_offset,
                            },
                            Operand::Reg(if *src_size == 8 {
                                Register::R10
                            } else {
                                Register::R10d
                            }),
                        ),
                        Instruction::Mov(
                            Operand::Reg(if *dst_size == 8 {
                                Register::R10
                            } else {
                                Register::R10d
                            }),
                            Operand::Stack {
                                size: *dst_size,
                                offset: *dst_offset,
                            },
                        ),
                    ]
                }
                Instruction::Cmp(
                    Operand::Stack {
                        size: src_size,
                        offset: src_offset,
                    },
                    Operand::Stack {
                        size: dst_size,
                        offset: dst_offset,
                    },
                ) => {
                    vec![
                        Instruction::Mov(
                            Operand::Stack {
                                size: *src_size,
                                offset: *src_offset,
                            },
                            Operand::Reg(Register::R10d),
                        ),
                        Instruction::Cmp(
                            Operand::Reg(Register::R10d),
                            Operand::Stack {
                                size: *dst_size,
                                offset: *dst_offset,
                            },
                        ),
                    ]
                }
                Instruction::Ret => {
                    vec![
                        Instruction::Mov(Operand::Reg(Register::Rbp), Operand::Reg(Register::Rsp)),
                        Instruction::Pop(Operand::Reg(Register::Rbp)),
                        Instruction::Ret,
                    ]
                }
                Instruction::Binary(
                    op @ (BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::And
                    | BinaryOperator::Or
                    | BinaryOperator::Xor),
                    Operand::Stack {
                        size: lhs_size,
                        offset: lhs_offset,
                    },
                    Operand::Stack {
                        size: rhs_size,
                        offset: rhs_offset,
                    },
                ) => {
                    vec![
                        Instruction::Mov(
                            Operand::Stack {
                                size: *lhs_size,
                                offset: *lhs_offset,
                            },
                            Operand::Reg(Register::R10d),
                        ),
                        Instruction::Binary(
                            *op,
                            Operand::Reg(Register::R10d),
                            Operand::Stack {
                                size: *rhs_size,
                                offset: *rhs_offset,
                            },
                        ),
                    ]
                }
                Instruction::Binary(
                    BinaryOperator::Mul,
                    lhs,
                    Operand::Stack {
                        size: rhs_size,
                        offset: rhs_offset,
                    },
                ) => {
                    vec![
                        Instruction::Mov(
                            Operand::Stack {
                                size: *rhs_size,
                                offset: *rhs_offset,
                            },
                            Operand::Reg(Register::R11d),
                        ),
                        Instruction::Binary(
                            BinaryOperator::Mul,
                            lhs.clone(),
                            Operand::Reg(Register::R11d),
                        ),
                        Instruction::Mov(
                            Operand::Reg(Register::R11d),
                            Operand::Stack {
                                size: *rhs_size,
                                offset: *rhs_offset,
                            },
                        ),
                    ]
                }
                Instruction::Idiv(Operand::Imm(n)) => {
                    vec![
                        Instruction::Mov(Operand::Imm(*n), Operand::Reg(Register::R10d)),
                        Instruction::Idiv(Operand::Reg(Register::R10d)),
                    ]
                }
                Instruction::Cmp(lhs, Operand::Imm(n)) => {
                    vec![
                        Instruction::Mov(Operand::Imm(*n), Operand::Reg(Register::R11d)),
                        Instruction::Cmp(lhs.clone(), Operand::Reg(Register::R11d)),
                    ]
                }
                Instruction::SetCC(cc, Operand::Stack { size: _, offset }) => {
                    vec![Instruction::SetCC(
                        *cc,
                        Operand::Stack {
                            size: 1,
                            offset: *offset,
                        },
                    )]
                }
                Instruction::SetCC(cc, Operand::Reg(reg)) => {
                    vec![Instruction::SetCC(
                        *cc,
                        match reg {
                            Register::Eax => Operand::Reg(Register::Al),
                            Register::Edx => Operand::Reg(Register::Dl),
                            Register::R10d => Operand::Reg(Register::R10b),
                            Register::R11d => Operand::Reg(Register::R11b),
                            reg => Operand::Reg(*reg),
                        },
                    )]
                }
                _ => {
                    vec![instruction.clone()]
                }
            })
        }
    }

    fn assign_instructions(&mut self, function: &mut FunctionDefinition) {
        function.assign_body(self.instructions.get(function.name).unwrap());
    }
}
