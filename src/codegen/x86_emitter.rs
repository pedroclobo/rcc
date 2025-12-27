use super::{
    BinaryOperator, ConditionCode, FunctionDefinition, Instruction, InstructionFixer, Operand,
    Program, Register, X86EmitterError,
};
use crate::tacky;

pub struct X86Emitter<'a> {
    program: Option<Program<'a>>,
    function: Option<FunctionDefinition<'a>>,

    instructions: Vec<Instruction>,
}

impl Default for X86Emitter<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> X86Emitter<'a> {
    pub fn new() -> Self {
        X86Emitter {
            program: None,
            function: None,
            instructions: Vec::new(),
        }
    }

    pub fn get_program(&mut self) -> Option<Program<'_>> {
        if let Some(mut prog) = self.program.take() {
            let mut instruction_fixer = InstructionFixer::new();
            instruction_fixer.run(&mut prog);
            Some(prog)
        } else {
            None
        }
    }

    pub fn visit_program(&mut self, program: tacky::Program<'a>) -> Result<(), X86EmitterError> {
        let mut functions = Vec::new();

        for function in program.functions {
            self.visit_function_definition(function)?;
            if let Some(function) = self.function.take() {
                functions.push(function);
            }
        }

        self.program = Some(Program { functions });

        Ok(())
    }

    fn visit_function_definition(
        &mut self,
        function_definition: crate::tacky::FunctionDefinition<'a>,
    ) -> Result<(), X86EmitterError> {
        for instruction in function_definition.body {
            self.visit_instruction(instruction)?;
        }

        self.function = Some(FunctionDefinition {
            name: function_definition.name,
            body: self.instructions.drain(..).collect(),
        });

        Ok(())
    }

    fn visit_instruction(&mut self, statement: tacky::Instruction) -> Result<(), X86EmitterError> {
        self.instructions.extend(match statement {
            tacky::Instruction::Return(value) => {
                vec![
                    Instruction::Mov(value.into(), Operand::Reg(Register::Eax)),
                    Instruction::Ret,
                ]
            }
            tacky::Instruction::Unary(op, src, dst) => match op {
                tacky::UnaryOperator::BNot | tacky::UnaryOperator::Neg => {
                    vec![
                        Instruction::Mov((*src).into(), (*dst.clone()).into()),
                        Instruction::Unary(op.into(), (*dst).into()),
                    ]
                }
                tacky::UnaryOperator::Not => {
                    vec![
                        Instruction::Cmp(Operand::Imm(0), (*src).into()),
                        Instruction::Mov(Operand::Imm(0), (*dst).clone().into()),
                        Instruction::SetCC(ConditionCode::Eq, (*dst).into()),
                    ]
                }
            },
            tacky::Instruction::Binary(op, lhs, rhs, dst) => match op {
                tacky::BinaryOperator::Add
                | tacky::BinaryOperator::Sub
                | tacky::BinaryOperator::Mul
                | tacky::BinaryOperator::BAnd
                | tacky::BinaryOperator::BOr
                | tacky::BinaryOperator::Xor => {
                    vec![
                        Instruction::Mov((*lhs).into(), (*dst.clone()).into()),
                        Instruction::Binary(op.try_into()?, (*rhs).into(), (*dst).into()),
                    ]
                }
                tacky::BinaryOperator::Div | tacky::BinaryOperator::Mod => {
                    vec![
                        Instruction::Mov((*lhs).into(), Operand::Reg(Register::Eax)),
                        Instruction::Cdq,
                        Instruction::Idiv((*rhs).into()),
                        if matches!(op, tacky::BinaryOperator::Div) {
                            Instruction::Mov(Operand::Reg(Register::Eax), (*dst).into())
                        } else {
                            Instruction::Mov(Operand::Reg(Register::Edx), (*dst).into())
                        },
                    ]
                }
                tacky::BinaryOperator::Shl | tacky::BinaryOperator::Shr => {
                    vec![
                        Instruction::Mov((*lhs).into(), Operand::Reg(Register::Eax)),
                        Instruction::Mov((*rhs).into(), Operand::Reg(Register::Ecx)),
                        Instruction::Binary(
                            if matches!(op, tacky::BinaryOperator::Shl) {
                                BinaryOperator::Shl
                            } else {
                                BinaryOperator::Sar
                            },
                            Operand::Reg(Register::Cl),
                            Operand::Reg(Register::Eax),
                        ),
                        Instruction::Mov(Operand::Reg(Register::Eax), (*dst).into()),
                    ]
                }
                tacky::BinaryOperator::Eq
                | tacky::BinaryOperator::Neq
                | tacky::BinaryOperator::Lt
                | tacky::BinaryOperator::Gt
                | tacky::BinaryOperator::Le
                | tacky::BinaryOperator::Ge => {
                    vec![
                        Instruction::Cmp((*rhs).into(), (*lhs).into()),
                        Instruction::Mov(Operand::Imm(0), (*dst).clone().into()),
                        Instruction::SetCC(op.try_into()?, (*dst).into()),
                    ]
                }
            },
            tacky::Instruction::Copy(src, dst) => {
                vec![Instruction::Mov((*src).into(), (*dst).into())]
            }
            tacky::Instruction::Label(label) => {
                vec![Instruction::Label(label)]
            }
            tacky::Instruction::Jump(label) => {
                vec![Instruction::Jmp(label)]
            }
            tacky::Instruction::JumpIfZero(value, label) => {
                vec![
                    Instruction::Cmp((*value).into(), Operand::Imm(0)),
                    Instruction::JmpCC(ConditionCode::Eq, label),
                ]
            }
            tacky::Instruction::JumpIfNotZero(value, label) => {
                vec![
                    Instruction::Cmp((*value).into(), Operand::Imm(0)),
                    Instruction::JmpCC(ConditionCode::Ne, label),
                ]
            }
        });

        Ok(())
    }
}
