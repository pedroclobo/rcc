use super::{
    BinaryOperator, ConditionCode, FunctionDefinition, Instruction, InstructionFixer, Operand,
    Program, Register, X86EmitterError,
};
use crate::tacky;

pub struct X86Emitter<'a> {
    program: Option<Program<'a>>,
    function: Option<FunctionDefinition<'a>>,

    functions: Vec<String>,

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

            functions: Vec::new(),

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
                // TODO: `self.function` holds the names of functions defined in
                // the same translation unit. This is only needed to know when to
                // call `f` vs `f@PLT`.
                self.functions.push(function.name.to_string());
                functions.push(function);
            }
        }

        self.program = Some(Program { functions });

        Ok(())
    }

    fn visit_function_definition(
        &mut self,
        function_definition: tacky::FunctionDefinition<'a>,
    ) -> Result<(), X86EmitterError> {
        for (i, param) in function_definition.params.iter().enumerate() {
            if i < 6 {
                self.instructions.push(Instruction::Mov(
                    Operand::Reg(Register::arg(i)),
                    Operand::PseudoReg(param.to_string()),
                ));
            } else {
                self.instructions.push(Instruction::Mov(
                    Operand::Stack {
                        size: 4,
                        offset: 8 * (i - 6 + 2) as i32,
                    },
                    Operand::PseudoReg(param.to_string()),
                ));
            }
        }

        for instruction in function_definition.body {
            self.visit_instruction(instruction)?;
        }

        self.function = Some(FunctionDefinition {
            name: function_definition.name,
            body: self.instructions.drain(..).collect(),
        });

        Ok(())
    }

    fn visit_instruction(&mut self, instr: tacky::Instruction) -> Result<(), X86EmitterError> {
        self.instructions.extend(match instr {
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
            tacky::Instruction::FunctionCall(func, args, dst) => {
                let mut instructions = Vec::new();

                let mut iter = args.iter();
                for (i, arg) in iter.by_ref().enumerate().take(6) {
                    instructions.push(Instruction::Mov(
                        arg.clone().into(),
                        Operand::Reg(Register::arg(i)),
                    ));
                }

                // The stack pointer must be 16-byte after the `call` instruction.
                // As `call` pushes the return address onto the stack, we need
                // to ensure that `rsp % 16 == 8`. As local stack space is always
                // a multiple of 16, we just need to align the stack pointer when
                // passing an odd number of stack arguments.
                let mut stack_alignment = 0;
                if args.len() > 6 && args.len() % 2 != 0 {
                    stack_alignment = 8;
                    instructions.push(Instruction::Binary(
                        BinaryOperator::Sub,
                        Operand::Imm(8),
                        Operand::Reg(Register::Rsp),
                    ));
                }

                let mut stack_offset = 0;
                for arg in iter.rev() {
                    let arg = arg.clone().into();
                    stack_offset += 8;
                    match arg {
                        Operand::Imm(_) | Operand::Reg(_) => {
                            instructions.push(Instruction::Push(arg.clone()));
                        }
                        Operand::PseudoReg(_) | Operand::Stack { .. } => {
                            instructions
                                .push(Instruction::Mov(arg.clone(), Operand::Reg(Register::Eax)));
                            instructions.push(Instruction::Push(Operand::Reg(Register::Rax)));
                        }
                    }
                }

                if self.functions.contains(&func) {
                    instructions.push(Instruction::Call(func));
                } else {
                    instructions.push(Instruction::Call(format!("{}@PLT", func)));
                }

                instructions.extend(vec![
                    Instruction::Binary(
                        BinaryOperator::Add,
                        Operand::Imm(stack_alignment + stack_offset),
                        Operand::Reg(Register::Rsp),
                    ),
                    Instruction::Mov(Operand::Reg(Register::Eax), dst.clone().into()),
                ]);

                instructions
            }
        });

        Ok(())
    }
}
