use crate::tacky::{self, TackyVisitor};
use std::{collections::HashMap, error::Error, fmt::Display};

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<FunctionDefinition<'a>>,
}

impl std::fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, ".intel_syntax noprefix")?;
        for function in &self.functions {
            writeln!(f, "{}", function)?;
        }
        writeln!(f, ".section .note.GNU-stack,\"\",@progbits")?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub body: Vec<Instruction>,
}

impl FunctionDefinition<'_> {
    pub fn assign_body(&mut self, body: &[Instruction]) {
        self.body = body.to_vec();
    }
}

impl std::fmt::Display for FunctionDefinition<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "\t.globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        for instr in &self.body {
            match instr {
                Instruction::Label(_) => writeln!(f, "{}", instr)?,
                _ => writeln!(f, "\t{}", instr)?,
            }
        }
        Ok(())
    }
}

// TODO: implement Verifier:
// i.e. `mov` doesn't take constants in the destination operand
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Push(Operand),
    Pop(Operand),
    Cdq,
    Idiv(Operand),
    Ret,
    Label(String),
    Jmp(String),
    JmpCC(ConditionCode, String),
    SetCC(ConditionCode, Operand),
    Cmp(Operand, Operand),
}

impl Instruction {
    pub fn operands(&self) -> impl Iterator<Item = &Operand> {
        match self {
            Instruction::Mov(src, dst) => vec![dst, src],
            Instruction::Unary(_, arg) => vec![arg],
            Instruction::Binary(_, lhs, rhs) => vec![lhs, rhs],
            Instruction::Push(arg) => vec![arg],
            Instruction::Pop(arg) => vec![arg],
            Instruction::Ret => vec![],
            Instruction::Cdq => vec![],
            Instruction::Idiv(op) => vec![op],
            Instruction::Label(_) => vec![],
            Instruction::Jmp(_) => vec![],
            Instruction::JmpCC(_, _) => vec![],
            Instruction::SetCC(_, op) => vec![op],
            Instruction::Cmp(lhs, rhs) => vec![lhs, rhs],
        }
        .into_iter()
    }

    pub fn operands_mut(&mut self) -> impl Iterator<Item = &mut Operand> {
        match self {
            Instruction::Mov(src, dst) => vec![dst, src],
            Instruction::Unary(_, arg) => vec![arg],
            Instruction::Binary(_, lhs, rhs) => vec![lhs, rhs],
            Instruction::Push(arg) => vec![arg],
            Instruction::Pop(arg) => vec![arg],
            Instruction::Ret => vec![],
            Instruction::Cdq => vec![],
            Instruction::Idiv(op) => vec![op],
            Instruction::Label(_) => vec![],
            Instruction::Jmp(_) => vec![],
            Instruction::JmpCC(_, _) => vec![],
            Instruction::SetCC(_, op) => vec![op],
            Instruction::Cmp(lhs, rhs) => vec![lhs, rhs],
        }
        .into_iter()
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Mov(src, dst) => write!(f, "mov\t{}, {}", dst, src,),
            Instruction::Unary(op, arg) => write!(f, "{}\t{}", op, arg,),
            Instruction::Binary(op, lhs, rhs) => write!(f, "{}\t{}, {}", op, rhs, lhs),
            Instruction::Push(arg) => write!(f, "push\t{}", arg),
            Instruction::Pop(arg) => write!(f, "pop\t{}", arg),
            Instruction::Ret => write!(f, "ret"),
            Instruction::Cdq => write!(f, "cdq"),
            Instruction::Idiv(op) => write!(f, "idiv\t{}", op),
            Instruction::Label(label) => write!(f, ".L{}:", label),
            Instruction::Jmp(label) => write!(f, "jmp\t.L{}", label),
            Instruction::JmpCC(cc, label) => write!(
                f,
                "j{}\t.L{}",
                match cc {
                    ConditionCode::E => "e",
                    ConditionCode::Ne => "ne",
                    ConditionCode::Lt => "l",
                    ConditionCode::Gt => "g",
                    ConditionCode::Le => "le",
                    ConditionCode::Ge => "ge",
                },
                label
            ),
            Instruction::SetCC(cc, op) => write!(
                f,
                "set{}\t{}",
                match cc {
                    ConditionCode::E => "e",
                    ConditionCode::Ne => "ne",
                    ConditionCode::Lt => "l",
                    ConditionCode::Gt => "g",
                    ConditionCode::Le => "le",
                    ConditionCode::Ge => "ge",
                },
                op
            ),
            Instruction::Cmp(lhs, rhs) => write!(f, "cmp\t{}, {}", rhs, lhs),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ConditionCode {
    E,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl TryFrom<tacky::BinaryOperator> for ConditionCode {
    type Error = X86EmitterError;

    fn try_from(op: tacky::BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            tacky::BinaryOperator::Eq => Ok(ConditionCode::E),
            tacky::BinaryOperator::Neq => Ok(ConditionCode::Ne),
            tacky::BinaryOperator::Lt => Ok(ConditionCode::Lt),
            tacky::BinaryOperator::Gt => Ok(ConditionCode::Gt),
            tacky::BinaryOperator::Le => Ok(ConditionCode::Le),
            tacky::BinaryOperator::Ge => Ok(ConditionCode::Ge),
            _ => Err(X86EmitterError::NoMatchingConditionCode(op)),
        }
    }
}

impl Display for ConditionCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConditionCode::E => write!(f, "e"),
            ConditionCode::Ne => write!(f, "ne"),
            ConditionCode::Lt => write!(f, "lt"),
            ConditionCode::Gt => write!(f, "gt"),
            ConditionCode::Le => write!(f, "le"),
            ConditionCode::Ge => write!(f, "ge"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnaryOperator {
    Tilde,
    Minus,
    Not,
}

impl From<tacky::UnaryOperator> for UnaryOperator {
    fn from(op: tacky::UnaryOperator) -> Self {
        match op {
            tacky::UnaryOperator::Minus => UnaryOperator::Minus,
            tacky::UnaryOperator::Tilde => UnaryOperator::Tilde,
            tacky::UnaryOperator::Not => UnaryOperator::Not,
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // TODO: better naming here
            UnaryOperator::Minus => write!(f, "neg"),
            UnaryOperator::Tilde => write!(f, "not"),
            UnaryOperator::Not => write!(f, "not"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    Shl,
    Sar,
}

impl TryFrom<tacky::BinaryOperator> for BinaryOperator {
    type Error = X86EmitterError;

    fn try_from(op: tacky::BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            tacky::BinaryOperator::Add => Ok(BinaryOperator::Add),
            tacky::BinaryOperator::Sub => Ok(BinaryOperator::Sub),
            tacky::BinaryOperator::Mul => Ok(BinaryOperator::Mul),
            tacky::BinaryOperator::BAnd => Ok(BinaryOperator::And),
            tacky::BinaryOperator::BOr => Ok(BinaryOperator::Or),
            tacky::BinaryOperator::Xor => Ok(BinaryOperator::Xor),
            tacky::BinaryOperator::Shl => Ok(BinaryOperator::Shl),
            tacky::BinaryOperator::Shr => Ok(BinaryOperator::Sar),
            tacky::BinaryOperator::Div
            | tacky::BinaryOperator::Mod
            | tacky::BinaryOperator::Eq
            | tacky::BinaryOperator::Neq
            | tacky::BinaryOperator::Lt
            | tacky::BinaryOperator::Gt
            | tacky::BinaryOperator::Le
            | tacky::BinaryOperator::Ge => Err(X86EmitterError::UnsupportedBinaryOperator(op)),
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "add"),
            BinaryOperator::Sub => write!(f, "sub"),
            BinaryOperator::Mul => write!(f, "imul"),
            BinaryOperator::And => write!(f, "and"),
            BinaryOperator::Or => write!(f, "or"),
            BinaryOperator::Xor => write!(f, "xor"),
            BinaryOperator::Shl => write!(f, "shl"),
            BinaryOperator::Sar => write!(f, "sar"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    PseudoReg(String),
    Stack { size: i32, offset: i32 },
}

impl From<tacky::Value> for Operand {
    fn from(value: tacky::Value) -> Self {
        match value {
            tacky::Value::Constant(n) => Operand::Imm(n),
            tacky::Value::Var(id) => Operand::PseudoReg(id),
        }
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Imm(n) => write!(f, "{}", n),
            Operand::Reg(name) => write!(f, "{}", name),
            Operand::PseudoReg(name) => write!(f, "|{}", name),
            Operand::Stack { size, offset } => {
                write!(
                    f,
                    "{} [rsp {}]",
                    match size {
                        1 => "byte ptr",
                        2 => "word ptr",
                        4 => "dword ptr",
                        8 => "qword ptr",
                        _ => panic!("Invalid stack size"),
                    },
                    if *offset < 0 {
                        format!("- {}", -offset)
                    } else {
                        format!("+ {}", offset)
                    }
                )
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Register {
    Al,
    Eax,
    Ecx,
    Dl,
    Edx,
    Rsp,
    Rbp,
    R10b,
    R10d,
    R11b,
    R11d,
    Cl,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Eax => write!(f, "eax"),
            Register::Ecx => write!(f, "ecx"),
            Register::Edx => write!(f, "edx"),
            Register::Rsp => write!(f, "rsp"),
            Register::Rbp => write!(f, "rbp"),
            Register::R10d => write!(f, "r10d"),
            Register::R11d => write!(f, "r11d"),
            Register::Cl => write!(f, "cl"),
            Register::Al => write!(f, "al"),
            Register::Dl => write!(f, "dl"),
            Register::R10b => write!(f, "r10b"),
            Register::R11b => write!(f, "r11b"),
        }
    }
}

pub struct X86Emitter<'a> {
    program: Option<Program<'a>>,
    function: Option<FunctionDefinition<'a>>,

    instructions: Vec<Instruction>,
    operands: Vec<Operand>,
}

impl Default for X86Emitter<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl X86Emitter<'_> {
    pub fn new() -> Self {
        X86Emitter {
            program: None,
            function: None,
            instructions: Vec::new(),
            operands: Vec::new(),
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
}

impl<'a> TackyVisitor<'a> for X86Emitter<'a> {
    type Error = X86EmitterError;

    fn visit_program(&mut self, program: tacky::Program<'a>) -> Result<(), Self::Error> {
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
    ) -> Result<(), Self::Error> {
        self.instructions.clear();

        for instruction in function_definition.body {
            self.visit_instruction(instruction)?;
        }

        self.function = Some(FunctionDefinition {
            name: function_definition.name,
            body: self.instructions.drain(..).collect(),
        });

        Ok(())
    }

    fn visit_instruction(
        &mut self,
        statement: crate::tacky::Instruction,
    ) -> Result<(), Self::Error> {
        self.instructions.extend(match statement {
            tacky::Instruction::Return(value) => {
                vec![
                    Instruction::Mov(value.into(), Operand::Reg(Register::Eax)),
                    Instruction::Ret,
                ]
            }
            tacky::Instruction::Unary(op, src, dst) => match op {
                tacky::UnaryOperator::Tilde | tacky::UnaryOperator::Minus => {
                    vec![
                        Instruction::Mov((*src).into(), (*dst.clone()).into()),
                        Instruction::Unary(op.into(), (*dst).into()),
                    ]
                }
                tacky::UnaryOperator::Not => {
                    vec![
                        Instruction::Cmp(Operand::Imm(0), (*src).into()),
                        Instruction::Mov(Operand::Imm(0), (*dst).clone().into()),
                        Instruction::SetCC(ConditionCode::E, (*dst).into()),
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
                    let mut instructions = vec![
                        Instruction::Mov((*lhs).into(), Operand::Reg(Register::Eax)),
                        Instruction::Cdq,
                        Instruction::Idiv((*rhs).into()),
                    ];
                    if matches!(op, tacky::BinaryOperator::Div) {
                        instructions
                            .push(Instruction::Mov(Operand::Reg(Register::Eax), (*dst).into()));
                    } else {
                        instructions
                            .push(Instruction::Mov(Operand::Reg(Register::Edx), (*dst).into()));
                    }
                    instructions
                }
                tacky::BinaryOperator::Shl | tacky::BinaryOperator::Shr => {
                    let binary_op = if matches!(op, tacky::BinaryOperator::Shl) {
                        BinaryOperator::Shl
                    } else {
                        BinaryOperator::Sar
                    };
                    vec![
                        Instruction::Mov((*lhs).into(), Operand::Reg(Register::Eax)),
                        Instruction::Mov((*rhs).into(), Operand::Reg(Register::Ecx)),
                        Instruction::Binary(
                            binary_op,
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
                    Instruction::JmpCC(ConditionCode::E, label),
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

    fn visit_value(&mut self, value: crate::tacky::Value) -> Result<(), Self::Error> {
        self.operands.push(match value {
            tacky::Value::Constant(n) => Operand::Imm(n),
            tacky::Value::Var(id) => Operand::PseudoReg(id),
        });
        Ok(())
    }
}

pub trait X86Visitor<'a> {
    type Error;

    fn visit_program(&mut self, program: Program<'a>) -> Result<(), Self::Error>;
    fn visit_function(&mut self, function: FunctionDefinition<'a>) -> Result<(), Self::Error>;
    fn visit_instruction(&mut self, instruction: Instruction) -> Result<(), Self::Error>;
    fn visit_operand(&mut self, operand: Operand) -> Result<(), Self::Error>;
}

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
        let mut replacer = PseudoRegisterReplacer::new();
        replacer.run(program);
        self.collect_instructions(program, -replacer.offset);
        self.assign_instructions(program);
    }

    fn collect_instructions(&mut self, program: &Program<'_>, offset: i32) {
        for function in &program.functions {
            self.instructions
                .insert(function.name.to_string(), Vec::new());
            let instructions = self
                .instructions
                .get_mut(function.name)
                .expect("Function not found");

            instructions.push(Instruction::Push(Operand::Reg(Register::Rbp)));
            instructions.push(Instruction::Mov(
                Operand::Reg(Register::Rsp),
                Operand::Reg(Register::Rbp),
            ));
            instructions.push(Instruction::Binary(
                BinaryOperator::Sub,
                Operand::Imm(offset),
                Operand::Reg(Register::Rsp),
            ));

            for instruction in &function.body {
                match instruction {
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
                        instructions.push(Instruction::Mov(
                            Operand::Stack {
                                size: *src_size,
                                offset: *src_offset,
                            },
                            Operand::Reg(Register::R10d),
                        ));
                        instructions.push(Instruction::Mov(
                            Operand::Reg(Register::R10d),
                            Operand::Stack {
                                size: *dst_size,
                                offset: *dst_offset,
                            },
                        ));
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
                        instructions.push(Instruction::Mov(
                            Operand::Stack {
                                size: *src_size,
                                offset: *src_offset,
                            },
                            Operand::Reg(Register::R10d),
                        ));
                        instructions.push(Instruction::Cmp(
                            Operand::Reg(Register::R10d),
                            Operand::Stack {
                                size: *dst_size,
                                offset: *dst_offset,
                            },
                        ));
                    }
                    Instruction::Ret => {}
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
                        instructions.push(Instruction::Mov(
                            Operand::Stack {
                                size: *lhs_size,
                                offset: *lhs_offset,
                            },
                            Operand::Reg(Register::R10d),
                        ));
                        instructions.push(Instruction::Binary(
                            *op,
                            Operand::Reg(Register::R10d),
                            Operand::Stack {
                                size: *rhs_size,
                                offset: *rhs_offset,
                            },
                        ));
                    }
                    Instruction::Binary(
                        BinaryOperator::Mul,
                        lhs,
                        Operand::Stack {
                            size: rhs_size,
                            offset: rhs_offset,
                        },
                    ) => {
                        instructions.push(Instruction::Mov(
                            Operand::Stack {
                                size: *rhs_size,
                                offset: *rhs_offset,
                            },
                            Operand::Reg(Register::R11d),
                        ));
                        instructions.push(Instruction::Binary(
                            BinaryOperator::Mul,
                            lhs.clone(),
                            Operand::Reg(Register::R11d),
                        ));
                        instructions.push(Instruction::Mov(
                            Operand::Reg(Register::R11d),
                            Operand::Stack {
                                size: *rhs_size,
                                offset: *rhs_offset,
                            },
                        ));
                    }
                    Instruction::Idiv(Operand::Imm(n)) => {
                        instructions.push(Instruction::Mov(
                            Operand::Imm(*n),
                            Operand::Reg(Register::R10d),
                        ));
                        instructions.push(Instruction::Idiv(Operand::Reg(Register::R10d)));
                    }
                    Instruction::Cmp(lhs, Operand::Imm(n)) => {
                        instructions.push(Instruction::Mov(
                            Operand::Imm(*n),
                            Operand::Reg(Register::R11d),
                        ));
                        instructions
                            .push(Instruction::Cmp(lhs.clone(), Operand::Reg(Register::R11d)));
                    }
                    Instruction::SetCC(cc, Operand::Stack { size: _, offset }) => {
                        instructions.push(Instruction::SetCC(
                            cc.clone(),
                            Operand::Stack {
                                size: 1,
                                offset: *offset,
                            },
                        ));
                    }
                    Instruction::SetCC(cc, Operand::Reg(reg)) => match reg {
                        Register::Eax => instructions
                            .push(Instruction::SetCC(cc.clone(), Operand::Reg(Register::Al))),
                        Register::Edx => instructions
                            .push(Instruction::SetCC(cc.clone(), Operand::Reg(Register::Dl))),
                        Register::R10d => instructions
                            .push(Instruction::SetCC(cc.clone(), Operand::Reg(Register::R10b))),
                        Register::R11d => instructions
                            .push(Instruction::SetCC(cc.clone(), Operand::Reg(Register::R11b))),
                        _ => unreachable!(),
                    },
                    _ => {
                        instructions.push(instruction.clone());
                    }
                }
            }
            instructions.push(Instruction::Mov(
                Operand::Reg(Register::Rbp),
                Operand::Reg(Register::Rsp),
            ));
            instructions.push(Instruction::Pop(Operand::Reg(Register::Rbp)));
            instructions.push(Instruction::Ret);
        }
    }

    fn assign_instructions(&mut self, program: &mut Program) {
        for function in &mut program.functions {
            function.assign_body(self.instructions.get(function.name).unwrap());
        }
    }
}

#[derive(Debug)]
pub enum X86EmitterError {
    NoProgram,
    UnsupportedBinaryOperator(tacky::BinaryOperator),
    NoMatchingConditionCode(tacky::BinaryOperator),
}

impl Display for X86EmitterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            X86EmitterError::NoProgram => write!(f, "No program provided"),
            X86EmitterError::UnsupportedBinaryOperator(op) => {
                write!(f, "No matching binary operator for {}", op)
            }
            X86EmitterError::NoMatchingConditionCode(op) => {
                write!(f, "No matching condition code for {}", op)
            }
        }
    }
}

impl Error for X86EmitterError {}
