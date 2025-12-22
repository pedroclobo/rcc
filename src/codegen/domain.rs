use super::X86EmitterError;
use crate::tacky;

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
                    ConditionCode::Eq => "e",
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
                    ConditionCode::Eq => "e",
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
    Eq,
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
            tacky::BinaryOperator::Eq => Ok(ConditionCode::Eq),
            tacky::BinaryOperator::Neq => Ok(ConditionCode::Ne),
            tacky::BinaryOperator::Lt => Ok(ConditionCode::Lt),
            tacky::BinaryOperator::Gt => Ok(ConditionCode::Gt),
            tacky::BinaryOperator::Le => Ok(ConditionCode::Le),
            tacky::BinaryOperator::Ge => Ok(ConditionCode::Ge),
            _ => Err(X86EmitterError::NoMatchingConditionCode(op)),
        }
    }
}

impl std::fmt::Display for ConditionCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConditionCode::Eq => write!(f, "e"),
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
    BNot,
    Neg,
    Not,
}

impl From<tacky::UnaryOperator> for UnaryOperator {
    fn from(op: tacky::UnaryOperator) -> Self {
        match op {
            tacky::UnaryOperator::Neg => UnaryOperator::Neg,
            tacky::UnaryOperator::BNot => UnaryOperator::BNot,
            tacky::UnaryOperator::Not => UnaryOperator::Not,
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Neg => write!(f, "neg"),
            UnaryOperator::BNot => write!(f, "not"),
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
