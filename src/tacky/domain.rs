use super::TackyError;
use crate::parser;

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<FunctionDefinition<'a>>,
}

impl std::fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for function in &self.functions {
            write!(f, "{}", function)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub body: Vec<Instruction>,
}

impl std::fmt::Display for FunctionDefinition<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.name)?;
        for instruction in &self.body {
            match instruction {
                Instruction::Label(_) => writeln!(f, "{}", instruction)?,
                _ => writeln!(f, "\t{}", instruction)?,
            }
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Return(Value),
    Unary(UnaryOperator, Box<Value>, Box<Value>),
    Binary(BinaryOperator, Box<Value>, Box<Value>, Box<Value>),
    Copy(Box<Value>, Box<Value>),
    Label(String),
    Jump(String),
    JumpIfZero(Box<Value>, String),
    JumpIfNotZero(Box<Value>, String),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return(val) => write!(f, "return {}", val),
            Instruction::Unary(op, src, dst) => write!(f, "{} {} = {}", op, src, dst),
            Instruction::Binary(op, lhs, rhs, dst) => {
                write!(f, "{} = {} {} {}", dst, op, lhs, rhs)
            }
            Instruction::Copy(src, dst) => write!(f, "{} = id {}", dst, src),
            Instruction::Label(label) => write!(f, "{}: ", label),
            Instruction::Jump(label) => write!(f, "jmp {}", label),
            Instruction::JumpIfZero(value, label) => write!(f, "jz {} {}", value, label),
            Instruction::JumpIfNotZero(value, label) => write!(f, "jnz {} {}", value, label),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    BNot,
    Not,
    Neg,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::BNot => write!(f, "bnot"),
            UnaryOperator::Not => write!(f, "not"),
            UnaryOperator::Neg => write!(f, "neg"),
        }
    }
}

impl TryFrom<&parser::UnaryOperator> for UnaryOperator {
    type Error = TackyError;

    fn try_from(op: &parser::UnaryOperator) -> Result<Self, Self::Error> {
        match op {
            parser::UnaryOperator::BNot => Ok(UnaryOperator::BNot),
            parser::UnaryOperator::Neg => Ok(UnaryOperator::Neg),
            parser::UnaryOperator::Not => Ok(UnaryOperator::Not),
            _ => Err(TackyError::UnsupportedUnaryOperator { op: *op }),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BAnd,
    BOr,
    Xor,
    Shl,
    Shr,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "add"),
            BinaryOperator::Sub => write!(f, "sub"),
            BinaryOperator::Mul => write!(f, "mul"),
            BinaryOperator::Div => write!(f, "div"),
            BinaryOperator::Mod => write!(f, "mod"),
            BinaryOperator::BAnd => write!(f, "and"),
            BinaryOperator::BOr => write!(f, "or"),
            BinaryOperator::Xor => write!(f, "xor"),
            BinaryOperator::Shl => write!(f, "shl"),
            BinaryOperator::Shr => write!(f, "shr"),
            BinaryOperator::Eq => write!(f, "eq"),
            BinaryOperator::Neq => write!(f, "neq"),
            BinaryOperator::Lt => write!(f, "lt"),
            BinaryOperator::Gt => write!(f, "gt"),
            BinaryOperator::Le => write!(f, "le"),
            BinaryOperator::Ge => write!(f, "ge"),
        }
    }
}

impl TryFrom<&parser::BinaryOperator> for BinaryOperator {
    type Error = TackyError;

    fn try_from(op: &parser::BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            parser::BinaryOperator::Add => Ok(BinaryOperator::Add),
            parser::BinaryOperator::Sub => Ok(BinaryOperator::Sub),
            parser::BinaryOperator::Mul => Ok(BinaryOperator::Mul),
            parser::BinaryOperator::Div => Ok(BinaryOperator::Div),
            parser::BinaryOperator::Mod => Ok(BinaryOperator::Mod),
            parser::BinaryOperator::BAnd => Ok(BinaryOperator::BAnd),
            parser::BinaryOperator::BOr => Ok(BinaryOperator::BOr),
            parser::BinaryOperator::Xor => Ok(BinaryOperator::Xor),
            parser::BinaryOperator::LShift => Ok(BinaryOperator::Shl),
            parser::BinaryOperator::RShift => Ok(BinaryOperator::Shr),
            parser::BinaryOperator::Eq => Ok(BinaryOperator::Eq),
            parser::BinaryOperator::Neq => Ok(BinaryOperator::Neq),
            parser::BinaryOperator::Lt => Ok(BinaryOperator::Lt),
            parser::BinaryOperator::Gt => Ok(BinaryOperator::Gt),
            parser::BinaryOperator::Le => Ok(BinaryOperator::Le),
            parser::BinaryOperator::Ge => Ok(BinaryOperator::Ge),
            _ => Err(TackyError::UnsupportedBinaryOperator { op: *op }),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Constant(i32),
    Var(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Var(name) => write!(f, "%{}", name),
        }
    }
}
