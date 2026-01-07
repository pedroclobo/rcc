use super::TackyError;
use crate::{parser, sema};

#[derive(Debug)]
pub struct Program<'a> {
    pub decls: Vec<Decl<'a>>,
    pub globals: Vec<StaticVariable>,
}

impl std::fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for global in &self.globals {
            writeln!(f, "{}", global)?;
        }
        for decl in &self.decls {
            writeln!(f, "{}", decl)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Decl<'a> {
    Function(FunctionDefinition<'a>),
    Static(StaticVariable),
}

impl std::fmt::Display for Decl<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Function(func) => writeln!(f, "{}", func),
            Decl::Static(var) => writeln!(f, "{}", var),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub params: Vec<String>,
    pub body: Vec<Instruction>,
    pub is_global: bool,
}

impl std::fmt::Display for FunctionDefinition<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}({}):", self.name, self.params.join(", "))?;
        for instruction in &self.body {
            match instruction {
                Instruction::Label(_) => writeln!(f, "{}", instruction)?,
                _ => writeln!(f, "\t{}", instruction)?,
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct StaticVariable {
    pub name: String,
    pub value: i32,
    pub linkage: sema::Linkage,
    pub is_tentative: bool,
}

impl std::fmt::Display for StaticVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: better printing
        writeln!(f, "static {} = {}", self.name, self.value)
    }
}

// TODO: remove Box from Value
#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Return(Value),
    Unary(UnaryOperator, Value, Value),
    Binary(BinaryOperator, Value, Value, Value),
    Copy(Value, Value),
    Label(String),
    Jump(String),
    JumpIfZero(Value, String),
    JumpIfNotZero(Value, String),
    FunctionCall(String, Vec<Value>, Value),
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
            Instruction::FunctionCall(identifier, args, dst) => {
                write!(
                    f,
                    "{} = {}({})",
                    dst,
                    identifier,
                    args.iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
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
