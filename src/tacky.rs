use crate::ast;
use std::fmt;

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
            Instruction::Copy(src, dst) => write!(f, "id {} = {}", dst, src),
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

impl From<ast::UnaryOperator> for UnaryOperator {
    fn from(op: ast::UnaryOperator) -> Self {
        match op {
            ast::UnaryOperator::BNot => UnaryOperator::BNot,
            ast::UnaryOperator::Neg => UnaryOperator::Neg,
            ast::UnaryOperator::Not => UnaryOperator::Not,
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

impl TryFrom<ast::BinaryOperator> for BinaryOperator {
    type Error = TackyError;

    fn try_from(op: ast::BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            ast::BinaryOperator::Add => Ok(BinaryOperator::Add),
            ast::BinaryOperator::Sub => Ok(BinaryOperator::Sub),
            ast::BinaryOperator::Mul => Ok(BinaryOperator::Mul),
            ast::BinaryOperator::Div => Ok(BinaryOperator::Div),
            ast::BinaryOperator::Mod => Ok(BinaryOperator::Mod),
            ast::BinaryOperator::BAnd => Ok(BinaryOperator::BAnd),
            ast::BinaryOperator::BOr => Ok(BinaryOperator::BOr),
            ast::BinaryOperator::Xor => Ok(BinaryOperator::Xor),
            ast::BinaryOperator::LShift => Ok(BinaryOperator::Shl),
            ast::BinaryOperator::RShift => Ok(BinaryOperator::Shr),
            ast::BinaryOperator::Eq => Ok(BinaryOperator::Eq),
            ast::BinaryOperator::Neq => Ok(BinaryOperator::Neq),
            ast::BinaryOperator::Lt => Ok(BinaryOperator::Lt),
            ast::BinaryOperator::Gt => Ok(BinaryOperator::Gt),
            ast::BinaryOperator::Le => Ok(BinaryOperator::Le),
            ast::BinaryOperator::Ge => Ok(BinaryOperator::Ge),
            _ => Err(TackyError::UnsupportedBinaryOperator(op)),
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

pub struct TackyEmitter<'a> {
    program: Option<Program<'a>>,
    function: Option<FunctionDefinition<'a>>,
    instructions: Vec<Instruction>,
    counter: usize,
}

impl Default for TackyEmitter<'_> {
    fn default() -> Self {
        TackyEmitter::new()
    }
}

impl<'a> TackyEmitter<'a> {
    pub fn new() -> Self {
        TackyEmitter {
            program: None,
            function: None,
            instructions: Vec::new(),
            counter: 0,
        }
    }

    pub fn get_program(&mut self) -> Option<Program<'_>> {
        self.program.take()
    }

    fn make_tmp(&mut self) -> Value {
        let tmp = format!("tmp.{}", self.counter);
        self.counter += 1;
        Value::Var(tmp)
    }

    fn make_label(&mut self) -> String {
        let tmp = format!("{}", self.counter);
        self.counter += 1;
        tmp
    }

    pub fn visit_program(&mut self, program: ast::Program<'a>) -> Result<(), TackyError> {
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
        function_definition: ast::FunctionDefinition<'a>,
    ) -> Result<(), TackyError> {
        self.visit_statement(function_definition.body)?;

        self.function = Some(FunctionDefinition {
            name: function_definition.name,
            body: self.instructions.drain(..).collect(),
        });

        Ok(())
    }

    fn visit_statement(&mut self, statement: ast::Statement) -> Result<(), TackyError> {
        match statement {
            ast::Statement::Return(expression) => {
                let expr = self.visit_expression(expression)?;
                self.instructions.push(Instruction::Return(expr));
            }
        };

        Ok(())
    }

    fn visit_expression(&mut self, expression: ast::Expression) -> Result<Value, TackyError> {
        match expression {
            ast::Expression::Constant(n) => Ok(Value::Constant(n)),
            ast::Expression::Unary(op, expr) => {
                let dst = self.make_tmp();
                let expr = self.visit_expression(*expr)?;

                self.instructions.push(Instruction::Unary(
                    op.into(),
                    Box::new(expr),
                    Box::new(dst.clone()),
                ));

                Ok(dst)
            }
            ast::Expression::Binary(
                op @ (ast::BinaryOperator::And | ast::BinaryOperator::Or),
                lhs,
                rhs,
            ) => {
                let other_label = self.make_label();
                let end_label = self.make_label();

                let lhs = self.visit_expression(*lhs)?;
                let v1 = self.make_tmp();

                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(lhs.clone()), Box::new(v1)),
                    if matches!(op, ast::BinaryOperator::And) {
                        Instruction::JumpIfZero(Box::new(lhs.clone()), other_label.clone())
                    } else {
                        Instruction::JumpIfNotZero(Box::new(lhs.clone()), other_label.clone())
                    },
                ]);

                let rhs = self.visit_expression(*rhs)?;
                let v2 = self.make_tmp();
                let dst = self.make_tmp();

                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(rhs.clone()), Box::new(v2)),
                    if matches!(op, ast::BinaryOperator::And) {
                        Instruction::JumpIfZero(Box::new(rhs.clone()), other_label.clone())
                    } else {
                        Instruction::JumpIfNotZero(Box::new(rhs.clone()), other_label.clone())
                    },
                    Instruction::Copy(
                        Box::new(if matches!(op, ast::BinaryOperator::And) {
                            Value::Constant(1)
                        } else {
                            Value::Constant(0)
                        }),
                        Box::new(dst.clone()),
                    ),
                    Instruction::Jump(end_label.clone()),
                    Instruction::Label(other_label.clone()),
                    Instruction::Copy(
                        Box::new(if matches!(op, ast::BinaryOperator::And) {
                            Value::Constant(0)
                        } else {
                            Value::Constant(1)
                        }),
                        Box::new(dst.clone()),
                    ),
                    Instruction::Label(end_label.clone()),
                ]);

                Ok(dst)
            }
            ast::Expression::Binary(op, lhs, rhs) => {
                let lhs = self.visit_expression(*lhs)?;
                let rhs = self.visit_expression(*rhs)?;
                let dst = self.make_tmp();

                self.instructions.push(Instruction::Binary(
                    op.try_into()?,
                    Box::new(lhs),
                    Box::new(rhs),
                    Box::new(dst.clone()),
                ));

                Ok(dst)
            }
        }
    }
}

#[derive(Debug)]
pub enum TackyError {
    UnsupportedBinaryOperator(ast::BinaryOperator),
}

impl fmt::Display for TackyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TackyError::UnsupportedBinaryOperator(op) => {
                write!(f, "No matching binary operator for {}", op)
            }
        }
    }
}
