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
            writeln!(f, "\t{}", instr)?;
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
    Ret,
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
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl From<tacky::UnaryOperator> for UnaryOperator {
    fn from(op: tacky::UnaryOperator) -> Self {
        match op {
            tacky::UnaryOperator::Minus => UnaryOperator::Neg,
            tacky::UnaryOperator::Tilde => UnaryOperator::Not,
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Neg => write!(f, "neg"),
            UnaryOperator::Not => write!(f, "not"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BinaryOperator {
    Sub,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Sub => write!(f, "sub"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(String),
    PseudoReg(String),
    Stack(i32),
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
            Operand::Stack(offset) => {
                if *offset < 0 {
                    write!(f, "dword ptr [rsp - {}]", -offset)
                } else {
                    write!(f, "dword ptr [rsp + {}]", offset)
                }
            }
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
        match statement {
            tacky::Instruction::Return(value) => {
                self.instructions.push(Instruction::Mov(
                    value.into(),
                    Operand::Reg("eax".to_string()),
                ));
                self.instructions.push(Instruction::Ret);
            }
            tacky::Instruction::Unary(op, src, dst) => {
                self.instructions
                    .push(Instruction::Mov((*src).into(), (*dst.clone()).into()));
                self.instructions
                    .push(Instruction::Unary(op.into(), (*dst).into()));
            }
        }
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

    fn collect_pseudo_registers(&mut self, program: &mut Program) {
        for function in &program.functions {
            for instruction in &function.body {
                match instruction {
                    Instruction::Mov(src, dst) => {
                        if let Operand::PseudoReg(id) = src
                            && !self.operands.contains_key(id)
                        {
                            self.offset -= 4;
                            self.operands
                                .insert(id.clone(), Operand::Stack(self.offset));
                        }
                        if let Operand::PseudoReg(id) = dst
                            && !self.operands.contains_key(id)
                        {
                            self.offset -= 4;
                            self.operands
                                .insert(id.clone(), Operand::Stack(self.offset));
                        }
                    }
                    Instruction::Unary(_, op) => {
                        if let Operand::PseudoReg(id) = op
                            && !self.operands.contains_key(id)
                        {
                            self.offset -= 4;
                            self.operands
                                .insert(id.clone(), Operand::Stack(self.offset));
                        }
                    }
                    Instruction::Binary(_, left, right) => {
                        if let Operand::PseudoReg(id) = left
                            && !self.operands.contains_key(id)
                        {
                            self.offset -= 4;
                            self.operands
                                .insert(id.clone(), Operand::Stack(self.offset));
                        }
                        if let Operand::PseudoReg(id) = right
                            && !self.operands.contains_key(id)
                        {
                            self.offset -= 4;
                            self.operands
                                .insert(id.clone(), Operand::Stack(self.offset));
                        }
                    }
                    Instruction::Push(op) => {
                        if let Operand::PseudoReg(id) = op
                            && !self.operands.contains_key(id)
                        {
                            self.offset -= 4;
                            self.operands
                                .insert(id.clone(), Operand::Stack(self.offset));
                        }
                    }
                    Instruction::Pop(op) => {
                        if let Operand::PseudoReg(id) = op
                            && !self.operands.contains_key(id)
                        {
                            self.offset -= 4;
                            self.operands
                                .insert(id.clone(), Operand::Stack(self.offset));
                        }
                    }
                    Instruction::Ret => {}
                }
            }
        }
    }

    fn replace_pseudo_registers(&mut self, program: &mut Program) {
        for function in &mut program.functions {
            for instruction in &mut function.body {
                match instruction {
                    Instruction::Push(Operand::PseudoReg(id)) => {
                        *instruction = Instruction::Push(
                            self.operands.get(id).expect("Operand not found").clone(),
                        );
                    }
                    Instruction::Pop(Operand::PseudoReg(id)) => {
                        *instruction = Instruction::Pop(
                            self.operands.get(id).expect("Operand not found").clone(),
                        );
                    }
                    Instruction::Ret => {}
                    Instruction::Mov(Operand::PseudoReg(src_id), Operand::PseudoReg(dst_id)) => {
                        *instruction = Instruction::Mov(
                            self.operands
                                .get(src_id)
                                .expect("Operand not found")
                                .clone(),
                            self.operands
                                .get(dst_id)
                                .expect("Operand not found")
                                .clone(),
                        );
                    }
                    Instruction::Mov(src, Operand::PseudoReg(dst_id)) => {
                        *instruction = Instruction::Mov(
                            src.clone(),
                            self.operands
                                .get(dst_id)
                                .expect("Operand not found")
                                .clone(),
                        );
                    }
                    Instruction::Mov(Operand::PseudoReg(src_id), dst) => {
                        *instruction = Instruction::Mov(
                            self.operands
                                .get(src_id)
                                .expect("Operand not found")
                                .clone(),
                            dst.clone(),
                        );
                    }
                    Instruction::Unary(op, Operand::PseudoReg(id)) => {
                        *instruction = Instruction::Unary(
                            op.clone(),
                            self.operands.get(id).expect("Operand not found").clone(),
                        );
                    }
                    _ => {}
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

            instructions.push(Instruction::Push(Operand::Reg("rbp".to_string())));
            instructions.push(Instruction::Mov(
                Operand::Reg("rsp".to_string()),
                Operand::Reg("rbp".to_string()),
            ));
            instructions.push(Instruction::Binary(
                BinaryOperator::Sub,
                Operand::Imm(offset),
                Operand::Reg("rsp".to_string()),
            ));

            for instruction in &function.body {
                match instruction {
                    Instruction::Mov(Operand::Stack(src_offset), Operand::Stack(dst_offset)) => {
                        instructions.push(Instruction::Mov(
                            Operand::Stack(*src_offset),
                            Operand::Reg("r10d".to_string()),
                        ));
                        instructions.push(Instruction::Mov(
                            Operand::Reg("r10d".to_string()),
                            Operand::Stack(*dst_offset),
                        ));
                    }
                    Instruction::Ret => {}
                    _ => {
                        instructions.push(instruction.clone());
                    }
                }
            }
            instructions.push(Instruction::Mov(
                Operand::Reg("rbp".to_string()),
                Operand::Reg("rsp".to_string()),
            ));
            instructions.push(Instruction::Pop(Operand::Reg("rbp".to_string())));
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
}

impl Display for X86EmitterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "X86EmitterError")
    }
}

impl Error for X86EmitterError {}
