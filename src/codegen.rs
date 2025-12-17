use crate::ast::{AstVisitor, Expression, FunctionDefinition, Program, Statement};
use std::{error::Error, fmt::Display, io::Write};

pub struct CodeGen<W: Write> {
    out: W,
}

impl<W: Write> CodeGen<W> {
    pub fn new(out: W) -> Self {
        Self { out }
    }

    pub fn visit(&mut self, program: Program) -> Result<(), CodeGenError> {
        self.visit_program(program)?;
        Ok(())
    }
}

impl<W: Write> AstVisitor<'_> for CodeGen<W> {
    type Error = CodeGenError;

    fn visit_program(&mut self, program: Program) -> Result<(), CodeGenError> {
        writeln!(self.out, ".intel_syntax noprefix")?;
        for function in program.functions {
            self.visit_function_definition(function)?
        }
        writeln!(self.out, ".section .note.GNU-stack,\"\",@progbits")?;
        Ok(())
    }

    fn visit_function_definition(
        &mut self,
        function: FunctionDefinition,
    ) -> Result<(), CodeGenError> {
        writeln!(self.out, ".globl {}", function.name)?;
        writeln!(self.out, "{}:", function.name)?;
        self.visit_statement(function.body)?;
        Ok(())
    }

    fn visit_statement(&mut self, statement: Statement) -> Result<(), CodeGenError> {
        match statement {
            Statement::Return(expression) => {
                write!(self.out, "\tmov eax, ")?;
                self.visit_expression(expression)?;
                writeln!(self.out)?;
                writeln!(self.out, "\tret")?;
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expression: Expression) -> Result<(), CodeGenError> {
        match expression {
            Expression::Constant(constant) => write!(self.out, "{}", constant)?,
            Expression::Unary(_op, _expr) => todo!(),
        };
        Ok(())
    }
}

#[derive(Debug)]
pub enum CodeGenError {
    Io(std::io::Error),
}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeGenError::Io(error) => error.fmt(f),
        }
    }
}

impl Error for CodeGenError {}

impl From<std::io::Error> for CodeGenError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}
