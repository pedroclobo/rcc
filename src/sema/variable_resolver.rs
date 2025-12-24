use crate::parser;

use super::SemaError;

use std::collections::HashMap;

pub struct VariableResolver {
    vars: HashMap<String, String>,
    counter: u32,
}

impl VariableResolver {
    pub fn new() -> Self {
        VariableResolver {
            vars: HashMap::new(),
            counter: 0,
        }
    }

    pub fn run(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        for function in &mut program.functions {
            for instruction in &mut function.body {
                match instruction {
                    parser::BlockItem::Declaration(declaration) => {
                        *instruction =
                            parser::BlockItem::Declaration(self.resolve_declaration(declaration)?);
                    }
                    parser::BlockItem::Statement(statement) => {
                        *instruction =
                            parser::BlockItem::Statement(self.resolve_statement(statement)?);
                    }
                }
            }
        }
        Ok(())
    }

    fn resolve_declaration(
        &mut self,
        decl: &parser::Declaration,
    ) -> Result<parser::Declaration, SemaError> {
        let name = decl.name.clone();
        if self.vars.contains_key(&name) {
            return Err(SemaError::DuplicateDeclaration(name.to_string()));
        }
        let new_name = self.make_tmp(&name);
        self.vars.insert(name, new_name.clone());
        let new_initializer = if let Some(initializer) = &decl.initializer {
            Some(self.resolve_expression(initializer)?)
        } else {
            None
        };
        Ok(parser::Declaration {
            name: new_name,
            initializer: new_initializer,
        })
    }

    fn resolve_statement(
        &mut self,
        stmt: &parser::Statement,
    ) -> Result<parser::Statement, SemaError> {
        match stmt {
            parser::Statement::Return(expr) => {
                Ok(parser::Statement::Return(self.resolve_expression(expr)?))
            }
            parser::Statement::Expression(Some(expr)) => Ok(parser::Statement::Expression(Some(
                self.resolve_expression(expr)?,
            ))),
            parser::Statement::Expression(None) => Ok(stmt.clone()),
        }
    }

    fn resolve_expression(
        &mut self,
        expr: &parser::Expression,
    ) -> Result<parser::Expression, SemaError> {
        match expr {
            parser::Expression::Constant(_) => Ok(expr.clone()),
            parser::Expression::Var(name) => {
                if let Some(var) = self.vars.get(name) {
                    Ok(parser::Expression::Var(var.clone()))
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }
            parser::Expression::Unary(op, operand) => Ok(parser::Expression::Unary(
                *op,
                Box::new(self.resolve_expression(operand)?),
            )),
            parser::Expression::Binary(op, lhs, rhs) => Ok(parser::Expression::Binary(
                *op,
                Box::new(self.resolve_expression(lhs)?),
                Box::new(self.resolve_expression(rhs)?),
            )),
            parser::Expression::Assignment(lhs, rhs) => {
                if !matches!(&**lhs, parser::Expression::Var(_)) {
                    panic!("Invalid assignment target");
                }
                Ok(parser::Expression::Assignment(
                    Box::new(self.resolve_expression(lhs)?),
                    Box::new(self.resolve_expression(rhs)?),
                ))
            }
        }
    }

    fn make_tmp(&mut self, name: &str) -> String {
        let name = format!("{}.{}", name, self.counter);
        self.counter += 1;
        name
    }
}

impl Default for VariableResolver {
    fn default() -> Self {
        Self::new()
    }
}
