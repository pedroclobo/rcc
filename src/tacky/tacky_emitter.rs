use super::{FunctionDefinition, Instruction, Program, TackyError, Value};
use crate::parser;

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

    pub fn visit_program(&mut self, program: parser::Program<'a>) -> Result<(), TackyError> {
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
        function_definition: parser::FunctionDefinition<'a>,
    ) -> Result<(), TackyError> {
        let mut has_return = false;
        for item in &function_definition.body {
            if let parser::BlockItem::Stmt(parser::Stmt {
                kind: parser::StmtKind::Return(_),
                ..
            }) = item
            {
                has_return = true;
            }
        }

        for item in function_definition.body {
            match item {
                parser::BlockItem::Decl(decl) => self.visit_decl(decl)?,
                parser::BlockItem::Stmt(stmt) => self.visit_stmt(stmt)?,
            }
        }

        if function_definition.name == "main" && !has_return {
            self.instructions
                .push(Instruction::Return(Value::Constant(0)));
        }

        self.function = Some(FunctionDefinition {
            name: function_definition.name,
            body: self.instructions.drain(..).collect(),
        });

        Ok(())
    }

    fn visit_decl(&mut self, decl: parser::Decl) -> Result<(), TackyError> {
        let parser::Decl { kind, .. } = decl;

        if let Some(initializer) = kind.initializer {
            let expr = self.visit_expr(initializer)?;
            self.instructions.push(Instruction::Copy(
                Box::new(expr),
                Box::new(Value::Var(kind.name)),
            ));
        }

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: parser::Stmt) -> Result<(), TackyError> {
        let parser::Stmt { kind, .. } = stmt;

        match kind {
            parser::StmtKind::Return(expression) => {
                let expr = self.visit_expr(expression)?;
                self.instructions.push(Instruction::Return(expr));
            }
            parser::StmtKind::Expr(expression) => {
                if let Some(expression) = expression {
                    self.visit_expr(expression)?;
                }
            }
        };

        Ok(())
    }

    fn visit_expr(&mut self, expr: parser::Expr) -> Result<Value, TackyError> {
        let parser::Expr { kind, .. } = expr;
        match kind {
            parser::ExprKind::Constant(n) => Ok(Value::Constant(n)),
            parser::ExprKind::Unary(op, expr) => {
                let dst = self.make_tmp();
                let expr = self.visit_expr(*expr)?;

                self.instructions.push(Instruction::Unary(
                    op.into(),
                    Box::new(expr),
                    Box::new(dst.clone()),
                ));

                Ok(dst)
            }
            parser::ExprKind::Binary(
                op @ (parser::BinaryOperator::And | parser::BinaryOperator::Or),
                lhs,
                rhs,
            ) => {
                let other_label = self.make_label();
                let end_label = self.make_label();

                let lhs = self.visit_expr(*lhs)?;
                let v1 = self.make_tmp();

                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(lhs.clone()), Box::new(v1)),
                    if matches!(op, parser::BinaryOperator::And) {
                        Instruction::JumpIfZero(Box::new(lhs.clone()), other_label.clone())
                    } else {
                        Instruction::JumpIfNotZero(Box::new(lhs.clone()), other_label.clone())
                    },
                ]);

                let rhs = self.visit_expr(*rhs)?;
                let v2 = self.make_tmp();
                let dst = self.make_tmp();

                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(rhs.clone()), Box::new(v2)),
                    if matches!(op, parser::BinaryOperator::And) {
                        Instruction::JumpIfZero(Box::new(rhs.clone()), other_label.clone())
                    } else {
                        Instruction::JumpIfNotZero(Box::new(rhs.clone()), other_label.clone())
                    },
                    Instruction::Copy(
                        Box::new(if matches!(op, parser::BinaryOperator::And) {
                            Value::Constant(1)
                        } else {
                            Value::Constant(0)
                        }),
                        Box::new(dst.clone()),
                    ),
                    Instruction::Jump(end_label.clone()),
                    Instruction::Label(other_label.clone()),
                    Instruction::Copy(
                        Box::new(if matches!(op, parser::BinaryOperator::And) {
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
            parser::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.visit_expr(*lhs)?;
                let rhs = self.visit_expr(*rhs)?;
                let dst = self.make_tmp();

                self.instructions.push(Instruction::Binary(
                    op.try_into()?,
                    Box::new(lhs),
                    Box::new(rhs),
                    Box::new(dst.clone()),
                ));

                Ok(dst)
            }
            parser::ExprKind::Var(name) => Ok(Value::Var(name)),
            parser::ExprKind::Assignment(lhs, rhs) => {
                let lhs = self.visit_expr(*lhs)?;
                let rhs = self.visit_expr(*rhs)?;

                self.instructions
                    .push(Instruction::Copy(Box::new(rhs), Box::new(lhs.clone())));

                Ok(lhs)
            }
        }
    }
}
