use super::{FunctionDefinition, Instruction, Program, TackyError, Value};
use crate::{parser, tacky::BinaryOperator};

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
            parser::StmtKind::If {
                cond,
                then,
                r#else: None,
            } => {
                let end_label = self.make_label();

                let cond = self.visit_expr(cond)?;
                let v1 = self.make_tmp();

                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(cond), Box::new(v1.clone())),
                    Instruction::JumpIfZero(Box::new(v1), end_label.clone()),
                ]);

                self.visit_stmt(*then)?;

                self.instructions.push(Instruction::Label(end_label));
            }
            parser::StmtKind::If {
                cond,
                then,
                r#else: Some(r#else),
            } => {
                let else_label = self.make_label();
                let end_label = self.make_label();

                let cond = self.visit_expr(cond)?;
                let v1 = self.make_tmp();

                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(cond), Box::new(v1.clone())),
                    Instruction::JumpIfZero(Box::new(v1), else_label.clone()),
                ]);

                self.visit_stmt(*then)?;

                self.instructions.extend(vec![
                    Instruction::Jump(end_label.clone()),
                    Instruction::Label(else_label),
                ]);

                self.visit_stmt(*r#else)?;

                self.instructions
                    .extend(vec![Instruction::Label(end_label)]);
            }
        };

        Ok(())
    }

    fn visit_expr(&mut self, expr: parser::Expr) -> Result<Value, TackyError> {
        let parser::Expr { kind, .. } = expr;
        match kind {
            parser::ExprKind::Constant(n) => Ok(Value::Constant(n)),
            parser::ExprKind::Unary(
                op @ (parser::UnaryOperator::Neg
                | parser::UnaryOperator::Not
                | parser::UnaryOperator::BNot),
                expr,
            ) => {
                let dst = self.make_tmp();
                let expr = self.visit_expr(*expr)?;

                self.instructions.push(Instruction::Unary(
                    op.try_into()?,
                    Box::new(expr),
                    Box::new(dst.clone()),
                ));

                Ok(dst)
            }
            parser::ExprKind::Unary(
                op @ (parser::UnaryOperator::PreInc | parser::UnaryOperator::PreDec),
                expr,
            ) => {
                let dst = self.make_tmp();
                let expr = self.visit_expr(*expr)?;

                self.instructions.push(Instruction::Binary(
                    match op {
                        parser::UnaryOperator::PreInc => BinaryOperator::Add,
                        parser::UnaryOperator::PreDec => BinaryOperator::Sub,
                        _ => panic!("invalid unary operator"),
                    },
                    Box::new(expr.clone()),
                    Box::new(Value::Constant(1)),
                    Box::new(dst.clone()),
                ));
                self.instructions
                    .push(Instruction::Copy(Box::new(dst.clone()), Box::new(expr)));

                Ok(dst)
            }
            parser::ExprKind::Unary(
                op @ (parser::UnaryOperator::PostInc | parser::UnaryOperator::PostDec),
                expr,
            ) => {
                let dst_1 = self.make_tmp();
                let dst_2 = self.make_tmp();
                let expr = self.visit_expr(*expr)?;

                self.instructions.push(Instruction::Copy(
                    Box::new(expr.clone()),
                    Box::new(dst_1.clone()),
                ));
                self.instructions.push(Instruction::Binary(
                    match op {
                        parser::UnaryOperator::PostInc => BinaryOperator::Add,
                        parser::UnaryOperator::PostDec => BinaryOperator::Sub,
                        _ => panic!("invalid unary operator"),
                    },
                    Box::new(dst_1.clone()),
                    Box::new(Value::Constant(1)),
                    Box::new(dst_2.clone()),
                ));
                self.instructions
                    .push(Instruction::Copy(Box::new(dst_2), Box::new(expr)));

                Ok(dst_1)
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
            parser::ExprKind::Conditional { cond, then, r#else } => {
                let result = self.make_tmp();
                let else_label = self.make_label();
                let end_label = self.make_label();

                let cond = self.visit_expr(*cond)?;
                let c = self.make_tmp();
                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(cond), Box::new(c.clone())),
                    Instruction::JumpIfZero(Box::new(c), else_label.clone()),
                ]);

                let then = self.visit_expr(*then)?;
                let v1 = self.make_tmp();
                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(then), Box::new(v1.clone())),
                    Instruction::Copy(Box::new(v1), Box::new(result.clone())),
                    Instruction::Jump(end_label.clone()),
                ]);

                self.instructions.push(Instruction::Label(else_label));
                let r#else = self.visit_expr(*r#else)?;
                let v2 = self.make_tmp();
                self.instructions.extend(vec![
                    Instruction::Copy(Box::new(r#else), Box::new(v2.clone())),
                    Instruction::Copy(Box::new(v2), Box::new(result.clone())),
                ]);

                self.instructions.push(Instruction::Label(end_label));

                Ok(result)
            }
        }
    }
}
