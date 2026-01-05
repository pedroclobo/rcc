use std::collections::VecDeque;

use super::{BinaryOperator, FunctionDefinition, Instruction, Program, TackyError, Value};
use crate::{parser, sema};

#[derive(Debug, Clone)]
struct CaseInfo<'a> {
    expr: &'a parser::Expr,
    label: String,
}

#[derive(Debug, Clone)]
struct DefaultInfo {
    label: String,
}

pub struct TackyEmitter<'a> {
    program: Option<Program<'a>>,
    function: Option<FunctionDefinition<'a>>,
    instructions: Vec<Instruction>,
    counter: usize,

    break_labels: Vec<String>,
    continue_labels: Vec<String>,
    case_info: Vec<VecDeque<CaseInfo<'a>>>,
    default_info: Vec<DefaultInfo>,
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

            break_labels: Vec::new(),
            continue_labels: Vec::new(),
            case_info: Vec::new(),
            default_info: Vec::new(),
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

    pub fn visit_program(&mut self, program: &'a parser::Program) -> Result<(), TackyError> {
        let mut functions = Vec::new();

        for decl in &program.decls {
            self.visit_decl(decl)?;
            if let Some(function) = self.function.take() {
                functions.push(function);
            }
        }

        self.program = Some(Program { functions });
        Ok(())
    }

    fn visit_decl(&mut self, decl: &'a parser::Decl) -> Result<(), TackyError> {
        match &decl.kind {
            parser::DeclKind::VarDecl { name, initializer } => {
                if let Some(initializer) = &initializer {
                    let expr = self.visit_expr(initializer)?;
                    self.instructions
                        .push(Instruction::Copy(expr, Value::Var(name.clone())));
                }
            }
            parser::DeclKind::FunDecl { name, params, body } => {
                if let Some(body) = body {
                    let mut has_return = false;
                    for item in body {
                        if let parser::BlockItem::Stmt(parser::Stmt {
                            kind: parser::StmtKind::Return(_),
                            ..
                        }) = item
                        {
                            has_return = true;
                        }
                    }

                    let mut tacky_params = Vec::new();
                    for param in params {
                        tacky_params.push(param.name.clone());
                    }

                    for item in body {
                        match item {
                            parser::BlockItem::Decl(decl) => self.visit_decl(decl)?,
                            parser::BlockItem::Stmt(stmt) => self.visit_stmt(stmt)?,
                        }
                    }

                    if !has_return {
                        self.instructions
                            .push(Instruction::Return(Value::Constant(0)));
                    }

                    self.function = Some(FunctionDefinition {
                        name,
                        params: tacky_params,
                        body: self.instructions.drain(..).collect(),
                    });
                }
            }
        }

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &'a parser::Stmt) -> Result<(), TackyError> {
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
                    Instruction::Copy(cond, v1.clone()),
                    Instruction::JumpIfZero(v1, end_label.clone()),
                ]);

                self.visit_stmt(then)?;

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
                    Instruction::Copy(cond, v1.clone()),
                    Instruction::JumpIfZero(v1, else_label.clone()),
                ]);

                self.visit_stmt(then)?;

                self.instructions.extend(vec![
                    Instruction::Jump(end_label.clone()),
                    Instruction::Label(else_label),
                ]);

                self.visit_stmt(r#else)?;

                self.instructions
                    .extend(vec![Instruction::Label(end_label)]);
            }
            parser::StmtKind::Labeled { label, stmt } => {
                self.instructions
                    .push(Instruction::Label(label.name.clone()));
                self.visit_stmt(stmt)?;
            }
            parser::StmtKind::Goto(label) => self
                .instructions
                .push(Instruction::Jump(label.name.clone())),
            parser::StmtKind::Block(block) => {
                for item in &block.items {
                    match item {
                        parser::BlockItem::Decl(decl) => self.visit_decl(decl)?,
                        parser::BlockItem::Stmt(stmt) => self.visit_stmt(stmt)?,
                    }
                }
            }
            parser::StmtKind::Break => {
                let label = self
                    .break_labels
                    .last()
                    .expect("break outside of loop/switch")
                    .clone();
                self.instructions.push(Instruction::Jump(label));
            }
            parser::StmtKind::Continue => {
                let label = self
                    .continue_labels
                    .last()
                    .expect("continue outside of loop")
                    .clone();
                self.instructions.push(Instruction::Jump(label));
            }
            parser::StmtKind::While { cond, body } => {
                let label = self.make_label();
                let continue_label = format!("{}.continue", label);
                let break_label = format!("{}.break", label);
                self.break_labels.push(break_label.clone());
                self.continue_labels.push(continue_label.clone());

                self.instructions
                    .push(Instruction::Label(continue_label.clone()));

                let cond = self.visit_expr(cond)?;
                let v1 = self.make_tmp();
                self.instructions.extend(vec![
                    Instruction::Copy(cond, v1.clone()),
                    Instruction::JumpIfZero(v1, break_label.clone()),
                ]);

                self.visit_stmt(body)?;

                self.instructions.extend(vec![
                    Instruction::Jump(continue_label),
                    Instruction::Label(break_label),
                ]);

                self.break_labels.pop();
                self.continue_labels.pop();
            }
            parser::StmtKind::DoWhile { body, cond } => {
                let label = self.make_label();
                let start_label = format!("{}.start", label);
                let continue_label = format!("{}.continue", label);
                self.continue_labels.push(continue_label.clone());
                let break_label = format!("{}.break", label);
                self.break_labels.push(break_label.clone());

                self.instructions
                    .push(Instruction::Label(start_label.clone()));

                self.visit_stmt(body)?;

                self.instructions
                    .push(Instruction::Label(continue_label.clone()));

                let cond = self.visit_expr(cond)?;
                let v = self.make_tmp();
                self.instructions.extend(vec![
                    Instruction::Copy(cond, v.clone()),
                    Instruction::JumpIfNotZero(v, start_label.clone()),
                ]);

                self.instructions.push(Instruction::Label(break_label));

                self.break_labels.pop();
                self.continue_labels.pop();
            }
            parser::StmtKind::For {
                init,
                cond,
                post,
                body,
            } => {
                let label = self.make_label();
                let start_label = format!("{}.start", label);
                let continue_label = format!("{}.continue", label);
                self.continue_labels.push(continue_label.clone());
                let break_label = format!("{}.break", label);
                self.break_labels.push(break_label.clone());

                match init {
                    parser::ForInit::Decl(decl) => self.visit_decl(decl)?,
                    parser::ForInit::Expr(Some(expr)) => {
                        let _ = self.visit_expr(expr)?;
                    }
                    parser::ForInit::Expr(None) => {}
                }

                self.instructions
                    .push(Instruction::Label(start_label.clone()));

                if let Some(cond) = cond {
                    let cond = self.visit_expr(cond)?;
                    let v = self.make_tmp();
                    self.instructions.extend(vec![
                        Instruction::Copy(cond, v.clone()),
                        Instruction::JumpIfZero(v, break_label.clone()),
                    ]);
                }

                self.visit_stmt(body)?;

                self.instructions
                    .push(Instruction::Label(continue_label.clone()));

                if let Some(post) = post {
                    self.visit_expr(post)?;
                }

                self.instructions.extend(vec![
                    Instruction::Jump(start_label),
                    Instruction::Label(break_label),
                ]);

                self.break_labels.pop();
                self.continue_labels.pop();
            }
            parser::StmtKind::Switch { expr, body } => {
                // Annotate switch cases with a label
                self.case_info.push(VecDeque::new());
                for case_stmt in &sema::InstructionCollector::collect_cases(body) {
                    if let parser::StmtKind::Case { expr, .. } = &case_stmt.kind {
                        let label = self.make_label();
                        self.case_info
                            .last_mut()
                            .unwrap()
                            .push_back(CaseInfo { expr, label });
                    }
                }
                // Annotate default case with a label
                if sema::InstructionCollector::collect_default(body).is_some() {
                    let label = self.make_label();
                    self.default_info.push(DefaultInfo { label });
                }

                let sexpr = self.visit_expr(expr)?;
                let dst_sexpr = self.make_tmp();
                self.instructions
                    .push(Instruction::Copy(sexpr, dst_sexpr.clone()));

                for CaseInfo { expr, label, .. } in self.case_info.last().unwrap().clone() {
                    let cexpr = self.visit_expr(expr)?;
                    let dst_cexpr = self.make_tmp();
                    self.instructions.push(Instruction::Binary(
                        BinaryOperator::Eq,
                        dst_sexpr.clone(),
                        cexpr.clone(),
                        dst_cexpr.clone(),
                    ));
                    self.instructions
                        .push(Instruction::JumpIfNotZero(dst_cexpr, label.to_string()));
                }

                let end_label = self.make_label();
                self.break_labels.push(end_label.clone());
                if let Some(DefaultInfo { label, .. }) = &self.default_info.last() {
                    self.instructions.push(Instruction::Jump(label.clone()));
                } else {
                    self.instructions.push(Instruction::Jump(end_label.clone()));
                }

                self.visit_stmt(body)?;

                self.case_info.pop();

                self.instructions
                    .push(Instruction::Label(end_label.clone()));
            }
            parser::StmtKind::Case { body, .. } => {
                let label = self
                    .case_info
                    .last_mut()
                    .expect("case outside of switch")
                    .pop_front()
                    .expect("no label for case")
                    .label;
                self.instructions.push(Instruction::Label(label.clone()));
                self.visit_stmt(body)?;
            }
            parser::StmtKind::Default { body } => {
                let label = self
                    .default_info
                    .pop()
                    .expect("default outside of switch")
                    .label
                    .clone();
                self.instructions.push(Instruction::Label(label.clone()));
                self.visit_stmt(body)?;
            }
        };

        Ok(())
    }

    fn visit_expr(&mut self, expr: &parser::Expr) -> Result<Value, TackyError> {
        let parser::Expr { kind, .. } = expr;
        match kind {
            parser::ExprKind::Constant(n) => Ok(Value::Constant(*n)),
            parser::ExprKind::Unary(
                op @ (parser::UnaryOperator::Neg
                | parser::UnaryOperator::Not
                | parser::UnaryOperator::BNot),
                expr,
            ) => {
                let dst = self.make_tmp();
                let expr = self.visit_expr(expr)?;

                self.instructions
                    .push(Instruction::Unary(op.try_into()?, expr, dst.clone()));

                Ok(dst)
            }
            parser::ExprKind::Unary(
                op @ (parser::UnaryOperator::PreInc | parser::UnaryOperator::PreDec),
                expr,
            ) => {
                let dst = self.make_tmp();
                let expr = self.visit_expr(expr)?;

                self.instructions.push(Instruction::Binary(
                    match op {
                        parser::UnaryOperator::PreInc => BinaryOperator::Add,
                        parser::UnaryOperator::PreDec => BinaryOperator::Sub,
                        _ => panic!("invalid unary operator"),
                    },
                    expr.clone(),
                    Value::Constant(1),
                    dst.clone(),
                ));
                self.instructions.push(Instruction::Copy(dst.clone(), expr));

                Ok(dst)
            }
            parser::ExprKind::Unary(
                op @ (parser::UnaryOperator::PostInc | parser::UnaryOperator::PostDec),
                expr,
            ) => {
                let dst_1 = self.make_tmp();
                let dst_2 = self.make_tmp();
                let expr = self.visit_expr(expr)?;

                self.instructions
                    .push(Instruction::Copy(expr.clone(), dst_1.clone()));
                self.instructions.push(Instruction::Binary(
                    match op {
                        parser::UnaryOperator::PostInc => BinaryOperator::Add,
                        parser::UnaryOperator::PostDec => BinaryOperator::Sub,
                        _ => panic!("invalid unary operator"),
                    },
                    dst_1.clone(),
                    Value::Constant(1),
                    dst_2.clone(),
                ));
                self.instructions.push(Instruction::Copy(dst_2, expr));

                Ok(dst_1)
            }
            parser::ExprKind::Binary(
                op @ (parser::BinaryOperator::And | parser::BinaryOperator::Or),
                lhs,
                rhs,
            ) => {
                let other_label = self.make_label();
                let end_label = self.make_label();

                let lhs = self.visit_expr(lhs)?;
                let v1 = self.make_tmp();

                self.instructions.extend(vec![
                    Instruction::Copy(lhs.clone(), v1),
                    if matches!(op, parser::BinaryOperator::And) {
                        Instruction::JumpIfZero(lhs.clone(), other_label.clone())
                    } else {
                        Instruction::JumpIfNotZero(lhs.clone(), other_label.clone())
                    },
                ]);

                let rhs = self.visit_expr(rhs)?;
                let v2 = self.make_tmp();
                let dst = self.make_tmp();

                self.instructions.extend(vec![
                    Instruction::Copy(rhs.clone(), v2),
                    if matches!(op, parser::BinaryOperator::And) {
                        Instruction::JumpIfZero(rhs.clone(), other_label.clone())
                    } else {
                        Instruction::JumpIfNotZero(rhs.clone(), other_label.clone())
                    },
                    Instruction::Copy(
                        if matches!(op, parser::BinaryOperator::And) {
                            Value::Constant(1)
                        } else {
                            Value::Constant(0)
                        },
                        dst.clone(),
                    ),
                    Instruction::Jump(end_label.clone()),
                    Instruction::Label(other_label.clone()),
                    Instruction::Copy(
                        if matches!(op, parser::BinaryOperator::And) {
                            Value::Constant(0)
                        } else {
                            Value::Constant(1)
                        },
                        dst.clone(),
                    ),
                    Instruction::Label(end_label.clone()),
                ]);

                Ok(dst)
            }
            parser::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.visit_expr(lhs)?;
                let rhs = self.visit_expr(rhs)?;
                let dst = self.make_tmp();

                self.instructions
                    .push(Instruction::Binary(op.try_into()?, lhs, rhs, dst.clone()));

                Ok(dst)
            }
            parser::ExprKind::Var(name) => Ok(Value::Var(name.clone())),
            parser::ExprKind::Assignment(lhs, rhs) => {
                let lhs = self.visit_expr(lhs)?;
                let rhs = self.visit_expr(rhs)?;

                self.instructions.push(Instruction::Copy(rhs, lhs.clone()));

                Ok(lhs)
            }
            parser::ExprKind::Conditional { cond, then, r#else } => {
                let result = self.make_tmp();
                let else_label = self.make_label();
                let end_label = self.make_label();

                let cond = self.visit_expr(cond)?;
                let c = self.make_tmp();
                self.instructions.extend(vec![
                    Instruction::Copy(cond, c.clone()),
                    Instruction::JumpIfZero(c, else_label.clone()),
                ]);

                let then = self.visit_expr(then)?;
                let v1 = self.make_tmp();
                self.instructions.extend(vec![
                    Instruction::Copy(then, v1.clone()),
                    Instruction::Copy(v1, result.clone()),
                    Instruction::Jump(end_label.clone()),
                ]);

                self.instructions.push(Instruction::Label(else_label));
                let r#else = self.visit_expr(r#else)?;
                let v2 = self.make_tmp();
                self.instructions.extend(vec![
                    Instruction::Copy(r#else, v2.clone()),
                    Instruction::Copy(v2, result.clone()),
                ]);

                self.instructions.push(Instruction::Label(end_label));

                Ok(result)
            }
            parser::ExprKind::FunctionCall {
                identifier,
                arguments,
            } => {
                let identifier = match &identifier.kind {
                    parser::ExprKind::Var(v) => v,
                    _ => panic!("invalid function"),
                };

                let mut args = Vec::new();
                for arg in arguments {
                    let arg = self.visit_expr(arg)?;
                    let dst = self.make_tmp();
                    self.instructions.push(Instruction::Copy(arg, dst.clone()));
                    args.push(dst.clone());
                }

                let dst = self.make_tmp();
                self.instructions.push(Instruction::FunctionCall(
                    identifier.clone(),
                    args,
                    dst.clone(),
                ));

                Ok(dst)
            }
        }
    }
}
