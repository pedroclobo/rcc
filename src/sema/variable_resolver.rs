use crate::parser;

use super::SemaError;

use std::collections::{HashMap, HashSet};

pub struct VariableResolver {
    vars: HashMap<String, String>,
    labels: HashSet<String>,
    counter: u32,
}

impl VariableResolver {
    pub fn new() -> Self {
        VariableResolver {
            vars: HashMap::new(),
            labels: HashSet::new(),
            counter: 0,
        }
    }

    pub fn run(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        self.resolve_vars(program)?;
        self.resolve_labels(program)?;
        Ok(())
    }

    fn resolve_vars(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        for function in &mut program.functions {
            for instruction in &mut function.body {
                match instruction {
                    parser::BlockItem::Decl(decl) => {
                        self.resolve_decl(decl)?;
                    }
                    parser::BlockItem::Stmt(stmt) => {
                        self.resolve_stmt(stmt)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn resolve_labels(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        for function in &mut program.functions {
            for instruction in &mut function.body {
                if let parser::BlockItem::Stmt(stmt) = instruction
                    && let parser::StmtKind::Goto(label) = &stmt.kind
                    && !self.labels.contains(&label.name)
                {
                    return Err(SemaError::UndeclaredLabel {
                        label: label.name.to_string(),
                        span: stmt.span.into(),
                    });
                }
            }
        }
        Ok(())
    }

    fn resolve_decl(&mut self, decl: &mut parser::Decl) -> Result<(), SemaError> {
        let parser::Decl { kind, .. } = decl;

        let name = kind.name.clone();
        if self.vars.contains_key(&name) {
            return Err(SemaError::DuplicateVariableDeclaration {
                var: name.to_string(),
                span: decl.span.into(),
            });
        }

        let new_name = self.make_tmp(&name);
        self.vars.insert(name, new_name.clone());

        kind.name = new_name;
        if let Some(initializer) = &mut kind.initializer {
            self.resolve_expr(initializer)?;
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut parser::Stmt) -> Result<(), SemaError> {
        let parser::Stmt { kind, .. } = stmt;

        match kind {
            parser::StmtKind::Return(expr) => self.resolve_expr(expr)?,
            parser::StmtKind::Expr(Some(expr)) => self.resolve_expr(expr)?,
            parser::StmtKind::Expr(None) => {}
            parser::StmtKind::If { cond, then, r#else } => {
                self.resolve_expr(cond)?;
                self.resolve_stmt(then)?;
                if let Some(r#else) = r#else {
                    self.resolve_stmt(r#else)?;
                }
            }
            parser::StmtKind::Labeled { label, stmt } => {
                if self.labels.contains(&label.name) {
                    return Err(SemaError::DuplicateLabelDeclaration {
                        label: label.name.clone(),
                        span: label.span.into(),
                    });
                }
                self.labels.insert(label.name.clone());
                self.resolve_stmt(stmt)?;
            }
            parser::StmtKind::Goto(_) => {}
        };

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut parser::Expr) -> Result<(), SemaError> {
        let parser::Expr { kind, .. } = expr;

        match kind {
            parser::ExprKind::Constant(_) => {}
            parser::ExprKind::Var(name) => {
                if let Some(var) = self.vars.get(name) {
                    *kind = parser::ExprKind::Var(var.clone());
                } else {
                    return Err(SemaError::UndeclaredVariable {
                        var: name.clone(),
                        span: expr.span.into(),
                    });
                }
            }
            parser::ExprKind::Unary(op, operand) => {
                if matches!(
                    op,
                    parser::UnaryOperator::PreDec
                        | parser::UnaryOperator::PreInc
                        | parser::UnaryOperator::PostDec
                        | parser::UnaryOperator::PostInc
                ) && !matches!(operand.kind, parser::ExprKind::Var(_))
                {
                    return Err(SemaError::InvalidAssignmentTarget {
                        expr: *operand.clone(),
                        span: operand.span.into(),
                    });
                } else {
                    self.resolve_expr(operand)?;
                }
            }
            parser::ExprKind::Binary(_, lhs, rhs) => {
                self.resolve_expr(lhs)?;
                self.resolve_expr(rhs)?;
            }
            parser::ExprKind::Assignment(lhs, rhs) => {
                if !matches!(lhs.kind, parser::ExprKind::Var(_)) {
                    return Err(SemaError::InvalidAssignmentTarget {
                        expr: *lhs.clone(),
                        span: lhs.span.into(),
                    });
                } else {
                    self.resolve_expr(lhs)?;
                    self.resolve_expr(rhs)?;
                }
            }
            parser::ExprKind::Conditional { cond, then, r#else } => {
                self.resolve_expr(cond)?;
                self.resolve_expr(then)?;
                self.resolve_expr(r#else)?;
            }
        };

        Ok(())
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
