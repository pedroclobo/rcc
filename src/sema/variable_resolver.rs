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
                    parser::BlockItem::Decl(decl) => {
                        *instruction = parser::BlockItem::Decl(self.resolve_decl(decl)?);
                    }
                    parser::BlockItem::Stmt(stmt) => {
                        *instruction = parser::BlockItem::Stmt(self.resolve_stmt(stmt)?);
                    }
                }
            }
        }
        Ok(())
    }

    fn resolve_decl(&mut self, decl: &parser::Decl) -> Result<parser::Decl, SemaError> {
        let parser::Decl { kind, .. } = decl;

        let name = kind.name.clone();
        if self.vars.contains_key(&name) {
            return Err(SemaError::DuplicateDeclaration {
                var: name.to_string(),
                span: decl.span.clone().into(),
            });
        }
        let new_name = self.make_tmp(&name);
        self.vars.insert(name, new_name.clone());
        let new_initializer = if let Some(initializer) = &kind.initializer {
            Some(self.resolve_expr(initializer)?)
        } else {
            None
        };
        Ok(parser::Decl {
            kind: parser::DeclKind {
                name: new_name,
                initializer: new_initializer,
            },
            span: decl.span.clone(),
        })
    }

    fn resolve_stmt(&mut self, stmt: &parser::Stmt) -> Result<parser::Stmt, SemaError> {
        let parser::Stmt { kind, .. } = stmt;

        match kind {
            parser::StmtKind::Return(expr) => Ok(parser::Stmt {
                kind: parser::StmtKind::Return(self.resolve_expr(expr)?),
                span: stmt.span.clone(),
            }),
            parser::StmtKind::Expr(Some(expr)) => Ok(parser::Stmt {
                kind: parser::StmtKind::Expr(Some(self.resolve_expr(expr)?)),
                span: stmt.span.clone(),
            }),
            parser::StmtKind::Expr(None) => Ok(stmt.clone()),
        }
    }

    fn resolve_expr(&mut self, expr: &parser::Expr) -> Result<parser::Expr, SemaError> {
        let parser::Expr { kind, .. } = expr;

        match kind {
            parser::ExprKind::Constant(_) => Ok(expr.clone()),
            parser::ExprKind::Var(name) => {
                if let Some(var) = self.vars.get(name) {
                    Ok(parser::Expr {
                        kind: parser::ExprKind::Var(var.clone()),
                        span: expr.span.clone(),
                    })
                } else {
                    Err(SemaError::UndeclaredVariable {
                        var: name.clone(),
                        span: expr.span.into(),
                    })
                }
            }
            parser::ExprKind::Unary(op, operand) => Ok(parser::Expr {
                kind: parser::ExprKind::Unary(*op, Box::new(self.resolve_expr(operand)?)),
                span: expr.span.clone(),
            }),
            parser::ExprKind::Binary(op, lhs, rhs) => Ok(parser::Expr {
                kind: parser::ExprKind::Binary(
                    *op,
                    Box::new(self.resolve_expr(lhs)?),
                    Box::new(self.resolve_expr(rhs)?),
                ),
                span: expr.span.clone(),
            }),
            parser::ExprKind::Assignment(lhs, rhs) => {
                if !matches!(lhs.kind, parser::ExprKind::Var(_)) {
                    Err(SemaError::InvalidAssignmentTarget {
                        expr: *lhs.clone(),
                        span: lhs.span.clone().into(),
                    })
                } else {
                    Ok(parser::Expr {
                        kind: parser::ExprKind::Assignment(
                            Box::new(self.resolve_expr(lhs)?),
                            Box::new(self.resolve_expr(rhs)?),
                        ),
                        span: expr.span.clone(),
                    })
                }
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
