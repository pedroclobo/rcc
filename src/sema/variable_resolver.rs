use crate::parser;

use super::SemaError;

use std::collections::{HashMap, HashSet};

pub struct VariableResolver {
    env: Env,
    labels: HashSet<String>,
    counter: u32,
}

#[derive(Debug)]
struct Scope {
    vars: HashMap<String, String>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            vars: HashMap::new(),
        }
    }

    fn add(&mut self, name: String, value: String) {
        self.vars.insert(name, value);
    }

    fn contains(&self, name: &str) -> bool {
        self.vars.contains_key(name)
    }

    fn get(&self, name: &str) -> Option<&String> {
        self.vars.get(name)
    }
}

#[derive(Debug)]
struct Env {
    scopes: Vec<Scope>,
}

impl Env {
    fn new() -> Self {
        Env {
            scopes: vec![Scope::new()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }

    fn add(&mut self, name: String, value: String) {
        self.scopes.last_mut().unwrap().add(name, value);
    }

    fn contains(&self, name: &str) -> bool {
        self.scopes.last().unwrap().contains(name)
    }

    fn get(&self, name: &str) -> Option<&String> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
}

impl VariableResolver {
    pub fn new() -> Self {
        VariableResolver {
            env: Env::new(),
            labels: HashSet::new(),
            counter: 0,
        }
    }

    pub fn run(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
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

    fn resolve_decl(&mut self, decl: &mut parser::Decl) -> Result<(), SemaError> {
        let parser::Decl { kind, .. } = decl;

        let name = kind.name.clone();
        if self.env.contains(&name) {
            return Err(SemaError::DuplicateVariableDeclaration {
                var: name.to_string(),
                span: decl.span.into(),
            });
        }

        let new_name = self.make_tmp(&name);
        self.env.add(name, new_name.clone());

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
            parser::StmtKind::Block(block) => {
                self.env.push();
                for item in block.items.iter_mut() {
                    match item {
                        parser::BlockItem::Decl(decl) => {
                            self.resolve_decl(decl)?;
                        }
                        parser::BlockItem::Stmt(stmt) => {
                            self.resolve_stmt(stmt)?;
                        }
                    }
                }
                self.env.pop();
            }
            parser::StmtKind::Break => {}
            parser::StmtKind::Continue => {}
            parser::StmtKind::While { cond, body, .. }
            | parser::StmtKind::DoWhile { body, cond, .. } => {
                self.resolve_stmt(body)?;
                self.resolve_expr(cond)?;
            }
            parser::StmtKind::For {
                init,
                cond,
                post,
                body,
                ..
            } => {
                self.env.push();
                self.resolve_for_init(init)?;
                if let Some(cond) = cond {
                    self.resolve_expr(cond)?;
                }
                if let Some(post) = post {
                    self.resolve_expr(post)?;
                }
                self.resolve_stmt(body)?;
                self.env.pop();
            }
            parser::StmtKind::Switch { expr, body } | parser::StmtKind::Case { expr, body } => {
                self.resolve_expr(expr)?;
                self.resolve_stmt(body)?;
            }
            parser::StmtKind::Default { body } => {
                self.resolve_stmt(body)?;
            }
        };

        Ok(())
    }

    fn resolve_for_init(&mut self, for_init: &mut parser::ForInit) -> Result<(), SemaError> {
        match for_init {
            parser::ForInit::Decl(decl) => self.resolve_decl(decl),
            parser::ForInit::Expr(Some(expr)) => self.resolve_expr(expr),
            parser::ForInit::Expr(None) => Ok(()),
        }
    }

    fn resolve_expr(&mut self, expr: &mut parser::Expr) -> Result<(), SemaError> {
        let parser::Expr { kind, .. } = expr;

        match kind {
            parser::ExprKind::Constant(_) => {}
            parser::ExprKind::Var(name) => {
                if let Some(var) = self.env.get(name) {
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
