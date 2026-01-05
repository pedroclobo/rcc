use crate::parser;

use super::SemaError;

use std::collections::{HashMap, HashSet};

pub struct VariableResolver {
    env: Env,
    counter: u32,
}

#[derive(Debug, Clone, Copy)]
enum Linkage {
    External,
    Internal,
}

#[derive(Debug)]
struct Scope {
    vars: HashMap<String, String>,
    defined: HashSet<String>,
    linkage: HashMap<String, Linkage>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            vars: HashMap::new(),
            defined: HashSet::new(),
            linkage: HashMap::new(),
        }
    }

    fn add(&mut self, name: String, value: String) {
        self.vars.insert(name.clone(), value);
    }

    fn define(&mut self, name: String) {
        self.defined.insert(name);
    }

    fn is_defined(&self, name: &str) -> bool {
        self.defined.contains(name)
    }

    fn contains(&self, name: &str) -> bool {
        self.vars.contains_key(name)
    }

    fn get(&self, name: &str) -> Option<&String> {
        self.vars.get(name)
    }

    fn set_linkage(&mut self, name: String, linkage: Linkage) {
        self.linkage.insert(name, linkage);
    }

    fn get_linkage(&self, name: &str) -> Option<Linkage> {
        self.linkage.get(name).copied()
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

    fn add(&mut self, name: String, value: String, linkage: Linkage) {
        self.scopes.last_mut().unwrap().add(name.clone(), value);
        self.scopes.last_mut().unwrap().set_linkage(name, linkage);
    }

    fn contains(&self, name: &str) -> bool {
        self.scopes.last().unwrap().contains(name)
    }

    fn is_defined(&self, name: &str) -> bool {
        self.scopes.iter().any(|scope| scope.is_defined(name))
    }

    fn define(&mut self, name: String) {
        self.scopes.last_mut().unwrap().define(name);
    }

    fn get(&self, name: &str) -> Option<&String> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    fn in_global_context(&self) -> bool {
        self.scopes.len() == 1
    }

    fn get_linkage(&self, name: &str) -> Option<Linkage> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_linkage(name))
    }
}

impl VariableResolver {
    pub fn new() -> Self {
        VariableResolver {
            env: Env::new(),
            counter: 0,
        }
    }

    pub fn run(&mut self, program: &mut parser::Program) -> Result<(), SemaError> {
        for decl in &mut program.decls {
            self.resolve_decl(decl)?;
        }
        Ok(())
    }

    fn resolve_block(&mut self, block: &mut parser::Block) -> Result<(), SemaError> {
        for item in block {
            match item {
                parser::BlockItem::Decl(decl) => {
                    self.resolve_decl(decl)?;
                }
                parser::BlockItem::Stmt(stmt) => {
                    self.resolve_stmt(stmt)?;
                }
            }
        }
        Ok(())
    }

    fn resolve_decl(&mut self, decl: &mut parser::Decl) -> Result<(), SemaError> {
        match &mut decl.kind {
            parser::DeclKind::VarDecl { name, initializer } => {
                if self.env.contains(name) {
                    return Err(SemaError::DuplicateVariableDeclaration {
                        var: name.to_string(),
                        span: decl.span.into(),
                    });
                }

                let new_name = self.make_tmp(name);
                self.env
                    .add(name.clone(), new_name.clone(), Linkage::Internal);
                self.env.define(new_name.clone());

                *name = new_name;
                if let Some(initializer) = initializer {
                    self.resolve_expr(initializer)?;
                }
            }
            parser::DeclKind::FunDecl {
                name,
                params: parameters,
                body,
            } => {
                if self.env.contains(name) {
                    if self.env.is_defined(name) {
                        return Err(SemaError::DuplicateVariableDeclaration {
                            var: name.to_string(),
                            span: decl.span.into(),
                        });
                    }
                    if matches!(self.env.get_linkage(name), Some(Linkage::Internal)) {
                        return Err(SemaError::InvalidRedefinition {
                            name: name.to_string(),
                            span: decl.span.into(),
                        });
                    }
                }

                let in_global_context = self.env.in_global_context();

                self.env.add(name.clone(), name.clone(), Linkage::External);
                if body.is_some() {
                    self.env.define(name.clone());
                }

                self.env.push();
                for param in parameters {
                    self.resolve_param(param)?;
                }
                if let Some(body) = body {
                    if !in_global_context {
                        return Err(SemaError::InvalidFunctionDeclaration {
                            func: name.to_string(),
                            span: decl.span.into(),
                        });
                    }
                    self.resolve_block(body)?;
                }
                self.env.pop();
            }
        }

        Ok(())
    }

    fn resolve_param(&mut self, param: &mut parser::Param) -> Result<(), SemaError> {
        if self.env.contains(&param.name) {
            return Err(SemaError::DuplicateVariableDeclaration {
                var: param.name.to_string(),
                span: param.span.into(),
            });
        }

        let new_name = self.make_tmp(&param.name);
        self.env
            .add(param.name.clone(), new_name.clone(), Linkage::Internal);
        self.env.define(new_name.clone());
        param.name = new_name.clone();

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut parser::Stmt) -> Result<(), SemaError> {
        match &mut stmt.kind {
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
            parser::StmtKind::Labeled { stmt, .. } => {
                self.resolve_stmt(stmt)?;
            }
            parser::StmtKind::Goto(_) => {}
            parser::StmtKind::Block(block) => {
                self.env.push();
                self.resolve_block(block)?;
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
            parser::ForInit::Decl(decl) => {
                if matches!(&decl.kind, parser::DeclKind::FunDecl { .. }) {
                    return Err(SemaError::InvalidFunctionDefinition {
                        span: decl.span.into(),
                    });
                }
                self.resolve_decl(decl)
            }
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
            parser::ExprKind::FunctionCall {
                identifier,
                arguments,
            } => {
                self.resolve_expr(identifier)?;
                for arg in arguments {
                    self.resolve_expr(arg)?;
                }
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
