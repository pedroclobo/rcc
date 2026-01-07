use crate::parser;

use super::{Linkage, SemaError};

use std::collections::{HashMap, HashSet};

pub struct VariableResolver {
    env: Env,
    counter: u32,
}

// TODO: refactor, could maybe use the other scope object
#[derive(Debug)]
struct Scope {
    vars: HashMap<String, String>,
    defined: HashSet<String>,
    linkage: HashMap<String, Linkage>,
    tentative: HashSet<String>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            vars: HashMap::new(),
            defined: HashSet::new(),
            linkage: HashMap::new(),
            tentative: HashSet::new(),
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

    fn add_tentative(&mut self, name: &str) {
        self.tentative.insert(name.to_string());
    }

    fn remove_tentative(&mut self, name: &str) {
        self.tentative.remove(name);
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

    fn define(&mut self, name: &str) {
        self.scopes.last_mut().unwrap().define(name.to_string());
    }

    fn get(&self, name: &str) -> Option<&String> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    fn get_current_scope(&self, name: &str) -> Option<&String> {
        self.scopes.last().unwrap().get(name)
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
        let storage = decl.storage.clone();
        let span = decl.span;

        match &mut decl.kind {
            parser::DeclKind::VarDecl { name, initializer } if self.env.in_global_context() => {
                self.resolve_global_var_decl(storage, span, name, initializer)?;
            }
            parser::DeclKind::VarDecl { name, initializer } => {
                self.resolve_local_var_decl(storage, span, name, initializer)?;
            }
            parser::DeclKind::FunDecl {
                name,
                params: parameters,
                body,
            } => {
                self.resolve_fun_decl(storage, span, name, parameters, body)?;
            }
        }

        Ok(())
    }

    fn resolve_global_var_decl(
        &mut self,
        storage: Option<parser::StorageClass>,
        span: parser::Span,
        name: &str,
        initializer: &mut Option<parser::Expr>,
    ) -> Result<(), SemaError> {
        let linkage = match storage {
            Some(parser::StorageClass::Static) => Linkage::Internal,
            Some(parser::StorageClass::Extern) => Linkage::External,
            None => Linkage::External,
        };

        if let Some(old_linkage) = self.env.get_linkage(name) {
            // `extern` declarations inherit the linkage of the original declaration
            if matches!(storage, Some(parser::StorageClass::Extern)) {
                return Ok(());
            }

            if old_linkage != linkage {
                return Err(SemaError::InvalidRedeclaration {
                    name: name.to_string(),
                    span: span.into(),
                });
            }

            // Tentative definition
            if initializer.is_none() && !matches!(storage, Some(parser::StorageClass::Extern)) {
                self.env.scopes[0].add_tentative(name);
                return Ok(());
            }
        } else {
            self.env.add(name.to_string(), name.to_string(), linkage);
        }

        if let Some(initializer) = initializer {
            self.env.define(name);
            self.env.scopes[0].remove_tentative(name);
            self.resolve_expr(initializer)?;
        } else if !matches!(storage, Some(parser::StorageClass::Extern)) {
            // Tentative definition
            self.env.scopes[0].add_tentative(name);
        }

        Ok(())
    }

    fn resolve_local_var_decl(
        &mut self,
        storage: Option<parser::StorageClass>,
        span: parser::Span,
        name: &mut String,
        initializer: &mut Option<parser::Expr>,
    ) -> Result<(), SemaError> {
        match storage {
            Some(parser::StorageClass::Extern) => {
                // For an `extern` declaration, check if it is already declared in the current scope
                if let Some(existing_name) = self.env.get_current_scope(name) {
                    let existing_linkage = self.env.get_linkage(existing_name);
                    // There can be multiple `extern` declarations if they have linkage
                    if matches!(
                        existing_linkage,
                        Some(Linkage::External | Linkage::Internal)
                    ) {
                        return Ok(());
                    } else {
                        return Err(SemaError::DuplicateVariableDeclaration {
                            var: name.clone(),
                            span: span.into(),
                        });
                    }
                }

                // No declaration was found in the current scope, look in the global scope
                if let Some(existing_name) = self.env.scopes.first().unwrap().get(name) {
                    let existing_linkage =
                        self.env.scopes.first().unwrap().get_linkage(existing_name);
                    if matches!(
                        existing_linkage,
                        Some(Linkage::External | Linkage::Internal)
                    ) {
                        self.env.add(
                            name.clone(),
                            existing_name.clone(),
                            existing_linkage.unwrap(),
                        );
                    }
                } else {
                    self.env.add(name.clone(), name.clone(), Linkage::External);
                }
            }
            Some(parser::StorageClass::Static) => {
                if self.env.get_current_scope(name).is_some() {
                    return Err(SemaError::DuplicateVariableDeclaration {
                        var: name.clone(),
                        span: span.into(),
                    });
                }

                // Static local variable
                let new_name = self.make_tmp(name);
                self.env
                    .add(name.clone(), new_name.clone(), Linkage::Internal);
                *name = new_name.clone();

                if let Some(initializer) = initializer {
                    self.env.define(&new_name);
                    self.resolve_expr(initializer)?;
                }
            }
            None => {
                if self.env.get_current_scope(name).is_some() {
                    return Err(SemaError::DuplicateVariableDeclaration {
                        var: name.clone(),
                        span: span.into(),
                    });
                }

                let new_name = self.make_tmp(name);
                self.env.add(name.clone(), new_name.clone(), Linkage::None);
                *name = new_name.clone();

                if let Some(initializer) = initializer {
                    self.env.define(&new_name);
                    self.resolve_expr(initializer)?;
                }
            }
        }

        Ok(())
    }

    fn resolve_fun_decl(
        &mut self,
        storage: Option<parser::StorageClass>,
        span: parser::Span,
        name: &mut String,
        parameters: &mut Vec<parser::Param>,
        body: &mut Option<parser::Block>,
    ) -> Result<(), SemaError> {
        let linkage = match storage {
            Some(parser::StorageClass::Static) => Linkage::Internal,
            Some(parser::StorageClass::Extern) => Linkage::External,
            None => self.env.get_linkage(name).unwrap_or(Linkage::External),
        };

        if self.env.contains(name) {
            let existing_linkage = self.env.get_linkage(name);

            if !matches!(storage, Some(parser::StorageClass::Extern)) {
                if let Some(existing_linkage) = existing_linkage
                    && existing_linkage != linkage
                {
                    return Err(SemaError::InvalidRedeclaration {
                        name: name.clone(),
                        span: span.into(),
                    });
                }

                // Function redefined as variable
                if matches!(existing_linkage, Some(Linkage::None)) {
                    return Err(SemaError::InvalidRedefinition {
                        name: name.clone(),
                        span: span.into(),
                    });
                }
            }

            if self.env.is_defined(name) && body.is_some() {
                return Err(SemaError::DuplicateVariableDeclaration {
                    var: name.clone(),
                    span: span.into(),
                });
            }
        } else {
            self.env.add(name.clone(), name.clone(), linkage);
        }

        if body.is_some() {
            if !self.env.in_global_context() {
                return Err(SemaError::InvalidFunctionDeclaration {
                    func: name.to_string(),
                    span: span.into(),
                });
            }
            self.env.define(name);
        }

        if !self.env.in_global_context() && matches!(storage, Some(parser::StorageClass::Static)) {
            return Err(SemaError::InvalidFunctionDeclaration {
                func: name.to_string(),
                span: span.into(),
            });
        }

        self.env.push();
        for param in parameters {
            self.resolve_param(param)?;
        }
        if let Some(body) = body {
            self.resolve_block(body)?;
        }
        self.env.pop();

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
            .add(param.name.clone(), new_name.clone(), Linkage::None);
        self.env.define(&new_name);
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
                // Storage classes are not allowed in for loop declarations
                if decl.storage.is_some() {
                    return Err(SemaError::InvalidStorageClassInForLoop {
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
