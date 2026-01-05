use crate::parser::{self, BlockItem};

use super::SemaError;

use std::collections::HashMap;

pub struct LabelResolver {
    labels: HashMap<String, String>,
    counter: u32,
}

impl LabelResolver {
    pub fn new() -> Self {
        LabelResolver {
            labels: HashMap::new(),
            counter: 0,
        }
    }

    pub fn run(&mut self, program: &mut parser::Program) -> Result<(), SemaError> {
        for decl in &mut program.decls {
            self.labels.clear();
            self.collect_decl(decl)?;
            self.resolve_decl(decl)?;
        }

        Ok(())
    }

    fn collect_decl(&mut self, decl: &mut parser::Decl) -> Result<(), SemaError> {
        if let parser::DeclKind::FunDecl { body, .. } = &mut decl.kind
            && let Some(body) = body
        {
            for instruction in body {
                if let BlockItem::Stmt(stmt) = instruction {
                    self.collect_stmt(stmt)?;
                }
            }
        }
        Ok(())
    }

    fn collect_stmt(&mut self, stmt: &mut parser::Stmt) -> Result<(), SemaError> {
        let parser::Stmt { kind, .. } = stmt;

        match kind {
            parser::StmtKind::Labeled { label, stmt } => {
                if self.labels.contains_key(&label.name) {
                    return Err(SemaError::DuplicateLabelDeclaration {
                        label: label.name.clone(),
                        span: label.span.into(),
                    });
                }
                let new_label = self.mk_label(label.name.clone());
                self.labels.insert(label.name.clone(), new_label.clone());
                label.name = new_label;
                self.collect_stmt(stmt)?;
            }
            parser::StmtKind::Block(block) => {
                for item in block.items.iter_mut() {
                    if let parser::BlockItem::Stmt(stmt) = item {
                        self.collect_stmt(stmt)?;
                    }
                }
            }
            parser::StmtKind::If { then, r#else, .. } => {
                self.collect_stmt(then)?;
                if let Some(r#else) = r#else {
                    self.collect_stmt(r#else)?;
                }
            }
            parser::StmtKind::While { body, .. }
            | parser::StmtKind::DoWhile { body, .. }
            | parser::StmtKind::For { body, .. } => {
                self.collect_stmt(body)?;
            }
            parser::StmtKind::Switch { body, .. }
            | parser::StmtKind::Case { body, .. }
            | parser::StmtKind::Default { body } => {
                self.collect_stmt(body)?;
            }
            parser::StmtKind::Return(_)
            | parser::StmtKind::Expr(_)
            | parser::StmtKind::Goto(_)
            | parser::StmtKind::Break
            | parser::StmtKind::Continue => {}
        };

        Ok(())
    }

    fn resolve_decl(&mut self, decl: &mut parser::Decl) -> Result<(), SemaError> {
        if let parser::DeclKind::FunDecl { body, .. } = &mut decl.kind
            && let Some(body) = body
        {
            for instruction in body {
                if let BlockItem::Stmt(stmt) = instruction {
                    self.resolve_stmt(stmt)?;
                }
            }
        }
        Ok(())
    }

    fn resolve_stmt(&self, stmt: &mut parser::Stmt) -> Result<(), SemaError> {
        let parser::Stmt { kind, .. } = stmt;

        match kind {
            parser::StmtKind::If { then, r#else, .. } => {
                self.resolve_stmt(then)?;
                if let Some(r#else) = r#else {
                    self.resolve_stmt(r#else)?;
                }
            }
            parser::StmtKind::Labeled { stmt, .. } => {
                self.resolve_stmt(stmt)?;
            }
            parser::StmtKind::Block(block) => {
                for item in block.items.iter_mut() {
                    if let parser::BlockItem::Stmt(stmt) = item {
                        self.resolve_stmt(stmt)?;
                    }
                }
            }
            parser::StmtKind::While { body, .. }
            | parser::StmtKind::DoWhile { body, .. }
            | parser::StmtKind::For { body, .. } => {
                self.resolve_stmt(body)?;
            }
            parser::StmtKind::Switch { body, .. }
            | parser::StmtKind::Case { body, .. }
            | parser::StmtKind::Default { body } => {
                self.resolve_stmt(body)?;
            }
            parser::StmtKind::Goto(label) => {
                if !self.labels.contains_key(&label.name) {
                    return Err(SemaError::UndeclaredLabel {
                        label: label.name.clone(),
                        span: label.span.into(),
                    });
                }
                label.name = self
                    .labels
                    .get(&label.name)
                    .expect("undeclared label")
                    .clone();
            }
            parser::StmtKind::Return(_) => {}
            parser::StmtKind::Expr(_) => {}
            parser::StmtKind::Break => {}
            parser::StmtKind::Continue => {}
        };

        Ok(())
    }

    fn mk_label(&mut self, name: String) -> String {
        let label = format!("{}.{}", name, self.counter);
        self.counter += 1;
        label
    }
}

impl Default for LabelResolver {
    fn default() -> Self {
        Self::new()
    }
}
