use crate::parser::{self, BlockItem};

use super::SemaError;

use std::collections::HashSet;

pub struct LabelResolver {
    labels: HashSet<String>,
}

impl LabelResolver {
    pub fn new() -> Self {
        LabelResolver {
            labels: HashSet::new(),
        }
    }

    pub fn run(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        self.collect_labels(program)?;
        self.resolve_labels(program)?;
        Ok(())
    }

    fn collect_labels(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        for function in &mut program.functions {
            for instruction in &mut function.body {
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
                if self.labels.contains(&label.name) {
                    return Err(SemaError::DuplicateLabelDeclaration {
                        label: label.name.clone(),
                        span: label.span.into(),
                    });
                }
                self.labels.insert(label.name.clone());
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

    fn resolve_labels(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        for function in &mut program.functions {
            for instruction in &mut function.body {
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
                if !self.labels.contains(&label.name) {
                    return Err(SemaError::UndeclaredLabel {
                        label: label.name.clone(),
                        span: label.span.into(),
                    });
                }
            }
            parser::StmtKind::Return(_) => {}
            parser::StmtKind::Expr(_) => {}
            parser::StmtKind::Break => {}
            parser::StmtKind::Continue => {}
        };

        Ok(())
    }
}

impl Default for LabelResolver {
    fn default() -> Self {
        Self::new()
    }
}
