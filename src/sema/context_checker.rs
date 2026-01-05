use std::collections::HashSet;

use super::SemaError;
use crate::parser::{self, Span};

pub struct ContextChecker {
    context: Stack,
}

impl<'a> ContextChecker {
    pub fn new() -> Self {
        ContextChecker {
            context: Stack::new(),
        }
    }

    pub fn run(&mut self, program: &'a parser::Program) -> Result<(), SemaError> {
        self.check(program)?;
        Ok(())
    }

    fn check(&mut self, program: &'a parser::Program) -> Result<(), SemaError> {
        for decl in &program.decls {
            if let parser::DeclKind::FunDecl { body, .. } = &decl.kind
                && let Some(body) = body
            {
                for instruction in body {
                    if let parser::BlockItem::Stmt(stmt) = instruction {
                        self.check_stmt(stmt)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &'a parser::Stmt) -> Result<(), SemaError> {
        let parser::Stmt { kind, .. } = stmt;

        match kind {
            parser::StmtKind::While { body, .. }
            | parser::StmtKind::DoWhile { body, .. }
            | parser::StmtKind::For { body, .. } => {
                self.context.push(ContextKind::Loop);
                self.check_stmt(body)?;
                self.context.pop();
            }
            parser::StmtKind::Break => {
                if !self.context.inside_loop_or_switch() {
                    return Err(SemaError::BreakOutsideLoopOrSwitch {
                        span: stmt.span.into(),
                    });
                }
            }
            parser::StmtKind::Continue => {
                if !self.context.inside_loop() {
                    return Err(SemaError::ContinueOutsideLoop {
                        span: stmt.span.into(),
                    });
                }
            }
            parser::StmtKind::Switch { body, .. } => {
                let mut seen = HashSet::new();
                for case in Self::collect_cases(body)? {
                    if !seen.insert(case.expr) {
                        return Err(SemaError::DuplicateCase {
                            value: case.expr.to_string(),
                            span: case.span.into(),
                        });
                    }
                }

                self.context.push(ContextKind::Switch);
                self.check_stmt(body)?;
                self.context.pop();
            }
            parser::StmtKind::Case { body, .. } => {
                if !self.context.inside_switch() {
                    return Err(SemaError::CaseOutsideSwitch {
                        span: stmt.span.into(),
                    });
                }
                self.check_stmt(body)?;
            }
            parser::StmtKind::Default { body } => {
                if !self.context.inside_switch() {
                    return Err(SemaError::DefaultOutsideSwitch {
                        span: stmt.span.into(),
                    });
                }
                self.check_stmt(body)?;
            }
            parser::StmtKind::If { then, r#else, .. } => {
                self.check_stmt(then)?;
                if let Some(r#else) = r#else {
                    self.check_stmt(r#else)?;
                }
            }
            parser::StmtKind::Labeled { stmt, .. } => {
                self.check_stmt(stmt)?;
            }
            parser::StmtKind::Block(block) => {
                for item in &block.items {
                    if let parser::BlockItem::Stmt(stmt) = item {
                        self.check_stmt(stmt)?;
                    }
                }
            }
            parser::StmtKind::Return(_) => {}
            parser::StmtKind::Expr(_) => {}
            parser::StmtKind::Goto(_) => {}
        };

        Ok(())
    }

    fn collect_cases(stmt: &'a parser::Stmt) -> Result<Vec<CaseInfo>, SemaError> {
        let mut cases = Vec::new();
        Self::_collect_cases(stmt, &mut cases)?;
        Ok(cases)
    }

    fn _collect_cases(stmt: &'a parser::Stmt, cases: &mut Vec<CaseInfo>) -> Result<(), SemaError> {
        match &stmt.kind {
            parser::StmtKind::If { then, r#else, .. } => {
                Self::_collect_cases(then, cases)?;
                if let Some(r#else) = r#else {
                    Self::_collect_cases(r#else, cases)?;
                }
            }
            parser::StmtKind::Labeled { stmt, .. } => {
                Self::_collect_cases(stmt, cases)?;
            }
            parser::StmtKind::Block(block) => {
                for item in &block.items {
                    if let parser::BlockItem::Stmt(stmt) = &item {
                        Self::_collect_cases(stmt, cases)?;
                    }
                }
            }
            parser::StmtKind::While { body, .. }
            | parser::StmtKind::DoWhile { body, .. }
            | parser::StmtKind::For { body, .. } => {
                Self::_collect_cases(body, cases)?;
            }
            parser::StmtKind::Case { body, expr } => {
                match expr.kind {
                    parser::ExprKind::Constant(n) => {
                        cases.push(CaseInfo {
                            expr: n,
                            span: stmt.span,
                        });
                        Self::_collect_cases(body, cases)?;
                    }
                    _ => {
                        return Err(SemaError::InvalidCaseExpression {
                            span: expr.span.into(),
                        });
                    }
                };
            }
            parser::StmtKind::Default { .. }
            | parser::StmtKind::Return(_)
            | parser::StmtKind::Goto(_)
            | parser::StmtKind::Switch { .. }
            | parser::StmtKind::Break
            | parser::StmtKind::Continue
            | parser::StmtKind::Expr(_) => {}
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Stack {
    stack: Vec<ContextKind>,
}

impl Stack {
    fn new() -> Self {
        Stack { stack: Vec::new() }
    }

    fn push(&mut self, context: ContextKind) {
        self.stack.push(context);
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn inside_loop(&self) -> bool {
        self.stack
            .iter()
            .any(|kind| matches!(kind, ContextKind::Loop))
    }

    fn inside_switch(&self) -> bool {
        self.stack
            .iter()
            .any(|kind| matches!(kind, ContextKind::Switch))
    }

    fn inside_loop_or_switch(&self) -> bool {
        self.inside_loop() || self.inside_switch()
    }
}

#[derive(Debug)]
enum ContextKind {
    Loop,
    Switch,
}

struct CaseInfo {
    expr: i32,
    span: Span,
}
