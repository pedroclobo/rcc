use crate::parser;

pub struct InstructionCollector {}

impl<'a> InstructionCollector {
    pub fn collect_default(stmt: &'a parser::Stmt) -> Option<&'a parser::Stmt> {
        match &stmt.kind {
            parser::StmtKind::If { then, r#else, .. } => {
                if let Some(default) = Self::collect_default(then) {
                    return Some(default);
                }
                if let Some(r#else) = r#else {
                    return Self::collect_default(r#else).or(None);
                }
                None
            }
            parser::StmtKind::Labeled { stmt, .. } => Self::collect_default(stmt),
            parser::StmtKind::Block(block) => {
                for item in &block.items {
                    if let parser::BlockItem::Stmt(stmt) = &item
                        && let Some(default) = Self::collect_default(stmt)
                    {
                        return Some(default);
                    }
                }
                None
            }
            parser::StmtKind::While { body, .. }
            | parser::StmtKind::DoWhile { body, .. }
            | parser::StmtKind::For { body, .. } => {
                if let Some(default) = Self::collect_default(body) {
                    return Some(default);
                }
                None
            }
            parser::StmtKind::Default { .. } => Some(stmt),
            parser::StmtKind::Case { .. }
            | parser::StmtKind::Return(_)
            | parser::StmtKind::Goto(_)
            | parser::StmtKind::Switch { .. }
            | parser::StmtKind::Break
            | parser::StmtKind::Continue
            | parser::StmtKind::Expr(_) => None,
        }
    }

    pub fn collect_cases(stmt: &'a parser::Stmt) -> Vec<&'a parser::Stmt> {
        let mut cases = Vec::new();
        Self::_collect_cases(stmt, &mut cases);
        cases
    }

    fn _collect_cases(stmt: &'a parser::Stmt, cases: &mut Vec<&'a parser::Stmt>) {
        match &stmt.kind {
            parser::StmtKind::If { then, r#else, .. } => {
                Self::_collect_cases(then, cases);
                if let Some(r#else) = r#else {
                    Self::_collect_cases(r#else, cases);
                }
            }
            parser::StmtKind::Labeled { stmt, .. } => {
                Self::_collect_cases(stmt, cases);
            }
            parser::StmtKind::Block(block) => {
                for item in &block.items {
                    if let parser::BlockItem::Stmt(stmt) = &item {
                        Self::_collect_cases(stmt, cases);
                    }
                }
            }
            parser::StmtKind::While { body, .. }
            | parser::StmtKind::DoWhile { body, .. }
            | parser::StmtKind::For { body, .. } => {
                Self::_collect_cases(body, cases);
            }
            parser::StmtKind::Case { body, .. } => {
                cases.push(stmt);
                Self::_collect_cases(body, cases);
            }
            parser::StmtKind::Default { .. }
            | parser::StmtKind::Return(_)
            | parser::StmtKind::Goto(_)
            | parser::StmtKind::Switch { .. }
            | parser::StmtKind::Break
            | parser::StmtKind::Continue
            | parser::StmtKind::Expr(_) => {}
        }
    }
}
