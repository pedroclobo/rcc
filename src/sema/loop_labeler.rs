use super::SemaError;
use crate::parser::{self, Label};

#[derive(Debug)]
struct Stack {
    labels: Vec<String>,
}

impl Stack {
    fn new() -> Self {
        Stack { labels: Vec::new() }
    }

    fn push(&mut self, label: String) {
        self.labels.push(label);
    }

    fn pop(&mut self) {
        self.labels.pop();
    }

    fn get(&self) -> Option<&String> {
        self.labels.last()
    }
}

pub struct LoopLabeler {
    labels: Stack,
    counter: u32,
}

impl LoopLabeler {
    pub fn new() -> Self {
        LoopLabeler {
            labels: Stack::new(),
            counter: 0,
        }
    }

    pub fn run(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        self.label(program)?;
        Ok(())
    }

    fn label(&mut self, program: &mut parser::Program<'_>) -> Result<(), SemaError> {
        for function in &mut program.functions {
            for instruction in &mut function.body {
                if let parser::BlockItem::Stmt(stmt) = instruction {
                    self.label_stmt(stmt)?;
                }
            }
        }
        Ok(())
    }

    fn label_stmt(&mut self, stmt: &mut parser::Stmt) -> Result<(), SemaError> {
        let parser::Stmt { kind, .. } = stmt;

        match kind {
            parser::StmtKind::While { body, label, .. } => {
                let new_label = self.make_label();
                *label = Some(Label {
                    name: new_label.clone(),
                    span: stmt.span,
                });
                self.labels.push(new_label);
                self.label_stmt(body)?;
                self.labels.pop();
            }
            parser::StmtKind::DoWhile { body, label, .. } => {
                let new_label = self.make_label();
                *label = Some(Label {
                    name: new_label.clone(),
                    span: stmt.span,
                });
                self.labels.push(new_label);
                self.label_stmt(body)?;
                self.labels.pop();
            }
            parser::StmtKind::For { body, label, .. } => {
                let new_label = self.make_label();
                *label = Some(Label {
                    name: new_label.clone(),
                    span: stmt.span,
                });
                self.labels.push(new_label);
                self.label_stmt(body)?;
                self.labels.pop();
            }
            parser::StmtKind::Break(_) => {
                if let Some(label) = self.labels.get() {
                    let label = Label {
                        name: label.clone(),
                        span: stmt.span,
                    };
                    *kind = parser::StmtKind::Break(Some(label));
                } else {
                    return Err(SemaError::BreakOutsideLoop {
                        span: stmt.span.into(),
                    });
                }
            }
            parser::StmtKind::Continue(_) => {
                if let Some(label) = self.labels.get() {
                    let label = Label {
                        name: label.clone(),
                        span: stmt.span,
                    };
                    *kind = parser::StmtKind::Continue(Some(label));
                } else {
                    return Err(SemaError::ContinueOutsideLoop {
                        span: stmt.span.into(),
                    });
                }
            }
            parser::StmtKind::If { then, r#else, .. } => {
                self.label_stmt(then)?;
                if let Some(r#else) = r#else {
                    self.label_stmt(r#else)?;
                }
            }
            parser::StmtKind::Labeled { stmt, .. } => {
                self.label_stmt(stmt)?;
            }
            parser::StmtKind::Block(block) => {
                for item in block.items.iter_mut() {
                    if let parser::BlockItem::Stmt(stmt) = item {
                        self.label_stmt(stmt)?;
                    }
                }
            }
            parser::StmtKind::Return(_) => {}
            parser::StmtKind::Expr(_) => {}
            parser::StmtKind::Goto(_) => {}
        };

        Ok(())
    }

    fn make_label(&mut self) -> String {
        let label = format!("{}", self.counter);
        self.counter += 1;
        label
    }
}
