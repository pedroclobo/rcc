use std::collections::HashMap;

use crate::parser;

use super::SemaError;

#[derive(Debug)]
struct Scope {
    types: HashMap<String, parser::Type>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            types: HashMap::new(),
        }
    }

    fn add(&mut self, name: String, ty: parser::Type) {
        self.types.insert(name, ty);
    }

    fn get(&self, name: &str) -> Option<&parser::Type> {
        self.types.get(name)
    }
}

#[derive(Debug)]
struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope::new()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }

    fn add(&mut self, name: String, ty: parser::Type) {
        self.scopes.last_mut().unwrap().add(name, ty);
    }

    fn get(&self, name: &str) -> Option<&parser::Type> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
}

pub struct TypeChecker {
    symtab: SymbolTable,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker::default()
    }

    pub fn run(&mut self, program: &parser::Program) -> Result<(), SemaError> {
        for decl in &program.decls {
            self.check_decl(decl)?;
        }
        Ok(())
    }

    fn check_decl(&mut self, decl: &parser::Decl) -> Result<(), SemaError> {
        match &decl.kind {
            parser::DeclKind::VarDecl { name, initializer } => {
                self.symtab.add(name.clone(), parser::Type::Int);
                if let Some(initializer) = initializer {
                    self.check_expr(initializer)?;
                }
            }
            parser::DeclKind::FunDecl { name, params, body } => {
                let ty = parser::Type::FunType {
                    ret: Box::new(parser::Type::Int),
                    params: vec![parser::Type::Int; params.len()],
                };

                if let Some(ety) = self.symtab.get(name)
                    && ety != &ty
                {
                    return Err(SemaError::InvalidRedeclaration {
                        name: name.clone(),
                        span: decl.span.into(),
                    });
                }

                self.symtab.add(name.clone(), ty);
                self.symtab.push();
                for param in params {
                    self.symtab.add(param.name.clone(), parser::Type::Int);
                }
                if let Some(body) = body {
                    self.check_block(body)?;
                }
                self.symtab.pop();
            }
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: &parser::Expr) -> Result<parser::Type, SemaError> {
        match &expr.kind {
            parser::ExprKind::Constant(_) => Ok(parser::Type::Int),
            parser::ExprKind::Var(v) => Ok(self.symtab.get(v).expect("undefined variable").clone()),
            parser::ExprKind::Unary(_, expr) => {
                let ty = self.check_expr(expr)?;
                if matches!(ty, parser::Type::FunType { .. }) {
                    return Err(SemaError::UnaryExpressionTypeMismatch {
                        ty,
                        span: expr.span.into(),
                    });
                }
                Ok(parser::Type::Int)
            }
            parser::ExprKind::Binary(_, lhs, rhs) => {
                let lhs_ty = self.check_expr(lhs)?;
                let rhs_ty = self.check_expr(rhs)?;
                if lhs_ty != rhs_ty {
                    return Err(SemaError::BinaryExpressionTypeMismatch {
                        lhs_ty,
                        rhs_ty,
                        span: expr.span.into(),
                    });
                }
                Ok(parser::Type::Int)
            }
            parser::ExprKind::Assignment(lhs, rhs) => {
                let lhs_ty = self.check_expr(lhs)?;
                let rhs_ty = self.check_expr(rhs)?;
                if lhs_ty != rhs_ty {
                    Err(SemaError::AssignmentTypeMismatch {
                        lhs_ty,
                        rhs_ty,
                        span: expr.span.into(),
                    })
                } else {
                    Ok(lhs_ty)
                }
            }
            parser::ExprKind::Conditional { cond, then, r#else } => {
                self.check_expr(cond)?;
                self.check_expr(then)?;
                self.check_expr(r#else)?;
                Ok(parser::Type::Int)
            }
            parser::ExprKind::FunctionCall {
                identifier,
                arguments,
            } => {
                self.check_expr(identifier)?;
                let mut arg_tys = Vec::new();
                for arg in arguments {
                    arg_tys.push(self.check_expr(arg)?);
                }

                match &identifier.kind {
                    parser::ExprKind::Var(v) => {
                        let ty = self.symtab.get(v).expect("function not found");
                        if let parser::Type::FunType { params, .. } = ty {
                            if params.len() != arg_tys.len() {
                                return Err(SemaError::FunctionArgumentCountMismatch {
                                    name: v.clone(),
                                    expected: params.len(),
                                    actual: arg_tys.len(),
                                    span: identifier.span.into(),
                                });
                            }

                            for (param_ty, arg_ty) in params.iter().zip(arg_tys.iter()) {
                                if param_ty != arg_ty {
                                    return Err(SemaError::FunctionArgumentTypeMismatch {
                                        name: v.clone(),
                                        expected_type: param_ty.clone(),
                                        actual_type: arg_ty.clone(),
                                        span: identifier.span.into(),
                                    });
                                }
                            }
                        } else {
                            return Err(SemaError::InvalidFunctionCallable {
                                name: v.clone(),
                                span: identifier.span.into(),
                            });
                        }
                    }
                    _ => panic!("invalid function call"),
                };

                Ok(parser::Type::Int)
            }
        }
    }

    fn check_block(&mut self, body: &parser::Block) -> Result<(), SemaError> {
        for item in body {
            match item {
                parser::BlockItem::Decl(decl) => self.check_decl(decl)?,
                parser::BlockItem::Stmt(stmt) => self.check_stmt(stmt)?,
            }
        }

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &parser::Stmt) -> Result<(), SemaError> {
        match &stmt.kind {
            parser::StmtKind::Return(expr) => {
                self.check_expr(expr)?;
            }
            parser::StmtKind::Expr(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(expr)?;
                }
            }
            parser::StmtKind::If { cond, then, r#else } => {
                self.check_expr(cond)?;
                self.check_stmt(then)?;
                if let Some(r#else) = r#else {
                    self.check_stmt(r#else)?;
                }
            }
            parser::StmtKind::Labeled { stmt, .. } => {
                self.check_stmt(stmt)?;
            }
            parser::StmtKind::Goto(_) => {}
            parser::StmtKind::Block(block) => {
                self.symtab.push();
                self.check_block(block)?;
                self.symtab.pop();
            }
            parser::StmtKind::Break => {}
            parser::StmtKind::Continue => {}
            parser::StmtKind::While { cond, body } => {
                self.check_expr(cond)?;
                self.check_stmt(body)?;
            }
            parser::StmtKind::DoWhile { body, cond } => {
                self.check_stmt(body)?;
                self.check_expr(cond)?;
            }
            parser::StmtKind::For {
                init,
                cond,
                post,
                body,
            } => {
                self.symtab.push();
                self.check_for_init(init)?;
                if let Some(cond) = cond {
                    self.check_expr(cond)?;
                }
                if let Some(post) = post {
                    self.check_expr(post)?;
                }
                self.check_stmt(body)?;
                self.symtab.pop();
            }
            parser::StmtKind::Switch { expr, body } | parser::StmtKind::Case { expr, body } => {
                let ty = self.check_expr(expr)?;
                if matches!(ty, parser::Type::FunType { .. }) {
                    return Err(SemaError::SwitchStatementTypeMismatch {
                        ty,
                        span: expr.span.into(),
                    });
                }
                self.check_stmt(body)?;
            }
            parser::StmtKind::Default { body } => {
                self.check_stmt(body)?;
            }
        }

        Ok(())
    }

    fn check_for_init(&mut self, init: &parser::ForInit) -> Result<(), SemaError> {
        match init {
            parser::ForInit::Decl(decl) => self.check_decl(decl)?,
            parser::ForInit::Expr(Some(expr)) => {
                self.check_expr(expr)?;
            }
            parser::ForInit::Expr(None) => {}
        };

        Ok(())
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        TypeChecker {
            symtab: SymbolTable::new(),
        }
    }
}
