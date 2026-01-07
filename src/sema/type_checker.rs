use std::collections::HashMap;

use super::Linkage;
use crate::parser;

use super::SemaError;

#[derive(Debug)]
pub struct Scope {
    syms: HashMap<String, Symbol>,
}

// TODO: host
#[derive(Debug)]
pub struct Symbol {
    pub ty: parser::Type,
    pub linkage: Linkage,
    pub init: InitValue,
}

// TODO: host
#[derive(Debug, Clone, Copy)]
pub enum InitValue {
    Local,
    None,
    Tentative,
    Const(i32),
    Fun,
}

impl Scope {
    fn new() -> Self {
        Scope {
            syms: HashMap::new(),
        }
    }

    fn add(&mut self, name: &str, ty: parser::Type, linkage: Linkage, init: InitValue) {
        self.syms
            .insert(name.to_string(), Symbol { ty, linkage, init });
    }

    fn get_ty(&self, name: &str) -> Option<&parser::Type> {
        self.syms.get(name).map(|sym| &sym.ty)
    }

    fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.syms.get(name)
    }

    fn get_linkage(&self, name: &str) -> Option<Linkage> {
        self.syms.get(name).map(|sym| sym.linkage)
    }

    fn get_init(&self, name: &str) -> Option<InitValue> {
        self.syms.get(name).map(|sym| &sym.init).copied()
    }

    fn symbols(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.syms.iter()
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    syms: Vec<Scope>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            syms: vec![Scope::new()],
        }
    }

    pub fn push(&mut self) {
        self.syms.push(Scope::new());
    }

    pub fn pop(&mut self) {
        self.syms.pop();
    }

    pub fn add_local(&mut self, name: &str, ty: parser::Type) {
        self.syms
            .last_mut()
            .unwrap()
            .add(name, ty, Linkage::None, InitValue::Local);
    }

    pub fn get_local_ty(&self, name: &str) -> Option<&parser::Type> {
        self.syms.iter().rev().find_map(|scope| scope.get_ty(name))
    }

    pub fn add_global(&mut self, name: &str, ty: parser::Type, linkage: Linkage, init: InitValue) {
        self.syms.first_mut().unwrap().add(name, ty, linkage, init);
    }

    pub fn get_global_ty(&self, name: &str) -> Option<&parser::Type> {
        self.syms.first().unwrap().get_ty(name)
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.syms
            .iter()
            .rev()
            .find_map(|scope| scope.get_symbol(name))
    }

    pub fn get_linkage(&self, name: &str) -> Option<Linkage> {
        self.syms.first().unwrap().get_linkage(name)
    }

    pub fn get_init(&self, name: &str) -> Option<InitValue> {
        self.syms.first().unwrap().get_init(name)
    }

    pub fn symbols(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.syms.iter().flat_map(|scope| scope.symbols())
    }

    pub fn is_global_scope(&self) -> bool {
        self.syms.len() == 1
    }
}

pub struct TypeChecker<'a> {
    symtab: &'a mut SymbolTable,
}

impl<'a> TypeChecker<'a> {
    pub fn new(symtab: &'a mut SymbolTable) -> Self {
        TypeChecker { symtab }
    }

    pub fn run(&mut self, program: &parser::Program) -> Result<(), SemaError> {
        for decl in &program.decls {
            self.check_decl(decl)?;
        }
        Ok(())
    }

    fn check_decl(&mut self, decl: &parser::Decl) -> Result<(), SemaError> {
        match &decl.kind {
            parser::DeclKind::VarDecl { name, initializer } if self.symtab.is_global_scope() => {
                if let Some(ty) = self.symtab.get_global_ty(name)
                    && !matches!(ty, parser::Type::Int)
                {
                    return Err(SemaError::InvalidRedeclaration {
                        name: name.clone(),
                        span: decl.span.into(),
                    });
                }

                let mut linkage = match decl.storage {
                    Some(parser::StorageClass::Static) => Linkage::Internal,
                    _ => Linkage::External,
                };
                let mut initial_value = match initializer {
                    None => match decl.storage {
                        Some(parser::StorageClass::Extern) => InitValue::None,
                        _ => InitValue::Tentative,
                    },
                    Some(parser::Expr {
                        kind: parser::ExprKind::Constant(i),
                        ..
                    }) => InitValue::Const(*i),
                    Some(expr) => {
                        return Err(SemaError::InvalidInitializer {
                            span: expr.span.into(),
                        });
                    }
                };

                if let (Some(old_linkage), Some(old_init_value)) =
                    (self.symtab.get_linkage(name), self.symtab.get_init(name))
                {
                    // `extern` declarations inherit the linkage of the previous declaration
                    if matches!(decl.storage, Some(parser::StorageClass::Extern)) {
                        linkage = old_linkage;
                        if initializer.is_some() {
                            return Err(SemaError::InvalidRedeclaration {
                                name: name.clone(),
                                span: decl.span.into(),
                            });
                        }
                    // Non-`extern` declarations must agree on the linkage
                    } else if old_linkage != linkage {
                        return Err(SemaError::InvalidRedeclaration {
                            name: name.clone(),
                            span: decl.span.into(),
                        });
                    }

                    initial_value = match (old_init_value, initial_value) {
                        (InitValue::Tentative, InitValue::Tentative | InitValue::None) => {
                            InitValue::Tentative
                        }
                        (InitValue::Const(_), InitValue::Const(_)) => {
                            return Err(SemaError::DuplicateVariableDeclaration {
                                var: name.clone(),
                                span: decl.span.into(),
                            });
                        }
                        (InitValue::Const(i), _) => InitValue::Const(i),
                        _ => initial_value,
                    };
                }

                self.symtab
                    .add_global(name, parser::Type::Int, linkage, initial_value);
            }
            parser::DeclKind::VarDecl { name, initializer } => match decl.storage {
                Some(parser::StorageClass::Extern) => {
                    if initializer.is_some() {
                        return Err(SemaError::InvalidRedeclaration {
                            name: name.clone(),
                            span: decl.span.into(),
                        });
                    }

                    if let Some(ty) = self.symtab.get_global_ty(name) {
                        if ty != &parser::Type::Int {
                            return Err(SemaError::InvalidRedeclaration {
                                name: name.clone(),
                                span: decl.span.into(),
                            });
                        }
                    } else {
                        self.symtab.add_global(
                            name,
                            parser::Type::Int,
                            Linkage::External,
                            InitValue::None,
                        );
                    }
                }
                Some(parser::StorageClass::Static) => {
                    let init = match initializer {
                        Some(parser::Expr {
                            kind: parser::ExprKind::Constant(i),
                            ..
                        }) => InitValue::Const(*i),
                        None => InitValue::None,
                        _ => {
                            return Err(SemaError::InvalidInitializer {
                                span: initializer.as_ref().unwrap().span.into(),
                            });
                        }
                    };
                    self.symtab
                        .add_global(name, parser::Type::Int, Linkage::Internal, init);
                }
                None => {
                    self.symtab.add_local(name, parser::Type::Int);
                    if let Some(initializer) = initializer {
                        self.check_expr(initializer)?;
                    }
                }
            },
            parser::DeclKind::FunDecl { name, params, body } => {
                let ty = parser::Type::FunType {
                    ret: Box::new(parser::Type::Int),
                    params: vec![parser::Type::Int; params.len()],
                };

                if let Some(ety) = self.symtab.get_local_ty(name)
                    && ety != &ty
                {
                    return Err(SemaError::InvalidRedeclaration {
                        name: name.clone(),
                        span: decl.span.into(),
                    });
                }

                let linkage = match decl.storage {
                    Some(parser::StorageClass::Static) => Linkage::Internal,
                    Some(parser::StorageClass::Extern) => Linkage::External,
                    None => self.symtab.get_linkage(name).unwrap_or(Linkage::External),
                };

                let (linkage, init) = if let (Some(old_linkage), Some(old_init)) =
                    (self.symtab.get_linkage(name), self.symtab.get_init(name))
                {
                    // `extern` declarations inherit the old declaration's linkage
                    let linkage = match decl.storage {
                        Some(parser::StorageClass::Extern) => old_linkage,
                        _ => {
                            if old_linkage != linkage {
                                return Err(SemaError::InvalidRedeclaration {
                                    name: name.clone(),
                                    span: decl.span.into(),
                                });
                            }
                            linkage
                        }
                    };
                    (
                        linkage,
                        match (old_init, body.is_some()) {
                            (InitValue::Fun, _) => InitValue::Fun,
                            (InitValue::None, true) => InitValue::Fun,
                            (InitValue::None, false) => InitValue::None,
                            (_, _) => panic!("Invalid initializer for function"),
                        },
                    )
                } else {
                    (
                        linkage,
                        if body.is_some() {
                            InitValue::Fun
                        } else {
                            InitValue::None
                        },
                    )
                };

                self.symtab.add_global(name, ty, linkage, init);
                self.symtab.push();
                for param in params {
                    self.symtab.add_local(&param.name, parser::Type::Int);
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
            parser::ExprKind::Var(v) => Ok(self
                .symtab
                .get_local_ty(v)
                .expect("undefined variable")
                .clone()),
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
                        let ty = self.symtab.get_local_ty(v).expect("function not found");
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
                    _ => {
                        return Err(SemaError::InvalidFunctionCallable {
                            name: "unknown".to_string(),
                            span: identifier.span.into(),
                        });
                    }
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
