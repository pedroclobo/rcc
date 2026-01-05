mod ast;
mod error;

pub use ast::{
    BinaryOperator, Block, BlockItem, Decl, DeclKind, Expr, ExprKind, ForInit, Label, Param,
    Program, Span, Stmt, StmtKind, Type, UnaryOperator,
};
pub use error::ParserError;

use crate::lexer::{Lexer, Token, TokenKind};

use std::{collections::VecDeque, iter::Peekable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment,     // =, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=
    Conditional,    // ? :
    LogicalOr,      // ||
    LogicalAnd,     // &&
    BitwiseOr,      // |
    BitwiseXor,     // ^
    BitwiseAnd,     // &
    Equality,       // ==, !=
    Relational,     // <, >, <=, >=
    Shift,          // <<, >>
    Additive,       // +, -
    Multiplicative, // *, /, %
    Highest,
}

impl Precedence {
    fn increment(self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Conditional,
            Precedence::Conditional => Precedence::LogicalOr,
            Precedence::LogicalOr => Precedence::LogicalAnd,
            Precedence::LogicalAnd => Precedence::BitwiseOr,
            Precedence::BitwiseOr => Precedence::BitwiseXor,
            Precedence::BitwiseXor => Precedence::BitwiseAnd,
            Precedence::BitwiseAnd => Precedence::Equality,
            Precedence::Equality => Precedence::Relational,
            Precedence::Relational => Precedence::Shift,
            Precedence::Shift => Precedence::Additive,
            Precedence::Additive => Precedence::Multiplicative,
            Precedence::Multiplicative => Precedence::Highest,
            Precedence::Highest => Precedence::Highest,
        }
    }
}

fn is_prefix_op(token: &Token) -> bool {
    matches!(
        token.kind,
        TokenKind::Minus
            | TokenKind::Tilde
            | TokenKind::Bang
            | TokenKind::PlusPlus
            | TokenKind::MinusMinus
            | TokenKind::Ge,
    )
}

fn is_binop(token: &Token) -> bool {
    matches!(
        token.kind,
        TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Mod
            | TokenKind::Ampersand
            | TokenKind::Pipe
            | TokenKind::Caret
            | TokenKind::LShift
            | TokenKind::RShift
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::Le
            | TokenKind::Ge,
    )
}

fn is_assignment(token: &Token) -> bool {
    matches!(
        token.kind,
        TokenKind::Eq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::MulEq
            | TokenKind::DivEq
            | TokenKind::ModEq
            | TokenKind::AmpersandEq
            | TokenKind::PipeEq
            | TokenKind::CaretEq
            | TokenKind::LShiftEq
            | TokenKind::RShiftEq
    )
}

fn is_conditional(token: &Token) -> bool {
    matches!(token.kind, TokenKind::QMark)
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    tok: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(input).peekable(),
            tok: None,
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut decls = Vec::new();
        while self.lexer.peek().is_some() {
            decls.push(self.parse_decl()?);
        }
        Ok(Program { decls })
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token<'a>, ParserError> {
        match self.lexer.next() {
            Some(Ok(tok)) if tok.kind == kind => {
                self.tok = Some(tok.clone());
                Ok(tok)
            }
            Some(Ok(tok)) => Err(ParserError::Expected {
                expected: kind,
                got: tok.kind,
                span: tok.span.into(),
            }),
            Some(Err(e)) => Err(ParserError::LexerError(e)),
            None => Err(ParserError::NoMoreTokens {
                span: if let Some(tok) = &self.tok {
                    tok.span.into()
                } else {
                    (0, 1).into()
                },
            }),
        }
    }

    fn next(&mut self) -> Result<Token<'a>, ParserError> {
        match self.lexer.next() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(e)) => Err(ParserError::LexerError(e.clone())),
            None => Err(ParserError::NoMoreTokens {
                span: if let Some(tok) = &self.tok {
                    tok.span.into()
                } else {
                    (0, 1).into()
                },
            }),
        }
    }

    fn peek(&mut self) -> Result<&Token<'a>, ParserError> {
        match self.lexer.peek() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(e)) => Err(ParserError::LexerError(e.clone())),
            None => Err(ParserError::NoMoreTokens {
                span: if let Some(tok) = &self.tok {
                    tok.span.into()
                } else {
                    (0, 1).into()
                },
            }),
        }
    }

    // <block> ::= { <body_item> }
    fn parse_block(&mut self) -> Result<Block, ParserError> {
        let start = self.expect(TokenKind::LBrace)?.span.start;

        let mut body = VecDeque::new();
        while let Ok(tok) = self.peek() {
            match tok.kind {
                TokenKind::RBrace => break,
                _ => body.push_back(self.parse_body_item()?),
            }
        }

        let end = self.expect(TokenKind::RBrace)?.span.end;

        Ok(Block {
            items: body,
            span: Span::new(start, end),
        })
    }

    // <body_item> ::= <stmt> | <decl>
    fn parse_body_item(&mut self) -> Result<BlockItem, ParserError> {
        match self.peek()?.kind {
            TokenKind::Int => Ok(BlockItem::Decl(self.parse_decl()?)),
            _ => Ok(BlockItem::Stmt(self.parse_stmt()?)),
        }
    }

    // <decl>     ::= <var_decl> | <fun_decl>
    // <var_decl> ::= "int" <identifier> [ "=" <exp> ] ";"
    // <fun_decl> ::= "int" <identifier> "(" <param-list> ")" ( "{" { <block> } "}" | ";" )
    fn parse_decl(&mut self) -> Result<Decl, ParserError> {
        let start = self.expect(TokenKind::Int)?.span.start;
        let id = self.expect(TokenKind::Identifier)?;
        let name = id.lexeme.to_string();

        match self.peek()?.kind {
            TokenKind::Eq => {
                self.expect(TokenKind::Eq)?;
                let initializer = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                let end = initializer.span.end;

                Ok(Decl {
                    kind: DeclKind::VarDecl {
                        name,
                        initializer: Some(initializer),
                    },
                    span: Span::new(start, end),
                })
            }
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let params = self.parse_params()?;
                self.expect(TokenKind::RParen)?;

                let (body, end) = if matches!(self.peek()?.kind, TokenKind::LBrace) {
                    let body = self.parse_block()?;
                    let end = body.span.end;
                    (Some(body), end)
                } else {
                    (None, self.expect(TokenKind::Semicolon)?.span.end)
                };

                Ok(Decl {
                    kind: DeclKind::FunDecl {
                        name: name.to_string(),
                        body,
                        params,
                    },
                    span: Span::new(start, end),
                })
            }
            _ => {
                self.expect(TokenKind::Semicolon)?;
                Ok(Decl {
                    kind: DeclKind::VarDecl {
                        name,
                        initializer: None,
                    },
                    span: Span::new(start, id.span.end),
                })
            }
        }
    }

    // <param-list> ::= "void" | "int" <identifier> { "," "int" <identifier> }
    fn parse_params(&mut self) -> Result<Vec<Param>, ParserError> {
        let mut params = Vec::new();

        loop {
            match self.peek()?.kind {
                TokenKind::Int => {
                    let start = self.expect(TokenKind::Int)?.span.start;
                    let id = self.expect(TokenKind::Identifier)?;
                    let end = id.span.end;
                    let name = id.lexeme.to_string();
                    params.push(Param {
                        name,
                        ty: Type::Int,
                        span: Span::new(start, end),
                    });
                }
                TokenKind::Void => {
                    self.expect(TokenKind::Void)?;
                }
                TokenKind::Comma => {
                    self.expect(TokenKind::Comma)?;
                    if matches!(self.peek()?.kind, TokenKind::RParen) {
                        return Err(ParserError::ExpectedParameter {
                            got: TokenKind::RParen,
                            span: self.peek()?.span.into(),
                        });
                    }
                }
                TokenKind::RParen => {
                    break;
                }
                kind => {
                    return Err(ParserError::ExpectedParameter {
                        got: kind,
                        span: self.peek()?.span.into(),
                    });
                }
            }
        }

        Ok(params)
    }

    // <stmt> ::= "return" <exp> ";" |
    //            <exp> ";" |
    //            "if" "(" <exp> ")" <stmt> [ "else" <stmt> ] ";" |
    //            <block> |
    //            <label>:
    //            <block> |
    //            "goto" <label> ";" |
    //            "break" ";" |
    //            "continue" ";" |
    //            "while" "(" <exp> ")" <stmt> |
    //            "do" <statement> "while" "(" <exp> ")" ";" |
    //            "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <stmt> |
    //            "switch" "(" <exp> ")" <stmt> |
    //            "case" <exp> ":" <stmt> |
    //            "default" ":" <stmt> |
    //            ";"
    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        let tok = self.peek()?;

        match tok.kind {
            TokenKind::Return => {
                let start = self.expect(TokenKind::Return)?.span.start;
                let expr = self.parse_expr()?;
                let end = expr.span.end;
                self.expect(TokenKind::Semicolon)?;
                Ok(Stmt {
                    kind: StmtKind::Return(expr),
                    span: Span::new(start, end),
                })
            }
            //            "switch" "(" <exp> ")" <stmt> |
            //            "case" <exp> ":" <stmt> |
            //            "default" ":" <stmt> |
            TokenKind::Switch => {
                let start = self.expect(TokenKind::Switch)?.span.start;
                self.expect(TokenKind::LParen)?;
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                let body = self.parse_stmt()?;
                let end = body.span.end;

                Ok(Stmt {
                    kind: StmtKind::Switch {
                        expr,
                        body: Box::new(body),
                    },
                    span: Span::new(start, end),
                })
            }
            TokenKind::Case => {
                let start = self.expect(TokenKind::Case)?.span.start;
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Colon)?;
                let stmt = self.parse_stmt()?;
                let end = stmt.span.end;

                Ok(Stmt {
                    kind: StmtKind::Case {
                        expr,
                        body: Box::new(stmt),
                    },
                    span: Span::new(start, end),
                })
            }
            TokenKind::Default => {
                let start = self.expect(TokenKind::Default)?.span.start;
                self.expect(TokenKind::Colon)?;
                let body = self.parse_stmt()?;
                let end = body.span.end;

                Ok(Stmt {
                    kind: StmtKind::Default {
                        body: Box::new(body),
                    },
                    span: Span::new(start, end),
                })
            }
            TokenKind::LBrace => {
                let body = self.parse_block()?;
                let span = body.span;

                Ok(Stmt {
                    kind: StmtKind::Block(body),
                    span,
                })
            }
            TokenKind::Semicolon => {
                let span = self.expect(TokenKind::Semicolon)?.span;
                Ok(Stmt {
                    kind: StmtKind::Expr(None),
                    span,
                })
            }
            TokenKind::Break => {
                let span = self.expect(TokenKind::Break)?.span;
                self.expect(TokenKind::Semicolon)?;
                Ok(Stmt {
                    kind: StmtKind::Break,
                    span,
                })
            }
            TokenKind::Continue => {
                let span = self.expect(TokenKind::Continue)?.span;
                self.expect(TokenKind::Semicolon)?;
                Ok(Stmt {
                    kind: StmtKind::Continue,
                    span,
                })
            }
            TokenKind::While => {
                let start = self.expect(TokenKind::While)?.span.start;

                self.expect(TokenKind::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                let body = self.parse_stmt()?;

                let end = body.span.end;

                Ok(Stmt {
                    kind: StmtKind::While {
                        cond,
                        body: Box::new(body),
                    },
                    span: Span::new(start, end),
                })
            }
            TokenKind::Do => {
                let start = self.expect(TokenKind::Do)?.span.start;
                let body = self.parse_stmt()?;
                self.expect(TokenKind::While)?;
                self.expect(TokenKind::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                let end = self.expect(TokenKind::Semicolon)?.span.end;

                Ok(Stmt {
                    kind: StmtKind::DoWhile {
                        body: Box::new(body),
                        cond,
                    },
                    span: Span::new(start, end),
                })
            }
            TokenKind::For => {
                let start = self.expect(TokenKind::For)?.span.start;
                self.expect(TokenKind::LParen)?;

                let init = self.parse_for_init()?;

                let cond = if matches!(self.peek()?.kind, TokenKind::Semicolon) {
                    self.next()?;
                    None
                } else {
                    let cond = self.parse_expr()?;
                    self.expect(TokenKind::Semicolon)?;
                    Some(cond)
                };

                let post = if matches!(self.peek()?.kind, TokenKind::RParen) {
                    self.next()?;
                    None
                } else {
                    let post = self.parse_expr()?;
                    self.expect(TokenKind::RParen)?;
                    Some(post)
                };

                let body = self.parse_stmt()?;
                let end = body.span.end;

                Ok(Stmt {
                    kind: StmtKind::For {
                        init,
                        cond,
                        post,
                        body: Box::new(body),
                    },
                    span: Span::new(start, end),
                })
            }
            TokenKind::Goto => {
                let start = tok.span.start;
                self.expect(TokenKind::Goto)?;

                let id = self.expect(TokenKind::Identifier)?;
                let label = Label {
                    name: id.lexeme.to_string(),
                    span: id.span,
                };
                let end = self.expect(TokenKind::Semicolon)?.span.end;

                Ok(Stmt {
                    kind: StmtKind::Goto(label),
                    span: Span::new(start, end),
                })
            }
            TokenKind::If => {
                let start = self.next()?.span.start;

                self.expect(TokenKind::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;

                let then = self.parse_stmt()?;
                let then_end = then.span.end;

                if let Some(Ok(Token {
                    kind: TokenKind::Else,
                    ..
                })) = self.lexer.peek()
                {
                    self.next()?;
                    let r#else = self.parse_stmt()?;
                    let else_end = r#else.span.end;
                    Ok(Stmt {
                        kind: StmtKind::If {
                            cond,
                            then: Box::new(then),
                            r#else: Some(Box::new(r#else)),
                        },
                        span: Span::new(start, else_end),
                    })
                } else {
                    Ok(Stmt {
                        kind: StmtKind::If {
                            cond,
                            then: Box::new(then),
                            r#else: None,
                        },
                        span: Span::new(start, then_end),
                    })
                }
            }
            _ => {
                let is_label = {
                    let mut lookahead = self.lexer.clone();
                    lookahead.next();
                    matches!(
                        lookahead.peek(),
                        Some(Ok(Token {
                            kind: TokenKind::Colon,
                            ..
                        }))
                    )
                };

                if is_label {
                    let id = self.expect(TokenKind::Identifier)?;
                    let name = id.lexeme.to_string();

                    self.expect(TokenKind::Colon)?;

                    let stmt = self.parse_stmt()?;
                    let end = stmt.span.end;

                    Ok(Stmt {
                        kind: StmtKind::Labeled {
                            label: Label {
                                name,
                                span: id.span,
                            },
                            stmt: Box::new(stmt),
                        },
                        span: Span::new(id.span.start, end),
                    })
                } else {
                    let expr = self.parse_expr()?;
                    let span = expr.span;

                    self.expect(TokenKind::Semicolon)?;
                    Ok(Stmt {
                        kind: StmtKind::Expr(Some(expr)),
                        span,
                    })
                }
            }
        }
    }

    // <for-init> ::= <declaration> | [ <exp> ] ";"
    fn parse_for_init(&mut self) -> Result<ForInit, ParserError> {
        match self.peek()?.kind {
            TokenKind::Int => Ok(ForInit::Decl(self.parse_decl()?)),
            TokenKind::Semicolon => {
                self.expect(TokenKind::Semicolon)?;
                Ok(ForInit::Expr(None))
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(ForInit::Expr(Some(expr)))
            }
        }
    }

    fn precedence(token: &Token) -> Precedence {
        use TokenKind::*;

        match token.kind {
            Mul | Div | Mod => Precedence::Multiplicative,
            Plus | Minus => Precedence::Additive,
            LShift | RShift => Precedence::Shift,
            Lt | Gt | Le | Ge => Precedence::Relational,
            EqEq | Neq => Precedence::Equality,
            Ampersand => Precedence::BitwiseAnd,
            Caret => Precedence::BitwiseXor,
            Pipe => Precedence::BitwiseOr,
            And => Precedence::LogicalAnd,
            Or => Precedence::LogicalOr,
            QMark | Colon => Precedence::Conditional,
            Eq | PlusEq | MinusEq | MulEq | DivEq | ModEq | AmpersandEq | CaretEq | PipeEq
            | LShiftEq | RShiftEq => Precedence::Assignment,
            _ => Precedence::None,
        }
    }

    // <exp> ::= <factor> | <binexp> | <exp> "?" <exp> ":" <exp>
    // <binexp> ::= <factor> <binop> <factor>
    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self._parse_expr(Precedence::None)
    }

    fn _parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_factor()?;
        let lhs_span = lhs.span;

        while let Ok(token) = self.peek()
            && Self::precedence(token) >= precedence
            && (is_binop(token) || is_assignment(token) || is_conditional(token))
        {
            match token.kind {
                TokenKind::Eq => {
                    let precedence = Self::precedence(token);
                    self.next()?;

                    let rhs = self._parse_expr(precedence)?;
                    let rhs_span = rhs.span;
                    lhs = Expr {
                        kind: ExprKind::Assignment(Box::new(lhs), Box::new(rhs)),
                        span: lhs_span.clone().merge(&rhs_span),
                    }
                }
                _ if is_assignment(token) => {
                    let op = BinaryOperator::try_from(token)?;
                    let precedence = Self::precedence(token);
                    self.next()?;

                    let mut rhs = self._parse_expr(precedence)?;
                    let rhs_span = rhs.span;
                    rhs = Expr {
                        kind: ExprKind::Binary(op, Box::new(lhs.clone()), Box::new(rhs)),
                        span: rhs_span,
                    };
                    lhs = Expr {
                        kind: ExprKind::Assignment(Box::new(lhs), Box::new(rhs)),
                        span: lhs_span.clone().merge(&rhs_span),
                    }
                }
                _ if is_conditional(token) => {
                    let start = token.span.start;
                    let precedence = Self::precedence(token);

                    self.expect(TokenKind::QMark)?;
                    let then = self.parse_expr()?;
                    self.expect(TokenKind::Colon)?;

                    let r#else = self._parse_expr(precedence)?;
                    let end = r#else.span.end;

                    lhs = Expr {
                        kind: ExprKind::Conditional {
                            cond: Box::new(lhs),
                            then: Box::new(then),
                            r#else: Box::new(r#else),
                        },
                        span: Span::new(start, end),
                    }
                }
                _ => {
                    let op = BinaryOperator::try_from(token)?;
                    let precedence = Self::precedence(token);
                    self.next()?;

                    let rhs = self._parse_expr(precedence.increment())?;
                    let rhs_span = rhs.span;
                    lhs = Expr {
                        kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                        span: lhs_span.clone().merge(&rhs_span),
                    }
                }
            }
        }
        Ok(lhs)
    }

    // <factor> ::= constant | <identifier> [ "(" <arg-list> ")" ] | "(" <exp> ")" | <prefix_expr>
    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let tok = self.peek()?;
        match tok.kind {
            TokenKind::Constant => {
                let constant = self.expect(TokenKind::Constant)?;
                Ok(Expr {
                    kind: ExprKind::Constant(constant.lexeme.parse()?),
                    span: constant.span,
                })
            }
            TokenKind::Identifier => {
                let tok = self.expect(TokenKind::Identifier)?;
                let expr = Expr {
                    kind: ExprKind::Var(tok.lexeme.to_string()),
                    span: tok.span,
                };

                match self.lexer.peek() {
                    Some(Ok(Token {
                        kind: TokenKind::PlusPlus | TokenKind::MinusMinus,
                        ..
                    })) => self.parse_postfix_expr(expr),
                    Some(Ok(Token {
                        kind: TokenKind::LParen,
                        ..
                    })) => {
                        self.expect(TokenKind::LParen)?;
                        let args = self.parse_args()?;
                        let end = self.expect(TokenKind::RParen)?.span.end;
                        Ok(Expr {
                            kind: ExprKind::FunctionCall {
                                identifier: Box::new(expr),
                                arguments: args,
                            },
                            span: Span::new(tok.span.start, end),
                        })
                    }
                    _ => Ok(expr),
                }
            }
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                let tok = self.peek()?;
                match tok.kind {
                    TokenKind::PlusPlus | TokenKind::MinusMinus => self.parse_postfix_expr(expr),
                    _ => Ok(expr),
                }
            }
            _ if is_prefix_op(tok) => self.parse_prefix_expr(),
            _ => Err(ParserError::ExpectedExpression {
                got: tok.kind,
                span: tok.span.into(),
            }),
        }
    }

    // <arg-list> ::= <exp> { "," <exp> }
    fn parse_args(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = Vec::new();
        if matches!(self.peek()?.kind, TokenKind::RParen) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expr()?);
            match self.peek()?.kind {
                TokenKind::Comma => {
                    self.expect(TokenKind::Comma)?;
                }
                TokenKind::RParen => {
                    break;
                }
                kind => Err(ParserError::ExpectedArgument {
                    got: kind,
                    span: self.peek()?.span.into(),
                })?,
            }
        }

        Ok(args)
    }

    // <preexp> ::= <preop> <factor>
    // <preop>  ::= "-" | "~" | "!" | "++" | "--"
    fn parse_prefix_expr(&mut self) -> Result<Expr, ParserError> {
        let tok = self.next()?;
        let start = tok.span.start;
        let op = UnaryOperator::try_from(&tok)?;
        let factor = self.parse_factor()?;
        let end = factor.span.end;

        Ok(Expr {
            kind: ExprKind::Unary(op, Box::new(factor)),
            span: Span::new(start, end),
        })
    }

    // <postexp> ::= <factor> <postop>
    // <postop>  ::= "++" | "--"
    fn parse_postfix_expr(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        let tok = self.next()?;
        let inner_span = expr.span;

        match tok.kind {
            TokenKind::PlusPlus | TokenKind::MinusMinus => {
                let op = if matches!(tok.kind, TokenKind::PlusPlus) {
                    UnaryOperator::PostInc
                } else {
                    UnaryOperator::PostDec
                };

                Ok(Expr {
                    kind: ExprKind::Unary(op, Box::new(expr)),
                    span: inner_span.merge(&tok.span),
                })
            }
            _ => Err(ParserError::InvalidUnaryOperator {
                op: tok.kind,
                span: tok.span.into(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Macros to simplify test writing
    macro_rules! dummy_span {
        () => {
            Span::new(0, 0)
        };
    }

    macro_rules! expr {
        ($kind:expr) => {
            Expr {
                kind: $kind,
                span: dummy_span!(),
            }
        };
    }

    macro_rules! if_expr {
        ($cond:expr, $then:expr, $else:expr) => {
            expr!(ExprKind::Conditional {
                cond: Box::new($cond),
                then: Box::new($then),
                r#else: Box::new($else)
            })
        };
    }

    macro_rules! stmt {
        ($kind:expr) => {
            Stmt {
                kind: $kind,
                span: dummy_span!(),
            }
        };
    }

    macro_rules! constant {
        ($val:expr) => {
            expr!(ExprKind::Constant($val))
        };
    }

    macro_rules! var {
        ($name:expr) => {
            expr!(ExprKind::Var($name.to_string()))
        };
    }

    macro_rules! unary {
        ($op:expr, $expr:expr) => {
            expr!(ExprKind::Unary($op, Box::new($expr)))
        };
    }

    macro_rules! binary {
        ($op:expr, $left:expr, $right:expr) => {
            expr!(ExprKind::Binary($op, Box::new($left), Box::new($right)))
        };
    }

    macro_rules! assign {
        ($left:expr, $right:expr) => {
            expr!(ExprKind::Assignment(Box::new($left), Box::new($right)))
        };
    }

    macro_rules! fun_call {
        ($identifier:expr, $arguments:expr) => {
            expr!(ExprKind::FunctionCall {
                identifier: Box::new($identifier),
                arguments: $arguments,
            })
        };
    }

    macro_rules! return_stmt {
        ($expr:expr) => {
            stmt!(StmtKind::Return($expr))
        };
    }

    macro_rules! if_stmt {
        ($cond:expr, $then:expr) => {
            stmt!(StmtKind::If {
                cond: $cond,
                then: Box::new($then),
                r#else: None
            })
        };
        ($cond:expr, $then:expr, $else:expr) => {
            stmt!(StmtKind::If {
                cond: $cond,
                then: Box::new($then),
                r#else: Some(Box::new($else))
            })
        };
    }

    macro_rules! label {
        ($name:expr) => {
            Label {
                name: $name.to_string(),
                span: dummy_span!(),
            }
        };
    }

    macro_rules! labeled_stmt {
        ($label:expr, $stmt:expr) => {
            stmt!(StmtKind::Labeled {
                label: label!($label),
                stmt: Box::new($stmt),
            })
        };
    }

    macro_rules! goto_stmt {
        ($label:expr) => {
            stmt!(StmtKind::Goto(label!($label)))
        };
    }

    macro_rules! block_stmt {
        ($($item:expr),* $(,)?) => {
            stmt!(StmtKind::Block(block![$(BlockItem::Stmt($item)),*]))
        };
    }

    macro_rules! while_stmt {
        ($cond:expr, $body:expr) => {
            stmt!(StmtKind::While {
                cond: $cond,
                body: Box::new($body),
            })
        };
    }

    macro_rules! do_while_stmt {
        ($body:expr, $cond:expr) => {
            stmt!(StmtKind::DoWhile {
                body: Box::new($body),
                cond: $cond,
            })
        };
    }

    macro_rules! for_stmt {
        ($init:expr, $cond:expr, $post:expr, $body:expr) => {
            stmt!(StmtKind::For {
                init: $init,
                cond: $cond,
                post: $post,
                body: Box::new($body),
            })
        };
    }

    macro_rules! break_stmt {
        () => {
            stmt!(StmtKind::Break)
        };
    }

    macro_rules! continue_stmt {
        () => {
            stmt!(StmtKind::Continue)
        };
    }

    macro_rules! switch_stmt {
        ($expr:expr, $body:expr) => {
            stmt!(StmtKind::Switch {
                expr: $expr,
                body: Box::new($body),
            })
        };
    }

    macro_rules! case_stmt {
        ($expr:expr, $body:expr) => {
            stmt!(StmtKind::Case {
                expr: $expr,
                body: Box::new($body),
            })
        };
    }

    macro_rules! default_stmt {
        ($body:expr) => {
            stmt!(StmtKind::Default {
                body: Box::new($body),
            })
        };
    }

    macro_rules! var_decl {
        ($name:expr) => {
            Decl {
                kind: DeclKind::VarDecl {
                    name: $name.to_string(),
                    initializer: None,
                },
                span: dummy_span!(),
            }
        };
        ($name:expr, $init:expr) => {
            Decl {
                kind: DeclKind::VarDecl {
                    name: $name.to_string(),
                    initializer: Some($init),
                },
                span: dummy_span!(),
            }
        };
    }

    macro_rules! fun_decl {
        ($name:expr, $params:expr) => {
            Decl {
                kind: DeclKind::FunDecl {
                    name: $name.to_string(),
                    params: $params,
                    body: None,
                },
                span: dummy_span!(),
            }
        };
        ($name:expr, $params:expr, $body:expr) => {
            Decl {
                kind: DeclKind::FunDecl {
                    name: $name.to_string(),
                    params: $params,
                    body: Some($body),
                },
                span: dummy_span!(),
            }
        };
    }

    macro_rules! param {
        ($name:expr, $type:expr) => {
            Param {
                name: $name.to_string(),
                ty: $type,
                span: dummy_span!(),
            }
        };
    }

    macro_rules! block {
        ($($item:expr),* $(,)?) => {
            Block {
                items: VecDeque::from(vec![$($item),*]),
                span: dummy_span!(),
            }
        };
    }

    #[test]
    fn return_0() {
        let mut parser = Parser::new("int main(void) { return 0; }");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            decl,
            fun_decl!(
                "main",
                vec![],
                block![BlockItem::Stmt(return_stmt!(constant!(0)))]
            )
        );
    }

    #[test]
    fn return_2() {
        let mut parser = Parser::new("int main(void) { return 2; }");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            decl,
            fun_decl!(
                "main",
                vec![],
                block![BlockItem::Stmt(return_stmt!(constant!(2)))]
            )
        );
    }

    #[test]
    fn return_minus_2() {
        let mut parser = Parser::new("int main(void) { return -2; }");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            decl,
            fun_decl!(
                "main",
                vec![],
                block![BlockItem::Stmt(return_stmt!(unary!(
                    UnaryOperator::Neg,
                    constant!(2)
                )))]
            )
        );
    }

    #[test]
    fn return_neg_2() {
        let mut parser = Parser::new("int main(void) { return ~2; }");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            decl,
            fun_decl!(
                "main",
                vec![],
                block![BlockItem::Stmt(return_stmt!(unary!(
                    UnaryOperator::BNot,
                    constant!(2)
                )))]
            )
        );
    }

    #[test]
    fn return_neg_minus_2() {
        let mut parser = Parser::new("int main(void) { return ~(-2); }");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            decl,
            fun_decl!(
                "main",
                vec![],
                block![BlockItem::Stmt(return_stmt!(unary!(
                    UnaryOperator::BNot,
                    unary!(UnaryOperator::Neg, constant!(2))
                )))]
            )
        );
    }

    #[test]
    fn left_associativity() {
        let mut parser = Parser::new("1 + 2 - 3");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            binary!(
                BinaryOperator::Sub,
                binary!(BinaryOperator::Add, constant!(1), constant!(2)),
                constant!(3)
            )
        );
    }

    #[test]
    fn precedence() {
        let mut parser = Parser::new("1 + 2 * 3");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            binary!(
                BinaryOperator::Add,
                constant!(1),
                binary!(BinaryOperator::Mul, constant!(2), constant!(3))
            )
        );
    }

    #[test]
    fn bitwise() {
        let mut parser = Parser::new("1 << 2 & 3 ^ 4 | 5");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            binary!(
                BinaryOperator::BOr,
                binary!(
                    BinaryOperator::Xor,
                    binary!(
                        BinaryOperator::BAnd,
                        binary!(BinaryOperator::LShift, constant!(1), constant!(2)),
                        constant!(3)
                    ),
                    constant!(4)
                ),
                constant!(5)
            )
        );
    }

    #[test]
    fn logical_and_comparison() {
        let mut parser = Parser::new("1 < 2 && 3 || 4");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            binary!(
                BinaryOperator::Or,
                binary!(
                    BinaryOperator::And,
                    binary!(BinaryOperator::Lt, constant!(1), constant!(2)),
                    constant!(3)
                ),
                constant!(4)
            )
        );
    }

    #[test]
    fn bang() {
        let mut parser = Parser::new("!1");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(expr, unary!(UnaryOperator::Not, constant!(1)));
    }

    #[test]
    fn decl() {
        let mut parser = Parser::new("int i;");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(decl, var_decl!("i"));
    }

    #[test]
    fn decl_with_initializer() {
        let mut parser = Parser::new("int i = 3 + 1;");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            decl,
            var_decl!(
                "i",
                binary!(BinaryOperator::Add, constant!(3), constant!(1))
            )
        );
    }

    #[test]
    fn assignment_precedence() {
        let mut parser = Parser::new("a = b = 3");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(expr, assign!(var!("a"), assign!(var!("b"), constant!(3))));
    }

    #[test]
    fn compound_assignment() {
        let mut parser = Parser::new("a += b -= c *= d /= e %= f &= g ^= h |= i <<= j >>= k");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            assign!(
                var!("a"),
                binary!(
                    BinaryOperator::Add,
                    var!("a"),
                    assign!(
                        var!("b"),
                        binary!(
                            BinaryOperator::Sub,
                            var!("b"),
                            assign!(
                                var!("c"),
                                binary!(
                                    BinaryOperator::Mul,
                                    var!("c"),
                                    assign!(
                                        var!("d"),
                                        binary!(
                                            BinaryOperator::Div,
                                            var!("d"),
                                            assign!(
                                                var!("e"),
                                                binary!(
                                                    BinaryOperator::Mod,
                                                    var!("e"),
                                                    assign!(
                                                        var!("f"),
                                                        binary!(
                                                            BinaryOperator::BAnd,
                                                            var!("f"),
                                                            assign!(
                                                                var!("g"),
                                                                binary!(
                                                                    BinaryOperator::Xor,
                                                                    var!("g"),
                                                                    assign!(
                                                                        var!("h"),
                                                                        binary!(
                                                                            BinaryOperator::BOr,
                                                                            var!("h"),
                                                                            assign!(
                                                                                var!("i"),
                                                                                binary!(
                                                                                    BinaryOperator::LShift,
                                                                                    var!("i"),
                                                                                    assign!(
                                                                                        var!("j"),
                                                                                        binary!(
                                                                                            BinaryOperator::RShift,
                                                                                            var!("j"),
                                                                                            var!("k")
                                                                                        )
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn pre_incr_decr() {
        let mut parser = Parser::new("--a + ++b");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            binary!(
                BinaryOperator::Add,
                unary!(UnaryOperator::PreDec, var!("a")),
                unary!(UnaryOperator::PreInc, var!("b"))
            ),
        );
    }

    #[test]
    fn post_incr_decr() {
        let mut parser = Parser::new("a-- + b++");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            binary!(
                BinaryOperator::Add,
                unary!(UnaryOperator::PostDec, var!("a")),
                unary!(UnaryOperator::PostInc, var!("b"))
            )
        );
    }

    #[test]
    fn post_incr_paren() {
        let mut parser = Parser::new("!(a)++");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            unary!(
                UnaryOperator::Not,
                unary!(UnaryOperator::PostInc, var!("a"))
            ),
        );
    }

    #[test]
    fn conditional_stmt() {
        let mut parser = Parser::new(
            "if (a)
        if (a > 10)
            return a;
        else
            return 10 - a;",
        );
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            if_stmt!(
                var!("a"),
                if_stmt!(
                    binary!(BinaryOperator::Gt, var!("a"), constant!(10)),
                    stmt!(StmtKind::Return(var!("a"))),
                    stmt!(StmtKind::Return(binary!(
                        BinaryOperator::Sub,
                        constant!(10),
                        var!("a")
                    )))
                )
            )
        )
    }

    #[test]
    fn conditional_expr() {
        let mut parser = Parser::new("a ? 1 : 0");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(expr, if_expr!(var!("a"), constant!(1), constant!(0)))
    }

    #[test]
    fn labeled_stmt() {
        let mut parser = Parser::new("label: return 1;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(stmt, labeled_stmt!("label", return_stmt!(constant!(1))))
    }

    #[test]
    fn goto_stmt() {
        let mut parser = Parser::new("goto label;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(stmt, goto_stmt!("label"))
    }

    #[test]
    fn block() {
        let mut parser = Parser::new("{ return 1; return 2; }");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            block_stmt!(return_stmt!(constant!(1)), return_stmt!(constant!(2)))
        );
    }

    #[test]
    fn while_loop() {
        let mut parser = Parser::new("while (x < 10) x = x + 1;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            while_stmt!(
                binary!(BinaryOperator::Lt, var!("x"), constant!(10)),
                stmt!(StmtKind::Expr(Some(assign!(
                    var!("x"),
                    binary!(BinaryOperator::Add, var!("x"), constant!(1))
                ))))
            )
        );
    }

    #[test]
    fn do_while_loop() {
        let mut parser = Parser::new("do { x = x + 1; } while (x < 10);");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            do_while_stmt!(
                block_stmt!(stmt!(StmtKind::Expr(Some(assign!(
                    var!("x"),
                    binary!(BinaryOperator::Add, var!("x"), constant!(1))
                ))))),
                binary!(BinaryOperator::Lt, var!("x"), constant!(10))
            )
        );
    }

    #[test]
    fn for_loop() {
        let mut parser = Parser::new("for (int i = 0; i < 10; i = i + 1) break;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            for_stmt!(
                ForInit::Decl(var_decl!("i", constant!(0))),
                Some(binary!(BinaryOperator::Lt, var!("i"), constant!(10))),
                Some(assign!(
                    var!("i"),
                    binary!(BinaryOperator::Add, var!("i"), constant!(1))
                )),
                break_stmt!()
            )
        );
    }

    #[test]
    fn for_loop_with_expression_init() {
        let mut parser = Parser::new("for (i = 0; i < 5; ++i) continue;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            for_stmt!(
                ForInit::Expr(Some(assign!(var!("i"), constant!(0)))),
                Some(binary!(BinaryOperator::Lt, var!("i"), constant!(5))),
                Some(unary!(UnaryOperator::PreInc, var!("i"))),
                continue_stmt!()
            )
        );
    }

    #[test]
    fn for_loop_empty_parts() {
        let mut parser = Parser::new("for (;;) break;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            for_stmt!(ForInit::Expr(None), None, None, break_stmt!())
        );
    }

    #[test]
    fn while_with_break_and_continue() {
        let mut parser = Parser::new("while (1) { if (x > 5) break; continue; }");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            while_stmt!(
                constant!(1),
                block_stmt!(
                    if_stmt!(
                        binary!(BinaryOperator::Gt, var!("x"), constant!(5)),
                        break_stmt!()
                    ),
                    continue_stmt!()
                )
            )
        );
    }

    #[test]
    fn switch() {
        let mut parser =
            Parser::new("switch (a) { case 1: return 0; case 2: return 3; default: return 1; }");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            stmt,
            switch_stmt!(
                var!("a"),
                block_stmt!(
                    case_stmt!(constant!(1), return_stmt!(constant!(0))),
                    case_stmt!(constant!(2), return_stmt!(constant!(3))),
                    default_stmt!(return_stmt!(constant!(1)))
                )
            )
        );
    }

    #[test]
    fn fun_decl() {
        let mut parser = Parser::new("int x(void);");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(decl, fun_decl!("x", vec![]));
    }

    #[test]
    fn fun_decl_with_params() {
        let mut parser = Parser::new("int x(int i, int j);");
        let decl = parser.parse_decl().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            decl,
            fun_decl!("x", vec![param!("i", Type::Int), param!("j", Type::Int)])
        );
    }

    #[test]
    fn fun_call() {
        let mut parser = Parser::new("x()");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(expr, fun_call!(var!("x"), vec![]));
    }

    #[test]
    fn fun_call_with_args() {
        let mut parser = Parser::new("x(1, 2, 3)");
        let expr = parser.parse_expr().unwrap();
        assert!(parser.lexer.next().is_none());

        assert_eq!(
            expr,
            fun_call!(var!("x"), vec![constant!(1), constant!(2), constant!(3)])
        );
    }
}
