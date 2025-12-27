mod ast;
mod error;

pub use ast::{
    BinaryOperator, BlockItem, Decl, DeclKind, Expr, ExprKind, FunctionDefinition, Program, Span,
    Stmt, StmtKind, UnaryOperator,
};
pub use error::ParserError;

use crate::lexer::{Lexer, Token, TokenKind};

use std::iter::Peekable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment,     // =
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

impl Precedence {
    fn increment(self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::LogicalOr,
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

    pub fn parse(&mut self) -> Result<Program<'a>, ParserError> {
        let mut functions = Vec::new();
        while self.lexer.peek().is_some() {
            functions.push(self.parse_function_definition()?);
        }
        Ok(Program { functions })
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

    // <function> ::= "int" <identifier> "(" "void" ")" "{" { <body_item> } "}"
    fn parse_function_definition(&mut self) -> Result<FunctionDefinition<'a>, ParserError> {
        self.expect(TokenKind::Int)?;

        let name = self.expect(TokenKind::Identifier)?.lexeme;

        self.expect(TokenKind::LParen)?;
        self.expect(TokenKind::Void)?;
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while let Ok(tok) = self.peek() {
            match tok.kind {
                TokenKind::RBrace => break,
                _ => body.push(self.parse_body_item()?),
            }
        }
        self.expect(TokenKind::RBrace)?;

        Ok(FunctionDefinition { name, body })
    }

    // <body_item> ::= <stmt> | <decl>
    fn parse_body_item(&mut self) -> Result<BlockItem, ParserError> {
        match self.peek()?.kind {
            TokenKind::Int => Ok(BlockItem::Decl(self.parse_decl()?)),
            _ => Ok(BlockItem::Stmt(self.parse_stmt()?)),
        }
    }

    // <decl> :: "int" <identifier> [ "=" <exp> ] ";"
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
                    kind: DeclKind {
                        name,
                        initializer: Some(initializer),
                    },
                    span: Span::new(start, end),
                })
            }
            _ => {
                self.expect(TokenKind::Semicolon)?;
                Ok(Decl {
                    kind: DeclKind {
                        name,
                        initializer: None,
                    },
                    span: Span::new(start, id.span.end),
                })
            }
        }
    }

    // <stmt> ::= "return" <exp> ";" | <exp> ";" |  ";"
    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        match self.peek()?.kind {
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
            TokenKind::Semicolon => {
                let span = self.expect(TokenKind::Semicolon)?.span;
                Ok(Stmt {
                    kind: StmtKind::Expr(None),
                    span: span.clone(),
                })
            }
            _ => {
                let expr = self.parse_expr()?;
                let span = expr.span.clone();
                self.expect(TokenKind::Semicolon)?;
                Ok(Stmt {
                    kind: StmtKind::Expr(Some(expr)),
                    span: span,
                })
            }
        }
    }

    fn precedence(token: &Token) -> Precedence {
        match token.kind {
            TokenKind::Mul | TokenKind::Div | TokenKind::Mod => Precedence::Multiplicative,
            TokenKind::Plus | TokenKind::Minus => Precedence::Additive,
            TokenKind::LShift | TokenKind::RShift => Precedence::Shift,
            TokenKind::Lt | TokenKind::Gt | TokenKind::Le | TokenKind::Ge => Precedence::Relational,
            TokenKind::EqEq | TokenKind::Neq => Precedence::Equality,
            TokenKind::Ampersand => Precedence::BitwiseAnd,
            TokenKind::Caret => Precedence::BitwiseXor,
            TokenKind::Pipe => Precedence::BitwiseOr,
            TokenKind::And => Precedence::LogicalAnd,
            TokenKind::Or => Precedence::LogicalOr,
            TokenKind::Eq => Precedence::Assignment,
            _ => Precedence::None,
        }
    }

    // <exp> ::= <factor> | <binexp>
    // <binexp> ::= <factor> <binop> <factor>
    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self._parse_expr(Precedence::None)
    }

    fn _parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_factor()?;
        let span = lhs.span;

        while let Ok(token) = self.peek()
            && Self::precedence(token) >= precedence
            && (is_binop(token) || matches!(token.kind, TokenKind::Eq))
        {
            match token.kind {
                TokenKind::Eq => {
                    let precedence = Self::precedence(token);
                    self.next()?;

                    let rhs = self._parse_expr(precedence)?;
                    lhs = Expr {
                        kind: ExprKind::Assignment(Box::new(lhs), Box::new(rhs)),
                        // TODO: should probably merge here
                        span: span.clone(),
                    }
                }
                _ => {
                    let op = BinaryOperator::try_from(token)?;
                    let precedence = Self::precedence(token);
                    self.next()?;

                    let rhs = self._parse_expr(precedence.increment())?;
                    lhs = Expr {
                        kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                        // TODO: should probably merge here
                        span: span.clone(),
                    }
                }
            }
        }
        Ok(lhs)
    }

    // <factor> ::= constant | <identifier> | "(" <exp> ")" | <unexp>
    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let tok = self.peek()?;
        match tok.kind {
            TokenKind::Constant => {
                let constant = self.expect(TokenKind::Constant)?;
                Ok(Expr {
                    kind: ExprKind::Constant(constant.lexeme.parse()?),
                    span: constant.span.clone(),
                })
            }
            TokenKind::Identifier => {
                let tok = self.expect(TokenKind::Identifier)?;
                let identifier = tok.lexeme.to_string();
                Ok(Expr {
                    kind: ExprKind::Var(identifier),
                    span: tok.span.clone(),
                })
            }
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::Minus | TokenKind::Tilde | TokenKind::Bang => self.parse_unary_expr(),
            _ => Err(ParserError::ExpectedExpression {
                got: tok.kind,
                span: tok.span.into(),
            }),
        }
    }

    // <unexp> ::= <unop> <factor>
    // <unop>  ::= "-" | "~" | "!"
    fn parse_unary_expr(&mut self) -> Result<Expr, ParserError> {
        let tok = self.next()?;
        let start = tok.span.start;

        let op = match tok.kind {
            TokenKind::Minus => UnaryOperator::Neg,
            TokenKind::Tilde => UnaryOperator::BNot,
            TokenKind::Bang => UnaryOperator::Not,
            _ => {
                return Err(ParserError::InvalidUnaryOperator {
                    op: tok.kind,
                    span: tok.span.into(),
                });
            }
        };

        let factor = self.parse_factor()?;
        let end = factor.span.end;

        Ok(Expr {
            kind: ExprKind::Unary(op, Box::new(factor)),
            span: Span::new(start, end),
        })
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

    macro_rules! return_stmt {
        ($expr:expr) => {
            stmt!(StmtKind::Return($expr))
        };
    }

    macro_rules! decl {
        ($name:expr) => {
            Decl {
                kind: DeclKind {
                    name: $name.to_string(),
                    initializer: None,
                },
                span: dummy_span!(),
            }
        };
        ($name:expr, $init:expr) => {
            Decl {
                kind: DeclKind {
                    name: $name.to_string(),
                    initializer: Some($init),
                },
                span: dummy_span!(),
            }
        };
    }

    #[test]
    fn return_0() {
        let mut parser = Parser::new("int main(void) { return 0; }");
        let ast = parser.parse().unwrap();

        assert_eq!(ast.functions.len(), 1);
        assert_eq!(ast.functions[0].name, "main");
        assert_eq!(
            ast.functions[0].body,
            vec![BlockItem::Stmt(return_stmt!(constant!(0)))]
        );
    }

    #[test]
    fn return_2() {
        let mut parser = Parser::new("int main(void) { return 2; }");
        let ast = parser.parse().unwrap();

        assert_eq!(ast.functions.len(), 1);
        assert_eq!(ast.functions[0].name, "main");
        assert_eq!(
            ast.functions[0].body,
            vec![BlockItem::Stmt(return_stmt!(constant!(2)))]
        );
    }

    #[test]
    fn return_minus_2() {
        let mut parser = Parser::new("int main(void) { return -2; }");
        let ast = parser.parse().unwrap();

        assert_eq!(ast.functions.len(), 1);
        assert_eq!(ast.functions[0].name, "main");
        assert_eq!(
            ast.functions[0].body,
            vec![BlockItem::Stmt(return_stmt!(unary!(
                UnaryOperator::Neg,
                constant!(2)
            )))]
        );
    }

    #[test]
    fn return_neg_2() {
        let mut parser = Parser::new("int main(void) { return ~2; }");
        let ast = parser.parse().unwrap();

        assert_eq!(ast.functions.len(), 1);
        assert_eq!(ast.functions[0].name, "main");
        assert_eq!(
            ast.functions[0].body,
            vec![BlockItem::Stmt(return_stmt!(unary!(
                UnaryOperator::BNot,
                constant!(2)
            )))]
        );
    }

    #[test]
    fn return_neg_minus_2() {
        let mut parser = Parser::new("int main(void) { return ~(-2); }");
        let ast = parser.parse().unwrap();

        assert_eq!(ast.functions.len(), 1);
        assert_eq!(ast.functions[0].name, "main");
        assert_eq!(
            ast.functions[0].body,
            vec![BlockItem::Stmt(return_stmt!(unary!(
                UnaryOperator::BNot,
                unary!(UnaryOperator::Neg, constant!(2))
            )))]
        );
    }

    #[test]
    fn left_associativity() {
        let mut parser = Parser::new("1 + 2 - 3");
        let expr = parser.parse_expr().unwrap();

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

        assert_eq!(expr, unary!(UnaryOperator::Not, constant!(1)));
    }

    #[test]
    fn decl() {
        let mut parser = Parser::new("int i;");
        let decl = parser.parse_decl().unwrap();

        assert_eq!(decl, decl!("i"));
    }

    #[test]
    fn decl_with_initializer() {
        let mut parser = Parser::new("int i = 3 + 1;");
        let decl = parser.parse_decl().unwrap();

        assert_eq!(
            decl,
            decl!(
                "i",
                binary!(BinaryOperator::Add, constant!(3), constant!(1))
            )
        );
    }

    #[test]
    fn assignment_precedence() {
        let mut parser = Parser::new("a = b = 3");
        let expr = parser.parse_expr().unwrap();

        assert_eq!(expr, assign!(var!("a"), assign!(var!("b"), constant!(3))));
    }
}
