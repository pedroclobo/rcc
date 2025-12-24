mod ast;
mod error;

pub use ast::{
    BinaryOperator, BlockItem, Declaration, Expression, FunctionDefinition, Program, Statement,
    UnaryOperator,
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
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Program<'a>, ParserError<'a>> {
        let mut functions = Vec::new();
        while self.lexer.peek().is_some() {
            functions.push(self.parse_function_definition()?);
        }
        Ok(Program { functions })
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token<'a>, ParserError<'a>> {
        match self.lexer.next() {
            Some(Ok(tok)) if tok.kind == kind => Ok(tok),
            Some(Ok(tok)) => Err(ParserError::Expected(kind, tok.kind)),
            Some(Err(e)) => Err(ParserError::LexerError(e)),
            None => Err(ParserError::NoMoreTokens),
        }
    }

    fn expect_any(&mut self, kinds: &'a [TokenKind]) -> Result<Token<'a>, ParserError<'a>> {
        match self.lexer.next() {
            Some(Ok(tok)) if kinds.contains(&tok.kind) => Ok(tok),
            Some(Ok(tok)) => Err(ParserError::ExpectedAny(kinds, tok.kind)),
            Some(Err(e)) => Err(ParserError::LexerError(e)),
            None => Err(ParserError::NoMoreTokens),
        }
    }

    // <function> ::= "int" <identifier> "(" "void" ")" "{" { <body_item> } "}"
    fn parse_function_definition(&mut self) -> Result<FunctionDefinition<'a>, ParserError<'a>> {
        self.expect(TokenKind::Int)?;

        let name = self.expect(TokenKind::Identifier)?.lexeme;

        self.expect(TokenKind::LParen)?;
        self.expect(TokenKind::Void)?;
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while let Some(Ok(tok)) = self.lexer.peek() {
            match tok.kind {
                TokenKind::RBrace => break,
                _ => body.push(self.parse_body_item()?),
            }
        }
        self.expect(TokenKind::RBrace)?;

        Ok(FunctionDefinition { name, body })
    }

    // <body_item> ::= <statement> | <declaration>
    fn parse_body_item(&mut self) -> Result<BlockItem, ParserError<'a>> {
        let tok = match self.lexer.peek() {
            Some(Ok(tok)) => tok,
            Some(Err(e)) => return Err(ParserError::LexerError(*e)),
            None => return Err(ParserError::NoMoreTokens),
        };

        match tok.kind {
            TokenKind::Int => Ok(BlockItem::Declaration(self.parse_declaration()?)),
            _ => Ok(BlockItem::Statement(self.parse_statement()?)),
        }
    }

    // <declaration> :: "int" <identifier> [ "=" <exp> ] ";"
    fn parse_declaration(&mut self) -> Result<Declaration, ParserError<'a>> {
        self.expect(TokenKind::Int)?;

        let name = self.expect(TokenKind::Identifier)?.lexeme.to_string();

        let tok = match self.lexer.peek() {
            Some(Ok(tok)) => tok,
            Some(Err(e)) => return Err(ParserError::LexerError(*e)),
            None => return Err(ParserError::NoMoreTokens),
        };
        match tok.kind {
            TokenKind::Eq => {
                self.expect(TokenKind::Eq)?;
                let initializer = Some(self.parse_expression(Precedence::None)?);
                self.expect(TokenKind::Semicolon)?;

                Ok(Declaration { name, initializer })
            }
            _ => {
                self.expect(TokenKind::Semicolon)?;
                Ok(Declaration {
                    name,
                    initializer: None,
                })
            }
        }
    }

    // <statement> ::= "return" <exp> ";" | <exp> ";" |  ";"
    fn parse_statement(&mut self) -> Result<Statement, ParserError<'a>> {
        let tok = match self.lexer.peek() {
            Some(Ok(tok)) => tok,
            Some(Err(e)) => return Err(ParserError::LexerError(*e)),
            None => return Err(ParserError::NoMoreTokens),
        };

        match tok.kind {
            TokenKind::Return => {
                self.expect(TokenKind::Return)?;
                let expr = self.parse_expression(Precedence::None)?;
                self.expect(TokenKind::Semicolon)?;
                Ok(Statement::Return(expr))
            }
            TokenKind::Semicolon => {
                self.expect(TokenKind::Semicolon)?;
                Ok(Statement::Expression(None))
            }
            _ => {
                let expr = self.parse_expression(Precedence::None)?;
                self.expect(TokenKind::Semicolon)?;
                Ok(Statement::Expression(Some(expr)))
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
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError<'a>> {
        let mut lhs = self.parse_factor()?;
        while let Some(Ok(token)) = self.lexer.peek()
            && Self::precedence(token) >= precedence
            && (is_binop(token) || matches!(token.kind, TokenKind::Eq))
        {
            match token.kind {
                TokenKind::Eq => {
                    let precedence = Self::precedence(token);
                    self.lexer.next();

                    let rhs = self.parse_expression(precedence)?;
                    lhs = Expression::Assignment(Box::new(lhs), Box::new(rhs));
                }
                _ => {
                    let op = BinaryOperator::try_from(token.kind)?;
                    let precedence = Self::precedence(token);
                    self.lexer.next();

                    let rhs = self.parse_expression(precedence.increment())?;
                    lhs = Expression::Binary(op, Box::new(lhs), Box::new(rhs));
                }
            }
        }
        Ok(lhs)
    }

    // <factor> ::= constant | <identifier> | "(" <exp> ")" | <unexp>
    fn parse_factor(&mut self) -> Result<Expression, ParserError<'a>> {
        let tok = match self.lexer.peek() {
            Some(Ok(tok)) => tok,
            Some(Err(e)) => return Err(ParserError::LexerError(*e)),
            None => return Err(ParserError::NoMoreTokens),
        };

        match tok.kind {
            TokenKind::Constant => {
                let constant = self.expect(TokenKind::Constant)?;
                Ok(Expression::Constant(constant.lexeme.parse()?))
            }
            TokenKind::Identifier => {
                let identifier = self.expect(TokenKind::Identifier)?.lexeme.to_string();
                Ok(Expression::Var(identifier))
            }
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let expr = self.parse_expression(Precedence::None)?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::Minus | TokenKind::Tilde | TokenKind::Bang => self.parse_unary_expression(),
            _ => Err(ParserError::ExpectedAny(
                &[
                    TokenKind::Constant,
                    TokenKind::LParen,
                    TokenKind::Minus,
                    TokenKind::Tilde,
                    TokenKind::Bang,
                ],
                tok.kind,
            )),
        }
    }

    // <unexp> ::= <unop> <factor>
    // <unop>  ::= "-" | "~" | "!"
    fn parse_unary_expression(&mut self) -> Result<Expression, ParserError<'a>> {
        let tok = self.expect_any(&[TokenKind::Minus, TokenKind::Tilde, TokenKind::Bang])?;

        let op = match tok.kind {
            TokenKind::Minus => UnaryOperator::Neg,
            TokenKind::Tilde => UnaryOperator::BNot,
            TokenKind::Bang => UnaryOperator::Not,
            _ => return Err(ParserError::InvalidUnaryOperator(tok.kind)),
        };

        let factor = self.parse_factor()?;
        Ok(Expression::Unary(op, Box::new(factor)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn return_0() {
        let mut parser = Parser::new("int main(void) { return 0; }");
        let ast = parser.parse().unwrap();

        assert_eq!(ast.functions.len(), 1);
        assert_eq!(ast.functions[0].name, "main");
        assert_eq!(
            ast.functions[0].body,
            vec![BlockItem::Statement(Statement::Return(
                Expression::Constant(0)
            ))]
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
            vec![BlockItem::Statement(Statement::Return(
                Expression::Constant(2)
            ))]
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
            vec![BlockItem::Statement(Statement::Return(Expression::Unary(
                UnaryOperator::Neg,
                Box::new(Expression::Constant(2))
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
            vec![BlockItem::Statement(Statement::Return(Expression::Unary(
                UnaryOperator::BNot,
                Box::new(Expression::Constant(2))
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
            vec![BlockItem::Statement(Statement::Return(Expression::Unary(
                UnaryOperator::BNot,
                Box::new(Expression::Unary(
                    UnaryOperator::Neg,
                    Box::new(Expression::Constant(2))
                ))
            )))]
        );
    }

    #[test]
    fn left_associativity() {
        let mut parser = Parser::new("1 + 2 - 3");
        let expr = parser.parse_expression(Precedence::None).unwrap();

        assert_eq!(
            expr,
            Expression::Binary(
                BinaryOperator::Sub,
                Box::new(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2))
                )),
                Box::new(Expression::Constant(3))
            )
        );
    }

    #[test]
    fn precedence() {
        let mut parser = Parser::new("1 + 2 * 3");
        let expr = parser.parse_expression(Precedence::None).unwrap();

        assert_eq!(
            expr,
            Expression::Binary(
                BinaryOperator::Add,
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Binary(
                    BinaryOperator::Mul,
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(3))
                )),
            )
        );
    }

    #[test]
    fn bitwise() {
        let mut parser = Parser::new("1 << 2 & 3 ^ 4 | 5");
        let expr = parser.parse_expression(Precedence::None).unwrap();

        assert_eq!(
            expr,
            Expression::Binary(
                BinaryOperator::BOr,
                Box::new(Expression::Binary(
                    BinaryOperator::Xor,
                    Box::new(Expression::Binary(
                        BinaryOperator::BAnd,
                        Box::new(Expression::Binary(
                            BinaryOperator::LShift,
                            Box::new(Expression::Constant(1)),
                            Box::new(Expression::Constant(2)),
                        )),
                        Box::new(Expression::Constant(3)),
                    )),
                    Box::new(Expression::Constant(4)),
                )),
                Box::new(Expression::Constant(5)),
            )
        );
    }

    #[test]
    fn logical_and_comparison() {
        let mut parser = Parser::new("1 < 2 && 3 || 4");
        let expr = parser.parse_expression(Precedence::None).unwrap();

        assert_eq!(
            expr,
            Expression::Binary(
                BinaryOperator::Or,
                Box::new(Expression::Binary(
                    BinaryOperator::And,
                    Box::new(Expression::Binary(
                        BinaryOperator::Lt,
                        Box::new(Expression::Constant(1)),
                        Box::new(Expression::Constant(2)),
                    )),
                    Box::new(Expression::Constant(3)),
                )),
                Box::new(Expression::Constant(4)),
            )
        );
    }

    #[test]
    fn bang() {
        let mut parser = Parser::new("!1");
        let expr = parser.parse_expression(Precedence::None).unwrap();

        assert_eq!(
            expr,
            Expression::Unary(UnaryOperator::Not, Box::new(Expression::Constant(1)))
        );
    }

    #[test]
    fn declaration() {
        let mut parser = Parser::new("int i;");
        let decl = parser.parse_declaration().unwrap();

        assert_eq!(
            decl,
            Declaration {
                name: "i".to_string(),
                initializer: None
            }
        );
    }

    #[test]
    fn declaration_with_initializer() {
        let mut parser = Parser::new("int i = 3 + 1;");
        let decl = parser.parse_declaration().unwrap();

        assert_eq!(
            decl,
            Declaration {
                name: "i".to_string(),
                initializer: Some(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(3)),
                    Box::new(Expression::Constant(1))
                ))
            }
        );
    }

    #[test]
    fn assignment_precedence() {
        let mut parser = Parser::new("a = b = 3");
        let expr = parser.parse_expression(Precedence::None).unwrap();

        assert_eq!(
            expr,
            Expression::Assignment(
                Box::new(Expression::Var("a".to_string())),
                Box::new(Expression::Assignment(
                    Box::new(Expression::Var("b".to_string())),
                    Box::new(Expression::Constant(3))
                ))
            )
        );
    }
}
