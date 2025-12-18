use std::{error::Error, fmt::Display, iter::Peekable, num::ParseIntError};

use crate::{
    ast::{BinaryOperator, Expression, FunctionDefinition, Program, Statement, UnaryOperator},
    lexer::{Lexer, LexerError, Token, TokenKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Low,
    High,
    Highest,
}

impl Precedence {
    fn increment(self) -> Self {
        match self {
            Precedence::None => Precedence::Low,
            Precedence::Low => Precedence::High,
            Precedence::High => Precedence::Highest,
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

    // <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
    fn parse_function_definition(&mut self) -> Result<FunctionDefinition<'a>, ParserError<'a>> {
        self.expect(TokenKind::Int)?;

        let name = self.expect(TokenKind::Identifier)?.lexeme;

        self.expect(TokenKind::LParen)?;
        self.expect(TokenKind::Void)?;
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;
        let body = self.parse_statement()?;
        self.expect(TokenKind::RBrace)?;

        Ok(FunctionDefinition { name, body })
    }

    // <statement> ::= "return" <exp> ";"
    fn parse_statement(&mut self) -> Result<Statement, ParserError<'a>> {
        self.expect(TokenKind::Return)?;
        let expr = self.parse_expression(Precedence::None)?;

        self.expect(TokenKind::Semicolon)?;

        Ok(Statement::Return(expr))
    }

    fn precedence(token: &Token) -> Precedence {
        match token.kind {
            TokenKind::Plus | TokenKind::Minus => Precedence::Low,
            TokenKind::Mul | TokenKind::Div | TokenKind::Mod => Precedence::High,
            _ => Precedence::Low,
        }
    }

    // <exp> ::= <factor> | <binexp>
    // <binexp> ::= <factor> ("+" | "-" | "*" | "/" | "%") <factor>
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError<'a>> {
        let mut lhs = self.parse_factor()?;
        while let Some(Ok(token)) = self.lexer.peek()
            && Self::precedence(token) >= precedence
        {
            match token.kind {
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Mul
                | TokenKind::Div
                | TokenKind::Mod => {
                    let op = match token.kind {
                        TokenKind::Plus => BinaryOperator::Add,
                        TokenKind::Minus => BinaryOperator::Sub,
                        TokenKind::Mul => BinaryOperator::Mul,
                        TokenKind::Div => BinaryOperator::Div,
                        TokenKind::Mod => BinaryOperator::Mod,
                        _ => return Err(ParserError::InvalidUnaryOperator(token.kind)),
                    };
                    let precedence = Self::precedence(token);
                    self.lexer.next();

                    let rhs = self.parse_expression(precedence.increment())?;
                    lhs = Expression::Binary(op, Box::new(lhs), Box::new(rhs));
                }
                _ => {
                    break;
                }
            }
        }
        Ok(lhs)
    }

    // <factor> ::= constant | "(" <exp> ")" | <unexp>
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
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let expr = self.parse_expression(Precedence::None)?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::Minus | TokenKind::Tilde => self.parse_unary_expression(),
            _ => Err(ParserError::ExpectedAny(
                &[
                    TokenKind::Constant,
                    TokenKind::LParen,
                    TokenKind::Minus,
                    TokenKind::Tilde,
                ],
                tok.kind,
            )),
        }
    }

    // <unexp> ::= <unop> <factor>
    // <unop>  ::= "-" | "~"
    fn parse_unary_expression(&mut self) -> Result<Expression, ParserError<'a>> {
        let tok = self.expect_any(&[TokenKind::Minus, TokenKind::Tilde])?;

        let op = match tok.kind {
            TokenKind::Minus => UnaryOperator::Minus,
            TokenKind::Tilde => UnaryOperator::Tilde,
            _ => return Err(ParserError::InvalidUnaryOperator(tok.kind)),
        };

        let factor = self.parse_factor()?;
        Ok(Expression::Unary(op, Box::new(factor)))
    }
}

#[derive(Debug)]
pub enum ParserError<'a> {
    NoMoreTokens,
    Expected(TokenKind, TokenKind),
    ExpectedAny(&'a [TokenKind], TokenKind),
    LexerError(LexerError<'a>),
    ParseIntError(ParseIntError),
    InvalidUnaryOperator(TokenKind),
    InvalidBinaryOperator(TokenKind),
}

impl Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::NoMoreTokens => write!(f, "All tokens were exhausted"),
            ParserError::Expected(expected, got) => {
                write!(f, "Expected {:?}, got {:?}", expected, got)
            }
            ParserError::LexerError(e) => e.fmt(f),
            ParserError::ParseIntError(e) => e.fmt(f),
            ParserError::ExpectedAny(expected, got) => {
                write!(f, "Expected any of {:?}, got {:?}", expected, got)
            }
            ParserError::InvalidUnaryOperator(op) => write!(f, "Invalid unary operator: {:?}", op),
            ParserError::InvalidBinaryOperator(op) => {
                write!(f, "Invalid binary operator: {:?}", op)
            }
        }
    }
}

impl Error for ParserError<'_> {}

impl<'a> From<LexerError<'a>> for ParserError<'a> {
    fn from(e: LexerError<'a>) -> Self {
        Self::LexerError(e)
    }
}

impl<'a> From<ParseIntError> for ParserError<'a> {
    fn from(e: ParseIntError) -> Self {
        Self::ParseIntError(e)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::BinaryOperator;

    use super::*;

    #[test]
    fn return_0() {
        let mut parser = Parser::new("int main(void) { return 0; }");
        let ast = parser.parse().unwrap();

        assert_eq!(ast.functions.len(), 1);
        assert_eq!(ast.functions[0].name, "main");
        assert_eq!(
            ast.functions[0].body,
            Statement::Return(Expression::Constant(0))
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
            Statement::Return(Expression::Constant(2))
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
            Statement::Return(Expression::Unary(
                UnaryOperator::Minus,
                Box::new(Expression::Constant(2))
            ))
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
            Statement::Return(Expression::Unary(
                UnaryOperator::Tilde,
                Box::new(Expression::Constant(2))
            ))
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
            Statement::Return(Expression::Unary(
                UnaryOperator::Tilde,
                Box::new(Expression::Unary(
                    UnaryOperator::Minus,
                    Box::new(Expression::Constant(2))
                ))
            ))
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
}
