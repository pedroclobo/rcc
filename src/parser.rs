use std::{error::Error, fmt::Display, iter::Peekable, num::ParseIntError};

use crate::{
    LexerError, Token,
    ast::{Expression, FunctionDefinition, Program, Statement},
    lexer::{Lexer, TokenKind},
};

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

    fn parse_statement(&mut self) -> Result<Statement, ParserError<'a>> {
        self.expect(TokenKind::Return)?;
        let expr = self.parse_expression()?;

        self.expect(TokenKind::Semicolon)?;

        Ok(Statement::Return(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError<'a>> {
        Ok(Expression::Constant(
            self.expect(TokenKind::Constant)?.lexeme.parse()?,
        ))
    }
}

#[derive(Debug)]
pub enum ParserError<'a> {
    NoMoreTokens,
    Expected(TokenKind, TokenKind),
    LexerError(LexerError<'a>),
    ParseIntError(ParseIntError),
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
}
