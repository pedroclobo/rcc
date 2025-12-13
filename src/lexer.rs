use std::{error::Error, fmt::Display, iter::Peekable, str::Chars};

#[derive(Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind, lexeme: &'a str) -> Self {
        Self { kind, lexeme }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}(\"{}\")", self.kind, self.lexeme)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Identifier,
    LParen,
    RParen,
    Void,
    Int,
    LBrace,
    RBrace,
    Return,
    Constant,
    Semicolon,
    Plus,
    Minus,
    Mul,
    Div,
    Increment,
    Decrement,
    Tilde,
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    idx: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().peekable(),
            idx: 0,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token<'a>>, LexerError<'a>> {
        self.by_ref().collect()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.idx += 1;
        Some(c)
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn skip_whitespace(&mut self) {
        loop {
            let mut advanced = false;

            while let Some(&c) = self.peek_char() {
                if c.is_whitespace() {
                    self.next_char();
                    advanced = true;
                } else {
                    break;
                }
            }

            if let (Some('/'), Some('/')) = (self.peek_char().cloned(), self.chars.clone().nth(1)) {
                self.next_char();
                self.next_char();
                advanced = true;

                while let Some(c) = self.next_char() {
                    if c == '\n' {
                        break;
                    }
                }
            }

            if let (Some('/'), Some('*')) = (self.peek_char().cloned(), self.chars.clone().nth(1)) {
                self.next_char();
                self.next_char();
                advanced = true;

                while let Some(c) = self.next_char() {
                    if c == '*'
                        && let Some(&'/') = self.peek_char()
                    {
                        self.next_char();
                        break;
                    }
                }
            }

            if !advanced {
                break;
            }
        }
    }

    fn consume_constant(&mut self) -> Result<&'a str, LexerError<'a>> {
        let start = self.idx - 1;
        while let Some(&d) = self.peek_char() {
            if d.is_ascii_digit() {
                self.next_char();
            } else if d.is_alphabetic() {
                return Err(LexerError::InvalidConstant(
                    &self.input[start..self.idx + 1],
                ));
            } else {
                break;
            }
        }
        Ok(&self.input[start..self.idx])
    }

    fn consume_identifier(&mut self) -> &'a str {
        let start = self.idx - 1;
        while let Some(&d) = self.peek_char() {
            if d.is_alphanumeric() {
                self.next_char();
            } else {
                break;
            }
        }
        &self.input[start..self.idx]
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let tok = match self.next_char()? {
            '(' => Ok(Token::new(TokenKind::LParen, "(")),
            ')' => Ok(Token::new(TokenKind::RParen, ")")),
            '{' => Ok(Token::new(TokenKind::LBrace, "{")),
            '}' => Ok(Token::new(TokenKind::RBrace, "}")),
            ';' => Ok(Token::new(TokenKind::Semicolon, ";")),
            '+' => {
                if let Some('+') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(TokenKind::Increment, "++"))
                } else {
                    Ok(Token::new(TokenKind::Plus, "+"))
                }
            }
            '-' => {
                if let Some('-') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(TokenKind::Decrement, "--"))
                } else {
                    Ok(Token::new(TokenKind::Minus, "-"))
                }
            }
            '*' => Ok(Token::new(TokenKind::Mul, "*")),
            '/' => Ok(Token::new(TokenKind::Div, "/")),
            '~' => Ok(Token::new(TokenKind::Tilde, "~")),

            '0'..='9' => self
                .consume_constant()
                .map(|lexeme| Token::new(TokenKind::Constant, lexeme)),

            c if c.is_alphanumeric() => match self.consume_identifier() {
                "return" => Ok(Token::new(TokenKind::Return, "return")),
                "void" => Ok(Token::new(TokenKind::Void, "void")),
                "int" => Ok(Token::new(TokenKind::Int, "int")),
                id => Ok(Token::new(TokenKind::Identifier, id)),
            },

            c => Err(LexerError::InvalidChar(c)),
        };

        Some(tok)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LexerError<'a> {
    InvalidChar(char),
    InvalidConstant(&'a str),
}

impl Display for LexerError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::InvalidChar(c) => write!(f, "Invalid char: {}", c),
            LexerError::InvalidConstant(c) => write!(f, "Invalid constant literal: {}", c),
        }
    }
}

impl Error for LexerError<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Result<Vec<Token<'_>>, LexerError<'_>> {
        Ok(Lexer::new(input).into_iter().flatten().collect::<Vec<_>>())
    }

    fn tok(kind: TokenKind, lexeme: &'_ str) -> Token<'_> {
        Token::new(kind, lexeme)
    }

    #[test]
    fn single_char_tokens() {
        assert_eq!(
            lex("( ) {}  ;").unwrap(),
            vec![
                tok(TokenKind::LParen, "("),
                tok(TokenKind::RParen, ")"),
                tok(TokenKind::LBrace, "{"),
                tok(TokenKind::RBrace, "}"),
                tok(TokenKind::Semicolon, ";"),
            ]
        );
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            lex("int main return void a").unwrap(),
            vec![
                tok(TokenKind::Int, "int"),
                tok(TokenKind::Identifier, "main"),
                tok(TokenKind::Return, "return"),
                tok(TokenKind::Void, "void"),
                tok(TokenKind::Identifier, "a"),
            ]
        );
    }

    #[test]
    fn constants() {
        assert_eq!(
            lex("1 123 3 99").unwrap(),
            vec![
                tok(TokenKind::Constant, "1"),
                tok(TokenKind::Constant, "123"),
                tok(TokenKind::Constant, "3"),
                tok(TokenKind::Constant, "99"),
            ]
        );
    }

    #[test]
    fn return_2() {
        assert_eq!(
            lex("int main(void) { return 2; }").unwrap(),
            vec![
                tok(TokenKind::Int, "int"),
                tok(TokenKind::Identifier, "main"),
                tok(TokenKind::LParen, "("),
                tok(TokenKind::Void, "void"),
                tok(TokenKind::RParen, ")"),
                tok(TokenKind::LBrace, "{"),
                tok(TokenKind::Return, "return"),
                tok(TokenKind::Constant, "2"),
                tok(TokenKind::Semicolon, ";"),
                tok(TokenKind::RBrace, "}"),
            ]
        );
    }

    #[test]
    fn unary() {
        assert_eq!(
            lex("-- - ++ + ~").unwrap(),
            vec![
                tok(TokenKind::Decrement, "--"),
                tok(TokenKind::Minus, "-"),
                tok(TokenKind::Increment, "++"),
                tok(TokenKind::Plus, "+"),
                tok(TokenKind::Tilde, "~"),
            ]
        );
    }
}
