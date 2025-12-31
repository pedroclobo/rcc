mod error;
mod token;

use miette::SourceSpan;
use std::{iter::Peekable, str::Chars};
pub use token::{Token, TokenKind};

pub use error::LexerError;

use crate::parser::Span;

#[derive(Debug, Clone)]
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

    pub fn lex(&mut self) -> Result<Vec<Token<'a>>, LexerError> {
        self.by_ref().collect()
    }

    pub fn span(&self) -> SourceSpan {
        SourceSpan::new(self.idx.into(), self.input.len() - self.idx)
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

            if let Some('#') = self.peek_char() {
                self.next_char();
                advanced = true;

                while let Some(c) = self.next_char() {
                    if c == '\n' {
                        break;
                    }
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

    fn consume_constant(&mut self) -> Result<&'a str, LexerError> {
        let start = self.idx - 1;
        while let Some(&d) = self.peek_char() {
            if d.is_ascii_digit() {
                self.next_char();
            } else if d.is_alphabetic() {
                return Err(LexerError::InvalidIntegerLiteral {
                    constant: self.input[start..self.idx + 1].to_string(),
                    span: SourceSpan::new(start.into(), (self.idx + 1) - start),
                });
            } else {
                break;
            }
        }
        Ok(&self.input[start..self.idx])
    }

    fn consume_identifier(&mut self) -> &'a str {
        let start = self.idx - 1;
        while let Some(&d) = self.peek_char() {
            if d.is_alphanumeric() || d == '_' {
                self.next_char();
            } else {
                break;
            }
        }
        &self.input[start..self.idx]
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let start = self.idx;

        let tok = match self.next_char()? {
            '(' => Ok(Token::new(
                TokenKind::LParen,
                "(",
                Span::new(start, self.idx),
            )),
            ')' => Ok(Token::new(
                TokenKind::RParen,
                ")",
                Span::new(start, self.idx),
            )),
            '{' => Ok(Token::new(
                TokenKind::LBrace,
                "{",
                Span::new(start, self.idx),
            )),
            '}' => Ok(Token::new(
                TokenKind::RBrace,
                "}",
                Span::new(start, self.idx),
            )),
            ';' => Ok(Token::new(
                TokenKind::Semicolon,
                ";",
                Span::new(start, self.idx),
            )),
            '+' => {
                if let Some('+') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::PlusPlus,
                        "++",
                        Span::new(start, self.idx),
                    ))
                } else if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::PlusEq,
                        "+=",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(TokenKind::Plus, "+", Span::new(start, self.idx)))
                }
            }
            '-' => {
                if let Some('-') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::MinusMinus,
                        "--",
                        Span::new(start, self.idx),
                    ))
                } else if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::MinusEq,
                        "-=",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(
                        TokenKind::Minus,
                        "-",
                        Span::new(start, self.idx),
                    ))
                }
            }
            '*' => {
                if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::MulEq,
                        "*=",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(TokenKind::Mul, "*", Span::new(start, self.idx)))
                }
            }
            '/' => {
                if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::DivEq,
                        "/=",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(TokenKind::Div, "/", Span::new(start, self.idx)))
                }
            }
            '%' => {
                if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::ModEq,
                        "%=",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(TokenKind::Mod, "%", Span::new(start, self.idx)))
                }
            }
            '~' => Ok(Token::new(
                TokenKind::Tilde,
                "~",
                Span::new(start, self.idx),
            )),
            '&' => {
                if let Some('&') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(TokenKind::And, "&&", Span::new(start, self.idx)))
                } else if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::AmpersandEq,
                        "&=",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(
                        TokenKind::Ampersand,
                        "&",
                        Span::new(start, self.idx),
                    ))
                }
            }
            '|' => {
                if let Some('|') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(TokenKind::Or, "||", Span::new(start, self.idx)))
                } else if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::PipeEq,
                        "|=",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(TokenKind::Pipe, "|", Span::new(start, self.idx)))
                }
            }
            '^' => {
                if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::CaretEq,
                        "^=",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(
                        TokenKind::Caret,
                        "^",
                        Span::new(start, self.idx),
                    ))
                }
            }
            '!' => {
                if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(TokenKind::Neq, "!=", Span::new(start, self.idx)))
                } else {
                    Ok(Token::new(TokenKind::Bang, "!", Span::new(start, self.idx)))
                }
            }
            '<' => {
                if let Some('<') = self.peek_char() {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        Ok(Token::new(
                            TokenKind::LShiftEq,
                            "<<=",
                            Span::new(start, self.idx),
                        ))
                    } else {
                        Ok(Token::new(
                            TokenKind::LShift,
                            "<<",
                            Span::new(start, self.idx),
                        ))
                    }
                } else if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(TokenKind::Le, "<=", Span::new(start, self.idx)))
                } else {
                    Ok(Token::new(TokenKind::Lt, "<", Span::new(start, self.idx)))
                }
            }
            '>' => {
                if let Some('>') = self.peek_char() {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        Ok(Token::new(
                            TokenKind::RShiftEq,
                            ">>=",
                            Span::new(start, self.idx),
                        ))
                    } else {
                        Ok(Token::new(
                            TokenKind::RShift,
                            ">>",
                            Span::new(start, self.idx),
                        ))
                    }
                } else if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(TokenKind::Ge, ">=", Span::new(start, self.idx)))
                } else {
                    Ok(Token::new(TokenKind::Gt, ">", Span::new(start, self.idx)))
                }
            }
            '=' => {
                if let Some('=') = self.peek_char() {
                    self.next_char();
                    Ok(Token::new(
                        TokenKind::EqEq,
                        "==",
                        Span::new(start, self.idx),
                    ))
                } else {
                    Ok(Token::new(TokenKind::Eq, "=", Span::new(start, self.idx)))
                }
            }
            '?' => Ok(Token::new(
                TokenKind::QMark,
                "?",
                Span::new(start, self.idx),
            )),
            ':' => Ok(Token::new(
                TokenKind::Colon,
                ":",
                Span::new(start, self.idx),
            )),

            '0'..='9' => self
                .consume_constant()
                .map(|lexeme| Token::new(TokenKind::Constant, lexeme, Span::new(start, self.idx))),

            c if c.is_alphanumeric() || c == '_' => match self.consume_identifier() {
                "return" => Ok(Token::new(
                    TokenKind::Return,
                    "return",
                    Span::new(start, self.idx),
                )),
                "void" => Ok(Token::new(
                    TokenKind::Void,
                    "void",
                    Span::new(start, self.idx),
                )),
                "int" => Ok(Token::new(
                    TokenKind::Int,
                    "int",
                    Span::new(start, self.idx),
                )),
                "if" => Ok(Token::new(TokenKind::If, "if", Span::new(start, self.idx))),
                "else" => Ok(Token::new(
                    TokenKind::Else,
                    "else",
                    Span::new(start, self.idx),
                )),
                "goto" => Ok(Token::new(
                    TokenKind::Goto,
                    "goto",
                    Span::new(start, self.idx),
                )),
                "do" => Ok(Token::new(TokenKind::Do, "do", Span::new(start, self.idx))),
                "while" => Ok(Token::new(
                    TokenKind::While,
                    "while",
                    Span::new(start, self.idx),
                )),
                "for" => Ok(Token::new(
                    TokenKind::For,
                    "for",
                    Span::new(start, self.idx),
                )),
                "break" => Ok(Token::new(
                    TokenKind::Break,
                    "break",
                    Span::new(start, self.idx),
                )),
                "continue" => Ok(Token::new(
                    TokenKind::Continue,
                    "continue",
                    Span::new(start, self.idx),
                )),
                id => Ok(Token::new(
                    TokenKind::Identifier,
                    id,
                    Span::new(start, self.idx),
                )),
            },

            c => Err(LexerError::InvalidChar {
                char: c,
                span: SourceSpan::new(start.into(), 1),
            }),
        };

        Some(tok)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Result<Vec<Token<'_>>, LexerError> {
        Ok(Lexer::new(input).into_iter().flatten().collect::<Vec<_>>())
    }

    fn tok(kind: TokenKind, lexeme: &'_ str) -> Token<'_> {
        Token::new(kind, lexeme, Span::new(0, 0))
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
                tok(TokenKind::MinusMinus, "--"),
                tok(TokenKind::Minus, "-"),
                tok(TokenKind::PlusPlus, "++"),
                tok(TokenKind::Plus, "+"),
                tok(TokenKind::Tilde, "~"),
            ]
        );
    }

    #[test]
    fn bitwise() {
        assert_eq!(
            lex("& | ^ << >>").unwrap(),
            vec![
                tok(TokenKind::Ampersand, "&"),
                tok(TokenKind::Pipe, "|"),
                tok(TokenKind::Caret, "^"),
                tok(TokenKind::LShift, "<<"),
                tok(TokenKind::RShift, ">>"),
            ]
        );
    }

    #[test]
    fn logical_and_relational() {
        assert_eq!(
            lex("&& || == != < > <= >=").unwrap(),
            vec![
                tok(TokenKind::And, "&&"),
                tok(TokenKind::Or, "||"),
                tok(TokenKind::EqEq, "=="),
                tok(TokenKind::Neq, "!="),
                tok(TokenKind::Lt, "<"),
                tok(TokenKind::Gt, ">"),
                tok(TokenKind::Le, "<="),
                tok(TokenKind::Ge, ">="),
            ]
        );
    }

    #[test]
    fn assignment() {
        assert_eq!(lex("=").unwrap(), vec![tok(TokenKind::Eq, "="),]);
    }

    #[test]
    fn compound_assignment() {
        assert_eq!(
            lex("+= -= *= /= %= &= |= ^= <<= >>=").unwrap(),
            vec![
                tok(TokenKind::PlusEq, "+="),
                tok(TokenKind::MinusEq, "-="),
                tok(TokenKind::MulEq, "*="),
                tok(TokenKind::DivEq, "/="),
                tok(TokenKind::ModEq, "%="),
                tok(TokenKind::AmpersandEq, "&="),
                tok(TokenKind::PipeEq, "|="),
                tok(TokenKind::CaretEq, "^="),
                tok(TokenKind::LShiftEq, "<<="),
                tok(TokenKind::RShiftEq, ">>="),
            ]
        );
    }

    #[test]
    fn conditionals() {
        assert_eq!(
            lex("if else ? :").unwrap(),
            vec![
                tok(TokenKind::If, "if"),
                tok(TokenKind::Else, "else"),
                tok(TokenKind::QMark, "?"),
                tok(TokenKind::Colon, ":"),
            ]
        );
    }

    #[test]
    fn goto() {
        assert_eq!(
            lex("goto _label;").unwrap(),
            vec![
                tok(TokenKind::Goto, "goto"),
                tok(TokenKind::Identifier, "_label"),
                tok(TokenKind::Semicolon, ";"),
            ]
        );
    }

    #[test]
    fn loops() {
        assert_eq!(
            lex("do while for break continue").unwrap(),
            vec![
                tok(TokenKind::Do, "do"),
                tok(TokenKind::While, "while"),
                tok(TokenKind::For, "for"),
                tok(TokenKind::Break, "break"),
                tok(TokenKind::Continue, "continue"),
            ]
        );
    }
}
