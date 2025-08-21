use crate::interning::InternedStr;
use derive_more::{Debug, Display};
use std::num::NonZeroUsize;
use thiserror::Error;

#[derive(Debug, Display, Clone, Copy)]
#[debug("{filepath}:{line}:{column} at byte {position}")]
#[display("{filepath}:{line}:{column}")]
pub struct SourceLocation {
    pub filepath: InternedStr,
    pub position: usize,
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
}

#[derive(Debug, Display, Clone)]
#[display("{kind}")]
pub struct Token {
    pub location: SourceLocation,
    pub kind: TokenKind,
}

#[derive(Debug, Display, Clone)]
pub enum TokenKind {
    #[display("{{newline}}")]
    Newline,

    #[display("(")]
    OpenParenthesis,
    #[display(")")]
    CloseParenthesis,
    #[display("[")]
    OpenBracket,
    #[display("]")]
    CloseBracket,
    #[display("{{")]
    OpenBrace,
    #[display("}}")]
    CloseBrace,

    #[display(":")]
    Colon,
    #[display(",")]
    Comma,
    #[display("=")]
    Equals,
    #[display("=>")]
    FatRightArrow,
    #[display("-")]
    Minus,
    #[display("->")]
    RightArrow,

    #[display("{_0}")]
    Name(InternedStr),
    #[display("_")]
    Discard,
    #[display("fn keyword")]
    FnKeyword,
    #[display("struct keyword")]
    StructKeyword,
    #[display("enum keyword")]
    EnumKeyword,
    #[display("let keyword")]
    LetKeyword,
    #[display("match keyword")]
    MatchKeyword,
    #[display("const keyword")]
    ConstKeyword,
}

#[derive(Debug, Clone)]
pub struct Lexer<'source> {
    location: SourceLocation,
    source: &'source str,
}

#[derive(Debug, Clone, Error)]
#[error("{location}: {kind}")]
pub struct LexingError {
    pub location: SourceLocation,
    pub kind: LexerErrorKind,
}

#[derive(Debug, Display, Clone)]
pub enum LexerErrorKind {
    #[display("Unexpected EOF")]
    UnexpectedEOF,
    #[display("Unexpected character {_0:?}")]
    UnexpectedChar(char),
}

impl<'source> Lexer<'source> {
    pub fn new(filepath: InternedStr, source: &'source str) -> Self {
        Self {
            location: SourceLocation {
                filepath,
                position: 0,
                line: NonZeroUsize::MIN,
                column: NonZeroUsize::MIN,
            },
            source,
        }
    }

    pub fn location(&self) -> SourceLocation {
        self.location
    }

    pub fn source(&self) -> &'source str {
        self.source
    }

    fn peek_char(&self) -> Option<char> {
        self.source[self.location.position..].chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;

        self.location.position += c.len_utf8();
        if c == '\n' {
            self.location.line = self.location.line.checked_add(1).unwrap();
            self.location.column = NonZeroUsize::MIN;
        } else {
            self.location.column = self.location.column.checked_add(1).unwrap();
        }

        Some(c)
    }

    pub fn is_empty(&self) -> bool {
        self.peek_token()
            .is_err_and(|e| matches!(e.kind, LexerErrorKind::UnexpectedEOF))
    }

    pub fn peek_token(&self) -> Result<Token, LexingError> {
        self.clone().next_token()
    }

    pub fn next_token(&mut self) -> Result<Token, LexingError> {
        loop {
            let start_location = self.location;
            break Ok(Token {
                location: start_location,
                kind: match self.next_char().ok_or(LexingError {
                    location: start_location,
                    kind: LexerErrorKind::UnexpectedEOF,
                })? {
                    // whitespace
                    ' ' | '\t' | '\r' => continue,

                    '\n' => TokenKind::Newline,

                    '(' => TokenKind::OpenParenthesis,
                    ')' => TokenKind::CloseParenthesis,
                    '[' => TokenKind::OpenBracket,
                    ']' => TokenKind::CloseBracket,
                    '{' => TokenKind::OpenBrace,
                    '}' => TokenKind::CloseBrace,

                    ':' => TokenKind::Colon,
                    ',' => TokenKind::Comma,
                    '=' => {
                        if let Some('>') = self.peek_char() {
                            self.next_char();
                            TokenKind::FatRightArrow
                        } else {
                            TokenKind::Equals
                        }
                    }
                    '-' => {
                        if let Some('>') = self.peek_char() {
                            self.next_char();
                            TokenKind::RightArrow
                        } else {
                            TokenKind::Minus
                        }
                    }

                    // identifiers
                    'A'..='Z' | 'a'..='z' | '_' => {
                        while let Some('A'..='Z' | 'a'..='z' | '0'..='9' | '_') = self.peek_char() {
                            self.next_char();
                        }

                        match &self.source[start_location.position..self.location.position] {
                            "_" => TokenKind::Discard,
                            "fn" => TokenKind::FnKeyword,
                            "struct" => TokenKind::StructKeyword,
                            "enum" => TokenKind::EnumKeyword,
                            "let" => TokenKind::LetKeyword,
                            "match" => TokenKind::MatchKeyword,
                            "const" => TokenKind::ConstKeyword,
                            name => TokenKind::Name(name.into()),
                        }
                    }

                    c => {
                        return Err(LexingError {
                            location: start_location,
                            kind: LexerErrorKind::UnexpectedChar(c),
                        });
                    }
                },
            });
        }
    }
}
