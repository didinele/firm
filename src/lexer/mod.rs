pub mod token;

use std::iter::Peekable;

use miette::SourceSpan;
use token::{Token, TokenKind};

use crate::error::CompilerError;

#[derive(Debug)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<CompilerError>,
    pub fatal: bool,
}

#[derive(Debug)]
pub struct Lexer {
    input: Peekable<std::str::Chars<'static>>,
    offset: usize,
    errors: Vec<CompilerError>,
}

impl Lexer {
    pub fn new(src: &'static str) -> Self {
        Self {
            input: src.chars().peekable(),
            offset: 0,
            errors: vec![],
        }
    }

    fn span_now(&mut self, len: usize) -> SourceSpan {
        let span = SourceSpan::new(self.offset.into(), len);
        self.offset += len;
        span
    }

    fn token_now(&mut self, kind: TokenKind, len: usize) -> Token {
        Token::new(kind, self.span_now(len))
    }

    /// Attempts to read the next token from the input stream.
    /// If it encounters an error, it will push it to the errors vector and return None.
    /// Additionally, it will also return None for whitespaces.
    /// This method is to be called until an EOF token is reached.
    fn try_get_token(&mut self) -> Option<Token> {
        match self.input.next() {
            // 1/2 char tokens
            Some(c) => Some(match c {
                '.' => self.token_now(TokenKind::Dot, 1),
                ',' => self.token_now(TokenKind::Comma, 1),
                ':' => self.token_now(TokenKind::Colon, 1),
                ';' => self.token_now(TokenKind::Semicolon, 1),
                '{' => self.token_now(TokenKind::LeftCurly, 1),
                '}' => self.token_now(TokenKind::RightCurly, 1),
                '(' => self.token_now(TokenKind::LeftParen, 1),
                ')' => self.token_now(TokenKind::RightParen, 1),
                '[' => self.token_now(TokenKind::LeftSquare, 1),
                ']' => self.token_now(TokenKind::RightSquare, 1),
                '+' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::PlusEquals, 2)
                    } else if self.input.next_if_eq(&'+').is_some() {
                        self.token_now(TokenKind::PlusPlus, 2)
                    } else {
                        self.token_now(TokenKind::Plus, 1)
                    }
                }
                '-' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::MinusEquals, 2)
                    } else if self.input.next_if_eq(&'-').is_some() {
                        self.token_now(TokenKind::MinusMinus, 2)
                    } else {
                        self.token_now(TokenKind::Minus, 1)
                    }
                }
                '*' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::StarEquals, 2)
                    } else {
                        self.token_now(TokenKind::Star, 1)
                    }
                }
                '/' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::SlashEquals, 2)
                    } else if self.input.next_if_eq(&'/').is_some() {
                        // Start with the slashes
                        let mut len = 2;
                        // Consume the rest of the line
                        while self.input.peek().is_some_and(|c| *c != '\n') {
                            self.input.next().unwrap();
                            len += 1;
                        }

                        self.token_now(TokenKind::Comment, len)
                    } else {
                        self.token_now(TokenKind::Slash, 1)
                    }
                }
                '%' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::PercentEquals, 2)
                    } else {
                        self.token_now(TokenKind::Percent, 1)
                    }
                }
                '=' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::EqualsEquals, 2)
                    } else if self.input.next_if_eq(&'>').is_some() {
                        self.token_now(TokenKind::Arrow, 2)
                    } else {
                        self.token_now(TokenKind::Equals, 1)
                    }
                }
                '!' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::BangEquals, 2)
                    } else {
                        self.token_now(TokenKind::Bang, 1)
                    }
                }
                '>' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::GreaterEquals, 2)
                    } else {
                        self.token_now(TokenKind::Greater, 1)
                    }
                }
                '<' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        self.token_now(TokenKind::LessEquals, 2)
                    } else {
                        self.token_now(TokenKind::Less, 1)
                    }
                }
                // Literals
                '"' => {
                    let mut lexed = String::new();
                    // We need to make sure we don't treat an escaped quote
                    // as the end of the string
                    let mut escaped = false;

                    let mut recovery_offset = None;
                    let mut input_clone = self.input.clone();
                    let mut local_offset = self.offset;
                    let mut found = false;
                    let mut additional = 0;

                    while let Some(c) = input_clone.next() {
                        local_offset += 1;

                        match c {
                            '\\' => {
                                escaped = !escaped;
                                additional += 1;
                            }
                            '"' if !escaped => {
                                // We found the end of the string
                                found = true;
                                break;
                            }
                            c => {
                                // Few recovery cases, while at it
                                if recovery_offset.is_none() && (c == ')' || c == ';' || c == ',') {
                                    // fn("something);
                                    // let x = "something;
                                    // fn("something, "something else");
                                    recovery_offset = Some(local_offset);
                                }

                                if escaped {
                                    continue;
                                }

                                lexed.push(c);
                            }
                        };
                    }

                    if found {
                        // We can advance by just overwriting self.input
                        self.input = input_clone;
                        return Some(
                            self.token_now(TokenKind::String, lexed.len() + additional + 2),
                        );
                    }

                    // Let's try recovering
                    if let Some(recovery_offset) = recovery_offset {
                        let mut lexed = String::new();
                        let mut start = self.offset;
                        // Consume everything from offset to recovery_offset
                        while start < recovery_offset - 1 {
                            if let Some(c) = self.input.next() {
                                lexed.push(c);
                                start += 1;
                            }
                        }

                        // At this point, we pretend this character is a quote and that we have a complete string
                        let span = self.span_now(lexed.len() + additional + 2);
                        self.errors.push(
                            CompilerError::UnterminatedStringLiteral {
                                at: span,
                                advice: Some(SourceSpan::new(
                                    (span.offset() + span.len() - 1).into(),
                                    1,
                                )),
                                fatal: false,
                            }
                            .into(),
                        );

                        Token::new(TokenKind::String, span)
                    } else {
                        // We have no recovery offset, so we need to return an error and also end the iterator
                        let at = self.span_now(lexed.len() + additional);
                        self.errors.push(CompilerError::UnterminatedStringLiteral {
                            at,
                            advice: None,
                            fatal: true,
                        });
                        Token::new(TokenKind::EOF, 0.into())
                    }
                }
                // Whitespace
                ' ' | '\r' | '\t' | '\n' => {
                    self.offset += 1;
                    return None;
                }
                // Keywords and identifiers
                c => {
                    if c.is_digit(10) {
                        let mut len = 1;
                        while self
                            .input
                            .peek()
                            .is_some_and(|c| c.is_digit(10) || *c == '.')
                        {
                            len += 1;
                            self.input.next().unwrap();
                        }

                        self.token_now(TokenKind::Number, len)
                    } else if c.is_alphanumeric() || c == '_' {
                        let mut identifier = String::from(c);
                        while self
                            .input
                            .peek()
                            .is_some_and(|c| c.is_alphanumeric() || *c == '_')
                        {
                            identifier.push(self.input.next().unwrap());
                        }

                        // Check if the identifier is a keyword
                        match identifier.as_str() {
                            "true" => self.token_now(TokenKind::True, 4),
                            "false" => self.token_now(TokenKind::False, 5),
                            "if" => self.token_now(TokenKind::If, 2),
                            "else" => self.token_now(TokenKind::Else, 4),
                            "while" => self.token_now(TokenKind::While, 5),
                            "for" => self.token_now(TokenKind::For, 3),
                            "return" => self.token_now(TokenKind::Return, 6),
                            "break" => self.token_now(TokenKind::Break, 5),
                            "continue" => self.token_now(TokenKind::Continue, 8),
                            "function" => self.token_now(TokenKind::Function, 8),
                            "let" => self.token_now(TokenKind::Let, 3),
                            "import" => self.token_now(TokenKind::Import, 6),
                            "as" => self.token_now(TokenKind::As, 2),
                            "type" => self.token_now(TokenKind::Type, 4),
                            "pure" => self.token_now(TokenKind::Pure, 4),
                            "const" => self.token_now(TokenKind::Const, 5),
                            "struct" => self.token_now(TokenKind::Struct, 6),
                            "enum" => self.token_now(TokenKind::Enum, 4),
                            "pub" => self.token_now(TokenKind::Pub, 3),
                            "static" => self.token_now(TokenKind::Static, 6),
                            _ => self.token_now(TokenKind::Identifier, identifier.len()),
                        }
                    } else {
                        let at = self.span_now(1);
                        self.errors.push(CompilerError::UnknownCharacter { at });
                        return None;
                    }
                }
            }),
            None => Some(self.token_now(TokenKind::EOF, 0)),
        }
    }

    pub fn lex(mut self) -> LexerResult {
        let mut tokens = vec![];

        loop {
            match self.try_get_token() {
                Some(token) => match token.kind() {
                    TokenKind::EOF => {
                        tokens.push(token);
                        break;
                    }
                    _ => tokens.push(token),
                },
                // We encountered an error
                None => continue,
            }
        }

        let mut fatal = false;
        let errors = self
            .errors
            .into_iter()
            .inspect(|err| {
                if err.is_fatal() {
                    fatal = true;
                }
            })
            .collect();

        LexerResult {
            tokens,
            errors,
            fatal,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use miette::SourceSpan;

    #[test]
    fn assignment() {
        let src = "let x = 42;";
        let lexer = Lexer::new(src);
        let result = lexer.lex();

        assert!(!result.fatal);
        assert!(result.errors.is_empty());
        assert_eq!(
            result.tokens,
            vec![
                Token::new(TokenKind::Let, SourceSpan::new(0.into(), 3)),
                Token::new(TokenKind::Identifier, SourceSpan::new(4.into(), 1)),
                Token::new(TokenKind::Equals, SourceSpan::new(6.into(), 1)),
                Token::new(TokenKind::Number, SourceSpan::new(8.into(), 2)),
                Token::new(TokenKind::Semicolon, SourceSpan::new(10.into(), 1)),
                Token::new(TokenKind::EOF, SourceSpan::new(11.into(), 0)),
            ]
        );
    }

    #[test]
    fn string() {
        let src = "\"Hello, world!\";";
        let lexer = Lexer::new(src);
        let result = lexer.lex();

        assert!(!result.fatal);
        assert!(result.errors.is_empty());
        assert_eq!(
            result.tokens,
            vec![
                Token::new(TokenKind::String, SourceSpan::new(0.into(), 15)),
                Token::new(TokenKind::Semicolon, SourceSpan::new(15.into(), 1)),
                Token::new(TokenKind::EOF, SourceSpan::new(16.into(), 0)),
            ]
        );
    }

    #[test]
    fn unterminated_string() {
        let src = "\"Hello world!;";
        let lexer = Lexer::new(src);
        let result = lexer.lex();

        assert!(!result.fatal);
        assert_eq!(
            result.tokens,
            vec![
                Token::new(TokenKind::String, SourceSpan::new(0.into(), 14)),
                Token::new(TokenKind::Semicolon, SourceSpan::new(14.into(), 1)),
                Token::new(TokenKind::EOF, SourceSpan::new(15.into(), 0)),
            ]
        );

        assert_eq!(result.errors.len(), 1);
        assert!(matches!(
            result.errors[0],
            CompilerError::UnterminatedStringLiteral {
                at: _,
                advice: Some(_),
                fatal: false,
            }
        ));
    }

    #[test]
    fn unterminated_string_fatal() {
        let src = "\"Hello world";
        let lexer = Lexer::new(src);
        let result = lexer.lex();

        assert!(result.fatal);
        assert_eq!(
            result.tokens,
            vec![Token::new(TokenKind::EOF, SourceSpan::new(0.into(), 0))]
        );

        assert_eq!(result.errors.len(), 1);
        assert!(matches!(
            result.errors[0],
            CompilerError::UnterminatedStringLiteral {
                at: _,
                advice: None,
                fatal: true,
            }
        ));
    }

    #[test]
    fn unexpected_char() {
        let src = "let x = 10; 💝 f(x);";
        let lexer = Lexer::new(src);
        let result = lexer.lex();

        assert!(!result.fatal);
        assert_eq!(
            result.tokens,
            vec![
                Token::new(TokenKind::Let, SourceSpan::new(0.into(), 3)),
                Token::new(TokenKind::Identifier, SourceSpan::new(4.into(), 1)),
                Token::new(TokenKind::Equals, SourceSpan::new(6.into(), 1)),
                Token::new(TokenKind::Number, SourceSpan::new(8.into(), 2)),
                Token::new(TokenKind::Semicolon, SourceSpan::new(10.into(), 1)),
                Token::new(TokenKind::Identifier, SourceSpan::new(14.into(), 1)),
                Token::new(TokenKind::LeftParen, SourceSpan::new(15.into(), 1)),
                Token::new(TokenKind::Identifier, SourceSpan::new(16.into(), 1)),
                Token::new(TokenKind::RightParen, SourceSpan::new(17.into(), 1)),
                Token::new(TokenKind::Semicolon, SourceSpan::new(18.into(), 1)),
                Token::new(TokenKind::EOF, SourceSpan::new(19.into(), 0)),
            ]
        );

        assert_eq!(result.errors.len(), 1);
        assert!(matches!(
            result.errors[0],
            CompilerError::UnknownCharacter { at: _ }
        ));
    }
}
