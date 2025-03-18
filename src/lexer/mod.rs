mod token;

use std::{collections::VecDeque, iter::Peekable};

use miette::SourceSpan;
use token::Token;

use crate::error::CompilerError;

#[derive(Debug)]
pub struct Lexer<Input: Iterator<Item = char> + Clone> {
    input: Peekable<Input>,
    offset: usize,
    pending_errors: VecDeque<CompilerError>,
    encountered_fatal: bool,
    eof: bool,
}

impl<Input: Iterator<Item = char> + Clone> Lexer<Input> {
    pub fn new(input: Input) -> Self {
        Self {
            input: input.peekable(),
            offset: 0,
            pending_errors: VecDeque::new(),
            encountered_fatal: false,
            eof: false,
        }
    }

    fn span_now(&mut self, len: usize) -> miette::SourceSpan {
        let span = miette::SourceSpan::new(self.offset.into(), len);
        self.offset += len;
        span
    }
}

impl<Input: Iterator<Item = char> + Clone> Iterator for Lexer<Input> {
    type Item = miette::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.encountered_fatal || self.eof {
            return None;
        }

        // My thoughts as I'm writing this is that this is a little messy,
        // I'm used to using JS generators to lex/parse, where I can yield as many errors as I want
        // before yielding an actual token when doing error recovery, but Rust iterators don't allow something like that.
        // I'm still thinking if using an iterator really is the right approach, but the more I think about it,
        // the more I like this error pattern. Take, for instance, `let str = "abc;`
        // This lexer will correctly recover, return the string token, and then return an error on the subsequeunt
        // token. This works well, since the caller only really wants the next token, while errors encountered
        // throughout are just collected for the final reporting stage.
        // That said, it might be worth considering a static vector instead that just collects errors throughout
        // the parser's lifetime; I'm unsure if the flexibility this current API offers is really worth it.
        if self.pending_errors.len() > 0 {
            return Some(Err(self.pending_errors.pop_front().unwrap().into()));
        }

        match self.input.next() {
            // 1/2 char tokens
            Some(c) => Some(match c {
                '.' => Ok(Token::Dot(self.span_now(1))),
                ',' => Ok(Token::Comma(self.span_now(1))),
                ':' => Ok(Token::Colon(self.span_now(1))),
                ';' => Ok(Token::Semicolon(self.span_now(1))),
                '{' => Ok(Token::LeftBracket(self.span_now(1))),
                '}' => Ok(Token::RightBracket(self.span_now(1))),
                '(' => Ok(Token::LeftParen(self.span_now(1))),
                ')' => Ok(Token::RightParen(self.span_now(1))),
                '[' => Ok(Token::LeftBrace(self.span_now(1))),
                ']' => Ok(Token::RightBrace(self.span_now(1))),
                '+' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::PlusEquals(self.span_now(2))
                } else if self.input.next_if_eq(&'+').is_some() {
                    Token::PlusPlus(self.span_now(2))
                } else {
                    Token::Plus(self.span_now(1))
                }),
                '-' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::MinusEquals(self.span_now(2))
                } else if self.input.next_if_eq(&'-').is_some() {
                    Token::MinusMinus(self.span_now(2))
                } else {
                    Token::Minus(self.span_now(1))
                }),
                '*' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::StarEquals(self.span_now(2))
                } else {
                    Token::Star(self.span_now(1))
                }),
                '/' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::SlashEquals(self.span_now(2))
                } else if self.input.next_if_eq(&'/').is_some() {
                    // Consume the rest of the line
                    let mut lexed = String::new();
                    while self.input.peek().is_some_and(|c| *c != '\n') {
                        lexed.push(self.input.next().unwrap());
                    }

                    Token::Comment(self.span_now(lexed.len() + 2))
                } else {
                    Token::Slash(self.span_now(1))
                }),
                '%' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::PercentEquals(self.span_now(2))
                } else {
                    Token::Percent(self.span_now(1))
                }),
                '=' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::EqualsEquals(self.span_now(2))
                } else if self.input.next_if_eq(&'>').is_some() {
                    Token::Arrow(self.span_now(2))
                } else {
                    Token::Equals(self.span_now(1))
                }),
                '!' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::BangEquals(self.span_now(2))
                } else {
                    Token::Bang(self.span_now(1))
                }),
                '>' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::GreaterEquals(self.span_now(2))
                } else {
                    Token::Greater(self.span_now(1))
                }),
                '<' => Ok(if self.input.next_if_eq(&'=').is_some() {
                    Token::LessEquals(self.span_now(2))
                } else {
                    Token::Less(self.span_now(1))
                }),
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
                                if c == ')' || c == ';' || c == ',' {
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
                        return Some(Ok(Token::String(
                            self.span_now(lexed.len() + additional + 2),
                            lexed,
                        )));
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
                        self.pending_errors.push_back(
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

                        return Some(Ok(Token::String(span, lexed)));
                    } else {
                        // We have no recovery offset, so we need to return an error and also end the iterator
                        self.encountered_fatal = true;
                        return Some(Err(CompilerError::UnterminatedStringLiteral {
                            at: self.span_now(lexed.len() + additional),
                            advice: None,
                            fatal: true,
                        }
                        .into()));
                    }
                }
                // Whitespace
                ' ' | '\r' | '\t' | '\n' => {
                    self.offset += 1;
                    return self.next();
                }
                // Keywords and identifiers
                c => {
                    if c.is_digit(10) {
                        let mut num = String::from(c);
                        while self
                            .input
                            .peek()
                            .is_some_and(|c| c.is_digit(10) || *c == '.')
                        {
                            num.push(self.input.next().unwrap());
                        }

                        return Some(Ok(Token::Number(self.span_now(num.len()), num)));
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
                            "true" => return Some(Ok(Token::True(self.span_now(4)))),
                            "false" => return Some(Ok(Token::False(self.span_now(5)))),
                            "if" => return Some(Ok(Token::If(self.span_now(2)))),
                            "else" => return Some(Ok(Token::Else(self.span_now(4)))),
                            "while" => return Some(Ok(Token::While(self.span_now(5)))),
                            "for" => return Some(Ok(Token::For(self.span_now(3)))),
                            "return" => return Some(Ok(Token::Return(self.span_now(6)))),
                            "break" => return Some(Ok(Token::Break(self.span_now(5)))),
                            "continue" => return Some(Ok(Token::Continue(self.span_now(8)))),
                            "function" => {
                                return Some(Ok(Token::Function(self.span_now(8))));
                            }
                            "let" => return Some(Ok(Token::Let(self.span_now(3)))),
                            "namespace" => {
                                return Some(Ok(Token::Namespace(self.span_now(9))));
                            }
                            "import" => return Some(Ok(Token::Import(self.span_now(6)))),
                            "as" => return Some(Ok(Token::As(self.span_now(2)))),
                            "type" => return Some(Ok(Token::Type(self.span_now(4)))),
                            "pure" => return Some(Ok(Token::Pure(self.span_now(4)))),
                            "struct" => {
                                return Some(Ok(Token::Struct(self.span_now(6))));
                            }
                            "enum" => {
                                return Some(Ok(Token::Enum(self.span_now(4))));
                            }
                            "pub" => {
                                return Some(Ok(Token::Pub(self.span_now(3))));
                            }
                            _ => {
                                return Some(Ok(Token::Identifier(
                                    self.span_now(identifier.len()),
                                    identifier,
                                )));
                            }
                        }
                    } else {
                        return Some(Err(CompilerError::UnknownCharacter {
                            at: self.span_now(1),
                        }
                        .into()));
                    }
                }
            }),
            None => {
                self.eof = true;
                return Some(Ok(Token::EOF));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use miette::SourceSpan;

    #[test]
    fn assignment() {
        let input = "let x = 42;".chars();
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Let(SourceSpan::new(0.into(), 3))
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Identifier(SourceSpan::new(4.into(), 1), "x".to_string())
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Equals(SourceSpan::new(6.into(), 1))
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Number(SourceSpan::new(8.into(), 2), "42".to_string())
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Semicolon(SourceSpan::new(10.into(), 1))
        );
    }

    #[test]
    fn string() {
        let input = "\"Hello, world!\";".chars();
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::String(SourceSpan::new(0.into(), 15), "Hello, world!".to_string())
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Semicolon(SourceSpan::new(15.into(), 1))
        );
        assert_eq!(lexer.next().unwrap().unwrap(), Token::EOF);
        assert!(lexer.next().is_none());
    }

    #[test]
    fn unterminated_string() {
        let input = "\"Hello, world!;".chars();
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::String(SourceSpan::new(0.into(), 15), "Hello, world!".to_string())
        );
        assert!(lexer.next().unwrap().is_err_and(|e| matches!(
            e.downcast().unwrap(),
            CompilerError::UnterminatedStringLiteral {
                at: _,
                advice: _,
                fatal: false,
            }
        )));
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Semicolon(SourceSpan::new(15.into(), 1))
        );
        assert_eq!(lexer.next().unwrap().unwrap(), Token::EOF);
        assert!(lexer.next().is_none());
    }

    #[test]
    fn unterminated_string_fatal() {
        let input = "\"Hello world".chars();
        let mut lexer = Lexer::new(input);

        assert!(lexer.next().unwrap().is_err_and(|e| matches!(
            e.downcast().unwrap(),
            CompilerError::UnterminatedStringLiteral {
                at: _,
                advice: _,
                fatal: true,
            }
        )));
        assert!(lexer.next().is_none());
    }

    #[test]
    fn unexpected_char() {
        let input = "let x = 10; 💝 f(x);".chars();
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Let(SourceSpan::new(0.into(), 3))
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Identifier(SourceSpan::new(4.into(), 1), "x".to_string())
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Equals(SourceSpan::new(6.into(), 1))
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Number(SourceSpan::new(8.into(), 2), "10".to_string())
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Semicolon(SourceSpan::new(10.into(), 1))
        );
        assert!(lexer.next().unwrap().is_err_and(|e| matches!(
            e.downcast().unwrap(),
            CompilerError::UnknownCharacter {
                at: _
            }
        )));
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Identifier(SourceSpan::new(14.into(), 1), "f".to_string())
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::LeftParen(SourceSpan::new(15.into(), 1))
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Identifier(SourceSpan::new(16.into(), 1), "x".to_string())
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::RightParen(SourceSpan::new(17.into(), 1))
        );
        assert_eq!(
            lexer.next().unwrap().unwrap(),
            Token::Semicolon(SourceSpan::new(18.into(), 1))
        );
        assert_eq!(lexer.next().unwrap().unwrap(), Token::EOF);
        assert!(lexer.next().is_none());
    }
}
