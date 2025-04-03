pub mod token;

use std::iter::Peekable;

use miette::SourceSpan;
use token::Token;

use crate::error::CompilerError;

#[derive(Debug)]
pub struct LexerResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<CompilerError>,
    pub fatal: bool,
}

#[derive(Debug)]
pub struct Lexer<Input: Iterator<Item = char> + Clone> {
    input: Peekable<Input>,
    offset: usize,
    pub errors: Vec<CompilerError>,
}

impl<Input: Iterator<Item = char> + Clone> Lexer<Input> {
    pub fn new(input: Input) -> Self {
        Self {
            input: input.peekable(),
            offset: 0,
            errors: vec![],
        }
    }

    fn span_now(&mut self, len: usize) -> SourceSpan {
        let span = SourceSpan::new(self.offset.into(), len);
        self.offset += len;
        span
    }

    /// Attempts to read the next token from the input stream.
    /// If it encounters an error, it will push it to the errors vector and return None.
    /// Additionally, it will also return None for whitespaces.
    /// This method is to be called until an EOF token is reached.
    fn try_get_token(&mut self) -> Option<Token> {
        match self.input.next() {
            // 1/2 char tokens
            Some(c) => Some(match c {
                '.' => Token::Dot(self.span_now(1)),
                ',' => Token::Comma(self.span_now(1)),
                ':' => Token::Colon(self.span_now(1)),
                ';' => Token::Semicolon(self.span_now(1)),
                '{' => Token::LeftCurly(self.span_now(1)),
                '}' => Token::RightCurly(self.span_now(1)),
                '(' => Token::LeftParen(self.span_now(1)),
                ')' => Token::RightParen(self.span_now(1)),
                '[' => Token::LeftSquare(self.span_now(1)),
                ']' => Token::RightSquare(self.span_now(1)),
                '+' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Token::PlusEquals(self.span_now(2))
                    } else if self.input.next_if_eq(&'+').is_some() {
                        Token::PlusPlus(self.span_now(2))
                    } else {
                        Token::Plus(self.span_now(1))
                    }
                }
                '-' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Token::MinusEquals(self.span_now(2))
                    } else if self.input.next_if_eq(&'-').is_some() {
                        Token::MinusMinus(self.span_now(2))
                    } else {
                        Token::Minus(self.span_now(1))
                    }
                }
                '*' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Token::StarEquals(self.span_now(2))
                    } else {
                        Token::Star(self.span_now(1))
                    }
                }
                '/' => {
                    if self.input.next_if_eq(&'=').is_some() {
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
                    }
                }
                '%' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Token::PercentEquals(self.span_now(2))
                    } else {
                        Token::Percent(self.span_now(1))
                    }
                }
                '=' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Token::EqualsEquals(self.span_now(2))
                    } else if self.input.next_if_eq(&'>').is_some() {
                        Token::Arrow(self.span_now(2))
                    } else {
                        Token::Equals(self.span_now(1))
                    }
                }
                '!' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Token::BangEquals(self.span_now(2))
                    } else {
                        Token::Bang(self.span_now(1))
                    }
                }
                '>' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Token::GreaterEquals(self.span_now(2))
                    } else {
                        Token::Greater(self.span_now(1))
                    }
                }
                '<' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Token::LessEquals(self.span_now(2))
                    } else {
                        Token::Less(self.span_now(1))
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
                        return Some(Token::String(
                            self.span_now(lexed.len() + additional + 2),
                            lexed,
                        ));
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

                        Token::String(span, lexed)
                    } else {
                        // We have no recovery offset, so we need to return an error and also end the iterator
                        let at = self.span_now(lexed.len() + additional);
                        self.errors.push(CompilerError::UnterminatedStringLiteral {
                            at,
                            advice: None,
                            fatal: true,
                        });
                        Token::EOF(0.into())
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
                        let mut num = String::from(c);
                        while self
                            .input
                            .peek()
                            .is_some_and(|c| c.is_digit(10) || *c == '.')
                        {
                            num.push(self.input.next().unwrap());
                        }

                        Token::Number(self.span_now(num.len()), num)
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
                            "true" => Token::True(self.span_now(4)),
                            "false" => Token::False(self.span_now(5)),
                            "if" => Token::If(self.span_now(2)),
                            "else" => Token::Else(self.span_now(4)),
                            "while" => Token::While(self.span_now(5)),
                            "for" => Token::For(self.span_now(3)),
                            "return" => Token::Return(self.span_now(6)),
                            "break" => Token::Break(self.span_now(5)),
                            "continue" => Token::Continue(self.span_now(8)),
                            "function" => Token::Function(self.span_now(8)),
                            "let" => Token::Let(self.span_now(3)),
                            "import" => Token::Import(self.span_now(6)),
                            "as" => Token::As(self.span_now(2)),
                            "type" => Token::Type(self.span_now(4)),
                            "pure" => Token::Pure(self.span_now(4)),
                            "const" => Token::Const(self.span_now(5)),
                            "struct" => Token::Struct(self.span_now(6)),
                            "enum" => Token::Enum(self.span_now(4)),
                            "pub" => Token::Pub(self.span_now(3)),
                            "static" => Token::Static(self.span_now(6)),
                            _ => Token::Identifier(self.span_now(identifier.len()), identifier),
                        }
                    } else {
                        let at = self.span_now(1);
                        self.errors.push(CompilerError::UnknownCharacter { at });
                        return None;
                    }
                }
            }),
            None => Some(Token::EOF(self.span_now(0))),
        }
    }

    pub fn lex(mut self) -> LexerResult {
        let mut tokens = vec![];

        loop {
            match self.try_get_token() {
                Some(token @ Token::EOF(..)) => {
                    tokens.push(token);
                    break;
                }
                Some(token) => tokens.push(token),
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
        let input = "let x = 42;".chars();
        let lexer = Lexer::new(input);
        let result = lexer.lex();

        assert!(!result.fatal);
        assert!(result.errors.is_empty());
        assert_eq!(
            result.tokens,
            vec![
                Token::Let(SourceSpan::new(0.into(), 3)),
                Token::Identifier(SourceSpan::new(4.into(), 1), "x".to_string()),
                Token::Equals(SourceSpan::new(6.into(), 1)),
                Token::Number(SourceSpan::new(8.into(), 2), "42".to_string()),
                Token::Semicolon(SourceSpan::new(10.into(), 1)),
                Token::EOF(SourceSpan::new(11.into(), 0)),
            ]
        );
    }

    #[test]
    fn string() {
        let input = "\"Hello, world!\";".chars();
        let lexer = Lexer::new(input);
        let result = lexer.lex();

        assert!(!result.fatal);
        assert!(result.errors.is_empty());
        assert_eq!(
            result.tokens,
            vec![
                Token::String(SourceSpan::new(0.into(), 15), "Hello, world!".to_string()),
                Token::Semicolon(SourceSpan::new(15.into(), 1)),
                Token::EOF(SourceSpan::new(16.into(), 0)),
            ]
        );
    }

    #[test]
    fn unterminated_string() {
        let input = "\"Hello world!;".chars();
        let lexer = Lexer::new(input);
        let result = lexer.lex();

        assert!(!result.fatal);
        assert_eq!(
            result.tokens,
            vec![
                Token::String(SourceSpan::new(0.into(), 14), "Hello world!".to_string()),
                Token::Semicolon(SourceSpan::new(14.into(), 1)),
                Token::EOF(SourceSpan::new(15.into(), 0)),
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
        let input = "\"Hello world".chars();
        let lexer = Lexer::new(input);
        let result = lexer.lex();

        assert!(result.fatal);
        assert_eq!(
            result.tokens,
            vec![Token::EOF(SourceSpan::new(0.into(), 0))]
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
        let input = "let x = 10; 💝 f(x);".chars();
        let lexer = Lexer::new(input);
        let result = lexer.lex();

        assert!(!result.fatal);
        assert_eq!(
            result.tokens,
            vec![
                Token::Let(SourceSpan::new(0.into(), 3)),
                Token::Identifier(SourceSpan::new(4.into(), 1), "x".to_string()),
                Token::Equals(SourceSpan::new(6.into(), 1)),
                Token::Number(SourceSpan::new(8.into(), 2), "10".to_string()),
                Token::Semicolon(SourceSpan::new(10.into(), 1)),
                Token::Identifier(SourceSpan::new(14.into(), 1), "f".to_string()),
                Token::LeftParen(SourceSpan::new(15.into(), 1)),
                Token::Identifier(SourceSpan::new(16.into(), 1), "x".to_string()),
                Token::RightParen(SourceSpan::new(17.into(), 1)),
                Token::Semicolon(SourceSpan::new(18.into(), 1)),
                Token::EOF(SourceSpan::new(19.into(), 0)),
            ]
        );

        assert_eq!(result.errors.len(), 1);
        assert!(matches!(
            result.errors[0],
            CompilerError::UnknownCharacter { at: _ }
        ));
    }
}
