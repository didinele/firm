pub mod lang;

use lang::span_from;
use miette::SourceSpan;

use crate::error::CompilerError;
use crate::lexer::LexerResult;
use crate::lexer::token::Token;

#[derive(Debug)]
pub struct ApplicationFile {
    /// Lexer + Parser errors
    pub errors: Vec<CompilerError>,
    pub imports: Vec<lang::ImportStmt>,
    pub enums: Vec<lang::EnumStmt>,
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    /// Throughout the lifetime of this struct, this instance will basically always be in a partial
    /// state. It is the return value of the `parse` function, at which point it will be fully populated.
    file: ApplicationFile,
    // If Some, implies that the last token was the `pub` modifier.
    pub_token: Option<Token>,
}

impl Parser {
    pub fn new(lexed: LexerResult) -> Self {
        assert!(
            !lexed.fatal,
            "Parser called with previous lexer fatal errors. This is a compiler bug."
        );

        Self {
            tokens: lexed.tokens,
            file: ApplicationFile {
                errors: lexed.errors,
                imports: vec![],
                enums: vec![],
            },
            pub_token: None,
        }
    }

    pub fn parse(mut self) -> ApplicationFile {
        let mut tokens = self.tokens.into_iter().peekable();

        macro_rules! check_bad_pub {
            () => {
                if let Some(ref token) = self.pub_token {
                    self.file
                        .errors
                        .push(CompilerError::UnexpectedPubModifier { at: token.span() });
                };
                self.pub_token = None;
            };
        }

        macro_rules! next_of_type {
            ($Variant:pat, $Ret:expr, $Fallback:expr) => {
                match tokens.next() {
                    Some($Variant) => $Ret,
                    Some(Token::EOF(span)) => {
                        self.file.errors.push(CompilerError::UnexpectedEndOfFile {
                            at: span,
                            advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                        });

                        break;
                    }
                    Some(token) => {
                        self.file.errors.push(CompilerError::UnexpectedToken {
                            at: token.span(),
                            advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                        });

                        $Fallback
                    }
                    None => {
                        debug_assert!(false, "We should never reach this code path");
                        break;
                    }
                }
            };
        }

        'main: loop {
            match tokens.next() {
                Some(token @ Token::Pub(..)) => {
                    if let Some(ref token) = self.pub_token {
                        self.file.errors.push(CompilerError::UnexpectedToken {
                            at: token.span(),
                            advice: Some("Try removing the 2nd `pub`".to_string()),
                        });
                    } else {
                        self.pub_token = Some(token);
                    }
                }
                Some(Token::Import(import_span)) => {
                    check_bad_pub!();

                    let (mod_span, module) = next_of_type!(
                        Token::Identifier(span, identifier),
                        (span, identifier),
                        (
                            SourceSpan::new((import_span.offset() + import_span.len()).into(), 0),
                            "__PLACEHOLDER__".to_string()
                        )
                    );

                    let alias = match tokens.peek() {
                        Some(Token::As(..)) => {
                            // Consume the `as` token
                            tokens.next();
                            next_of_type!(
                                Token::Identifier(span, identifier),
                                Some((span, identifier)),
                                None
                            )
                        }
                        _ => None,
                    };

                    self.file.imports.push(lang::ImportStmt {
                        module,
                        span: if let Some(ref alias) = alias {
                            span_from(&import_span, &alias.0)
                        } else {
                            span_from(&import_span, &mod_span)
                        },
                        alias: alias.map(|alias| alias.1),
                    });
                }
                Some(Token::Enum(enum_span)) => {
                    let (_, name) = next_of_type!(
                        Token::Identifier(span, identifier),
                        (span, identifier),
                        (
                            SourceSpan::new((enum_span.offset() + enum_span.len()).into(), 0),
                            "__PLACEHOLDER__".to_string()
                        )
                    );

                    let mut enum_stmt = lang::EnumStmt {
                        name,
                        variants: vec![],
                        is_pub: self.pub_token.is_some(),
                        span: self
                            .pub_token
                            .as_ref()
                            .map(|token| token.span())
                            .unwrap_or(enum_span),
                    };

                    next_of_type!(Token::LeftCurly(..), (), ());
                    loop {
                        match tokens.next() {
                            Some(Token::RightCurly(span)) => {
                                enum_stmt.span = span_from(&enum_stmt.span, &span);
                                break;
                            }
                            Some(Token::Identifier(span, identifier)) => match tokens.next() {
                                Some(Token::Equals(..)) => {
                                    let value = next_of_type!(
                                        Token::Number(_, number),
                                        number,
                                        "__PLACEHOLDER__".to_string()
                                    );
                                    enum_stmt.variants.push((identifier, Some(value)));

                                    let is_curly = match tokens.peek() {
                                        Some(Token::RightCurly(..)) => true,
                                        _ => false,
                                    };

                                    if is_curly {
                                        tokens.next();
                                        break;
                                    } else {
                                        next_of_type!(Token::Comma(..), (), ());
                                    }
                                }
                                Some(Token::Comma(..)) => {
                                    enum_stmt.variants.push((identifier, None))
                                }
                                Some(Token::RightCurly(..)) => {
                                    enum_stmt.span = span_from(&enum_stmt.span, &span);
                                    enum_stmt.variants.push((identifier, None));
                                    break;
                                }
                                Some(Token::EOF(..)) => {
                                    self.file.errors.push(CompilerError::UnexpectedEndOfFile {
                                        at: span,
                                        advice: Some(
                                            "Expected a `,`, `=`, or `}` token".to_string(),
                                        ),
                                    });
                                    break 'main;
                                }
                                Some(token) => {
                                    self.file.errors.push(CompilerError::UnexpectedToken {
                                        at: token.span(),
                                        advice: Some(
                                            "Expected a `,`, `=`, or `}` token".to_string(),
                                        ),
                                    });
                                    break;
                                }
                                None => {
                                    debug_assert!(false, "We should never reach this code path");
                                }
                            },
                            Some(Token::EOF(at)) => {
                                self.file.errors.push(CompilerError::UnexpectedEndOfFile {
                                    at,
                                    advice: Some("Expected a `,`, `=`, or `}` token".to_string()),
                                });
                                break 'main;
                            }
                            Some(token) => {
                                self.file.errors.push(CompilerError::UnexpectedToken {
                                    at: token.span(),
                                    advice: None,
                                });
                                break;
                            }
                            None => {
                                debug_assert!(false, "We should never reach this code path");
                            }
                        }
                    }

                    self.file.enums.push(enum_stmt);
                }
                Some(Token::EOF(..)) => {
                    // We are done parsing
                    break;
                }
                Some(token) => {
                    self.file.errors.push(CompilerError::UnexpectedToken {
                        at: token.span(),
                        advice: None,
                    });
                }
                None => {
                    debug_assert!(false, "We should never reach this code path");
                    break;
                }
            }
        }

        self.file
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    mod imports {
        use super::*;

        #[test]
        fn simple() {
            let input = "import foo".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "foo");
            assert_eq!(file.imports[0].alias, None);
        }

        #[test]
        fn with_as() {
            let input = "import foo as bar".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "foo");
            assert_eq!(file.imports[0].alias, Some("bar".to_string()));
            // Might as well check the span on this one
            assert_eq!(file.imports[0].span, SourceSpan::new(0.into(), 17));
        }

        #[test]
        fn bad_token() {
            let input = "import \"foo\"".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 1);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "__PLACEHOLDER__");
        }

        #[test]
        fn unexpected_eof() {
            let input = "import foo as".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 1);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedEndOfFile { at: _, advice: _ }
            ));
            // Fatal error end the parser early
            assert_eq!(file.imports.len(), 0);
        }

        #[test]
        fn double_bad_token() {
            let input = "import \"foo\" as \"bar\"".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 2);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert!(matches!(
                file.errors[1],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "__PLACEHOLDER__");
            assert_eq!(file.imports[0].alias, None);
        }

        #[test]
        fn bad_identifier() {
            let input = "import 123".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 1);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "__PLACEHOLDER__");
            assert_eq!(file.imports[0].alias, None);
        }

        #[test]
        fn empty() {
            let input = "import".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 1);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedEndOfFile { at: _, advice: _ }
            ));
            assert_eq!(file.imports.len(), 0);
        }

        #[test]
        fn numerics() {
            let input = "import foo123 as bar456".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "foo123");
            assert_eq!(file.imports[0].alias, Some("bar456".to_string()));
        }

        #[test]
        fn bad_pub() {
            let input = "pub import foo as bar".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 1);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedPubModifier { at: _ }
            ));
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "foo");
            assert_eq!(file.imports[0].alias, Some("bar".to_string()));
        }
    }

    mod enums {
        use super::*;

        #[test]
        fn one_element() {
            let input = "enum Foo { Bar }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 1);
            assert_eq!(file.enums[0].variants[0].0, "Bar");
            assert_eq!(file.enums[0].variants[0].1, None);
        }

        #[test]
        fn multi_element() {
            let input = "enum Foo { Bar, Baz }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 2);
            assert_eq!(file.enums[0].variants[0].0, "Bar");
            assert_eq!(file.enums[0].variants[0].1, None);
            assert_eq!(file.enums[0].variants[1].0, "Baz");
            assert_eq!(file.enums[0].variants[1].1, None);
        }

        #[test]
        fn specified_values() {
            let input = "enum Foo { Bar = 1, Baz = 2 }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 2);
            assert_eq!(file.enums[0].variants[0], ("Bar".to_string(), Some("1".to_string())));
            assert_eq!(file.enums[0].variants[1], ("Baz".to_string(), Some("2".to_string())));
        }

        #[test]
        // Note: This is valid at the parser level, but should fail at the typechecker
        fn some_specified_values() {
            let input = "enum Foo { A, B = 1, C }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 3);
            assert_eq!(file.enums[0].variants[0], ("A".to_string(), None));
            assert_eq!(file.enums[0].variants[1], ("B".to_string(), Some("1".to_string())));
            assert_eq!(file.enums[0].variants[2], ("C".to_string(), None));
        }

        #[test]
        fn trailing_comma() {
            let input = "enum Foo { A, B, C, }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 3);
            assert_eq!(file.enums[0].variants[0], ("A".to_string(), None));
            assert_eq!(file.enums[0].variants[1], ("B".to_string(), None));
            assert_eq!(file.enums[0].variants[2], ("C".to_string(), None));
        }

        #[test]
        fn pub_enum() {
            let input = "pub enum Foo { A }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 1);
            assert_eq!(file.enums[0].variants[0], ("A".to_string(), None));
        }

        #[test]
        fn double_pub_enum() {
            let input = "pub pub enum Foo { A }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 1);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 1);
            assert_eq!(file.enums[0].variants[0], ("A".to_string(), None));
        }
    }
}
