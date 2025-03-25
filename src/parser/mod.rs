pub mod lang;

use std::iter::Peekable;
use std::vec;

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
    pub types: Vec<lang::TypeStmt>,
    pub structs: Vec<lang::StructStmt>,
    // TODO: Consider wrapper type
    pub associated: Vec<lang::Stmt>,
}

macro_rules! next_of_type {
    ($Token:expr, $Errors:expr, $Variant:pat) => {
        match $Token {
            Some($Variant) => Ok(()),
            Some(Token::EOF(span)) => {
                $Errors.push(CompilerError::UnexpectedEndOfFile {
                    at: span,
                    advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                });

                Err(())
            }
            Some(token) => {
                $Errors.push(CompilerError::UnexpectedToken {
                    at: token.span(),
                    advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                });
                Ok(())
            }
            None => {
                debug_assert!(false, "We should never reach this code path");
                Err(())
            }
        }
    };
    ($Token:expr, $Errors:expr, $Variant:pat, $Ret:expr, $Fallback:expr) => {
        match $Token {
            Some($Variant) => Ok($Ret),
            Some(Token::EOF(span)) => {
                $Errors.push(CompilerError::UnexpectedEndOfFile {
                    at: span,
                    advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                });

                Err(())
            }
            Some(token) => {
                $Errors.push(CompilerError::UnexpectedToken {
                    at: token.span(),
                    advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                });

                Ok($Fallback)
            }
            None => {
                debug_assert!(false, "We should never reach this code path");
                Err(())
            }
        }
    };
}

macro_rules! parser_unwrap {
    ($Token:expr) => {
        match $Token {
            Ok(token) => token,
            Err(..) => break,
        }
    };
}

#[derive(Debug)]
pub struct Parser {
    tokens: Peekable<vec::IntoIter<Token>>,
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
            tokens: lexed.tokens.into_iter().peekable(),
            file: ApplicationFile {
                errors: lexed.errors,
                imports: vec![],
                enums: vec![],
                types: vec![],
                structs: vec![],
                associated: vec![],
            },
            pub_token: None,
        }
    }

    fn check_bad_pub(&mut self) {
        if let Some(ref token) = self.pub_token {
            self.file
                .errors
                .push(CompilerError::UnexpectedPubModifier { at: token.span() });
        }

        self.pub_token = None;
    }

    fn parse_type_ref(&mut self, fallback_span: SourceSpan) -> Result<lang::TypeRefExpr, ()> {
        let (span, name) = next_of_type!(
            self.tokens.next(),
            self.file.errors,
            Token::Identifier(span, name),
            (span, name),
            (fallback_span, "__PLACEHOLDER__".to_string())
        )?;

        let associated = self.file.associated.len();

        let arg_count = if self
            .tokens
            .peek()
            .is_some_and(|token| matches!(token, Token::Less(..)))
        {
            // Consume <
            self.tokens.next();

            let mut parameter_count = 0;
            loop {
                match self.tokens.next() {
                    Some(Token::Greater(..)) => break,
                    Some(Token::Identifier(span, _)) => {
                        // Obviously passing this span is a bit silly, we'll never fall into the fallback of next_of_type!
                        // from this callsite
                        let typeref = self.parse_type_ref(span)?;
                        self.file
                            .associated
                            .push(lang::Stmt::Expr(lang::Expr::TypeRef(typeref)));
                        parameter_count += 1;

                        next_of_type!(self.tokens.next(), self.file.errors, Token::Comma(..))?;
                    }
                    Some(Token::EOF(..)) => {
                        self.file.errors.push(CompilerError::UnexpectedEndOfFile {
                            at: span,
                            advice: Some(
                                "Expected a `>` token to end the parameter list".to_string(),
                            ),
                        });
                        return Err(());
                    }
                    Some(token) => {
                        self.file.errors.push(CompilerError::UnexpectedToken {
                            at: token.span(),
                            advice: Some(
                                "Expected an identifier or `>` to end the parameter list"
                                    .to_string(),
                            ),
                        });
                    }
                    None => {
                        debug_assert!(false, "We should never reach this code path");
                        return Err(());
                    }
                }
            }

            parameter_count
        } else {
            0
        };

        Ok(lang::TypeRefExpr {
            name,
            arg_count,
            span,
            associated,
        })
    }

    pub fn parse(mut self) -> ApplicationFile {
        'main: loop {
            match self.tokens.next() {
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
                    self.check_bad_pub();

                    let (mod_span, module) = parser_unwrap!(next_of_type!(
                        self.tokens.next(),
                        self.file.errors,
                        Token::Identifier(span, identifier),
                        (span, identifier),
                        (
                            SourceSpan::new((import_span.offset() + import_span.len()).into(), 0),
                            "__PLACEHOLDER__".to_string()
                        )
                    ));

                    let alias = match self.tokens.peek() {
                        Some(Token::As(..)) => {
                            // Consume the `as` token
                            self.tokens.next();
                            parser_unwrap!(next_of_type!(
                                self.tokens.next(),
                                self.file.errors,
                                Token::Identifier(span, identifier),
                                Some((span, identifier)),
                                None
                            ))
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
                    let name = parser_unwrap!(next_of_type!(
                        self.tokens.next(),
                        self.file.errors,
                        Token::Identifier(_, identifier),
                        identifier,
                        "__PLACEHOLDER__".to_string()
                    ));

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

                    parser_unwrap!(next_of_type!(
                        self.tokens.next(),
                        self.file.errors,
                        Token::LeftCurly(..)
                    ));
                    loop {
                        match self.tokens.next() {
                            Some(Token::RightCurly(span)) => {
                                enum_stmt.span = span_from(&enum_stmt.span, &span);
                                break;
                            }
                            Some(Token::Identifier(span, identifier)) => match self.tokens.next() {
                                Some(Token::Equals(..)) => {
                                    let value = parser_unwrap!(next_of_type!(
                                        self.tokens.next(),
                                        self.file.errors,
                                        Token::Number(_, number),
                                        number,
                                        "__PLACEHOLDER__".to_string()
                                    ));

                                    enum_stmt.variants.push((identifier, Some(value)));

                                    let is_curly = match self.tokens.peek() {
                                        Some(Token::RightCurly(..)) => true,
                                        _ => false,
                                    };

                                    if is_curly {
                                        enum_stmt.span = span_from(
                                            &enum_stmt.span,
                                            &self.tokens.next().unwrap().span(),
                                        );
                                        break;
                                    } else {
                                        parser_unwrap!(next_of_type!(
                                            self.tokens.next(),
                                            self.file.errors,
                                            Token::Comma(..)
                                        ));
                                    }
                                }
                                Some(Token::Comma(..)) => {
                                    enum_stmt.variants.push((identifier, None));
                                }
                                Some(Token::RightCurly(span)) => {
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
                                    break 'main;
                                }
                            },
                            Some(Token::EOF(at)) => {
                                self.file.errors.push(CompilerError::UnexpectedEndOfFile {
                                    at,
                                    advice: Some("Expected an identifier or `}`".to_string()),
                                });
                                break 'main;
                            }
                            Some(token) => {
                                self.file.errors.push(CompilerError::UnexpectedToken {
                                    at: token.span(),
                                    advice: Some("Expected an identifier or `}`".to_string()),
                                });
                                break;
                            }
                            None => {
                                debug_assert!(false, "We should never reach this code path");
                                break 'main;
                            }
                        }
                    }

                    self.pub_token = None;
                    self.file.enums.push(enum_stmt);
                }
                Some(Token::Type(type_span)) => {
                    let (name_span, name) = parser_unwrap!(next_of_type!(
                        self.tokens.next(),
                        self.file.errors,
                        Token::Identifier(span, identifier),
                        (span, identifier),
                        (
                            SourceSpan::new((type_span.offset() + type_span.len()).into(), 0),
                            "__PLACEHOLDER__".to_string()
                        )
                    ));

                    let mut typedecl = lang::TypeStmt {
                        name,
                        is_pub: self.pub_token.is_some(),
                        span: self
                            .pub_token
                            .as_ref()
                            .map(|token| token.span())
                            .unwrap_or(type_span),
                        associated: self.file.associated.len(),
                    };

                    parser_unwrap!(next_of_type!(
                        self.tokens.next(),
                        self.file.errors,
                        Token::Equals(..)
                    ));

                    let mut type_ref = parser_unwrap!(self.parse_type_ref(name_span));
                    let span = parser_unwrap!(next_of_type!(
                        self.tokens.next(),
                        self.file.errors,
                        Token::Semicolon(span),
                        span,
                        SourceSpan::new(
                            (type_ref.span.offset() + type_ref.span.len() + 1).into(),
                            0
                        )
                    ));

                    type_ref.span = span_from(&type_ref.span, &span);
                    typedecl.span = span_from(&typedecl.span, &type_ref.span);

                    self.file.types.push(typedecl);
                    self.file
                        .associated
                        .push(lang::Stmt::Expr(lang::Expr::TypeRef(type_ref)));

                    self.pub_token = None;
                }
                Some(Token::Struct(struct_span)) => {
                    let name = parser_unwrap!(next_of_type!(
                        self.tokens.next(),
                        self.file.errors,
                        Token::Identifier(_, identifier),
                        identifier,
                        "__PLACEHOLDER__".to_string()
                    ));

                    let mut struct_stmt = lang::StructStmt {
                        name,
                        field_names: vec![],
                        is_pub: self.pub_token.is_some(),
                        span: self
                            .pub_token
                            .as_ref()
                            .map(|token| token.span())
                            .unwrap_or(struct_span),
                        associated: self.file.associated.len(),
                    };

                    parser_unwrap!(next_of_type!(
                        self.tokens.next(),
                        self.file.errors,
                        Token::LeftCurly(..)
                    ));

                    let mut is_field_pub = false;

                    loop {
                        match self.tokens.next() {
                            Some(Token::RightCurly(span)) => {
                                struct_stmt.span = span_from(&struct_stmt.span, &span);
                                break;
                            }
                            Some(token @ Token::Pub(..)) => {
                                if is_field_pub {
                                    self.file.errors.push(CompilerError::UnexpectedToken {
                                        at: token.span(),
                                        advice: Some("Try removing the 2nd `pub`".to_string()),
                                    });
                                } else {
                                    is_field_pub = true;
                                }
                            }
                            Some(Token::Identifier(span, identifier)) => {
                                struct_stmt.field_names.push((is_field_pub, identifier));

                                parser_unwrap!(next_of_type!(
                                    self.tokens.next(),
                                    self.file.errors,
                                    Token::Colon(..)
                                ));

                                let typeref = parser_unwrap!(self.parse_type_ref(span));
                                self.file
                                    .associated
                                    .push(lang::Stmt::Expr(lang::Expr::TypeRef(typeref)));

                                let is_curly = match self.tokens.peek() {
                                    Some(Token::RightCurly(..)) => true,
                                    _ => false,
                                };

                                if is_curly {
                                    struct_stmt.span = span_from(
                                        &struct_stmt.span,
                                        &self.tokens.next().unwrap().span(),
                                    );
                                    break;
                                } else {
                                    parser_unwrap!(next_of_type!(
                                        self.tokens.next(),
                                        self.file.errors,
                                        Token::Comma(..)
                                    ));
                                }

                                is_field_pub = false;
                            }
                            Some(Token::EOF(at)) => {
                                self.file.errors.push(CompilerError::UnexpectedEndOfFile {
                                    at,
                                    advice: Some("Expected an identifier or `}`".to_string()),
                                });
                                break 'main;
                            }
                            Some(token) => {
                                self.file.errors.push(CompilerError::UnexpectedToken {
                                    at: token.span(),
                                    advice: Some("Expected an identifier or `}`".to_string()),
                                });
                                break;
                            }
                            None => {
                                debug_assert!(false, "We should never reach this code path");
                                break 'main;
                            }
                        }
                    }

                    self.pub_token = None;
                    self.file.structs.push(struct_stmt);
                }
                Some(Token::EOF(..)) => {
                    // We are done parsing
                    break 'main;
                }
                Some(token) => {
                    self.file.errors.push(CompilerError::UnexpectedToken {
                        at: token.span(),
                        advice: None,
                    });
                }
                None => {
                    debug_assert!(false, "We should never reach this code path");
                    break 'main;
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
            assert_eq!(file.enums[0].span, SourceSpan::new(0.into(), 16));
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
            assert_eq!(
                file.enums[0].variants[0],
                ("Bar".to_string(), Some("1".to_string()))
            );
            assert_eq!(
                file.enums[0].variants[1],
                ("Baz".to_string(), Some("2".to_string()))
            );
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
            assert_eq!(
                file.enums[0].variants[1],
                ("B".to_string(), Some("1".to_string()))
            );
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

        #[test]
        fn pub_no_linger() {
            let input = "pub enum Foo { A } enum Bar { B }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.enums.len(), 2);
            assert_eq!(file.enums[0].name, "Foo");
            assert!(file.enums[0].is_pub);
            assert_eq!(file.enums[1].name, "Bar");
            assert!(!file.enums[1].is_pub);
        }
    }

    mod types {
        use super::*;

        #[test]
        fn simple() {
            let input = "type Foo = Bar;".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            println!("{:?}", file.errors);
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.types.len(), 1);
            assert_eq!(file.types[0].is_pub, false);
            assert_eq!(file.types[0].name, "Foo");
            assert_eq!(file.types[0].span, SourceSpan::new(0.into(), 15));
            assert_eq!(file.types[0].associated, 0);
            assert_eq!(file.associated.len(), 1);
            assert!(matches!(
                file.associated[0],
                lang::Stmt::Expr(lang::Expr::TypeRef(_))
            ));
        }

        #[test]
        fn with_pub() {
            let input = "pub type Foo = Bar;".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.types.len(), 1);
            assert_eq!(file.types[0].is_pub, true);
            assert_eq!(file.types[0].name, "Foo");
            assert_eq!(file.types[0].span, SourceSpan::new(0.into(), 19));
        }

        #[test]
        fn double_pub() {
            let input = "pub pub type Foo = Bar;".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 1);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.types.len(), 1);
            assert_eq!(file.types[0].is_pub, true);
            assert_eq!(file.types[0].name, "Foo");
        }

        #[test]
        fn with_pub_no_linger() {
            let input = "pub type Foo = Bar; type Baz = Qux;".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.types.len(), 2);
            assert_eq!(file.types[0].is_pub, true);
            assert_eq!(file.types[0].name, "Foo");
            assert_eq!(file.types[1].is_pub, false);
            assert_eq!(file.types[1].name, "Baz");
        }
    }

    mod structs {
        use super::*;

        #[test]
        fn simple() {
            let input = "struct Foo { bar: Bar }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.structs.len(), 1);
            assert_eq!(file.structs[0].name, "Foo");
            assert_eq!(file.structs[0].field_names.len(), 1);
            assert_eq!(file.structs[0].field_names[0].1, "bar");
            assert_eq!(file.structs[0].span, SourceSpan::new(0.into(), 23));
        }

        #[test]
        fn with_pub() {
            let input = "pub struct Foo { bar: Bar }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.structs.len(), 1);
            assert_eq!(file.structs[0].is_pub, true);
            assert_eq!(file.structs[0].name, "Foo");
        }

        #[test]
        fn double_pub() {
            let input = "pub pub struct Foo { bar: Bar }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 1);
            assert!(matches!(
                file.errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.structs.len(), 1);
        }

        #[test]
        fn with_pub_no_linger() {
            let input = "pub struct Foo { bar: Bar } struct Baz { qux: Qux }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.structs.len(), 2);
            assert_eq!(file.structs[0].is_pub, true);
            assert_eq!(file.structs[0].name, "Foo");
            assert_eq!(file.structs[1].is_pub, false);
            assert_eq!(file.structs[1].name, "Baz");
        }

        #[test]
        fn with_pub_members() {
            let input = "pub struct Foo { x: T, pub y: U, z: V }".chars();
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(lexed);

            let file = parser.parse();
            assert_eq!(file.errors.len(), 0);
            assert_eq!(file.structs.len(), 1);
            assert_eq!(file.structs[0].is_pub, true);
            assert_eq!(file.structs[0].name, "Foo");
            assert_eq!(file.structs[0].field_names.len(), 3);
            assert_eq!(file.structs[0].field_names[0].0, false);
            assert_eq!(file.structs[0].field_names[0].1, "x");
            assert_eq!(file.structs[0].field_names[1].0, true);
            assert_eq!(file.structs[0].field_names[1].1, "y");
            assert_eq!(file.structs[0].field_names[2].0, false);
            assert_eq!(file.structs[0].field_names[2].1, "z");
        }
    }
}
