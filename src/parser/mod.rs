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

        macro_rules! next_of_type {
            ($Variant:pat, $Ret:expr, $Fallback:expr) => {
                match tokens.next() {
                    Some($Variant) => $Ret,
                    Some(Token::EOF(span)) => {
                        self.file.errors.push(CompilerError::UnexpectedEndOfFile {
                            at: span,
                            advice: Some(format!("Expected a {} token", stringify!($Variant))),
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

        macro_rules! check_bad_pub {
            () => {
                if let Some(ref token) = self.pub_token {
                    self.file.errors.push(CompilerError::UnexpectedPubModifier {
                        at: token.span(),
                    });
                };
                self.pub_token = None;
            }
        }

        loop {
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
                Some(Token::EOF(..)) => {
                    // We are done parsing
                    break;
                }
                Some(..) => {
                    self.file.errors.push(CompilerError::UnexpectedToken {
                        at: tokens.peek().unwrap().span(),
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
}
