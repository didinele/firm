pub mod lang;

use crate::error::CompilerError;
use crate::lexer::LexerResult;
use crate::lexer::token::Token;

#[derive(Debug)]
pub struct ApplicationFile {
    /// Lexer + Parser errors
    pub errors: Vec<CompilerError>,
    pub imports: Vec<lang::ImportStmt>,
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    /// Throughout the lifetime of this struct, this instance will basically always be in a partial
    /// state. It is the return value of the `parse` function, at which point it will be fully populated.
    file: ApplicationFile,
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
            },
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

        loop {
            match tokens.next() {
                Some(Token::Import(..)) => {
                    let module = next_of_type!(
                        Token::Identifier(_, identifier),
                        identifier,
                        "__PLACEHOLDER__".to_string()
                    );

                    let alias = match tokens.peek() {
                        Some(Token::As(..)) => {
                            // Consume the `as` token
                            tokens.next();
                            next_of_type!(
                                Token::Identifier(_, identifier),
                                Some(identifier),
                                None
                            )
                        },
                        _ => None,
                    };

                    self.file.imports.push(lang::ImportStmt {
                        module,
                        alias,
                    });
                }
                Some(Token::EOF(..)) => {
                    // We are done parsing
                    break;
                }
                Some(..) => unimplemented!(),
                None => {
                    debug_assert!(false, "We should never reach this code path");
                    break;
                },
            }
        }

        self.file
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn parse_import() {
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
    fn parse_import_as() {
        let input = "import foo as bar".chars();
        let lexer = Lexer::new(input);
        let lexed = lexer.lex();
        let parser = Parser::new(lexed);

        let file = parser.parse();
        assert_eq!(file.errors.len(), 0);
        assert_eq!(file.imports.len(), 1);
        assert_eq!(file.imports[0].module, "foo");
        assert_eq!(file.imports[0].alias, Some("bar".to_string()));
    }

    #[test]
    fn parse_import_with_bad_token() {
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
    fn parse_import_with_unexpected_eof() {
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
    fn parse_import_double_bad_token() {
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
    fn parse_import_bad_identifier() {
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
    fn parse_import_empty() {
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
    fn parse_import_contains_numerics() {
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
}
