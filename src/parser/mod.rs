pub mod lang;

use std::collections::HashMap;
use std::iter::Peekable;
use std::vec;

use crate::util::spans::{self, earliest};
use lang::ApplicationFile;
use miette::SourceSpan;

use crate::error::CompilerError;
use crate::lexer::LexerResult;
use crate::lexer::token::{Token, TokenKind};

static BAD_NEXT_MSG: &'static str = "Compiler bug. Tried to consume token but encountered None.";

#[derive(Debug)]
pub struct ParserResult {
    pub file: ApplicationFile,
    pub errors: Vec<CompilerError>,
}

impl ParserResult {
    pub fn with_pre_parse_errors(errors: Vec<CompilerError>) -> Self {
        Self {
            file: ApplicationFile::default(),
            errors,
        }
    }
}

macro_rules! next_of_type {
    ($Token:expr, $Errors:expr, $Variant:pat) => {{
        let token = $Token;
        match token.kind() {
            $Variant => Ok(()),
            TokenKind::EOF => {
                $Errors.push(CompilerError::UnexpectedEndOfFile {
                    at: token.span(),
                    advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                });

                Err(())
            }
            _ => {
                $Errors.push(CompilerError::UnexpectedToken {
                    at: token.span(),
                    advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                });

                Ok(())
            }
        }
    }};
    ($Token:expr, $Errors:expr, $Variant:pat, $Fallback:expr) => {{
        let token = $Token;
        match token.kind() {
            $Variant => Ok(token),
            TokenKind::EOF => {
                $Errors.push(CompilerError::UnexpectedEndOfFile {
                    at: token.span(),
                    advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                });

                Err(())
            }
            _ => {
                $Errors.push(CompilerError::UnexpectedToken {
                    at: token.span(),
                    advice: Some(format!("Expected a `{}` token", stringify!($Variant))),
                });

                Ok(token.with_span($Fallback))
            }
        }
    }};
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
    src: &'static str,
    tokens: Peekable<vec::IntoIter<Token>>,
    /// Throughout the lifetime of this struct, this instance will basically always be in a partial
    /// state. It is the return value of the `parse` function, at which point it will be fully populated.
    file: ApplicationFile,
    modifier_tokens: HashMap<TokenKind, Token>,
    errors: Vec<CompilerError>,
}

impl Parser {
    pub fn new(src: &'static str, lexed: LexerResult) -> Self {
        assert!(
            !lexed.fatal,
            "Parser called with previous lexer fatal errors. This is a compiler bug."
        );

        Self {
            src,
            tokens: lexed.tokens.into_iter().peekable(),
            errors: lexed.errors,
            file: ApplicationFile::default(),
            modifier_tokens: HashMap::new(),
        }
    }

    fn check_bad_modifiers(&mut self, allowed: &'static [TokenKind]) {
        if self.modifier_tokens.is_empty() {
            return;
        }

        let mut to_remove = vec![];
        for (kind, token) in &self.modifier_tokens {
            if !allowed.contains(kind) {
                self.errors
                    .push(CompilerError::UnexpectedModifier { at: token.span() });
                to_remove.push(*kind);
            }
        }

        for kind in to_remove {
            self.modifier_tokens.remove(&kind);
        }
    }

    fn parse_type_ref(&mut self, fallback_span: SourceSpan) -> Result<lang::TypeRefExpr, ()> {
        let identifier = next_of_type!(
            self.tokens.next().expect(BAD_NEXT_MSG),
            self.errors,
            TokenKind::Identifier,
            fallback_span
        )?;

        let first_arg_index = self.file.associated.len();
        let mut final_span = identifier.span();

        let arg_count = if self
            .tokens
            .peek()
            .is_some_and(|token| token.kind() == TokenKind::Less)
        {
            // Consume <
            self.tokens.next();

            let mut parameter_count = 0;
            loop {
                let token = self.tokens.next().expect(BAD_NEXT_MSG);
                final_span = token.span();
                match token.kind() {
                    TokenKind::Greater => break,
                    TokenKind::Identifier => {
                        // Obviously passing this span is a bit silly, we'll never fall into
                        // the fallback of next_of_type! from this callsite
                        let typeref = self.parse_type_ref(token.span())?;
                        self.file
                            .associated
                            .push(lang::Stmt::Expr(lang::Expr::TypeRef(typeref)));
                        parameter_count += 1;

                        next_of_type!(
                            self.tokens.next().expect(BAD_NEXT_MSG),
                            self.errors,
                            TokenKind::Comma
                        )?;
                    }
                    TokenKind::EOF => {
                        self.errors.push(CompilerError::UnexpectedEndOfFile {
                            at: identifier.span(),
                            advice: Some(
                                "Expected a `>` token to end the parameter list".to_string(),
                            ),
                        });
                        return Err(());
                    }
                    _ => {
                        self.errors.push(CompilerError::UnexpectedToken {
                            at: token.span(),
                            advice: Some(
                                "Expected an identifier or `>` to end the parameter list"
                                    .to_string(),
                            ),
                        });
                    }
                }
            }
            parameter_count
        } else {
            0
        };

        Ok(lang::TypeRefExpr {
            name: identifier.src(self.src),
            span: spans::from_range(&identifier.span(), &final_span),
            generic_args: (first_arg_index, arg_count),
        })
    }

    fn parse_block(&mut self, fallback_span: SourceSpan) -> Result<lang::BlockExpr, ()> {
        let left_curly = next_of_type!(
            self.tokens.next().expect(BAD_NEXT_MSG),
            self.errors,
            TokenKind::LeftCurly,
            fallback_span
        )?;

        let end_span;
        let expr_start = self.file.associated.len();
        let mut expr_count = 0;

        loop {
            let peeked = self.tokens.peek().expect(BAD_NEXT_MSG);
            if peeked.kind() == TokenKind::RightCurly {
                end_span = self.tokens.next().expect(BAD_NEXT_MSG).span();
                break;
            }

            if peeked.kind() == TokenKind::EOF {
                self.errors.push(CompilerError::UnexpectedEndOfFile {
                    at: left_curly.span(),
                    advice: Some("Expected a `}` token to end the block".to_string()),
                });

                return Err(());
            }

            let expr = self.parse_expr(fallback_span)?;
            self.file.associated.push(lang::Stmt::Expr(expr));
            expr_count += 1;
        }

        Ok(lang::BlockExpr {
            span: spans::from_range(&left_curly.span(), &end_span),
            exprs: (expr_start, expr_count),
        })
    }

    fn parse_expr(&mut self, fallback_span: SourceSpan) -> Result<lang::Expr, ()> {
        todo!()
    }

    pub fn parse(mut self) -> ParserResult {
        'main: loop {
            let token = self.tokens.next().expect(BAD_NEXT_MSG);

            match token.kind() {
                modifier @ (TokenKind::Pub | TokenKind::Pure | TokenKind::Const) => {
                    if let Some(token) = self.modifier_tokens.get(&modifier) {
                        self.errors.push(CompilerError::UnexpectedToken {
                            at: token.span(),
                            advice: Some(format!("Try removing the 2nd `{}`", modifier)),
                        });
                    } else {
                        self.modifier_tokens.insert(modifier, token);
                    }
                }
                TokenKind::Import => {
                    self.check_bad_modifiers(&[]);

                    let identifier = parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::Identifier,
                        spans::placeholder_from(&token.span())
                    ));

                    let alias = if self
                        .tokens
                        .peek()
                        .is_some_and(|token| token.kind() == TokenKind::As)
                    {
                        // Consume the `as` token
                        let as_token = self.tokens.next().unwrap();
                        Some(parser_unwrap!(next_of_type!(
                            self.tokens.next().expect(BAD_NEXT_MSG),
                            self.errors,
                            TokenKind::Identifier,
                            spans::placeholder_from(&as_token.span())
                        )))
                    } else {
                        None
                    };

                    self.file.imports.push(lang::ImportStmt {
                        module: identifier.src(self.src),
                        span: if let Some(ref alias) = alias {
                            spans::from_range(&token.span(), &alias.span())
                        } else {
                            spans::from_range(&token.span(), &token.span())
                        },
                        alias: alias
                            .map(|alias| {
                                // We *really* want to give None here when we find a non-identifier
                                // token for the alias, but that'd require refactoring next_of_type! again,
                                // so we just do this silly thing
                                let src = alias.src(self.src);
                                if src.is_empty() { None } else { Some(src) }
                            })
                            .flatten(),
                    });
                }
                TokenKind::Enum => {
                    self.check_bad_modifiers(&[TokenKind::Pub]);

                    let name = parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::Identifier,
                        spans::placeholder_from(&token.span())
                    ));

                    let pub_token = self.modifier_tokens.get(&TokenKind::Pub);
                    let mut enum_stmt = lang::EnumStmt {
                        name: name.src(self.src),
                        variants: vec![],
                        is_pub: pub_token.is_some(),
                        span: pub_token
                            .as_ref()
                            .map(|token| token.span())
                            .unwrap_or(token.span()),
                    };

                    parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::LeftCurly
                    ));

                    loop {
                        let enum_body_token = self.tokens.next().expect(BAD_NEXT_MSG);
                        match enum_body_token.kind() {
                            TokenKind::RightCurly => {
                                enum_stmt.span =
                                    spans::from_range(&enum_stmt.span, &enum_body_token.span());
                                break;
                            }
                            TokenKind::Identifier => {
                                let after_iden_token = self.tokens.next().expect(BAD_NEXT_MSG);
                                match after_iden_token.kind() {
                                    TokenKind::Equals => {
                                        let value = parser_unwrap!(next_of_type!(
                                            self.tokens.next().expect(BAD_NEXT_MSG),
                                            self.errors,
                                            TokenKind::Number,
                                            spans::placeholder_from(&after_iden_token.span())
                                        ));

                                        enum_stmt.variants.push((
                                            enum_body_token.src(self.src),
                                            Some(value.src(self.src)),
                                        ));

                                        if self.tokens.peek().is_some_and(|token| {
                                            token.kind() == TokenKind::RightCurly
                                        }) {
                                            enum_stmt.span = spans::from_range(
                                                &enum_stmt.span,
                                                &self.tokens.next().unwrap().span(),
                                            );
                                            break;
                                        } else {
                                            parser_unwrap!(next_of_type!(
                                                self.tokens.next().expect(BAD_NEXT_MSG),
                                                self.errors,
                                                TokenKind::Comma
                                            ));
                                        }
                                    }
                                    TokenKind::Comma => {
                                        enum_stmt
                                            .variants
                                            .push((enum_body_token.src(self.src), None));
                                    }
                                    TokenKind::RightCurly => {
                                        enum_stmt.span = spans::from_range(
                                            &enum_stmt.span,
                                            &after_iden_token.span(),
                                        );
                                        enum_stmt
                                            .variants
                                            .push((enum_body_token.src(self.src), None));
                                        break;
                                    }
                                    TokenKind::EOF => {
                                        self.errors.push(CompilerError::UnexpectedEndOfFile {
                                            at: after_iden_token.span(),
                                            advice: Some(
                                                "Expected a `,`, `=`, or `}` token".to_string(),
                                            ),
                                        });
                                        break 'main;
                                    }
                                    _ => {
                                        self.errors.push(CompilerError::UnexpectedToken {
                                            at: token.span(),
                                            advice: Some(
                                                "Expected a `,`, `=`, or `}` token".to_string(),
                                            ),
                                        });
                                    }
                                }
                            }
                            TokenKind::EOF => {
                                self.errors.push(CompilerError::UnexpectedEndOfFile {
                                    at: enum_body_token.span(),
                                    advice: Some("Expected an identifier or `}`".to_string()),
                                });
                                break 'main;
                            }
                            _ => {
                                self.errors.push(CompilerError::UnexpectedToken {
                                    at: enum_body_token.span(),
                                    advice: Some("Expected an identifier or `}`".to_string()),
                                });
                            }
                        }
                    }

                    self.modifier_tokens.clear();
                    self.file.enums.push(enum_stmt);
                }
                TokenKind::Type => {
                    self.check_bad_modifiers(&[TokenKind::Pub]);

                    let identifier = parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::Identifier,
                        spans::placeholder_from(&token.span())
                    ));

                    let pub_token = self.modifier_tokens.get(&TokenKind::Pub);
                    let mut typedecl = lang::TypeStmt {
                        name: identifier.src(self.src),
                        is_pub: pub_token.is_some(),
                        span: pub_token
                            .as_ref()
                            .map(|token| token.span())
                            .unwrap_or(token.span()),
                        definition: self.file.associated.len(),
                    };

                    parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::Equals
                    ));

                    let mut type_ref = parser_unwrap!(self.parse_type_ref(identifier.span()));
                    let semi = parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::Semicolon,
                        spans::placeholder_from(&type_ref.span)
                    ));

                    type_ref.span = spans::from_range(&type_ref.span, &semi.span());
                    typedecl.span = spans::from_range(&typedecl.span, &type_ref.span);

                    self.file.types.push(typedecl);
                    self.file
                        .associated
                        .push(lang::Stmt::Expr(lang::Expr::TypeRef(type_ref)));

                    self.modifier_tokens.clear();
                }
                TokenKind::Struct => {
                    self.check_bad_modifiers(&[TokenKind::Pub]);

                    let name = parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::Identifier,
                        spans::placeholder_from(&token.span())
                    ));

                    let pub_token = self.modifier_tokens.get(&TokenKind::Pub);
                    let mut struct_stmt = lang::StructStmt {
                        name: name.src(self.src),
                        field_names: vec![],
                        is_pub: pub_token.is_some(),
                        span: pub_token
                            .as_ref()
                            .map(|token| token.span())
                            .unwrap_or(token.span()),
                        field_types: (self.file.associated.len(), 0),
                    };

                    parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::LeftCurly
                    ));

                    let mut is_field_pub = false;
                    loop {
                        let struct_body_token = self.tokens.next().expect(BAD_NEXT_MSG);
                        match struct_body_token.kind() {
                            TokenKind::RightCurly => {
                                struct_stmt.span =
                                    spans::from_range(&struct_stmt.span, &struct_body_token.span());
                                break;
                            }
                            TokenKind::Pub => {
                                if is_field_pub {
                                    self.errors.push(CompilerError::UnexpectedToken {
                                        at: token.span(),
                                        advice: Some("Try removing the 2nd `pub`".to_string()),
                                    });
                                } else {
                                    is_field_pub = true;
                                }
                            }
                            TokenKind::Identifier => {
                                struct_stmt
                                    .field_names
                                    .push((is_field_pub, struct_body_token.src(self.src)));

                                parser_unwrap!(next_of_type!(
                                    self.tokens.next().expect(BAD_NEXT_MSG),
                                    self.errors,
                                    TokenKind::Colon
                                ));

                                let typeref =
                                    parser_unwrap!(self.parse_type_ref(struct_body_token.span()));
                                self.file
                                    .associated
                                    .push(lang::Stmt::Expr(lang::Expr::TypeRef(typeref)));

                                if self
                                    .tokens
                                    .peek()
                                    .is_some_and(|token| token.kind() == TokenKind::RightCurly)
                                {
                                    struct_stmt.span = spans::from_range(
                                        &struct_stmt.span,
                                        &self.tokens.next().unwrap().span(),
                                    );
                                    break;
                                } else {
                                    parser_unwrap!(next_of_type!(
                                        self.tokens.next().expect(BAD_NEXT_MSG),
                                        self.errors,
                                        TokenKind::Comma
                                    ));
                                }

                                is_field_pub = false;
                            }
                            TokenKind::EOF => {
                                self.errors.push(CompilerError::UnexpectedEndOfFile {
                                    at: token.span(),
                                    advice: Some("Expected an identifier or `}`".to_string()),
                                });
                                break 'main;
                            }
                            _ => {
                                self.errors.push(CompilerError::UnexpectedToken {
                                    at: token.span(),
                                    advice: Some("Expected an identifier or `}`".to_string()),
                                });
                                break;
                            }
                        }
                    }

                    struct_stmt.field_types.1 = struct_stmt.field_names.len();

                    self.modifier_tokens.clear();
                    self.file.structs.push(struct_stmt);
                }
                TokenKind::Function => {
                    self.check_bad_modifiers(&[TokenKind::Pub, TokenKind::Pure, TokenKind::Const]);

                    let name = parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::Identifier,
                        spans::placeholder_from(&token.span())
                    ));

                    let left_paren = parser_unwrap!(next_of_type!(
                        self.tokens.next().expect(BAD_NEXT_MSG),
                        self.errors,
                        TokenKind::LeftParen,
                        spans::placeholder_from(&name.span())
                    ));

                    let mut arg_names = vec![];
                    let arg_type_start = self.file.associated.len();
                    let mut arg_type_count = 0;

                    let mut latest_span = left_paren.span();

                    loop {
                        let func_tok = self.tokens.next().expect(BAD_NEXT_MSG);
                        match func_tok.kind() {
                            TokenKind::RightParen => break,
                            TokenKind::Identifier => {
                                arg_names.push(func_tok.src(self.src));
                                let colon = parser_unwrap!(next_of_type!(
                                    self.tokens.next().expect(BAD_NEXT_MSG),
                                    self.errors,
                                    TokenKind::Colon,
                                    spans::placeholder_from(&func_tok.span())
                                ));

                                let typeref = parser_unwrap!(self.parse_type_ref(colon.span()));
                                self.file
                                    .associated
                                    .push(lang::Stmt::Expr(lang::Expr::TypeRef(typeref)));
                                arg_type_count += 1;

                                let tok = parser_unwrap!(next_of_type!(
                                    self.tokens.next().expect(BAD_NEXT_MSG),
                                    self.errors,
                                    TokenKind::Colon | TokenKind::RightParen,
                                    spans::placeholder_from(&func_tok.span())
                                ));

                                if tok.kind() == TokenKind::RightParen {
                                    latest_span = tok.span();
                                    break;
                                }
                            }
                            TokenKind::EOF => {
                                self.errors.push(CompilerError::UnexpectedEndOfFile {
                                    at: func_tok.span(),
                                    advice: Some("Expected an identifier or `)`".to_string()),
                                });
                                break 'main;
                            }
                            _ => {
                                self.errors.push(CompilerError::UnexpectedToken {
                                    at: func_tok.span(),
                                    advice: Some("Expected an identifier or `)`".to_string()),
                                });
                                break;
                            }
                        }
                    }

                    let mut return_type = None;

                    if self.tokens.peek().unwrap().kind() == TokenKind::Arrow {
                        self.tokens.next().expect(BAD_NEXT_MSG);
                        let typeref = parser_unwrap!(self.parse_type_ref(name.span()));
                        return_type = Some(self.file.associated.len());
                        latest_span = typeref.span;
                        self.file
                            .associated
                            .push(lang::Stmt::Expr(lang::Expr::TypeRef(typeref)));
                    }

                    let body_block = parser_unwrap!(self.parse_block(latest_span));
                    let body = self.file.associated.len();
                    let body_span = body_block.span;

                    self.file
                        .associated
                        .push(lang::Stmt::Expr(lang::Expr::Block(body_block)));

                    let pub_token = self.modifier_tokens.get(&TokenKind::Pub);
                    let pure_token = self.modifier_tokens.get(&TokenKind::Pure);
                    let const_token = self.modifier_tokens.get(&TokenKind::Const);

                    let earliest = [pub_token, pure_token, const_token]
                        .iter()
                        .filter_map(|token| token.map(|token| token.span()))
                        .min_by_key(|token| token.offset());

                    let span = spans::from_range(&earliest.unwrap_or(token.span()), &body_span);

                    let function_stmt = lang::FunctionStmt {
                        name: name.src(self.src),
                        is_pub: pub_token.is_some(),
                        is_const: const_token.is_some(),
                        is_pure: pure_token.is_some(),
                        arg_names,
                        arg_types: (arg_type_start, arg_type_count),
                        span,
                        body,
                        return_type,
                    };

                    self.modifier_tokens.clear();
                    self.file.functions.push(function_stmt);
                }
                // We are done parsing
                TokenKind::EOF => break 'main,
                _ => {
                    self.errors.push(CompilerError::UnexpectedToken {
                        at: token.span(),
                        advice: None,
                    });
                }
            }
        }

        ParserResult {
            file: self.file,
            errors: self.errors,
        }
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
            let input = "import foo";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "foo");
            assert_eq!(file.imports[0].alias, None);
        }

        #[test]
        fn with_as() {
            let input = "import foo as bar";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "foo");
            assert_eq!(file.imports[0].alias, Some("bar"));
            // Might as well check the span on this one
            assert_eq!(file.imports[0].span, SourceSpan::new(0.into(), 17));
        }

        #[test]
        fn bad_token() {
            let input = "import \"foo\"";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "");
        }

        #[test]
        fn unexpected_eof() {
            let input = "import foo as";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedEndOfFile { at: _, advice: _ }
            ));
            // Fatal errors end the parser early
            assert_eq!(file.imports.len(), 0);
        }

        #[test]
        fn double_bad_token() {
            let input = "import \"foo\" as \"bar\"";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 2);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert!(matches!(
                errors[1],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "");
            assert_eq!(file.imports[0].alias, None);
        }

        #[test]
        fn bad_identifier() {
            let input = "import 123";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "");
            assert_eq!(file.imports[0].alias, None);
        }

        #[test]
        fn empty() {
            let input = "import";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedEndOfFile { at: _, advice: _ }
            ));
            assert_eq!(file.imports.len(), 0);
        }

        #[test]
        fn numerics() {
            let input = "import foo123 as bar456";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "foo123");
            assert_eq!(file.imports[0].alias, Some("bar456"));
        }

        #[test]
        fn bad_pub() {
            let input = "pub import foo as bar";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedModifier { at: _ }
            ));
            assert_eq!(file.imports.len(), 1);
            assert_eq!(file.imports[0].module, "foo");
            assert_eq!(file.imports[0].alias, Some("bar"));
        }
    }

    mod enums {
        use super::*;

        #[test]
        fn one_element() {
            let input = "enum Foo { Bar }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 1);
            assert_eq!(file.enums[0].variants[0].0, "Bar");
            assert_eq!(file.enums[0].variants[0].1, None);
            assert_eq!(file.enums[0].span, SourceSpan::new(0.into(), 16));
        }

        #[test]
        fn multi_element() {
            let input = "enum Foo { Bar, Baz }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
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
            let input = "enum Foo { Bar = 1, Baz = 2 }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 2);
            assert_eq!(file.enums[0].variants[0], ("Bar", Some("1")));
            assert_eq!(file.enums[0].variants[1], ("Baz", Some("2")));
        }

        #[test]
        // Note: This is valid at the parser level, but should fail at the typechecker
        fn some_specified_values() {
            let input = "enum Foo { A, B = 1, C }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 3);
            assert_eq!(file.enums[0].variants[0], ("A", None));
            assert_eq!(file.enums[0].variants[1], ("B", Some("1")));
            assert_eq!(file.enums[0].variants[2], ("C", None));
        }

        #[test]
        fn trailing_comma() {
            let input = "enum Foo { A, B, C, }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 3);
            assert_eq!(file.enums[0].variants[0], ("A", None));
            assert_eq!(file.enums[0].variants[1], ("B", None));
            assert_eq!(file.enums[0].variants[2], ("C", None));
        }

        #[test]
        fn pub_enum() {
            let input = "pub enum Foo { A }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 1);
            assert_eq!(file.enums[0].variants[0], ("A", None));
        }

        #[test]
        fn double_pub_enum() {
            let input = "pub pub enum Foo { A }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.enums.len(), 1);
            assert_eq!(file.enums[0].name, "Foo");
            assert_eq!(file.enums[0].variants.len(), 1);
            assert_eq!(file.enums[0].variants[0], ("A", None));
        }

        #[test]
        fn pub_no_linger() {
            let input = "pub enum Foo { A } enum Bar { B }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
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
            let input = "type Foo = Bar;";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.types.len(), 1);
            assert_eq!(file.types[0].is_pub, false);
            assert_eq!(file.types[0].name, "Foo");
            assert_eq!(file.types[0].span, SourceSpan::new(0.into(), 15));
            assert_eq!(file.types[0].definition, 0);
            assert_eq!(file.associated.len(), 1);
            assert!(matches!(
                file.associated[0],
                lang::Stmt::Expr(lang::Expr::TypeRef(_))
            ));
        }

        #[test]
        fn with_pub() {
            let input = "pub type Foo = Bar;";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.types.len(), 1);
            assert_eq!(file.types[0].is_pub, true);
            assert_eq!(file.types[0].name, "Foo");
            assert_eq!(file.types[0].span, SourceSpan::new(0.into(), 19));
        }

        #[test]
        fn double_pub() {
            let input = "pub pub type Foo = Bar;";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.types.len(), 1);
            assert_eq!(file.types[0].is_pub, true);
            assert_eq!(file.types[0].name, "Foo");
        }

        #[test]
        fn with_pub_no_linger() {
            let input = "pub type Foo = Bar; type Baz = Qux;";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
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
            let input = "struct Foo { bar: Bar }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.structs.len(), 1);
            assert_eq!(file.structs[0].name, "Foo");
            assert_eq!(file.structs[0].field_names.len(), 1);
            assert_eq!(file.structs[0].field_names[0].1, "bar");
            assert_eq!(file.associated.len(), 1);
            assert!(matches!(
                file.associated[0],
                lang::Stmt::Expr(lang::Expr::TypeRef(_))
            ));
            assert_eq!(file.structs[0].span, SourceSpan::new(0.into(), 23));
        }

        #[test]
        fn with_pub() {
            let input = "pub struct Foo { bar: Bar }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.structs.len(), 1);
            assert_eq!(file.structs[0].is_pub, true);
            assert_eq!(file.structs[0].name, "Foo");
        }

        #[test]
        fn double_pub() {
            let input = "pub pub struct Foo { bar: Bar }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedToken { at: _, advice: _ }
            ));
            assert_eq!(file.structs.len(), 1);
        }

        #[test]
        fn with_pub_no_linger() {
            let input = "pub struct Foo { bar: Bar } struct Baz { qux: Qux }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
            assert_eq!(file.structs.len(), 2);
            assert_eq!(file.structs[0].is_pub, true);
            assert_eq!(file.structs[0].name, "Foo");
            assert_eq!(file.structs[1].is_pub, false);
            assert_eq!(file.structs[1].name, "Baz");
            assert_eq!(file.associated.len(), 2);
            assert!(matches!(
                file.associated[0],
                lang::Stmt::Expr(lang::Expr::TypeRef(_))
            ));
            assert!(matches!(
                file.associated[1],
                lang::Stmt::Expr(lang::Expr::TypeRef(_))
            ));
        }

        #[test]
        fn with_pub_members() {
            let input = "pub struct Foo { x: T, pub y: U, z: V }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { file, errors } = parser.parse();
            assert_eq!(errors.len(), 0);
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
            assert_eq!(file.structs[0].field_types, (0, 3));
            assert_eq!(file.associated.len(), 3);
            assert!(matches!(
                file.associated[0],
                lang::Stmt::Expr(lang::Expr::TypeRef(_))
            ));
            assert!(matches!(
                file.associated[1],
                lang::Stmt::Expr(lang::Expr::TypeRef(_))
            ));
            assert!(matches!(
                file.associated[2],
                lang::Stmt::Expr(lang::Expr::TypeRef(_))
            ));
        }

        #[test]
        fn with_bad_modifier() {
            let input = "pure pub struct Foo { x: T, pub y: U, z: V }";
            let lexer = Lexer::new(input);
            let lexed = lexer.lex();
            let parser = Parser::new(input, lexed);

            let ParserResult { errors, .. } = parser.parse();

            assert_eq!(errors.len(), 1);
            assert!(matches!(
                errors[0],
                CompilerError::UnexpectedModifier { at: _ }
            ));
        }
    }
}
