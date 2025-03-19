use std::sync::RwLock;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
pub enum CompilerError {
    // Lexer errors
    #[error("Encountered an unterminated string literal")]
    UnterminatedStringLiteral {
        #[label("here")]
        at: SourceSpan,
        #[label("try adding a closing quote here")]
        advice: Option<SourceSpan>,
        fatal: bool,
    },
    #[error("Unknown character")]
    UnknownCharacter {
        #[label("here")]
        at: SourceSpan,
    },
}

impl CompilerError {
    pub fn is_fatal(&self) -> bool {
        match self {
            CompilerError::UnterminatedStringLiteral { fatal, .. } => *fatal,
            _ => false,
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("Encountered errors during compilation")]
#[diagnostic()]
pub struct CompilerErrors {
    #[source_code]
    pub source_code: String,
    #[related]
    pub related: Vec<CompilerError>,
}
