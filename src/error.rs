use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
pub enum CompilerError {
    // I/O
    #[error("Failed to open source file")]
    IOError {
        #[source]
        inner: std::io::Error,
    },

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
    // Parser errors
    #[error("Unexpected token")]
    UnexpectedToken {
        #[label("here")]
        at: SourceSpan,
        #[help]
        advice: Option<String>,
    },
    #[error("Unexpected end of file")]
    UnexpectedEndOfFile {
        #[label("here")]
        at: SourceSpan,
        #[help]
        advice: Option<String>,
    },
}

impl CompilerError {
    pub fn is_fatal(&self) -> bool {
        match self {
            CompilerError::IOError { .. } => true,
            CompilerError::UnterminatedStringLiteral { fatal, .. } => *fatal,
            CompilerError::UnexpectedEndOfFile { .. } => true,
            _ => false,
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("Encountered errors during compilation")]
#[diagnostic()]
pub struct CompilerErrors {
    #[source_code]
    // There's cases, like I/O errors, where we won't have source code.
    pub source_code: Option<String>,
    #[related]
    pub related: Vec<CompilerError>,
}
