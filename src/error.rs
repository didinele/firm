use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
#[diagnostic()]
pub enum CompilerError {
    // I/O & CLI
    #[error("Failed to open a source file or a directory")]
    IOError {
        #[source]
        inner: std::io::Error,
    },
    #[error("Failed to access the current working directory")]
    CurrentDirError {
        #[source]
        inner: std::io::Error,
    },
    #[error("Could not find a src directory within the root folder")]
    #[help(
        "The compiler looks for a src directory inside the root folder. By default, the root folder is the current working directory."
    )]
    MissingSrcDir,
    #[error("src is not a directory")]
    #[help("Your root directory has a file named src, but it is not a directory.")]
    SrcIsNotADirectory,
    #[error("No main file found")]
    #[help("The compiler looks for a file named main.firm inside the src directory.")]
    NoMainFileFound,

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
    #[error("Unexpected modifier")]
    UnexpectedModifier {
        #[label("here")]
        at: SourceSpan,
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
#[error("Ran into one or more errors while compiling {}", self.file_name)]
#[diagnostic()]
pub struct FileErrors {
    pub file_name: String,
    #[source_code]
    // There's cases, like I/O errors, where we won't have source code.
    pub source_code: Option<&'static str>,
    #[related]
    pub errors: Vec<CompilerError>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Encountered errors during compilation")]
#[diagnostic()]
pub struct CompilerErrors {
    #[related]
    pub file_errors: Vec<FileErrors>,
    #[related]
    // Like failing to read into the directory
    pub other_errors: Vec<CompilerError>,
}
