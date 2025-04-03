mod error;
mod lexer;
mod parser;

use std::collections::HashMap;

use clap::{Parser, Subcommand};
use error::{CompilerError, CompilerErrors, FileErrors};
use parser::ParserResult;
use threadpool::ThreadPool;

static THREAD_BUG_MSG: &'static str =
    "Compiler bug. The main thread did not wait for the worker thread to finish.";

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build a firm project
    Build {
        /// Path root to the project to compile. Inside of this dir, files inside of src/ are seeked.
        /// If not provided, the current working directory is used.
        root: Option<String>,
        /// How many threads to use for compilation. By default, the number of logical cores is used.
        threads: Option<usize>,
    },
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Build { root, threads } => {
            let root = root.unwrap_or(
                std::env::current_dir()
                    .map_err(|inner| CompilerError::CurrentDirError { inner })?
                    .to_string_lossy()
                    .to_string(),
            );

            // Check if src exists
            let exists = std::fs::exists(format!("{}/src", root))
                .map_err(|inner| CompilerError::IOError { inner })?;
            if !exists {
                return Err(CompilerError::MissingSrcDir.into());
            }

            // Check if src is a directory
            let metadata = std::fs::metadata(format!("{}/src", root))
                .map_err(|inner| CompilerError::IOError { inner })?;
            if !metadata.is_dir() {
                return Err(CompilerError::SrcIsNotADirectory.into());
            }

            // From this point on, we'll be collecting errors to build a `CompilerErrors`
            let mut other_errors: Vec<CompilerError> = vec![];
            let files = std::fs::read_dir(format!("{}/src", root))
                .map_err(|inner| CompilerError::IOError { inner })?
                .filter_map(|file| {
                    let file = file
                        .map_err(|inner| other_errors.push(CompilerError::IOError { inner }))
                        .ok()?;

                    let path = file.path().to_string_lossy().to_string();
                    // TODO: Recurse
                    return if path.ends_with(".firm") {
                        Some(path)
                    } else {
                        None
                    };
                })
                .collect::<Vec<_>>();
            let file_count = files.len();

            let pool = match threads {
                Some(threads) => ThreadPool::new(threads),
                None => ThreadPool::default(),
            };

            let (tx, rx) = std::sync::mpsc::channel::<(String, (Option<String>, ParserResult))>();

            for path in files {
                let tx = tx.clone();
                let root = root.clone();

                pool.execute(move || {
                    let src = std::fs::read_to_string(path.clone());
                    let src = match src {
                        Ok(src) => src,
                        Err(inner) => {
                            tx.send((
                                path,
                                (
                                    None,
                                    ParserResult::with_pre_parse_errors(vec![
                                        CompilerError::IOError { inner },
                                    ]),
                                ),
                            ))
                            .expect(THREAD_BUG_MSG);
                            return;
                        }
                    };

                    let lexer = lexer::Lexer::new(src.chars());
                    let lexed = lexer.lex();
                    if lexed.fatal {
                        tx.send((
                            path,
                            (Some(src), ParserResult::with_pre_parse_errors(lexed.errors)),
                        ))
                        .expect(THREAD_BUG_MSG);
                        return;
                    }

                    let parser = parser::Parser::new(lexed);
                    let file = parser.parse();

                    // At this point, let's strip the start of the path (i.e. <root>/src/)
                    let path = path
                        .strip_prefix(&format!("{}/src/", root))
                        .unwrap_or(&path)
                        .to_string();
                    tx.send((path, (Some(src), file))).expect(THREAD_BUG_MSG);
                });
            }

            let files = rx.iter().take(file_count).collect::<HashMap<_, _>>();

            if !files.contains_key("main.firm") {
                return Err(CompilerError::NoMainFileFound.into());
            }

            // TODO. Continue the compiler. Type checker, optimizer, code gen, etc.
            // println!("{:?}", files);

            // Lastly, collect errors for the final report if nothing crashed us so far
            let file_errors = files
                .into_iter()
                .filter_map(|(file_name, (source_code, ParserResult { errors, .. }))| {
                    if errors.is_empty() {
                        None
                    } else {
                        Some(FileErrors {
                            file_name,
                            source_code,
                            errors,
                        })
                    }
                })
                .collect::<Vec<_>>();

            if !file_errors.is_empty() || !other_errors.is_empty() {
                return Err(CompilerErrors {
                    file_errors,
                    other_errors,
                }
                .into());
            }
        }
    }

    Ok(())
}
