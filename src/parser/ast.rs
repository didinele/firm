use crate::error::{CompilerError, CompilerErrors};

// Everything uses index indirection into some big Vecs to prevent pointer chasing

#[derive(Debug)]
pub struct BlockExpr {
    statements: Vec<usize>,
}

#[derive(Debug)]
pub struct IfExpr {
    condition: usize,
    then_branch: usize,
    else_branch: usize,
}

#[derive(Debug)]
pub enum Expr {
    Block(BlockExpr),
    If(IfExpr),
}

// in firm, everything is a statement. expressions are statements that yield something
#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
}

#[derive(Debug)]
struct ApplicationFile {
    path: String,
    errors: Vec<CompilerError>,
}

impl ApplicationFile {
    pub fn new(path: String) -> Self {
        Self {
            path,
            errors: vec![],
        }
    }

    pub fn into_errors(self) -> CompilerErrors {
        CompilerErrors {
            source_code: self.path.clone(),
            related: self.errors,
        }
    }
}

#[derive(Debug)]
struct Application {
    files: Vec<ApplicationFile>,
}
