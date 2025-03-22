// TODO: Source spanning

/// Denoted by `{ ... }`, followed by `statement_count` arbitrary `Stmt`s
#[derive(Debug)]
pub struct BlockExpr {
    statement_count: usize,
}

/// Denoted by the `if` keyword, followed by an `Expr`, and
/// another `Expr` if `has_else_branch` is `true`.
#[derive(Debug)]
pub struct IfExpr {
    has_else_branch: bool,
}

/// Represents an arbitrary "name", i.e. alphanumerical (and _) set
/// of characters, not recognized as a keyword.
#[derive(Debug)]
pub struct IdentifierExpr {
    name: String,
}

/// Represents an arbitrary expression.
#[derive(Debug)]
pub enum Expr {
    Block(BlockExpr),
    If(IfExpr),
    Identifier(IdentifierExpr),
}

/// Denoted by the `import mod as alias` syntax.
#[derive(Debug)]
pub struct ImportStmt {
    pub module: String,
    pub alias: Option<String>,
}

/// In firm, everything is a statement. expressions are statements that yield something
#[derive(Debug)]
pub enum Stmt {
    Import(ImportStmt),

    Expr(Expr),
}
