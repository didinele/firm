use miette::SourceSpan;

pub fn span_from(start: &SourceSpan, end: &SourceSpan) -> SourceSpan {
    let len = end.offset() - start.offset() + end.len();
    SourceSpan::new(start.offset().into(), len)
}

/// Denoted by `{ ... }`, followed by `statement_count` arbitrary `Stmt`s
#[derive(Debug)]
pub struct BlockExpr {
    pub statement_count: usize,
    pub span: SourceSpan,
}

/// Denoted by the `if` keyword, followed by an `Expr`, and
/// another `Expr` if `has_else_branch` is `true`.
#[derive(Debug)]
pub struct IfExpr {
    pub has_else_branch: bool,
    pub span: SourceSpan,
}

/// Represents an arbitrary "name", i.e. alphanumerical (and _) set
/// of characters, not recognized as a keyword.
#[derive(Debug)]
pub struct IdentifierExpr {
    pub name: String,
    pub span: SourceSpan,
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
    pub span: SourceSpan,
}

/// Represents an enum, denoted by the `enum` keyword.
#[derive(Debug)]
pub struct EnumStmt {
    pub name: String,
    pub variants: Vec<String>,
    pub is_public: bool,
    pub span: SourceSpan,
}

/// In firm, everything is a statement. expressions are statements that yield something
#[derive(Debug)]
pub enum Stmt {
    Import(ImportStmt),

    Expr(Expr),
}
