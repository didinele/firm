use miette::SourceSpan;

pub fn span_from(start: &SourceSpan, end: &SourceSpan) -> SourceSpan {
    let len = end.offset() - start.offset() + end.len();
    SourceSpan::new(start.offset().into(), len)
}

/// Denoted by `{ ... }`. Associated with `statement_count` arbitrary `Stmt`s
#[derive(Debug)]
pub struct BlockExpr {
    pub statement_count: usize,
    pub span: SourceSpan,
    pub associated: usize,
}

/// Denoted by the `if` keyword, associated with an `Expr`, and
/// another `Expr` if `has_else_branch` is `true`.
#[derive(Debug)]
pub struct IfExpr {
    pub has_else_branch: bool,
    pub span: SourceSpan,
    pub associated: usize,
}

/// Represents an arbitrary "name", i.e. alphanumerical (and _) set
/// of characters, not recognized as a keyword.
#[derive(Debug)]
pub struct IdentifierExpr {
    pub name: String,
    pub span: SourceSpan,
}

/// Represents a string literal
#[derive(Debug)]
pub struct StringLiteralExpr {
    pub value: String,
    pub span: SourceSpan,
}

/// Represents a number literal
#[derive(Debug)]
pub struct NumberLiteralExpr {
    pub value: String,
    pub span: SourceSpan,
}

/// Represents a boolean literal
#[derive(Debug)]
pub struct BoolLiteralExpr {
    pub value: bool,
    pub span: SourceSpan,
}

/// Reperesents a type reference
/// Associated with `arg_count` number of `TypeRefExpr`s
#[derive(Debug)]
pub struct TypeRefExpr {
    pub name: String,
    pub arg_count: usize,
    pub span: SourceSpan,
    pub associated: usize,
}

/// Represents an arbitrary expression.
#[derive(Debug)]
pub enum Expr {
    Block(BlockExpr),
    If(IfExpr),
    Identifier(IdentifierExpr),
    StringLiteral(StringLiteralExpr),
    NumberLiteral(NumberLiteralExpr),
    BoolLiteral(BoolLiteralExpr),
    TypeRef(TypeRefExpr),
}

/// Denoted by the `import mod as alias` syntax.
#[derive(Debug)]
pub struct ImportStmt {
    pub module: String,
    pub alias: Option<String>,
    pub span: SourceSpan,
}

/// Represents an enum, denoted by the `enum` keyword.
/// The `variants` field contains a list of tuples, where each tuple
/// contains the name of the variant and an optional value.
/// Note that the parser guarantees nothing. It's up to the typechecker
/// to error if the enum 1) provides all None or all Some 2) all values and names are unique
/// 3) all values are acutally integers
#[derive(Debug)]
pub struct EnumStmt {
    pub name: String,
    pub variants: Vec<(String, Option<String>)>,
    pub is_pub: bool,
    pub span: SourceSpan,
}

/// Denoted by the `type` keyword, associated with a `TypeRefExpr` expression.
/// i.e. `type Foo = Bar<Baz>;`
#[derive(Debug)]
pub struct TypeStmt {
    pub name: String,
    pub is_pub: bool,
    pub span: SourceSpan,
    pub associated: usize,
}

/// Represents a struct definition, denoted by the `struct` keyword.
/// Associated with `field_names.len()` number of `TypeRefExpr`s.
#[derive(Debug)]
pub struct StructStmt {
    pub name: String,
    /// (is_pub, field_name)
    pub field_names: Vec<(bool, String)>,
    pub is_pub: bool,
    pub span: SourceSpan,
    pub associated: usize,
}

/// Represents a function definition, denoted by the `function` keyword.
/// Associated with `arg_names.len()` number of `TypeRefExpr`s, a `TypeRefExpr` return type,
/// and a `BlockExpr` body.
#[derive(Debug)]
pub struct FunctionStmt {
    pub name: String,
    pub arg_names: Vec<String>,
    pub is_pub: bool,
    pub is_pure: bool,
    pub span: SourceSpan,
    pub associated: usize,
}

/// In firm, everything is a statement. expressions are statements that yield something
#[derive(Debug)]
pub enum Stmt {
    Import(ImportStmt),
    Function(FunctionStmt),
    Enum(EnumStmt),
    Type(TypeStmt),
    Struct(StructStmt),

    Expr(Expr),
}

#[derive(Debug, Default)]
pub struct ApplicationFile {
    /// Lexer + Parser errors
    pub imports: Vec<ImportStmt>,
    pub enums: Vec<EnumStmt>,
    pub types: Vec<TypeStmt>,
    pub structs: Vec<StructStmt>,
    // TODO: Consider wrapper type
    pub associated: Vec<Stmt>,
}
