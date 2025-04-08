// An interesting thing to note throughout this file is the contrast between `&'static str` and `SourceSpan`
// In all actuality, they are essentially the same, both should be 2x usize, with &'static str
// being a pointer to the start + a len, while SourceSpan is an offset + a len
// We use `SourceSpan`s for our `span` properties as a sort of semantic, and also just to signal
// it's going to be used by miette for potential error reporting, as opposed to the `&'static str`s we use for
// things like `name`s and literal `value`s, which tap directly into the source code. We only need immutable reads
// into these things at a compile-time level, so it would be a total waste to copy those spans and allocate `String`s

use firm_macros::AstNode;
use miette::SourceSpan;

/// Denoted by `{ ... }`.
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct BlockExpr {
    pub statement_count: usize,
    pub span: SourceSpan,
    #[related_many]
    pub exprs: (usize, usize),
}

/// Denoted by the `if` keyword.
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct IfExpr {
    pub span: SourceSpan,
    #[related]
    pub if_branch: usize,
    #[related_maybe]
    pub else_branch: Option<usize>,
}

/// Represents an arbitrary "name", i.e. alphanumerical (and _) set
/// of characters, not recognized as a keyword.
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct IdentifierExpr {
    pub span: SourceSpan,
    pub name: &'static str,
}

/// Represents a string literal
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct StringLiteralExpr {
    pub span: SourceSpan,
    pub value: &'static str,
}

/// Represents a number literal
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct NumberLiteralExpr {
    pub span: SourceSpan,
    pub value: &'static str,
}

/// Represents a boolean literal
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct BoolLiteralExpr {
    pub span: SourceSpan,
    pub value: bool,
}

/// Reperesents a type reference
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct TypeRefExpr {
    pub span: SourceSpan,
    pub name: &'static str,
    #[related_many]
    pub generic_args: (usize, usize),
}

/// Represents an arbitrary expression.
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct ImportStmt {
    pub span: SourceSpan,
    pub module: &'static str,
    pub alias: Option<&'static str>,
}

/// Represents an enum, denoted by the `enum` keyword.
/// The `variants` field contains a list of tuples, where each tuple
/// contains the name of the variant and an optional value.
/// Note that the parser guarantees nothing. It's up to the typechecker
/// to error if the enum 1) provides all None or all Some 2) all values and names are unique
/// 3) all values are acutally integers
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct EnumStmt {
    pub span: SourceSpan,
    pub name: &'static str,
    pub variants: Vec<(&'static str, Option<&'static str>)>,
    pub is_pub: bool,
}

/// Denoted by the `type` keyword
/// i.e. `type Foo = Bar<Baz>;`
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct TypeStmt {
    pub name: &'static str,
    pub is_pub: bool,
    pub span: SourceSpan,
    #[related]
    pub definition: usize,
}

/// Represents a struct definition, denoted by the `struct` keyword.
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct StructStmt {
    pub span: SourceSpan,
    pub name: &'static str,
    /// (is_pub, field_name)
    pub field_names: Vec<(bool, &'static str)>,
    pub is_pub: bool,
    #[related_many]
    pub field_types: (usize, usize),
}

/// Represents a function definition, denoted by the `function` keyword.
#[derive(Debug, Clone, PartialEq, Eq, AstNode)]
pub struct FunctionStmt {
    pub span: SourceSpan,
    pub name: &'static str,
    pub is_pub: bool,
    pub is_pure: bool,
    pub is_const: bool,
    pub arg_names: Vec<&'static str>,
    #[related_many]
    pub arg_types: (usize, usize),
    #[related_maybe]
    pub return_type: Option<usize>,
    #[related]
    pub body: usize,
}

/// In firm, everything is a statement. expressions are statements that yield something
#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub imports: Vec<ImportStmt>,
    pub enums: Vec<EnumStmt>,
    pub types: Vec<TypeStmt>,
    pub structs: Vec<StructStmt>,
    pub associated: Vec<Stmt>,
}
