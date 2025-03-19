use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // 1 char
    Dot(SourceSpan),
    Comma(SourceSpan),
    Colon(SourceSpan),
    Semicolon(SourceSpan),
    LeftBracket(SourceSpan),
    RightBracket(SourceSpan),
    LeftParen(SourceSpan),
    RightParen(SourceSpan),
    LeftBrace(SourceSpan),
    RightBrace(SourceSpan),
    Plus(SourceSpan),
    Minus(SourceSpan),
    Star(SourceSpan),
    Slash(SourceSpan),
    Percent(SourceSpan),
    Equals(SourceSpan),
    Bang(SourceSpan),
    Greater(SourceSpan),
    Less(SourceSpan),

    // 2 char
    EqualsEquals(SourceSpan),
    BangEquals(SourceSpan),
    GreaterEquals(SourceSpan),
    LessEquals(SourceSpan),
    PlusPlus(SourceSpan),
    MinusMinus(SourceSpan),
    PlusEquals(SourceSpan),
    MinusEquals(SourceSpan),
    StarEquals(SourceSpan),
    SlashEquals(SourceSpan),
    PercentEquals(SourceSpan),
    Arrow(SourceSpan),

    // literals
    String(SourceSpan, String),
    Number(SourceSpan, String),

    // Keywords
    True(SourceSpan),
    False(SourceSpan),
    If(SourceSpan),
    Else(SourceSpan),
    While(SourceSpan),
    For(SourceSpan),
    Return(SourceSpan),
    Break(SourceSpan),
    Continue(SourceSpan),
    Function(SourceSpan),
    Let(SourceSpan),
    Namespace(SourceSpan),
    Import(SourceSpan),
    As(SourceSpan),
    Type(SourceSpan),
    Pure(SourceSpan),
    Struct(SourceSpan),
    Enum(SourceSpan),
    Pub(SourceSpan),

    // Misc
    Comment(SourceSpan),
    Identifier(SourceSpan, String),

    // Internal
    EOF(bool),
}
