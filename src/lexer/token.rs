use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // 1 char
    Dot(SourceSpan),
    Comma(SourceSpan),
    Colon(SourceSpan),
    Semicolon(SourceSpan),
    LeftCurly(SourceSpan),
    RightCurly(SourceSpan),
    LeftParen(SourceSpan),
    RightParen(SourceSpan),
    LeftSquare(SourceSpan),
    RightSquare(SourceSpan),
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
    Import(SourceSpan),
    As(SourceSpan),
    Type(SourceSpan),
    Pure(SourceSpan),
    Struct(SourceSpan),
    Enum(SourceSpan),
    Pub(SourceSpan),
    Static(SourceSpan),

    // Misc
    Comment(SourceSpan),
    Identifier(SourceSpan, String),

    // Internal
    EOF(SourceSpan),
}

impl Token {
    pub fn span(&self) -> SourceSpan {
        match self {
            Token::Dot(span)
            | Token::Comma(span)
            | Token::Colon(span)
            | Token::Semicolon(span)
            | Token::LeftCurly(span)
            | Token::RightCurly(span)
            | Token::LeftParen(span)
            | Token::RightParen(span)
            | Token::LeftSquare(span)
            | Token::RightSquare(span)
            | Token::Plus(span)
            | Token::Minus(span)
            | Token::Star(span)
            | Token::Slash(span)
            | Token::Percent(span)
            | Token::Equals(span)
            | Token::Bang(span)
            | Token::Greater(span)
            | Token::Less(span)
            | Token::EqualsEquals(span)
            | Token::BangEquals(span)
            | Token::GreaterEquals(span)
            | Token::LessEquals(span)
            | Token::PlusPlus(span)
            | Token::MinusMinus(span)
            | Token::PlusEquals(span)
            | Token::MinusEquals(span)
            | Token::StarEquals(span)
            | Token::SlashEquals(span)
            | Token::PercentEquals(span)
            | Token::Arrow(span)
            | Token::String(span, _)
            | Token::Number(span, _)
            | Token::True(span)
            | Token::False(span)
            | Token::If(span)
            | Token::Else(span)
            | Token::While(span)
            | Token::For(span)
            | Token::Return(span)
            | Token::Break(span)
            | Token::Continue(span)
            | Token::Function(span)
            | Token::Let(span)
            | Token::Import(span)
            | Token::As(span)
            | Token::Type(span)
            | Token::Pure(span)
            | Token::Struct(span)
            | Token::Enum(span)
            | Token::Pub(span)
            | Token::Static(span)
            | Token::Comment(span)
            | Token::Identifier(span, _)
            | Token::EOF(span) => *span,
        }
    }
}
