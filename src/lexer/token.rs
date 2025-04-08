use miette::SourceSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // 1 char
    Dot,
    Comma,
    Colon,
    Semicolon,
    LeftCurly,
    RightCurly,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equals,
    Bang,
    Greater,
    Less,

    // 2 char
    EqualsEquals,
    BangEquals,
    GreaterEquals,
    LessEquals,
    PlusPlus,
    MinusMinus,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,
    Arrow,

    // literals
    String,
    Number,

    // Keywords
    True,
    False,
    If,
    Else,
    While,
    For,
    Return,
    Break,
    Continue,
    Function,
    Let,
    Import,
    As,
    Type,
    Pure,
    Const,
    Struct,
    Enum,
    Pub,
    Static,

    // Misc
    Comment,
    Identifier,

    // Internal
    EOF,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            TokenKind::Dot => ".",
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Semicolon => ";",
            TokenKind::LeftCurly => "{",
            TokenKind::RightCurly => "}",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftSquare => "[",
            TokenKind::RightSquare => "]",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Equals => "=",
            TokenKind::Bang => "!",
            TokenKind::Greater => ">",
            TokenKind::Less => "<",
            TokenKind::EqualsEquals => "==",
            TokenKind::BangEquals => "!=",
            TokenKind::GreaterEquals => ">=",
            TokenKind::LessEquals => "<=",
            TokenKind::PlusPlus => "++",
            TokenKind::MinusMinus => "--",
            TokenKind::PlusEquals => "+=",
            TokenKind::MinusEquals => "-=",
            TokenKind::StarEquals => "*=",
            TokenKind::SlashEquals => "/=",
            TokenKind::PercentEquals => "%=",
            TokenKind::Arrow => "=>",
            TokenKind::String => "string",
            TokenKind::Number => "number",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::For => "for",
            TokenKind::Return => "return",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Function => "function",
            TokenKind::Let => "let",
            TokenKind::Import => "import",
            TokenKind::As => "as",
            TokenKind::Type => "type",
            TokenKind::Pure => "pure",
            TokenKind::Const => "const",
            TokenKind::Struct => "struct",
            TokenKind::Enum => "enum",
            TokenKind::Pub => "pub",
            TokenKind::Static => "static",
            TokenKind::Comment => "comment",
            TokenKind::Identifier => "identifier",
            TokenKind::EOF => "<EOF>",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    span: SourceSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }

    /// Equivelent to Token::new(other_token.span(), different_span)
    /// This is a bit of a hack to be used inside parsers' next_of_type! macro, where we take the $Pattern
    /// as a `pat`, which does not allow us to to use it within `Token::new()`
    pub fn with_span(&self, span: SourceSpan) -> Self {
        Self {
            kind: self.kind,
            span,
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }

    // To save some memory, we don't need to store a ref to the whole src in the token
    // when working with tokens, we should always have it available
    pub fn src(&self, src: &'static str) -> &'static str {
        &src[self.span.offset()..self.span.offset() + self.span.len()]
    }
}
