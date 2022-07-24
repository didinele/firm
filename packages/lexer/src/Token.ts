export enum TokenType {
	// Single character tokens
	Dot,
	Comma,
	Quote,

	Colon,
	Semicolon,

	LeftBrace,
	RightBrace,
	LeftSquareBracket,
	RightSquareBracket,
	LeftParen,
	RightParen,

	Plus,
	Minus,
	Star,
	Slash,
	Percent,

	Equals,
	LessThan,
	GreaterThan,
	Bang,

	// 2 character tokens
	ColonColon,

	PlusPlus,
	MinusMinus,

	EqualsEquals,
	LessThanEquals,
	GreaterThanEquals,
	NotEquals,

	// Comments
	Comment,
	DocComment,

	// Literals
	String,
	Number,
	True,
	False,

	// Keywords
	If,
	Else,

	For,
	While,
	Break,
	Continue,

	Return,

	Struct,
	Enum,
	Fn,
	Trait,
	Type,

	Let,
	Mut,

	Uniform,
	Pure,

	Namespace,
	Use,
	As,

	// Internal/for parsing stage
	Identifier,
	EOF,
}

export interface Token {
	readonly type: TokenType;
	readonly lexeme: string;
	readonly line: number;
	readonly column: number;
}
