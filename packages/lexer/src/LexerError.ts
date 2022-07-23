export enum LexerErrorCode {
	UnexpectedCharacter,
	UnterminatedString,
}

const MESSAGES = {
	[LexerErrorCode.UnexpectedCharacter]: 'Unexpected character',
	[LexerErrorCode.UnterminatedString]: 'An unterminated string was encountered',
} as const;

export class LexerError extends Error {
	public override readonly name = `${super.name} [${LexerErrorCode[this.code]!}]`;

	public constructor(
		public readonly code: LexerErrorCode,
		public readonly line: number,
		public readonly column: number,
		public readonly lexeme: string,
	) {
		super(MESSAGES[code]);
	}
}
