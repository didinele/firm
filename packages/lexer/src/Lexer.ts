import { Result, Option } from '@sapphire/result';
import { LexerError, LexerErrorCode } from './LexerError';
import { Token, TokenType } from './Token';

/**
 * Result output when a lexing operation takes place with no errors
 */
export interface OkLexerResult {
	ok: true;
	/**
	 * The tokens that were lexed
	 */
	tokens: Token[];
}

/**
 * Result output when a lexing operation takes place with errors
 */
export interface ErrLexerResult {
	ok: false;
	/**
	 * The tokens that were lexed.
	 * Error recovery might make this jank in some cases and cause parser errors
	 */
	tokens: Token[];
	/**
	 * The errors that were encountered
	 */
	errors: LexerError[];
}

export type LexerResult = OkLexerResult | ErrLexerResult;

/**
 * Stateful lexer used to tokenize firm code
 */
export class Lexer {
	/**
	 * Current index in the input string
	 */
	private index = 0;
	/**
	 * Index of where the current lexeme starts
	 */
	private currentLexemeStart = 0;
	/**
	 * Current line number
	 */
	private line = 1;
	/**
	 * Current column number
	 */
	private column = 1;

	public constructor(private readonly input: string) {}

	public get length(): number {
		return this.input.length;
	}

	public lex(): LexerResult {
		const tokens: Token[] = [];
		const errors: LexerError[] = [];

		for (const result of this.lexInput()) {
			if (result.isOk()) {
				tokens.push(result.unwrap());
			} else {
				errors.push(result.unwrapErr());
			}
		}

		if (tokens.at(-1)?.type !== TokenType.EOF) {
			tokens.push(this.makeToken(TokenType.EOF).unwrap());
		}

		if (errors.length) {
			return {
				ok: false,
				tokens,
				errors,
			};
		}

		return {
			ok: true,
			tokens,
		};
	}

	/**
	 * Whether the current index is at the end of the input string
	 */
	private get isAtEnd(): boolean {
		return this.index + 1 > this.length;
	}

	/**
	 * Represents the current lexeme
	 */
	private get currentLexeme(): string {
		return this.input.slice(this.currentLexemeStart, this.index);
	}

	/**
	 * Creates a new Token with the current state
	 */
	private makeToken(type: TokenType, lexeme?: string): Result.Ok<Token> {
		lexeme ??= type === TokenType.EOF ? 'EOF' : this.currentLexeme;
		this.currentLexemeStart = this.index;
		return Result.ok({
			type,
			lexeme,
			line: this.line,
			column: this.column,
		});
	}

	/**
	 * Creates a new LexerError with the current state
	 */
	private makeError(code: LexerErrorCode): Result.Err<LexerError> {
		return Result.err(new LexerError(code, this.line, this.column, this.currentLexeme));
	}

	/**
	 * Consumes the next token in the input - will throw if there's none left
	 */
	private advance(): string {
		if (this.isAtEnd) {
			throw new Error('Tried to advance despite being at the end of the input, this is a bug');
		}

		this.column++;
		return this.input[this.index++]!;
	}

	/**
	 * Returns the next token in the input without consuming it
	 */
	private peek(): Option<string> {
		return Option.from(this.input[this.index]);
	}

	/**
	 * Checks if the given character is a digit
	 */
	private isDigit(character: string): boolean {
		return !isNaN(parseInt(character, 10));
	}

	/**
	 * Checks if the given character can be part of an identifier
	 */
	private isIdentifier(character: string): boolean {
		return /[a-zA-Z_]/.test(character);
	}

	/**
	 * Lexes a string
	 */
	private *lexString(): IterableIterator<Result<Token, LexerError>> {
		// This is only called if we found a quote to begin with. We can safely advance
		yield this.makeToken(TokenType.Quote);

		let escape = false;
		let recoveryIdx: number | null = null;

		while (!this.isAtEnd) {
			const next = this.peek().unwrap();

			switch (next) {
				// Deal with escapes
				case '\\': {
					escape = !escape;
					this.advance();
					break;
				}

				case '"': {
					if (!escape) {
						yield this.makeToken(TokenType.String);
						this.advance();
						yield this.makeToken(TokenType.Quote);
						// Return to end the generator
						return;
					}

					escape = false;
					this.advance();
					break;
				}

				// Recovery chars
				// fn("something);
				case ')':
				// let x = "something;
				case ';':
				// fn("something, "something else");
				case ',': {
					escape = false;
					recoveryIdx ??= this.index;
					this.advance();
					break;
				}

				default: {
					escape = false;
					this.advance();
					break;
				}
			}
		}

		const error = this.makeError(LexerErrorCode.UnterminatedString);

		// If we reach the end of the input, we'll use the first `,`, `;`, or `)` as the end of the string for error recovery
		// If one isn't available, panic
		if (recoveryIdx === null) {
			throw error.unwrapErr();
		}

		yield error;

		// Reset the index to the recovery index, allowing the lexer to continue freely from here
		this.index = recoveryIdx;
		// Insert our string token
		yield this.makeToken(TokenType.String);
		// Lastly, forcefully insert a quote token
		yield this.makeToken(TokenType.Quote, '"');
	}

	/**
	 * Lexes a comment
	 */
	private *lexComment(): IterableIterator<Result.Ok<Token>> {
		// We've already advanced past //, we should check if there's a third slash (doc comment)
		const type = this.peek().match({
			some: (value) => {
				if (value === '/') {
					this.advance();
					return TokenType.DocComment;
				}

				return TokenType.Comment;
			},
			none: () => TokenType.Comment,
		});

		while (!this.isAtEnd && !this.peek().contains('\n')) {
			this.advance();
		}

		yield this.makeToken(type);
	}

	/**
	 * Lexes a number
	 */
	private *lexNumber(): IterableIterator<Result.Ok<Token>> {
		while (this.peek().isSomeAnd(this.isDigit) || this.peek().contains('.')) {
			this.advance();
		}

		yield this.makeToken(TokenType.Number);
	}

	/**
	 * Lexes an identifier
	 */
	private *lexIdentifier(): IterableIterator<Result.Ok<Token>> {
		while (this.peek().isSomeAnd(this.isIdentifier)) {
			this.advance();
		}

		const lexeme = this.currentLexeme;
		switch (lexeme) {
			// true/false literals
			case 'true': {
				yield this.makeToken(TokenType.True);
				break;
			}

			case 'false': {
				yield this.makeToken(TokenType.False);
				break;
			}

			// Keywords
			case 'if': {
				yield this.makeToken(TokenType.If);
				break;
			}

			case 'else': {
				yield this.makeToken(TokenType.Else);
				break;
			}

			case 'for': {
				yield this.makeToken(TokenType.For);
				break;
			}

			case 'while': {
				yield this.makeToken(TokenType.While);
				break;
			}

			case 'break': {
				yield this.makeToken(TokenType.Break);
				break;
			}

			case 'continue': {
				yield this.makeToken(TokenType.Continue);
				break;
			}

			case 'return': {
				yield this.makeToken(TokenType.Return);
				break;
			}

			case 'struct': {
				yield this.makeToken(TokenType.Struct);
				break;
			}

			case 'enum': {
				yield this.makeToken(TokenType.Enum);
				break;
			}

			case 'fn': {
				yield this.makeToken(TokenType.Fn);
				break;
			}

			case 'trait': {
				yield this.makeToken(TokenType.Trait);
				break;
			}

			case 'type': {
				yield this.makeToken(TokenType.Type);
				break;
			}

			case 'let': {
				yield this.makeToken(TokenType.Let);
				break;
			}

			case 'mut': {
				yield this.makeToken(TokenType.Mut);
				break;
			}

			case 'uniform': {
				yield this.makeToken(TokenType.Uniform);
				break;
			}

			case 'pure': {
				yield this.makeToken(TokenType.Pure);
				break;
			}

			case 'namespace': {
				yield this.makeToken(TokenType.Namespace);
				break;
			}

			case 'use': {
				yield this.makeToken(TokenType.Use);
				break;
			}

			case 'as': {
				yield this.makeToken(TokenType.As);
				break;
			}

			default: {
				yield this.makeToken(TokenType.Identifier);
			}
		}
	}

	/**
	 * Lexes the entire input
	 */
	private *lexInput(): IterableIterator<Result<Token, LexerError>> {
		while (!this.isAtEnd) {
			const character = this.advance();
			switch (character) {
				case '.': {
					yield this.makeToken(TokenType.Dot);
					break;
				}

				case ',': {
					yield this.makeToken(TokenType.Comma);
					break;
				}

				case '"': {
					yield* this.lexString();
					break;
				}

				case ':': {
					if (this.peek().contains(':')) {
						yield this.makeToken(TokenType.ColonColon);
						this.advance();
					} else {
						yield this.makeToken(TokenType.Colon);
					}

					break;
				}

				case ';': {
					yield this.makeToken(TokenType.Semicolon);
					break;
				}

				case '{': {
					yield this.makeToken(TokenType.LeftBrace);
					break;
				}

				case '}': {
					yield this.makeToken(TokenType.RightBrace);
					break;
				}

				case '[': {
					yield this.makeToken(TokenType.LeftSquareBracket);
					break;
				}

				case ']': {
					yield this.makeToken(TokenType.RightSquareBracket);
					break;
				}

				case '(': {
					yield this.makeToken(TokenType.LeftParen);
					break;
				}

				case ')': {
					yield this.makeToken(TokenType.RightParen);
					break;
				}

				case '+': {
					if (this.peek().contains('+')) {
						yield this.makeToken(TokenType.PlusPlus);
						this.advance();
					} else {
						yield this.makeToken(TokenType.Plus);
					}

					break;
				}

				case '-': {
					if (this.peek().contains('-')) {
						yield this.makeToken(TokenType.MinusMinus);
						this.advance();
					} else {
						yield this.makeToken(TokenType.Minus);
					}

					break;
				}

				case '*': {
					yield this.makeToken(TokenType.Star);
					break;
				}

				case '/': {
					if (this.peek().contains('/')) {
						this.advance();
						yield* this.lexComment();
					} else {
						yield this.makeToken(TokenType.Slash);
					}

					break;
				}

				case '%': {
					yield this.makeToken(TokenType.Percent);
					break;
				}

				case '=': {
					if (this.peek().contains('=')) {
						this.advance();
						yield this.makeToken(TokenType.EqualsEquals);
					} else {
						yield this.makeToken(TokenType.Equals);
					}

					break;
				}

				case '<': {
					if (this.peek().contains('=')) {
						this.advance();
						yield this.makeToken(TokenType.LessThanEquals);
					} else {
						yield this.makeToken(TokenType.LessThan);
					}

					break;
				}

				case '>': {
					if (this.peek().contains('=')) {
						this.advance();
						yield this.makeToken(TokenType.GreaterThanEquals);
					} else {
						yield this.makeToken(TokenType.GreaterThan);
					}

					break;
				}

				case '!': {
					if (this.peek().contains('=')) {
						this.advance();
						yield this.makeToken(TokenType.NotEquals);
					} else {
						yield this.makeToken(TokenType.Bang);
					}

					break;
				}

				case ' ':
				case '\r':
				case '\t': {
					this.currentLexemeStart++;
					break;
				}

				case '\n': {
					this.currentLexemeStart++;
					this.line++;
					this.column = 1;
					break;
				}

				default: {
					if (this.isDigit(character)) {
						yield* this.lexNumber();
					} else if (this.isIdentifier(character)) {
						yield* this.lexIdentifier();
					} else {
						this.currentLexemeStart++;
						yield this.makeError(LexerErrorCode.UnexpectedCharacter);
					}

					break;
				}
			}
		}
	}
}
