import { test, describe, expect } from 'vitest';
import { ErrLexerResult, Lexer, LexerError, LexerErrorCode, TokenType } from '../src';

// eslint-disable-next-line @typescript-eslint/no-unused-vars, @typescript-eslint/no-empty-function
function assertMatchingTypes<TExpected, _TReceived extends TExpected>(): void {}

test('it can lex single characters', () => {
	const characters = [
		'.',
		',',
		':',
		';',
		'{',
		'}',
		'[',
		']',
		'(',
		')',
		'+',
		'-',
		'*',
		'/',
		'%',
		'=',
		'<',
		'>',
		'!',
	] as const;
	const tokens = [
		TokenType.Dot,
		TokenType.Comma,
		TokenType.Colon,
		TokenType.Semicolon,
		TokenType.LeftBrace,
		TokenType.RightBrace,
		TokenType.LeftSquareBracket,
		TokenType.RightSquareBracket,
		TokenType.LeftParen,
		TokenType.RightParen,
		TokenType.Plus,
		TokenType.Minus,
		TokenType.Star,
		TokenType.Slash,
		TokenType.Percent,
		TokenType.Equals,
		TokenType.LessThan,
		TokenType.GreaterThan,
		TokenType.Bang,
	] as const;

	assertMatchingTypes<typeof characters['length'], typeof tokens['length']>();

	for (let i = 0; i < characters.length; i++) {
		const lexer = new Lexer(characters[i]);
		const result = lexer.lex();
		expect(result.ok).toBe(true);
		expect(result.tokens).toStrictEqual(
			expect.arrayContaining([
				expect.objectContaining({
					type: tokens[i],
				}),
			]),
		);
	}
});

test('it can lex 2 character tokens', () => {
	const characters = ['::', '++', '--', '==', '<=', '>=', '!='] as const;
	const tokens = [
		TokenType.ColonColon,
		TokenType.PlusPlus,
		TokenType.MinusMinus,
		TokenType.EqualsEquals,
		TokenType.LessThanEquals,
		TokenType.GreaterThanEquals,
		TokenType.NotEquals,
	] as const;

	assertMatchingTypes<typeof characters['length'], typeof tokens['length']>();

	for (let i = 0; i < characters.length; i++) {
		const lexer = new Lexer(characters[i]);
		const result = lexer.lex();
		expect(result.ok).toBe(true);
		expect(result.tokens).toStrictEqual(
			expect.arrayContaining([
				expect.objectContaining({
					type: tokens[i],
				}),
			]),
		);
	}
});

test('it can lex comments', () => {
	const input = `// hewwo world
*
/// hewwo world
`;

	const lexer = new Lexer(input);
	const result = lexer.lex();
	expect(result.ok).toBe(true);
	expect(result.tokens).toStrictEqual(
		expect.arrayContaining([
			expect.objectContaining({
				type: TokenType.Comment,
			}),
			expect.objectContaining({
				type: TokenType.Star,
			}),
			expect.objectContaining({
				type: TokenType.DocComment,
			}),
		]),
	);
});

describe('string literals', () => {
	test('it can lex string literals', () => {
		const input = `"hello world"`;

		const lexer = new Lexer(input);
		const result = lexer.lex();
		expect(result.ok).toBe(true);
		expect(result.tokens).toStrictEqual(
			expect.arrayContaining([
				expect.objectContaining({
					type: TokenType.Quote,
				}),
				expect.objectContaining({
					type: TokenType.String,
					lexeme: 'hello world',
				}),
				expect.objectContaining({
					type: TokenType.Quote,
				}),
			]),
		);
	});

	test('it can lex string literals with escape sequences', () => {
		const input = `"hello \\"owo\\" world"`;

		const lexer = new Lexer(input);
		const result = lexer.lex();
		expect(result.ok).toBe(true);
		expect(result.tokens).toStrictEqual(
			expect.arrayContaining([
				expect.objectContaining({
					type: TokenType.Quote,
				}),
				expect.objectContaining({
					type: TokenType.String,
					lexeme: 'hello \\"owo\\" world',
				}),
				expect.objectContaining({
					type: TokenType.Quote,
				}),
			]),
		);
	});

	test('it can recover from an unterminated string', () => {
		const input = `"hello world;`;

		const lexer = new Lexer(input);
		const result = lexer.lex();
		expect(result.ok).toBe(false);
		expect(result.tokens).toStrictEqual(
			expect.arrayContaining([
				expect.objectContaining({
					type: TokenType.Quote,
				}),
				expect.objectContaining({
					type: TokenType.String,
					lexeme: 'hello world',
				}),
				expect.objectContaining({
					type: TokenType.Quote,
				}),
				expect.objectContaining({
					type: TokenType.Semicolon,
				}),
			]),
		);
		const { errors } = result as ErrLexerResult;
		expect(errors.length).toEqual(1);
		expect(errors[0].code).toEqual(LexerErrorCode.UnterminatedString);
	});

	test("it panics if there's no usable token for recovery", () => {
		const input = `"hello world`;

		const lexer = new Lexer(input);
		expect(() => lexer.lex()).toThrow(LexerError);
	});
});

describe('number literals', () => {
	test('integer literals', () => {
		const input = '123';

		const lexer = new Lexer(input);
		const result = lexer.lex();
		expect(result.ok).toBe(true);
		expect(result.tokens).toStrictEqual(
			expect.arrayContaining([
				expect.objectContaining({
					type: TokenType.Number,
					lexeme: '123',
				}),
			]),
		);
	});

	test('decimal literals', () => {
		const input = '123.456';

		const lexer = new Lexer(input);
		const result = lexer.lex();
		expect(result.ok).toBe(true);
		expect(result.tokens).toStrictEqual(
			expect.arrayContaining([
				expect.objectContaining({
					type: TokenType.Number,
					lexeme: '123.456',
				}),
			]),
		);
	});

	test('floating decimal literal', () => {
		const input = '123.';

		const lexer = new Lexer(input);
		const result = lexer.lex();
		expect(result.ok).toBe(true);
		expect(result.tokens).toStrictEqual(
			expect.arrayContaining([
				expect.objectContaining({
					type: TokenType.Number,
					lexeme: '123.',
				}),
			]),
		);
	});
});

test('it can lex identifiers', () => {
	const input = 'let x = 123;';

	const lexer = new Lexer(input);
	const result = lexer.lex();
	expect(result.ok).toBe(true);
	expect(result.tokens).toStrictEqual(
		expect.arrayContaining([
			expect.objectContaining({
				type: TokenType.Let,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'x',
			}),
			expect.objectContaining({
				type: TokenType.Equals,
			}),
			expect.objectContaining({
				type: TokenType.Number,
				lexeme: '123',
			}),
			expect.objectContaining({
				type: TokenType.Semicolon,
			}),
		]),
	);
});

test('unexpected character', () => {
	const lexer = new Lexer('ă 123');
	const result = lexer.lex();

	expect(result.ok).toBe(false);
	expect(result.tokens).toStrictEqual(
		expect.arrayContaining([
			expect.objectContaining({
				type: TokenType.Number,
				lexeme: '123',
			}),
		]),
	);
	const { errors } = result as ErrLexerResult;
	expect(errors[0].code).toEqual(LexerErrorCode.UnexpectedCharacter);
});

test('it can lex a tiny example program', () => {
	const program = `fn main() {
		let x = 123;
		let y = 456;
		let z = x + y;
		println(z);
	}`;

	const lexer = new Lexer(program);
	const result = lexer.lex();

	expect(result.ok).toBe(true);
	expect(result.tokens).toStrictEqual(
		expect.arrayContaining([
			expect.objectContaining({
				type: TokenType.Fn,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'main',
			}),
			expect.objectContaining({
				type: TokenType.LeftParen,
			}),
			expect.objectContaining({
				type: TokenType.RightParen,
			}),
			expect.objectContaining({
				type: TokenType.LeftBrace,
			}),
			expect.objectContaining({
				type: TokenType.Let,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'x',
			}),
			expect.objectContaining({
				type: TokenType.Equals,
			}),
			expect.objectContaining({
				type: TokenType.Number,
				lexeme: '123',
			}),
			expect.objectContaining({
				type: TokenType.Semicolon,
			}),
			expect.objectContaining({
				type: TokenType.Let,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'y',
			}),
			expect.objectContaining({
				type: TokenType.Equals,
			}),
			expect.objectContaining({
				type: TokenType.Number,
				lexeme: '456',
			}),
			expect.objectContaining({
				type: TokenType.Semicolon,
			}),
			expect.objectContaining({
				type: TokenType.Let,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'z',
			}),
			expect.objectContaining({
				type: TokenType.Equals,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'x',
			}),
			expect.objectContaining({
				type: TokenType.Plus,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'y',
			}),
			expect.objectContaining({
				type: TokenType.Semicolon,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'println',
			}),
			expect.objectContaining({
				type: TokenType.LeftParen,
			}),
			expect.objectContaining({
				type: TokenType.Identifier,
				lexeme: 'z',
			}),
			expect.objectContaining({
				type: TokenType.RightParen,
			}),
			expect.objectContaining({
				type: TokenType.Semicolon,
			}),
			expect.objectContaining({
				type: TokenType.RightBrace,
			}),
		]),
	);
});
