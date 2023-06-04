"""Test lexing module."""

import doctest
import unittest

import carbonscript.lexer
from carbonscript.lexer import (
    DECL_KEYWORDS,
    LITERAL_KEYWORDS,
    PATTERNS,
    Lexer,
    Token,
    TokenType,
)


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.lexer))
    return tests


def lex_script(script: str) -> list[Token]:
    return Lexer().lex(script)


class TestPatterns(unittest.TestCase):
    def test_token_types_are_unique(self) -> None:
        types: list[str] = [x.name for x in TokenType]
        self.assertEqual(len(types), len(set(types)))

    def test_no_overlap_among_keywords(self) -> None:
        intersection: set = set.intersection(DECL_KEYWORDS, LITERAL_KEYWORDS)
        self.assertEqual(len(intersection), 0)

    def test_patterns_match_type(self) -> None:
        """Ensure every TokenType has a matching PATTERN.

        The aim is to avoid unused types or patterns.

        Exceptions:
        *KEYWORD: Keywords are a special case of IDENTIFIER.
        STRING: Whether a char is part of a string is based on context.
        UNKNOWN: Represents a token with no matching type.
        GARBAGE: Specific to the implementation of the lexer.
        """
        types: list[str] = [
            x.name
            for x in TokenType
            if x
            not in (
                TokenType.DECLKEYWORD,
                TokenType.LITKEYWORD,
                TokenType.STRING,
                TokenType.UNKNOWN,
                TokenType.GARBAGE,
            )
        ]
        pattern_types: list[str] = [x[1].name for x in PATTERNS]
        self.assertListEqual(types, pattern_types)


class TestToken(unittest.TestCase):
    def test_equality_equal(self) -> None:
        token1 = Token(TokenType.NUMBER, "123")
        token2 = Token(TokenType.NUMBER, "123")
        self.assertEqual(token1, token2)

    def test_equality_not_equal_token_type(self) -> None:
        token1 = Token(TokenType.NUMBER, "123")
        token2 = Token(TokenType.PLUS, "123")
        self.assertNotEqual(token1, token2)

    def test_equality_not_equal_value(self) -> None:
        token1 = Token(TokenType.NUMBER, "123")
        token2 = Token(TokenType.NUMBER, "456")
        self.assertNotEqual(token1, token2)

    def test_repr_without_value(self) -> None:
        token = Token(TokenType.EOF)
        self.assertEqual(repr(token), "Token(EOF)")

    def test_repr_with_value(self) -> None:
        token = Token(TokenType.NUMBER, "8")
        self.assertEqual(repr(token), "Token(NUMBER, '8')")

    def test_str_without_value(self) -> None:
        token = Token(TokenType.EOF)
        self.assertEqual(str(token), repr(token))

    def test_str_with_value(self) -> None:
        token = Token(TokenType.NUMBER, "8")
        self.assertEqual(str(token), repr(token))

    def test_repr_max_length_under(self) -> None:
        token = Token(TokenType.NUMBER, "123456789")
        self.assertEqual(repr(token), "Token(NUMBER, '123456789')")

    def test_repr_max_length_equal(self) -> None:
        token = Token(TokenType.NUMBER, "1234567890")
        self.assertEqual(repr(token), "Token(NUMBER, '1234567890')")

    def test_repr_max_length_over(self) -> None:
        token = Token(TokenType.NUMBER, "12345678901")
        self.assertEqual(repr(token), "Token(NUMBER, '123456789…')")


class TestLexer(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.script_using_all_tokens: str = (
            'abc 123 123.456***///%+-()\n "hello"! == != > >= < <= = ° '
            + " ".join(DECL_KEYWORDS)
            + " ".join(LITERAL_KEYWORDS)
        )
        cls._assert_script_uses_all_available_types(cls.script_using_all_tokens)

    @classmethod
    def _assert_script_uses_all_available_types(cls, script: str) -> None:
        tokens: list[Token] = lex_script(script)
        token_types_in_string: set[TokenType] = {x.type for x in tokens}
        available_token_types: set[TokenType] = set(TokenType) - {TokenType.GARBAGE}

        diff = available_token_types - token_types_in_string
        if diff:
            diff = ", ".join([x.name for x in diff])
            raise AssertionError(
                f"Input string does not use all tokens available: {diff}."
            )

    def test_no_side_effects_on_input(self) -> None:
        script: str = self.script_using_all_tokens
        lexer = Lexer()
        lexer.lex(script)
        self.assertIs(script, lexer.script)

    def test_no_part_of_input_missing_from_tokens(self) -> None:
        script: str = self.script_using_all_tokens
        tokens: list[Token] = lex_script(script)
        reconstructed_script = "".join([token.value for token in tokens])
        self.assertEqual(script, reconstructed_script)

    def test_line_and_column(self) -> None:
        script = "1+2\nabc\nc  "
        expected_tokens: list[Token] = [
            Token(TokenType.NUMBER, "1", 1, 1),
            Token(TokenType.PLUS, "+", 1, 2),
            Token(TokenType.NUMBER, "2", 1, 3),
            Token(TokenType.NEWLINE, "\n", 1, 4),
            Token(TokenType.IDENTIFIER, "abc", 2, 1),
            Token(TokenType.NEWLINE, "\n", 2, 4),
            Token(TokenType.IDENTIFIER, "c", 3, 1),
            Token(TokenType.WHITESPACE, "  ", 3, 2),
            Token(TokenType.EOF, "", 3, 4),
        ]
        tokens: list[Token] = lex_script(script)
        for token, expected_token in zip(tokens, expected_tokens):
            self.assertEqual(
                token,
                expected_token,
                f"{token} != Expected{expected_token}",
            )
            self.assertEqual(
                token.line,
                expected_token.line,
                f"{token}.line != Expected{expected_token}.line",
            )
            self.assertEqual(
                token.column,
                expected_token.column,
                f"{token}.column != Expected{expected_token}.column",
            )

    def test_token_literal_keywords(self) -> None:
        tokens: list[Token] = lex_script(" ".join(LITERAL_KEYWORDS) + " ")
        expected: list[Token] = []
        for kw in LITERAL_KEYWORDS:
            expected.append(Token(TokenType.LITKEYWORD, kw))
            expected.append(Token(TokenType.WHITESPACE, " "))
        expected.append(Token(TokenType.EOF))
        self.assertListEqual(tokens, expected)

    def test_token_string(self) -> None:
        tokens: list[Token] = lex_script('"hello, world\n" "#2"')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.DBLQUOTE, '"'),
                Token(TokenType.STRING, "hello, world\n"),
                Token(TokenType.DBLQUOTE, '"'),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.DBLQUOTE, '"'),
                Token(TokenType.STRING, "#2"),
                Token(TokenType.DBLQUOTE, '"'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_empty(self) -> None:
        tokens: list[Token] = lex_script('""')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.DBLQUOTE, '"'),
                Token(TokenType.DBLQUOTE, '"'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_unterminated(self) -> None:
        tokens: list[Token] = lex_script('"abc_123')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.DBLQUOTE, '"'),
                Token(TokenType.STRING, "abc_123"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_identifier(self) -> None:
        tokens: list[Token] = lex_script("abc_123 _0ABC")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.IDENTIFIER, "abc_123"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.IDENTIFIER, "_0ABC"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_keyword(self) -> None:
        tokens: list[Token] = lex_script("var const")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.DECLKEYWORD, "var"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.DECLKEYWORD, "const"),
                Token(TokenType.EOF),
            ],
        )

    def test_literal_keyword(self) -> None:
        tokens: list[Token] = lex_script("true false")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.LITKEYWORD, "true"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.LITKEYWORD, "false"),
                Token(TokenType.EOF),
            ],
        )

    def test_identifier_overlapping_keyword(self) -> None:
        # Overlaps "var".
        tokens: list[Token] = lex_script("variable")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.IDENTIFIER, "variable"),
                Token(TokenType.EOF),
            ],
        )

    def test_identifier_overlapping_literal_keyword(self) -> None:
        # Overlaps "true".
        tokens: list[Token] = lex_script("truevalue")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.IDENTIFIER, "truevalue"),
                Token(TokenType.EOF),
            ],
        )

    def test_identifier_overlapping_keywords(self) -> None:
        # Overlaps "true" and "var".
        tokens: list[Token] = lex_script("true0var")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.IDENTIFIER, "true0var"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_number(self) -> None:
        tokens: list[Token] = lex_script("123 123.456")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.NUMBER, "123.456"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_arithmetic_operators(self) -> None:
        tokens: list[Token] = lex_script("***///%+-")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.DBLSTAR, "**"),
                Token(TokenType.STAR, "*"),
                Token(TokenType.DBLSLASH, "//"),
                Token(TokenType.SLASH, "/"),
                Token(TokenType.PERCENT, "%"),
                Token(TokenType.PLUS, "+"),
                Token(TokenType.MINUS, "-"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_equality(self) -> None:
        tokens: list[Token] = lex_script("!===")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.BANGEQUAL, "!="),
                Token(TokenType.DBLEQUAL, "=="),
                Token(TokenType.EOF),
            ],
        )

    def test_token_comparison(self) -> None:
        tokens: list[Token] = lex_script(">>=<<=")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.GT, ">"),
                Token(TokenType.GTE, ">="),
                Token(TokenType.LT, "<"),
                Token(TokenType.LTE, "<="),
                Token(TokenType.EOF),
            ],
        )

    def test_token_bang(self) -> None:
        tokens: list[Token] = lex_script("!!=!")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.BANG, "!"),
                Token(TokenType.BANGEQUAL, "!="),
                Token(TokenType.BANG, "!"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_equal(self) -> None:
        tokens: list[Token] = lex_script("===!==")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.DBLEQUAL, "=="),
                Token(TokenType.EQUAL, "="),
                Token(TokenType.BANGEQUAL, "!="),
                Token(TokenType.EQUAL, "="),
                Token(TokenType.EOF),
            ],
        )

    def test_token_parentheses_empty(self) -> None:
        tokens: list[Token] = lex_script("()")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.LPAREN, "("),
                Token(TokenType.RPAREN, ")"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_parentheses_with_expression(self) -> None:
        tokens: list[Token] = lex_script("(1+2)")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.LPAREN, "("),
                Token(TokenType.NUMBER, "1"),
                Token(TokenType.PLUS, "+"),
                Token(TokenType.NUMBER, "2"),
                Token(TokenType.RPAREN, ")"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_newline(self) -> None:
        tokens: list[Token] = lex_script("123\n456")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.NUMBER, "456"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_newline_spaced(self) -> None:
        tokens: list[Token] = lex_script("123    \n  456")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.WHITESPACE, "    "),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.WHITESPACE, "  "),
                Token(TokenType.NUMBER, "456"),
                Token(TokenType.EOF),
            ],
        )


if __name__ == "__main__":
    unittest.main()
