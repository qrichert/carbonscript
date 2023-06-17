"""Test lexing module."""

import doctest
import unittest

import carbonscript.lexer
from carbonscript.lexer import KEYWORDS, PATTERNS, Lexer
from carbonscript.tokens import Token, TokenType


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
    def test_patterns_match_type(self) -> None:
        """Ensure every TokenType has a matching PATTERN.

        The aim is to avoid unused types or patterns.

        Exceptions:
        *KEYWORD: Keywords are a special case of IDENTIFIER.
        INDENT/DEDENT: Empty markers added by preprocessor.
        UNKNOWN: Represents a token with no matching type.
        GARBAGE: Specific to the implementation of the lexer.
        """
        types: list[str] = [
            x.name
            for x in TokenType
            if x
            not in (
                *KEYWORDS.values(),
                TokenType.INDENT,
                TokenType.DEDENT,
                TokenType.UNKNOWN,
                TokenType.GARBAGE,
            )
        ]
        pattern_types: list[str] = [x[1].name for x in PATTERNS]
        self.assertListEqual(types, pattern_types)


class TestLexer(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.script_using_all_tokens: str = (
            'abc 123 123.456***///%+-()[],\n "hello"! == != > >= < <= = ° '
            + "**= *= //= /= %= += -="
            + " ".join(KEYWORDS)
            + "##ml comment##\n# sl comment"
        )
        cls._assert_script_uses_all_available_types(cls.script_using_all_tokens)

    @classmethod
    def _assert_script_uses_all_available_types(cls, script: str) -> None:
        tokens: list[Token] = lex_script(script)
        token_types_in_string: set[TokenType] = {x.type for x in tokens}
        available_token_types: set[TokenType] = set(TokenType) - {
            TokenType.GARBAGE,
            TokenType.INDENT,
            TokenType.DEDENT,
        }

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
        reconstructed_script = "".join([token.lexeme for token in tokens])
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

    def test_token_keywords(self) -> None:
        tokens: list[Token] = lex_script(" ".join(KEYWORDS) + " ")
        expected: list[Token] = []
        for keyword, token_type in KEYWORDS.items():
            expected.append(Token(token_type, keyword))
            expected.append(Token(TokenType.WHITESPACE, " "))
        expected.append(Token(TokenType.EOF))
        self.assertListEqual(tokens, expected)

    def test_token_string(self) -> None:
        tokens: list[Token] = lex_script('"hello, world\n" "second string"')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.STRING, '"hello, world\n"'),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.STRING, '"second string"'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_empty(self) -> None:
        tokens: list[Token] = lex_script('""')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.STRING, '""'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_unterminated(self) -> None:
        tokens: list[Token] = lex_script('"abc_123')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.STRING, '"abc_123'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_with_ml_comment(self) -> None:
        tokens: list[Token] = lex_script('"abc ## 123 ## 456 ## def"')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.STRING, '"abc ## 123 ## 456 ## def"'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_with_sl_comment(self) -> None:
        tokens: list[Token] = lex_script('"abc # 123 # def"')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.STRING, '"abc # 123 # def"'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_double_quote_escaped(self) -> None:
        tokens: list[Token] = lex_script(r'"\"foo\""')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.STRING, r'"\"foo\""'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_double_quote_escaped_multiple_odd(self) -> None:
        tokens: list[Token] = lex_script(r'"\\\"foo\\\""')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.STRING, r'"\\\"foo\\\""'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_string_double_quote_escaped_multiple_even(self) -> None:
        tokens: list[Token] = lex_script(r'"\\"foo\\""')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.STRING, r'"\\"'),
                Token(TokenType.IDENTIFIER, "foo"),
                Token(TokenType.UNKNOWN, "\\"),
                Token(TokenType.UNKNOWN, "\\"),
                Token(TokenType.STRING, '""'),
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

    def test_token_declaration_keyword(self) -> None:
        tokens: list[Token] = lex_script("var const")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.DECLKW, "var"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.DECLKW, "const"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_literal_keyword(self) -> None:
        tokens: list[Token] = lex_script("true false")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.LITKW, "true"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.LITKW, "false"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_if_else(self) -> None:
        tokens: list[Token] = lex_script("if else")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.IF, "if"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.ELSE, "else"),
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

    def test_token_in_place_assignment_operators(self) -> None:
        tokens: list[Token] = lex_script("**=*=//=/=%=+=-=")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.DBLSTAREQ, "**="),
                Token(TokenType.STAREQ, "*="),
                Token(TokenType.DBLSLASHEQ, "//="),
                Token(TokenType.SLASHEQ, "/="),
                Token(TokenType.PERCENTEQ, "%="),
                Token(TokenType.PLUSEQ, "+="),
                Token(TokenType.MINUSEQ, "-="),
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

    def test_token_square_brackets_empty(self) -> None:
        tokens: list[Token] = lex_script("[]")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.LSQBRACKET, "["),
                Token(TokenType.RSQBRACKET, "]"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_square_brackets_with_expression(self) -> None:
        tokens: list[Token] = lex_script("[1,2]")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.LSQBRACKET, "["),
                Token(TokenType.NUMBER, "1"),
                Token(TokenType.COMMA, ","),
                Token(TokenType.NUMBER, "2"),
                Token(TokenType.RSQBRACKET, "]"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_comma(self) -> None:
        tokens: list[Token] = lex_script("123, 456")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.COMMA, ","),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.NUMBER, "456"),
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

    def test_token_ml_comment(self) -> None:
        tokens: list[Token] = lex_script("123 ## ml \n comment ## 456")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.MLCOMMENT, "## ml \n comment ##"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.NUMBER, "456"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_ml_comment_empty(self) -> None:
        tokens: list[Token] = lex_script("123 #### 456")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.MLCOMMENT, "####"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.NUMBER, "456"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_ml_comment_unterminated(self) -> None:
        tokens: list[Token] = lex_script("123 ## ml \n comment")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.MLCOMMENT, "## ml \n comment"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_ml_comment_with_string(self) -> None:
        tokens: list[Token] = lex_script('## "with \n string" ##')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.MLCOMMENT, '## "with \n string" ##'),
                Token(TokenType.EOF),
            ],
        )

    def test_token_sl_comment(self) -> None:
        tokens: list[Token] = lex_script("123 # sl comment\n 456")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.SLCOMMENT, "# sl comment"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.NUMBER, "456"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_sl_comment_empty(self) -> None:
        tokens: list[Token] = lex_script("123 #\n 456")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.SLCOMMENT, "#"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.NUMBER, "456"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_sl_comment_unterminated(self) -> None:
        tokens: list[Token] = lex_script("123 # sl comment")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.NUMBER, "123"),
                Token(TokenType.WHITESPACE, " "),
                Token(TokenType.SLCOMMENT, "# sl comment"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_sl_comment_nested(self) -> None:
        tokens: list[Token] = lex_script("# nested # comment")
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.SLCOMMENT, "# nested # comment"),
                Token(TokenType.EOF),
            ],
        )

    def test_token_sl_comment_with_string(self) -> None:
        tokens: list[Token] = lex_script('# "with string"\n')
        self.assertListEqual(
            tokens,
            [
                Token(TokenType.SLCOMMENT, '# "with string"'),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.EOF),
            ],
        )


if __name__ == "__main__":
    unittest.main()
