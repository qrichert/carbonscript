"""Test parsing module."""

import doctest
import unittest
from decimal import Decimal as D

import carbonscript.parser
from carbonscript.lexer import Lexer, Token, TokenType
from carbonscript.parser import (
    BinaryExpr,
    Expr,
    GroupExpr,
    Literal,
    ParseError,
    Parser,
    UnaryExpr,
)


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.parser))
    return tests


def parse_script(script: str) -> list[Expr]:
    tokens: list[Token] = Lexer().lex(script)
    return Parser().parse(tokens)


class TestParser(unittest.TestCase):
    def test_no_side_effects_on_input(self) -> None:
        tokens: list[Token] = [
            Token(TokenType.NUMBER, "3"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.NUMBER, "1"),
            Token(TokenType.EOF),
        ]
        parser = Parser()
        parser.parse(tokens)
        self.assertIs(tokens, parser.tokens)


class TestParserBasicExpressions(unittest.TestCase):
    def test_empty(self) -> None:
        self.assertListEqual(
            parse_script(""),
            [],
        )

    def test_with_whitespace(self) -> None:
        self.assertListEqual(
            parse_script("3   *   6   *   9 "),
            [
                BinaryExpr(
                    BinaryExpr(
                        Literal(TokenType.NUMBER, D("3")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("6")),
                    ),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("9")),
                )
            ],
        )

    def test_invalid_operator(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("1°3")
        self.assertEqual(ctx.exception.token, Token(TokenType.UNKNOWN, "°"))

    def test_number_integer(self) -> None:
        self.assertListEqual(
            parse_script("3"),
            [
                Literal(TokenType.NUMBER, D("3")),
            ],
        )

    def test_number_decimal(self) -> None:
        self.assertListEqual(
            parse_script("1.618"),
            [
                Literal(TokenType.NUMBER, D("1.618")),
            ],
        )

    def test_string(self) -> None:
        self.assertListEqual(
            parse_script('"hello, world\n"'),
            [
                Literal(TokenType.STRING, "hello, world\n"),
            ],
        )

    def test_string_multi(self) -> None:
        self.assertListEqual(
            parse_script('"hello, world\n"  "123""abc"'),
            [
                Literal(TokenType.STRING, "hello, world\n"),
                Literal(TokenType.STRING, "123"),
                Literal(TokenType.STRING, "abc"),
            ],
        )

    def test_string_empty(self) -> None:
        self.assertListEqual(
            parse_script('""'),
            [
                Literal(TokenType.STRING, ""),
            ],
        )

    def test_string_unterminated(self) -> None:
        with self.assertRaises(ParseError):
            parse_script('"foo')

    def test_string_unterminated_empty(self) -> None:
        with self.assertRaises(ParseError):
            parse_script('"')

    def test_literal_keyword_true(self) -> None:
        self.assertListEqual(
            parse_script("true"),
            [
                Literal(TokenType.LITKEYWORD, True),
            ],
        )

    def test_literal_keyword_false(self) -> None:
        self.assertListEqual(
            parse_script("false"),
            [
                Literal(TokenType.LITKEYWORD, False),
            ],
        )

    def test_literal_keyword_null(self) -> None:
        self.assertListEqual(
            parse_script("null"),
            [
                Literal(TokenType.LITKEYWORD, None),
            ],
        )

    def test_equality_equal(self) -> None:
        self.assertListEqual(
            parse_script("1==3"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("1")),
                    TokenType.DBLEQUAL,
                    Literal(TokenType.NUMBER, D("3")),
                )
            ],
        )

    def test_equality_not_equal(self) -> None:
        self.assertListEqual(
            parse_script("1!=3"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("1")),
                    TokenType.BANGEQUAL,
                    Literal(TokenType.NUMBER, D("3")),
                )
            ],
        )

    def test_comparison_greater_than(self) -> None:
        self.assertListEqual(
            parse_script("1>3"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("1")),
                    TokenType.GT,
                    Literal(TokenType.NUMBER, D("3")),
                )
            ],
        )

    def test_comparison_greater_than_or_equal(self) -> None:
        self.assertListEqual(
            parse_script("1>=3"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("1")),
                    TokenType.GTE,
                    Literal(TokenType.NUMBER, D("3")),
                )
            ],
        )

    def test_comparison_lesser_than(self) -> None:
        self.assertListEqual(
            parse_script("1<3"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("1")),
                    TokenType.LT,
                    Literal(TokenType.NUMBER, D("3")),
                )
            ],
        )

    def test_comparison_lesser_than_or_equal(self) -> None:
        self.assertListEqual(
            parse_script("1<=3"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("1")),
                    TokenType.LTE,
                    Literal(TokenType.NUMBER, D("3")),
                )
            ],
        )

    def test_term_addition(self) -> None:
        self.assertListEqual(
            parse_script("3+1"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("1")),
                )
            ],
        )

    def test_term_subtraction(self) -> None:
        self.assertListEqual(
            parse_script("3-1"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.MINUS,
                    Literal(TokenType.NUMBER, D("1")),
                )
            ],
        )

    def test_factor_multiplication(self) -> None:
        self.assertListEqual(
            parse_script("3*1"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("1")),
                )
            ],
        )

    def test_factor_division(self) -> None:
        self.assertListEqual(
            parse_script("3/1"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.SLASH,
                    Literal(TokenType.NUMBER, D("1")),
                )
            ],
        )

    def test_factor_integer_division(self) -> None:
        self.assertListEqual(
            parse_script("3//1"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.DBLSLASH,
                    Literal(TokenType.NUMBER, D("1")),
                )
            ],
        )

    def test_factor_modulo(self) -> None:
        self.assertListEqual(
            parse_script("3%1"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.PERCENT,
                    Literal(TokenType.NUMBER, D("1")),
                )
            ],
        )

    def test_power(self) -> None:
        self.assertListEqual(
            parse_script("3**1"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.DBLSTAR,
                    Literal(TokenType.NUMBER, D("1")),
                )
            ],
        )

    def test_unary_plus(self) -> None:
        self.assertListEqual(
            parse_script("+42"),
            [
                UnaryExpr(
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("42")),
                )
            ],
        )

    def test_unary_minus(self) -> None:
        self.assertListEqual(
            parse_script("-42"),
            [
                UnaryExpr(
                    TokenType.MINUS,
                    Literal(TokenType.NUMBER, D("42")),
                )
            ],
        )

    def test_unary_bang(self) -> None:
        self.assertListEqual(
            parse_script("!42"),
            [
                UnaryExpr(
                    TokenType.BANG,
                    Literal(TokenType.NUMBER, D("42")),
                )
            ],
        )

    def test_parenthesis(self) -> None:
        self.assertListEqual(
            parse_script("(7+108)"),
            [
                GroupExpr(
                    BinaryExpr(
                        Literal(TokenType.NUMBER, D("7")),
                        TokenType.PLUS,
                        Literal(TokenType.NUMBER, D("108")),
                    )
                )
            ],
        )

    def test_precedence(self) -> None:
        self.assertListEqual(
            parse_script("7+108*9"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("7")),
                    TokenType.PLUS,
                    BinaryExpr(
                        Literal(TokenType.NUMBER, D("108")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("9")),
                    ),
                )
            ],
        )

    def test_term_associativity(self) -> None:
        self.assertListEqual(
            parse_script("7+9+3"),
            [
                BinaryExpr(
                    BinaryExpr(
                        Literal(TokenType.NUMBER, D("7")),
                        TokenType.PLUS,
                        Literal(TokenType.NUMBER, D("9")),
                    ),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("3")),
                )
            ],
        )

    def test_factor_associativity(self) -> None:
        self.assertListEqual(
            parse_script("7*9*3"),
            [
                BinaryExpr(
                    BinaryExpr(
                        Literal(TokenType.NUMBER, D("7")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("9")),
                    ),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("3")),
                )
            ],
        )

    def test_power_associativity(self) -> None:
        self.assertListEqual(
            parse_script("7**9**3"),
            [
                BinaryExpr(
                    Literal(TokenType.NUMBER, D("7")),
                    TokenType.DBLSTAR,
                    BinaryExpr(
                        Literal(TokenType.NUMBER, D("9")),
                        TokenType.DBLSTAR,
                        Literal(TokenType.NUMBER, D("3")),
                    ),
                )
            ],
        )

    def test_unary_associativity_even(self) -> None:
        self.assertListEqual(
            parse_script("--3"),
            [
                UnaryExpr(
                    TokenType.MINUS,
                    UnaryExpr(
                        TokenType.MINUS,
                        Literal(TokenType.NUMBER, D("3")),
                    ),
                )
            ],
        )

    def test_unary_associativity_even_odd(self) -> None:
        self.assertListEqual(
            parse_script("---3"),
            [
                UnaryExpr(
                    TokenType.MINUS,
                    UnaryExpr(
                        TokenType.MINUS,
                        UnaryExpr(
                            TokenType.MINUS,
                            Literal(TokenType.NUMBER, D("3")),
                        ),
                    ),
                )
            ],
        )


if __name__ == "__main__":
    unittest.main()
