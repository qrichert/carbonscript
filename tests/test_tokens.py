"""Test tokens module."""

import doctest
import unittest

import carbonscript.tokens
from carbonscript.tokens import Token, TokenType


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.tokens))
    return tests


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


if __name__ == "__main__":
    unittest.main()
