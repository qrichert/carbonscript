"""Test interpreter module."""

import doctest
import unittest
from decimal import Decimal as D

import carbonscript.lexer
from carbonscript.interpreter import Interpreter, LiteralValue
from carbonscript.lexer import Lexer, Token, TokenType
from carbonscript.parser import BinaryExpr, Expr, Literal, Parser


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.interpreter))
    return tests


def interpret_script(script: str) -> LiteralValue:
    tokens: list[Token] = Lexer().lex(script)
    ast: list[Expr] = Parser().parse(tokens)
    return Interpreter().interpret(ast)


class TestInterpreter(unittest.TestCase):
    def test_no_side_effects_on_input(self) -> None:
        ast: list[Expr] = [
            BinaryExpr(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("1")),
            )
        ]
        interpreter = Interpreter()
        interpreter.interpret(ast)
        self.assertIs(ast, interpreter.ast)


class TestInterpreterBasicExpressions(unittest.TestCase):
    def test_equality_equal_true(self) -> None:
        self.assertEqual(interpret_script("3==3"), True)

    def test_equality_equal_false(self) -> None:
        self.assertEqual(interpret_script("1==3"), False)

    def test_equality_not_equal_true(self) -> None:
        self.assertEqual(interpret_script("1!=3"), True)

    def test_equality_not_equal_false(self) -> None:
        self.assertEqual(interpret_script("3!=3"), False)

    def test_comparison_greater_than_true(self) -> None:
        self.assertEqual(interpret_script("3>2"), True)

    def test_comparison_greater_than_false(self) -> None:
        self.assertEqual(interpret_script("2>2"), False)

    def test_comparison_greater_than_or_equal_true(self) -> None:
        self.assertEqual(interpret_script("2>=2"), True)

    def test_comparison_greater_than_or_equal_false(self) -> None:
        self.assertEqual(interpret_script("2>=3"), False)

    def test_comparison_lesser_than_true(self) -> None:
        self.assertEqual(interpret_script("2<3"), True)

    def test_comparison_lesser_than_false(self) -> None:
        self.assertEqual(interpret_script("2<2"), False)

    def test_comparison_lesser_than_or_equal_true(self) -> None:
        self.assertEqual(interpret_script("2<=2"), True)

    def test_comparison_lesser_than_or_equal_false(self) -> None:
        self.assertEqual(interpret_script("3<=2"), False)

    def test_term_addition(self) -> None:
        self.assertEqual(interpret_script("3+2"), D("5"))

    def test_term_subtraction(self) -> None:
        self.assertEqual(interpret_script("3-2"), D("1"))

    def test_factor_multiplication(self) -> None:
        self.assertEqual(interpret_script("3*2"), D("6"))

    def test_factor_division(self) -> None:
        self.assertEqual(interpret_script("3/2"), D("1.5"))

    def test_factor_integer_division(self) -> None:
        self.assertEqual(interpret_script("3//2"), D("1"))

    def test_factor_modulo(self) -> None:
        self.assertEqual(interpret_script("3%2"), D("1"))

    def test_power(self) -> None:
        self.assertEqual(interpret_script("3**2"), D("9"))

    def test_unary_plus(self) -> None:
        self.assertEqual(interpret_script("+42"), D("42"))

    def test_unary_minus(self) -> None:
        self.assertEqual(interpret_script("-42"), D("-42"))

    # TODO: What to do, maintain behavious !0 -> True !(!=0) -> False ?
    def test_unary_bang(self) -> None:
        self.assertEqual(interpret_script("!42"), False)

    def test_parenthesis(self) -> None:
        self.assertEqual(interpret_script("(7+108)"), D("115"))

    def test_precedence(self) -> None:
        self.assertEqual(interpret_script("7+108*9"), D("979"))

    def test_term_associativity(self) -> None:
        self.assertEqual(interpret_script("7+9+3"), D("19"))

    def test_factor_associativity(self) -> None:
        self.assertEqual(interpret_script("7*9*3"), D("189"))

    def test_power_associativity(self) -> None:
        self.assertEqual(interpret_script("4**3**2"), D("262144"))

    def test_unary_associativity_even(self) -> None:
        self.assertEqual(interpret_script("--3"), D("3"))

    def test_unary_associativity_odd(self) -> None:
        self.assertEqual(interpret_script("---3"), D("-3"))


class TestInterpreterComplexExpressions(unittest.TestCase):
    def test_operator_precedence(self) -> None:
        self.assertAlmostEqual(
            interpret_script("1+20*2*((3+1)*4+12)/3"),
            D("374.3333333"),
        )


if __name__ == "__main__":
    unittest.main()
