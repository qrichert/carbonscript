"""Test interpreter module."""

import doctest
import textwrap
import unittest
from decimal import Decimal as D

import carbonscript.interpreter
from carbonscript.ast import BinOp, ExprStmt, Literal, Stmt
from carbonscript.environment import Environment
from carbonscript.interpreter import Interpreter
from carbonscript.lexer import Lexer
from carbonscript.parser import Parser
from carbonscript.tokens import Token, TokenType
from carbonscript.values import LiteralValue

from .fixtures import THE_BIG_ONE


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.interpreter))
    return tests


def interpret_as_expression(statement: str) -> LiteralValue:
    tokens: list[Token] = Lexer().lex(statement)
    statements: list[Stmt] = Parser().parse(tokens)
    return Interpreter().interpret_one(statements[0])


def interpret_script(script: str) -> None:
    tokens: list[Token] = Lexer().lex(script)
    statements: list[Stmt] = Parser().parse(tokens)
    return Interpreter().interpret(statements)


def interpret_script_and_return_env(script: str) -> Environment:
    tokens: list[Token] = Lexer().lex(script)
    statements: list[Stmt] = Parser().parse(tokens)
    interpreter: Interpreter = Interpreter()
    interpreter.interpret(statements)
    return interpreter.env


def dedent(script: str) -> str:
    return textwrap.dedent(script).lstrip("\n")


class TestInterpreter(unittest.TestCase):
    def test_no_side_effects_on_input(self) -> None:
        statements: list[Stmt] = [
            ExprStmt(
                BinOp(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("1")),
                )
            )
        ]
        interpreter = Interpreter()
        interpreter.interpret(statements)
        self.assertIs(statements, interpreter.statements)

    def test_works_with_empty_lines(self) -> None:
        interpret_script("    \n\n\n        \n\n\n    ")


class TestStatements(unittest.TestCase):
    def test_var_declaration(self) -> None:
        env = interpret_script_and_return_env("var i = 42")
        self.assertEqual(env.get("i"), D("42"))

    def test_var_assignment(self) -> None:
        env = interpret_script_and_return_env("var i = 42\ni = 1337")
        self.assertEqual(env.get("i"), D("1337"))

    def test_var_operation_assignment(self) -> None:
        env = interpret_script_and_return_env("var i = 12 * 9")
        self.assertEqual(env.get("i"), D("108"))

    def test_var_chained_declaration(self) -> None:
        env = interpret_script_and_return_env(
            "var a = 1\nvar b = 2\nconst c = a = b = 108"
        )
        self.assertEqual(env.get("a"), D("108"))
        self.assertEqual(env.get("b"), D("108"))
        self.assertEqual(env.get("c"), D("108"))

    def test_var_chained_assignment(self) -> None:
        env = interpret_script_and_return_env("var i = null\nvar j = null\ni = j = 3")
        self.assertEqual(env.get("i"), D("3"))
        self.assertEqual(env.get("j"), D("3"))

    def test_const_declaration(self) -> None:
        env = interpret_script_and_return_env("const i = 42")
        self.assertEqual(env.get("i"), D("42"))

    def test_const_assignment(self) -> None:
        with self.assertRaises(RuntimeError):
            interpret_script("const i = 42\ni = 1337")

    def test_var_operation_with_var(self) -> None:
        env = interpret_script_and_return_env(
            "const a = 3\nconst b = 5\nconst c = a * b"
        )
        self.assertEqual(env.get("c"), D("15"))


class TestBasicExpressions(unittest.TestCase):
    def test_equality_equal_true(self) -> None:
        self.assertEqual(interpret_as_expression("3==3"), True)

    def test_equality_equal_false(self) -> None:
        self.assertEqual(interpret_as_expression("1==3"), False)

    def test_equality_not_equal_true(self) -> None:
        self.assertEqual(interpret_as_expression("1!=3"), True)

    def test_equality_not_equal_false(self) -> None:
        self.assertEqual(interpret_as_expression("3!=3"), False)

    def test_comparison_greater_than_true(self) -> None:
        self.assertEqual(interpret_as_expression("3>2"), True)

    def test_comparison_greater_than_false(self) -> None:
        self.assertEqual(interpret_as_expression("2>2"), False)

    def test_comparison_greater_than_or_equal_true(self) -> None:
        self.assertEqual(interpret_as_expression("2>=2"), True)

    def test_comparison_greater_than_or_equal_false(self) -> None:
        self.assertEqual(interpret_as_expression("2>=3"), False)

    def test_comparison_lesser_than_true(self) -> None:
        self.assertEqual(interpret_as_expression("2<3"), True)

    def test_comparison_lesser_than_false(self) -> None:
        self.assertEqual(interpret_as_expression("2<2"), False)

    def test_comparison_lesser_than_or_equal_true(self) -> None:
        self.assertEqual(interpret_as_expression("2<=2"), True)

    def test_comparison_lesser_than_or_equal_false(self) -> None:
        self.assertEqual(interpret_as_expression("3<=2"), False)

    def test_term_addition(self) -> None:
        self.assertEqual(interpret_as_expression("3+2"), D("5"))

    def test_term_subtraction(self) -> None:
        self.assertEqual(interpret_as_expression("3-2"), D("1"))

    def test_factor_multiplication(self) -> None:
        self.assertEqual(interpret_as_expression("3*2"), D("6"))

    def test_factor_division(self) -> None:
        self.assertEqual(interpret_as_expression("3/2"), D("1.5"))

    def test_factor_integer_division(self) -> None:
        self.assertEqual(interpret_as_expression("3//2"), D("1"))

    def test_factor_modulo(self) -> None:
        self.assertEqual(interpret_as_expression("3%2"), D("1"))

    def test_power(self) -> None:
        self.assertEqual(interpret_as_expression("3**2"), D("9"))

    def test_unary_plus(self) -> None:
        self.assertEqual(interpret_as_expression("+42"), D("42"))

    def test_unary_minus(self) -> None:
        self.assertEqual(interpret_as_expression("-42"), D("-42"))

    # TODO: What to do, maintain behavious !0 -> True !(!=0) -> False ?
    def test_unary_bang(self) -> None:
        self.assertEqual(interpret_as_expression("!42"), False)

    def test_parenthesis(self) -> None:
        self.assertEqual(interpret_as_expression("(7+108)"), D("115"))

    def test_precedence(self) -> None:
        self.assertEqual(interpret_as_expression("7+108*9"), D("979"))

    def test_term_associativity(self) -> None:
        self.assertEqual(interpret_as_expression("7+9+3"), D("19"))

    def test_factor_associativity(self) -> None:
        self.assertEqual(interpret_as_expression("7*9*3"), D("189"))

    def test_power_associativity(self) -> None:
        self.assertEqual(interpret_as_expression("4**3**2"), D("262144"))

    def test_unary_associativity_even(self) -> None:
        self.assertEqual(interpret_as_expression("--3"), D("3"))

    def test_unary_associativity_odd(self) -> None:
        self.assertEqual(interpret_as_expression("---3"), D("-3"))


class TestComplexExpressions(unittest.TestCase):
    def test_operator_precedence(self) -> None:
        self.assertAlmostEqual(
            interpret_as_expression("1+20*2*((3+1)*4+12)/3"),
            D("374.3333333"),
        )


class TestScope(unittest.TestCase):
    def test_modified_in_child_scope(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 42
                    foo = 108
                """
            ),
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("108"), False),
            },
        )

    def test_redefined_in_child_scope_keeps_value_in_parent_scope(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var bar = 3
                    var bar = 7
                """
            ),
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "bar": (D("3"), False),
            },
        )

    def test_redefined_in_child_scope_changes_value(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var bar = 3
                var biz = 0
                    var bar = 7
                    biz = bar
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "bar": (D("3"), False),
                "biz": (D("7"), False),
            },
        )

    def test_declared_in_child_scope_not_in_parent_scope(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                const placeholder = null
                    var baz = 1.618
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "placeholder": (None, True),
            },
        )

    def test_access_declared_in_child_scope(self) -> None:
        with self.assertRaises(RuntimeError):
            interpret_script_and_return_env(
                dedent(
                    """
                    const placeholder = null
                        const baz = 1.618
                    const foo = baz
                    """
                )
            )

    def test_mutate_const_in_child_scope(self) -> None:
        with self.assertRaises(RuntimeError):
            interpret_script_and_return_env(
                dedent(
                    """
                    const foo = null
                        foo = 1.618
                    """
                )
            )


class TestControlFlow(unittest.TestCase):
    def test_if_then(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 42
                if (true)
                    foo = 1.618
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("1.618"), False),
            },
        )

    def test_if_then_else(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 42
                if (false)
                    foo = -1
                else
                    foo = 1.618
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("1.618"), False),
            },
        )

    def test_if_then_else_if_else(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 42
                if (false)
                    foo = -1
                else if (true)
                    foo = 1.618
                else
                    foo = 0
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("1.618"), False),
            },
        )


class TestTheBigOne(unittest.TestCase):
    def test_the_big_one(self) -> None:
        with open(THE_BIG_ONE) as f:
            script: str = f.read()
        env = interpret_script_and_return_env(script)
        self.assertDictEqual(
            env.to_dict(),
            {
                "assignment": (True, True),
                "equal": (True, True),
                "not_equal": (True, True),
                "comp_gt": (True, True),
                "comp_gte": (True, True),
                "comp_lt": (True, True),
                "comp_lte": (True, True),
                "add": (D("7"), True),
                "subtract": (D("-1"), True),
                "multiply": (D("12"), True),
                "divide": (D("0.75"), True),
                "int_divide": (D("1"), True),
                "modulo": (D("0"), True),
                "power": (D("16"), True),
                "unary_plus": (D("7"), True),
                "unary_minus": (D("-7"), True),
                "unary_bang": (False, True),
                "number_int": (D("36"), True),
                "number_float": (D("1.618"), True),
                "string": ("hello, world\\n", True),
                "string_with_escapes": ('\\"foo\\"', True),
                "boolean_true": (True, True),
                "boolean_false": (False, True),
                "null_value": (None, True),
                "parenthesis": (D("15"), True),
                "identifier": (D("16"), True),
                "variable": ("redefined value", False),
                "chained_1": (D("9"), False),
                "chained_2": (D("9"), False),
                "chained_assign": (D("9"), True),
                "foo": (D("108"), False),
                "bar": (D("3"), False),
                "test_cond_a": (D("42"), False),
                "test_cond_b": (False, False),
            },
        )


if __name__ == "__main__":
    unittest.main()
