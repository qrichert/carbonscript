"""Test interpreter module."""

import doctest
import textwrap
import unittest
from decimal import Decimal as D

import carbonscript.interpreter
from carbonscript.ast import BinOp, ExprStmt, Literal, Stmt
from carbonscript.environment import Environment
from carbonscript.error import ParseError
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

    def test_var_operation_declaration(self) -> None:
        env = interpret_script_and_return_env("var i = 12 * 9")
        self.assertEqual(env.get("i"), D("108"))

    def test_var_chained_declaration(self) -> None:
        env = interpret_script_and_return_env(
            "var a = 1\nvar b = 2\nconst c = a = b = 108"
        )
        self.assertEqual(env.get("a"), D("108"))
        self.assertEqual(env.get("b"), D("108"))
        self.assertEqual(env.get("c"), D("108"))

    def test_const_declaration(self) -> None:
        env = interpret_script_and_return_env("const i = 42")
        self.assertEqual(env.get("i"), D("42"))


class TestBasicExpressions(unittest.TestCase):
    def test_assignment_var(self) -> None:
        env = interpret_script_and_return_env("var i = 42\ni = 1337")
        self.assertEqual(env.get("i"), D("1337"))

    def test_assignment_var_chained(self) -> None:
        env = interpret_script_and_return_env("var i = null\nvar j = null\ni = j = 3")
        self.assertEqual(env.get("i"), D("3"))
        self.assertEqual(env.get("j"), D("3"))

    def test_assignment_var_in_place_operation(self) -> None:
        env = interpret_script_and_return_env("var i = 0\ni += 2*2")
        self.assertEqual(env.get("i"), D("4"))

    def test_assignment_const(self) -> None:
        with self.assertRaises(RuntimeError):
            interpret_script("const i = 42\ni = 1337")

    def test_assignment_const_in_place_operation(self) -> None:
        with self.assertRaises(RuntimeError):
            interpret_script("const i = 42\ni += 1337")

    def test_assignment_in_place_with_assignment(self) -> None:
        env = interpret_script_and_return_env("var i = 1\nvar j = 0\ni += j = 4")
        self.assertEqual(env.get("i"), D("5"))
        self.assertEqual(env.get("j"), D("4"))

    def test_assignment_in_place_operation_chained(self) -> None:
        env = interpret_script_and_return_env("var a = 1\nvar b = 3\na += b += b += 1")
        self.assertEqual(env.get("a"), D("9"))
        self.assertEqual(env.get("b"), D("8"))

    def test_assignment_in_place_operation_chained_with_assignment_short(self) -> None:
        env = interpret_script_and_return_env("var foo = 42\nfoo += (foo = 3)")
        self.assertEqual(env.get("foo"), D("6"))

    def test_assignment_in_place_operation_chained_with_assignment_long(self) -> None:
        # RTL: 1 + (8 + (4 + (4))) (a = 17, b = 16)
        # LTR: 1 + (3 + 3 + 4) (a = 11, b = 10)
        env = interpret_script_and_return_env(
            "var a = 1\nvar b = 3\na += b += b += b = 4"
        )
        self.assertEqual(env.get("a"), D("17"))
        self.assertEqual(env.get("b"), D("16"))

    def test_assignment_var_operation_with_var(self) -> None:
        env = interpret_script_and_return_env(
            "const a = 3\nconst b = 5\nconst c = a * b"
        )
        self.assertEqual(env.get("c"), D("15"))

    def test_assignment_list(self) -> None:
        env = interpret_script_and_return_env("const foo = [0]\nfoo[0] = 36")
        self.assertEqual(env.get("foo"), [D("36")])

    def test_assignment_list_multiple(self) -> None:
        env = interpret_script_and_return_env("const list = [0, 1, 2]\nlist[1] = 12")
        self.assertEqual(env.get("list"), [D("0"), D("12"), D("2")])

    def test_assignment_list_assign_list(self) -> None:
        env = interpret_script_and_return_env("const foo = [0]\nfoo[0] = [1, 2, 3]")
        self.assertEqual(
            env.get("foo"),
            [[D("1"), D("2"), D("3")]],
        )

    def test_assignment_list_assign_list_expansion(self) -> None:
        env = interpret_script_and_return_env(
            "const foo = [0]\nfoo[0] = [1 + 2 + 3]",
        )
        self.assertEqual(env.get("foo"), [[D("6")]])

    def test_assignment_to_list_with_expression(self) -> None:
        env = interpret_script_and_return_env(
            "const list = [0, 1, 2]\nlist[1+1] = 12 * 2"
        )
        self.assertEqual(env.get("list"), [D("0"), D("1"), D("24")])

    def test_assignment_to_list_index_out_of_range(self) -> None:
        with self.assertRaises(RuntimeError):
            interpret_script("const list = [0, 1, 2]\nlist[10] = 0")

    def test_list_index_empty(self) -> None:
        # This is a parse error, it should never get to the interpreter.
        with self.assertRaises(ParseError) as ctx:
            interpret_script("foo[]")
        self.assertEqual(ctx.exception.token, Token(TokenType.RSQBRACKET, "]"))

    def test_logic_or(self) -> None:
        self.assertEqual(interpret_as_expression("true or true"), True)
        self.assertEqual(interpret_as_expression("false or true"), True)
        self.assertEqual(interpret_as_expression("true or false"), True)
        self.assertEqual(interpret_as_expression("false or false"), False)

    def test_logic_and(self) -> None:
        self.assertEqual(interpret_as_expression("true and true"), True)
        self.assertEqual(interpret_as_expression("false and true"), False)
        self.assertEqual(interpret_as_expression("true and false"), False)
        self.assertEqual(interpret_as_expression("false and false"), False)

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

    def test_in_place_term_addition(self) -> None:
        env = interpret_script_and_return_env("var foo = 3\nfoo += 2")
        self.assertEqual(env.get("foo"), D("5"))

    def test_in_place_term_subtraction(self) -> None:
        env = interpret_script_and_return_env("var foo = 3\nfoo -= 2")
        self.assertEqual(env.get("foo"), D("1"))

    def test_in_place_factor_multiplication(self) -> None:
        env = interpret_script_and_return_env("var foo = 3\nfoo *= 2")
        self.assertEqual(env.get("foo"), D("6"))

    def test_in_place_factor_division(self) -> None:
        env = interpret_script_and_return_env("var foo = 3\nfoo /= 2")
        self.assertEqual(env.get("foo"), D("1.5"))

    def test_in_place_factor_integer_division(self) -> None:
        env = interpret_script_and_return_env("var foo = 3\nfoo //= 2")
        self.assertEqual(env.get("foo"), D("1"))

    def test_in_place_factor_modulo(self) -> None:
        env = interpret_script_and_return_env("var foo = 3\nfoo %= 2")
        self.assertEqual(env.get("foo"), D("1"))

    def test_in_place_power(self) -> None:
        env = interpret_script_and_return_env("var foo = 3\nfoo **= 2")
        self.assertEqual(env.get("foo"), D("9"))

    def test_unary_plus(self) -> None:
        self.assertEqual(interpret_as_expression("+42"), D("42"))

    def test_unary_minus(self) -> None:
        self.assertEqual(interpret_as_expression("-42"), D("-42"))

    # TODO: What to do, maintain behaviour !0 -> True !(!=0) -> False ?
    def test_unary_bang(self) -> None:
        self.assertEqual(interpret_as_expression("!42"), False)

    def test_list_declaration(self) -> None:
        env = interpret_script_and_return_env("const foo = [1+2, 3]")
        self.assertEqual(env.get("foo"), [D("3"), D("3")])

    def test_list_declaration_empty(self) -> None:
        env = interpret_script_and_return_env("const foo = []")
        self.assertEqual(env.get("foo"), [])

    def test_list_expansion(self) -> None:
        env = interpret_script_and_return_env(
            "var bar = 5\nconst foo = [1+2, bar]\nbar = 0"
        )
        self.assertEqual(env.get("foo"), [D("3"), D("5")])
        self.assertEqual(env.get("bar"), D("0"))

    def test_list_index(self) -> None:
        env = interpret_script_and_return_env(
            "const foo = [1, 2, 3, 4]\nconst value = foo[3]"
        )
        self.assertEqual(env.get("value"), D("4"))

    def test_list_index_with_expression(self) -> None:
        env = interpret_script_and_return_env(
            "const foo = [1, 2, 3, 4]\nconst value = foo[2+1]"
        )
        self.assertEqual(env.get("value"), D("4"))

    def test_list_index_chained(self) -> None:
        env = interpret_script_and_return_env(
            "const foo = [1, 2, 3, [4, 5, [6, 7]]]\nconst value = foo[3][2][1]"
        )
        self.assertEqual(env.get("value"), D("7"))

    def test_list_index_index_error_out_of_range(self) -> None:
        with self.assertRaises(RuntimeError):
            interpret_script("const foo = [0]\nconst value = foo[1]")

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

    def test_logic_operator_precedence(self) -> None:
        self.assertEqual(interpret_as_expression("true and true or true"), True)
        self.assertEqual(interpret_as_expression("true and true or false"), True)
        self.assertEqual(interpret_as_expression("true and false or true"), True)
        self.assertEqual(interpret_as_expression("true and false or false"), False)
        self.assertEqual(interpret_as_expression("false and true or true"), True)
        self.assertEqual(interpret_as_expression("false and true or false"), False)
        self.assertEqual(interpret_as_expression("false and false or true"), True)
        self.assertEqual(interpret_as_expression("false and false or false"), False)

    def test_order_of_side_effects_in_place_assignment_right(self) -> None:
        # LTR: 1 + 3 = 4
        # RTL: 3 + 3 = 6 (correct)
        env = interpret_script_and_return_env("var a = 1\na += (a = 3)")
        self.assertEqual(env.get("a"), D("6"))

    def test_order_of_side_effects_equality_left(self) -> None:
        # LTR: 3 == 3 = True (correct)
        # RTL: 3 == 0 = False
        env = interpret_script_and_return_env("var a = 0\nconst b = (a = 3) == a")
        self.assertEqual(env.get("b"), True)

    def test_order_of_side_effects_comparison_left(self) -> None:
        # LTR: 3 <= 3 = True (correct)
        # RTL: 3 <= 0 = False
        env = interpret_script_and_return_env("var a = 0\nconst b = (a = 3) <= a")
        self.assertEqual(env.get("b"), True)

    def test_order_of_side_effects_term_left(self) -> None:
        # LTR: 1 + 3 = 4 (correct)
        # RTL: 3 + 3 = 6
        env = interpret_script_and_return_env("var a = 1\na = a + (a = 3)")
        self.assertEqual(env.get("a"), D("4"))

    def test_order_of_side_effects_factor_left(self) -> None:
        # LTR: 1 * 3 = 3 (correct)
        # RTL: 3 * 3 = 9
        env = interpret_script_and_return_env("var a = 1\na = a * (a = 3)")
        self.assertEqual(env.get("a"), D("3"))

    def test_order_of_side_effects_power_right(self) -> None:
        # LTR: 3**2 = 9
        # RTL: 6**2 = 36 (correct)
        env = interpret_script_and_return_env("var a = 1\na = (3 * a) ** (a = 2)")
        self.assertEqual(env.get("a"), D("36"))

    def test_logic_or_side_effects(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 0
                const bar = false or (foo = 42)
                """
            ),
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("42"), False),
                "bar": (D("42"), True),
            },
        )

    def test_logic_or_no_side_effects(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 0
                const bar = true or (foo = 42)
                """
            ),
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("0"), False),
                "bar": (True, True),
            },
        )

    def test_logic_and_side_effects(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 0
                const bar = true and (foo = 42)
                """
            ),
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("42"), False),
                "bar": (D("42"), True),
            },
        )

    def test_logic_and_no_side_effects(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 0
                const bar = false and (foo = 42)
                """
            ),
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("0"), False),
                "bar": (False, True),
            },
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

    def test_while(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 42
                while (foo > 30)
                    foo = foo - 1
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("30"), False),
            },
        )

    def test_while_break(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 42
                while (true)
                    foo = foo - 1
                    if (foo == 30)
                        break
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("30"), False),
            },
        )

    def test_while_continue(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var foo = 42
                while (true)
                    foo = foo - 1
                    if (foo > 30)
                        continue
                    break
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "foo": (D("30"), False),
            },
        )

    def test_nested_while_break_continue(self) -> None:
        env = interpret_script_and_return_env(
            dedent(
                """
                var while_counter = 0
                while (true)
                    while (true)
                        while_counter = while_counter + 1
                        if (while_counter == 2)
                            while_counter = 3
                            continue
                        if (while_counter >= 3)
                            break
                    while_counter = -while_counter
                    break
                """
            )
        )
        self.assertDictEqual(
            env.to_dict(),
            {
                "while_counter": (D("-4"), False),
            },
        )


class TestTheBigOne(unittest.TestCase):
    def test_the_big_one(self) -> None:
        with open(THE_BIG_ONE) as f:
            script: str = f.read()
        env = interpret_script_and_return_env(script)
        self.maxDiff = None
        # TODO: lists should be fixed when encountered
        # Python 3.11.3 (main, Apr  5 2023, 14:14:40) [GCC 7.5.0] on linux
        # Type "help", "copyright", "credits" or "license" for more information.
        # >>> foo = 3
        # >>> l = [1 + foo]
        # >>> l[0]
        # 4
        # >>> foo = 1
        # >>> l[0]
        # 4
        # >>> l
        # [4]
        # >>>
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
                "list_mutable": (D("0"), False),
                "list": ([[D("36")], [[D("5")], D("2"), D("36")]], True),
                "list_index": (D("5"), True),
                "variable": ("redefined value", False),
                "chained_1": (D("9"), False),
                "chained_2": (D("9"), False),
                "chained_assign": (D("9"), True),
                "in_place_a": (D("17"), False),
                "in_place_b": (D("16"), False),
                "logic_or": (True, True),
                "logic_and": (False, True),
                "no_side_effects_if_short_circuit": (False, True),
                "foo": (D("108"), False),
                "bar": (D("3"), False),
                "test_cond_a": (D("42"), False),
                "test_cond_b": (False, False),
                "while_counter": (D("-4"), False),
            },
        )


if __name__ == "__main__":
    unittest.main()
