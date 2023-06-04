"""Test parsing module."""

import doctest
import unittest
from decimal import Decimal as D

import carbonscript.parser
from carbonscript.lexer import Lexer, Token, TokenType
from carbonscript.parser import (
    Assign,
    BinOp,
    ConstDecl,
    Expr,
    ExprStmt,
    Group,
    Literal,
    ParseError,
    Parser,
    Stmt,
    Unary,
    VarDecl,
)


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.parser))
    return tests


def parse_expression(expression: str) -> Expr:
    tokens: list[Token] = Lexer().lex(expression)
    statements: list[Stmt] = Parser().parse(tokens)
    return statements[0].expression


def parse_script(script: str) -> list[Stmt]:
    tokens: list[Token] = Lexer().lex(script)
    return Parser().parse(tokens)


class TestExpr(unittest.TestCase):
    def test_repr_expr(self) -> None:
        expr = Expr()
        self.assertEqual(repr(expr), "Expr()")

    def test_str_expr(self) -> None:
        expr = Expr()
        self.assertEqual(str(expr), repr(expr))

    def test_repr_binop(self) -> None:
        expr = BinOp(
            Literal(TokenType.NUMBER, "42"),
            TokenType.PLUS,
            Literal(TokenType.NUMBER, "108"),
        )
        self.assertEqual(
            repr(expr),
            "BinOp(Literal(NUMBER, '42'), PLUS, Literal(NUMBER, '108'))",
        )

    def test_str_binop(self) -> None:
        expr = BinOp(
            Literal(TokenType.NUMBER, "42"),
            TokenType.PLUS,
            Literal(TokenType.NUMBER, "108"),
        )
        self.assertEqual(str(expr), repr(expr))

    def test_repr_unary(self) -> None:
        expr = Unary(TokenType.MINUS, Literal(TokenType.NUMBER, "42"))
        self.assertEqual(repr(expr), "Unary(MINUS, Literal(NUMBER, '42'))")

    def test_str_unary(self) -> None:
        expr = Unary(TokenType.MINUS, Literal(TokenType.NUMBER, "42"))
        self.assertEqual(str(expr), repr(expr))

    def test_repr_literal(self) -> None:
        expr = Literal(TokenType.NUMBER, "42")
        self.assertEqual(repr(expr), "Literal(NUMBER, '42')")

    def test_str_literal(self) -> None:
        expr = Literal(TokenType.NUMBER, "42")
        self.assertEqual(str(expr), repr(expr))

    def test_repr_group(self) -> None:
        expr = Group(Literal(TokenType.NUMBER, "42"))
        self.assertEqual(repr(expr), "Group(Literal(NUMBER, '42'))")

    def test_str_group(self) -> None:
        expr = Group(Literal(TokenType.NUMBER, "42"))
        self.assertEqual(str(expr), repr(expr))

    def test_repr_declare_var(self) -> None:
        expr = VarDecl(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(
            repr(expr),
            "VarDecl(Literal(IDENTIFIER, 'foo'), Literal(NUMBER, '42'))",
        )

    def test_str_declare_var(self) -> None:
        expr = VarDecl(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(str(expr), repr(expr))

    def test_repr_declare_const(self) -> None:
        expr = ConstDecl(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(
            repr(expr),
            "ConstDecl(Literal(IDENTIFIER, 'foo'), Literal(NUMBER, '42'))",
        )

    def test_str_declare_const(self) -> None:
        expr = ConstDecl(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(str(expr), repr(expr))

    def test_repr_assign(self) -> None:
        expr = Assign(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(
            repr(expr),
            "Assign(Literal(IDENTIFIER, 'foo'), Literal(NUMBER, '42'))",
        )

    def test_str_assign(self) -> None:
        expr = Assign(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(str(expr), repr(expr))


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


class TestStatements(unittest.TestCase):
    def test_empty(self) -> None:
        self.assertListEqual(
            parse_script(""),
            [],
        )

    def test_multiple_statements(self) -> None:
        self.assertListEqual(
            parse_script('"hello, world\n" \n "123"\n"abc"'),
            [
                ExprStmt(Literal(TokenType.STRING, "hello, world\n")),
                ExprStmt(Literal(TokenType.STRING, "123")),
                ExprStmt(Literal(TokenType.STRING, "abc")),
            ],
        )

    def test_multiple_statements_on_single_line(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1+2 3+4")
        self.assertEqual(ctx.exception.token, Token(TokenType.NUMBER, "3"))

    def test_empty_lines(self) -> None:
        self.assertListEqual(
            parse_script('\n\n\n"hello, world\n" \n "123"\n"abc"\n\n\n'),
            [
                ExprStmt(Literal(TokenType.STRING, "hello, world\n")),
                ExprStmt(Literal(TokenType.STRING, "123")),
                ExprStmt(Literal(TokenType.STRING, "abc")),
            ],
        )

    def test_newline_at_end_of_file(self) -> None:
        self.assertListEqual(
            parse_script('"newline ->"\n'),
            [
                ExprStmt(Literal(TokenType.STRING, "newline ->")),
            ],
        )

    def test_newline_at_end_of_file_no_newline(self) -> None:
        self.assertListEqual(
            parse_script('"no newline ->"'),
            [
                ExprStmt(Literal(TokenType.STRING, "no newline ->")),
            ],
        )

    def test_declaration_var(self) -> None:
        self.assertListEqual(
            parse_script("var a = 12 * 9"),
            [
                VarDecl(
                    Literal(TokenType.IDENTIFIER, "a"),
                    BinOp(
                        Literal(TokenType.NUMBER, D("12")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("9")),
                    ),
                )
            ],
        )

    def test_declaration_var_without_value(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("var foo"),
        self.assertEqual(ctx.exception.token, Token(TokenType.EOF))

    def test_declaration_var_assigning_to_constant(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("var 2 = 3"),
        self.assertEqual(ctx.exception.token, Token(TokenType.NUMBER, "2"))

    def test_declaration_var_assigning_to_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("var a + b = c"),
        self.assertEqual(ctx.exception.token, Token(TokenType.PLUS, "+"))

    def test_declaration_const(self) -> None:
        self.assertListEqual(
            parse_script("const a = 12 * 9"),
            [
                ConstDecl(
                    Literal(TokenType.IDENTIFIER, "a"),
                    BinOp(
                        Literal(TokenType.NUMBER, D("12")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("9")),
                    ),
                )
            ],
        )

    def test_declaration_const_without_value(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const foo"),
        self.assertEqual(ctx.exception.token, Token(TokenType.EOF))

    def test_declaration_const_assigning_to_constant(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const 2 = 3"),
        self.assertEqual(ctx.exception.token, Token(TokenType.NUMBER, "2"))

    def test_declaration_const_assigning_to_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const a + b = c"),
        self.assertEqual(ctx.exception.token, Token(TokenType.PLUS, "+"))

    def test_declaration_multiple_declarations_on_single_line(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const a = 2+2 var b = 3+3")
            self.assertEqual(ctx.exception.token, Token(TokenType.DECLKEYWORD, "var"))

    def test_declaration_assigning_multiple_expressions(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const a = 2+2 (3+3)")
            self.assertEqual(ctx.exception.token, Token(TokenType.LPAREN, "("))


class TestLiterals(unittest.TestCase):
    def test_number_integer(self) -> None:
        self.assertEqual(
            parse_expression("3"),
            Literal(TokenType.NUMBER, D("3")),
        )

    def test_number_decimal(self) -> None:
        self.assertEqual(
            parse_expression("1.618"),
            Literal(TokenType.NUMBER, D("1.618")),
        )

    def test_string(self) -> None:
        self.assertEqual(
            parse_expression('"hello, world\n"'),
            Literal(TokenType.STRING, "hello, world\n"),
        )

    def test_string_empty(self) -> None:
        self.assertEqual(
            parse_expression('""'),
            Literal(TokenType.STRING, ""),
        )

    def test_string_unterminated(self) -> None:
        with self.assertRaises(ParseError):
            parse_expression('"foo')

    def test_string_unterminated_empty(self) -> None:
        with self.assertRaises(ParseError):
            parse_expression('"')

    def test_literal_keyword_true(self) -> None:
        self.assertEqual(
            parse_expression("true"),
            Literal(TokenType.LITKEYWORD, True),
        )

    def test_literal_keyword_false(self) -> None:
        self.assertEqual(
            parse_expression("false"),
            Literal(TokenType.LITKEYWORD, False),
        )

    def test_literal_keyword_null(self) -> None:
        self.assertEqual(
            parse_expression("null"),
            Literal(TokenType.LITKEYWORD, None),
        )


class TestExpressions(unittest.TestCase):
    def test_with_whitespace(self) -> None:
        self.assertEqual(
            parse_expression("3   *   6   *   9 "),
            BinOp(
                BinOp(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("6")),
                ),
                TokenType.STAR,
                Literal(TokenType.NUMBER, D("9")),
            ),
        )

    def test_invalid_operator(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1°3")
        self.assertEqual(ctx.exception.token, Token(TokenType.UNKNOWN, "°"))

    def test_assignment(self) -> None:
        self.assertEqual(
            parse_expression("a=12*9"),
            Assign(
                Literal(TokenType.IDENTIFIER, "a"),
                BinOp(
                    Literal(TokenType.NUMBER, D("12")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("9")),
                ),
            ),
        )

    def test_assignment_chained(self) -> None:
        self.assertEqual(
            parse_expression("a=b=3"),
            Assign(
                Literal(TokenType.IDENTIFIER, "a"),
                Assign(
                    Literal(TokenType.IDENTIFIER, "b"),
                    Literal(TokenType.NUMBER, D("3")),
                ),
            ),
        )

    def test_assignment_assigning_to_constant(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("2=3"),
        self.assertEqual(ctx.exception.token, Token(TokenType.EQUAL, "="))

    def test_assignment_assigning_to_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("a+b=c"),
        self.assertEqual(ctx.exception.token, Token(TokenType.EQUAL, "="))

    def test_assignment_multiple_assignments_on_single_line(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("a = 2+2 b = 3+3")
        self.assertEqual(ctx.exception.token, Token(TokenType.IDENTIFIER, "b"))

    def test_assignment_assigning_multiple_expressions(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("a = 2+2 (3+3)")
        self.assertEqual(ctx.exception.token, Token(TokenType.LPAREN, "("))

    def test_equality_equal(self) -> None:
        self.assertEqual(
            parse_expression("1==3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.DBLEQUAL,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_equality_not_equal(self) -> None:
        self.assertEqual(
            parse_expression("1!=3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.BANGEQUAL,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_comparison_greater_than(self) -> None:
        self.assertEqual(
            parse_expression("1>3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.GT,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_comparison_greater_than_or_equal(self) -> None:
        self.assertEqual(
            parse_expression("1>=3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.GTE,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_comparison_lesser_than(self) -> None:
        self.assertEqual(
            parse_expression("1<3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.LT,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_comparison_lesser_than_or_equal(self) -> None:
        self.assertEqual(
            parse_expression("1<=3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.LTE,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_term_addition(self) -> None:
        self.assertEqual(
            parse_expression("3+1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_term_subtraction(self) -> None:
        self.assertEqual(
            parse_expression("3-1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.MINUS,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_factor_multiplication(self) -> None:
        self.assertEqual(
            parse_expression("3*1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.STAR,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_factor_division(self) -> None:
        self.assertEqual(
            parse_expression("3/1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.SLASH,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_factor_integer_division(self) -> None:
        self.assertEqual(
            parse_expression("3//1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.DBLSLASH,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_factor_modulo(self) -> None:
        self.assertEqual(
            parse_expression("3%1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.PERCENT,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_power(self) -> None:
        self.assertEqual(
            parse_expression("3**1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.DBLSTAR,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_unary_plus(self) -> None:
        self.assertEqual(
            parse_expression("+42"),
            Unary(
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("42")),
            ),
        )

    def test_unary_minus(self) -> None:
        self.assertEqual(
            parse_expression("-42"),
            Unary(
                TokenType.MINUS,
                Literal(TokenType.NUMBER, D("42")),
            ),
        )

    def test_unary_bang(self) -> None:
        self.assertEqual(
            parse_expression("!42"),
            Unary(
                TokenType.BANG,
                Literal(TokenType.NUMBER, D("42")),
            ),
        )

    def test_parenthesis(self) -> None:
        self.assertEqual(
            parse_expression("(7+108)"),
            Group(
                BinOp(
                    Literal(TokenType.NUMBER, D("7")),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("108")),
                )
            ),
        )

    def test_precedence(self) -> None:
        self.assertEqual(
            parse_expression("7+108*9"),
            BinOp(
                Literal(TokenType.NUMBER, D("7")),
                TokenType.PLUS,
                BinOp(
                    Literal(TokenType.NUMBER, D("108")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("9")),
                ),
            ),
        )

    def test_term_associativity(self) -> None:
        self.assertEqual(
            parse_expression("7+9+3"),
            BinOp(
                BinOp(
                    Literal(TokenType.NUMBER, D("7")),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("9")),
                ),
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_factor_associativity(self) -> None:
        self.assertEqual(
            parse_expression("7*9*3"),
            BinOp(
                BinOp(
                    Literal(TokenType.NUMBER, D("7")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("9")),
                ),
                TokenType.STAR,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_power_associativity(self) -> None:
        self.assertEqual(
            parse_expression("7**9**3"),
            BinOp(
                Literal(TokenType.NUMBER, D("7")),
                TokenType.DBLSTAR,
                BinOp(
                    Literal(TokenType.NUMBER, D("9")),
                    TokenType.DBLSTAR,
                    Literal(TokenType.NUMBER, D("3")),
                ),
            ),
        )

    def test_unary_associativity_even(self) -> None:
        self.assertEqual(
            parse_expression("--3"),
            Unary(
                TokenType.MINUS,
                Unary(
                    TokenType.MINUS,
                    Literal(TokenType.NUMBER, D("3")),
                ),
            ),
        )

    def test_unary_associativity_even_odd(self) -> None:
        self.assertEqual(
            parse_expression("---3"),
            Unary(
                TokenType.MINUS,
                Unary(
                    TokenType.MINUS,
                    Unary(
                        TokenType.MINUS,
                        Literal(TokenType.NUMBER, D("3")),
                    ),
                ),
            ),
        )


if __name__ == "__main__":
    unittest.main()
