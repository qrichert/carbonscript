"""Test AST module."""

import doctest
import unittest

import carbonscript.ast
from carbonscript.ast import (
    Assign,
    BinOp,
    Block,
    ConstDecl,
    Expr,
    ExprStmt,
    Group,
    IfStmt,
    Literal,
    Unary,
    VarDecl,
)
from carbonscript.tokens import TokenType


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.ast))
    return tests


class TestASTNodes(unittest.TestCase):
    def test_repr_expr(self) -> None:
        expr = Expr()
        self.assertEqual(repr(expr), "Expr()")

    def test_str_expr(self) -> None:
        expr = Expr()
        self.assertEqual(str(expr), repr(expr))

    def test_repr_block(self) -> None:
        block = Block(
            [
                ExprStmt(Literal(TokenType.STRING, "123")),
                ExprStmt(Literal(TokenType.STRING, "abc")),
            ]
        )
        self.assertEqual(
            repr(block),
            "Block(ExprStmt(Literal(STRING, '123')), ExprStmt(Literal(STRING, 'abc')))",
        )

    def test_str_block(self) -> None:
        block = Block(
            [
                ExprStmt(Literal(TokenType.STRING, "123")),
                ExprStmt(Literal(TokenType.STRING, "abc")),
            ]
        )
        self.assertEqual(str(block), repr(block))

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

    def test_repr_if_stmt(self) -> None:
        if_stmt = IfStmt(
            Literal(TokenType.LITKEYWORD, "true"),
            Block([ExprStmt(Literal(TokenType.LITKEYWORD, "true"))]),
            Block([ExprStmt(Literal(TokenType.LITKEYWORD, "false"))]),
        )
        self.assertEqual(
            repr(if_stmt),
            (
                "IfStmt(Cond(Literal(LITKEYWORD, 'true')), "
                "Then(Block(ExprStmt(Literal(LITKEYWORD, 'true')))), "
                "Else(Block(ExprStmt(Literal(LITKEYWORD, 'false')))))"
            ),
        )

    def test_repr_if_stmt_no_else(self) -> None:
        if_stmt = IfStmt(
            Literal(TokenType.LITKEYWORD, "true"),
            Block([ExprStmt(Literal(TokenType.LITKEYWORD, "true"))]),
            None,
        )
        self.assertEqual(
            repr(if_stmt),
            (
                "IfStmt(Cond(Literal(LITKEYWORD, 'true')), "
                "Then(Block(ExprStmt(Literal(LITKEYWORD, 'true')))))"
            ),
        )

    def test_str_if_stmt(self) -> None:
        if_stmt = IfStmt(
            Literal(TokenType.LITKEYWORD, "true"),
            Block([ExprStmt(Literal(TokenType.LITKEYWORD, "true"))]),
            Block([ExprStmt(Literal(TokenType.LITKEYWORD, "false"))]),
        )
        self.assertEqual(str(if_stmt), repr(if_stmt))

    def test_repr_var_decl(self) -> None:
        decl = VarDecl(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(
            repr(decl),
            "VarDecl(Literal(IDENTIFIER, 'foo'), Literal(NUMBER, '42'))",
        )

    def test_str_var_decl(self) -> None:
        decl = VarDecl(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(str(decl), repr(decl))

    def test_repr_const_decl(self) -> None:
        decl = ConstDecl(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(
            repr(decl),
            "ConstDecl(Literal(IDENTIFIER, 'foo'), Literal(NUMBER, '42'))",
        )

    def test_str_const_decl(self) -> None:
        decl = ConstDecl(
            Literal(TokenType.IDENTIFIER, "foo"),
            Literal(TokenType.NUMBER, "42"),
        )
        self.assertEqual(str(decl), repr(decl))

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


if __name__ == "__main__":
    unittest.main()
