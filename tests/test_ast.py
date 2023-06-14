"""Test AST module."""

import doctest
import unittest

import carbonscript.ast
from carbonscript.ast import (
    Assign,
    BinOp,
    BinOpRTL,
    Block,
    BreakStmt,
    ConstDecl,
    ContinueStmt,
    Expr,
    ExprStmt,
    Group,
    IfStmt,
    Literal,
    LogicOp,
    Unary,
    VarDecl,
    WhileStmt,
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

    def test_repr_logic_op(self) -> None:
        expr = LogicOp(
            Literal(TokenType.NUMBER, "42"),
            TokenType.AND,
            Literal(TokenType.NUMBER, "108"),
        )
        self.assertEqual(
            repr(expr),
            "LogicOp(Literal(NUMBER, '42'), AND, Literal(NUMBER, '108'))",
        )

    def test_str_logic_op(self) -> None:
        expr = LogicOp(
            Literal(TokenType.NUMBER, "42"),
            TokenType.AND,
            Literal(TokenType.NUMBER, "108"),
        )
        self.assertEqual(str(expr), repr(expr))

    def test_repr_bin_op(self) -> None:
        expr = BinOp(
            Literal(TokenType.NUMBER, "42"),
            TokenType.PLUS,
            Literal(TokenType.NUMBER, "108"),
        )
        self.assertEqual(
            repr(expr),
            "BinOp(Literal(NUMBER, '42'), PLUS, Literal(NUMBER, '108'))",
        )

    def test_str_bin_op(self) -> None:
        expr = BinOp(
            Literal(TokenType.NUMBER, "42"),
            TokenType.PLUS,
            Literal(TokenType.NUMBER, "108"),
        )
        self.assertEqual(str(expr), repr(expr))

    def test_repr_bin_op_rtl(self) -> None:
        expr = BinOpRTL(
            Literal(TokenType.NUMBER, "42"),
            TokenType.PLUS,
            Literal(TokenType.NUMBER, "108"),
        )
        self.assertEqual(
            repr(expr),
            "BinOpRTL(Literal(NUMBER, '42'), PLUS, Literal(NUMBER, '108'))",
        )

    def test_str_bin_op_rtl(self) -> None:
        expr = BinOpRTL(
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
            Literal(TokenType.LITKW, "true"),
            Block([ExprStmt(Literal(TokenType.LITKW, "true"))]),
            Block([ExprStmt(Literal(TokenType.LITKW, "false"))]),
        )
        self.assertEqual(
            repr(if_stmt),
            (
                "IfStmt(Cond(Literal(LITKW, 'true')), "
                "Then(Block(ExprStmt(Literal(LITKW, 'true')))), "
                "Else(Block(ExprStmt(Literal(LITKW, 'false')))))"
            ),
        )

    def test_repr_if_stmt_no_else(self) -> None:
        if_stmt = IfStmt(
            Literal(TokenType.LITKW, "true"),
            Block([ExprStmt(Literal(TokenType.LITKW, "true"))]),
            None,
        )
        self.assertEqual(
            repr(if_stmt),
            (
                "IfStmt(Cond(Literal(LITKW, 'true')), "
                "Then(Block(ExprStmt(Literal(LITKW, 'true')))))"
            ),
        )

    def test_str_if_stmt(self) -> None:
        if_stmt = IfStmt(
            Literal(TokenType.LITKW, "true"),
            Block([ExprStmt(Literal(TokenType.LITKW, "true"))]),
            Block([ExprStmt(Literal(TokenType.LITKW, "false"))]),
        )
        self.assertEqual(str(if_stmt), repr(if_stmt))

    def test_repr_while_stmt(self) -> None:
        while_stmt = WhileStmt(
            Literal(TokenType.LITKW, "true"),
            Block([ExprStmt(Literal(TokenType.LITKW, "true"))]),
        )
        self.assertEqual(
            repr(while_stmt),
            (
                "WhileStmt(Cond(Literal(LITKW, 'true')), "
                "Block(ExprStmt(Literal(LITKW, 'true'))))"
            ),
        )

    def test_str_while_stmt(self) -> None:
        while_stmt = WhileStmt(
            Literal(TokenType.LITKW, "true"),
            Block([ExprStmt(Literal(TokenType.LITKW, "true"))]),
        )
        self.assertEqual(str(while_stmt), repr(while_stmt))

    def test_repr_break_stmt(self) -> None:
        break_stmt = BreakStmt()
        self.assertEqual(repr(break_stmt), "BreakStmt")

    def test_str_break_stmt(self) -> None:
        break_stmt = BreakStmt()
        self.assertEqual(str(break_stmt), repr(break_stmt))

    def test_repr_continue_stmt(self) -> None:
        continue_stmt = ContinueStmt()
        self.assertEqual(repr(continue_stmt), "ContinueStmt")

    def test_str_continue_stmt(self) -> None:
        continue_stmt = ContinueStmt()
        self.assertEqual(str(continue_stmt), repr(continue_stmt))

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
