"""Interpreter module."""


from decimal import Decimal

from .lexer import TokenType
from .parser import BinaryExpr, Expr, GroupExpr, Literal, UnaryExpr

LiteralValue = Decimal | str | bool | None


class Interpreter:
    def __init__(self) -> None:
        self.ast: list[Expr] = []

    def interpret(self, ast: list[Expr]) -> LiteralValue:
        self.__init__()
        self.ast = ast

        expr: Expr
        for expr in ast:
            # TODO: rename BinaryExpr BinOp -> and put it in scoped file "expr"
            return self._interpret_expr(expr)

    def _interpret_expr(self, expr: Expr) -> LiteralValue:
        if isinstance(expr, BinaryExpr):
            return self._interpret_binop(expr)
        if isinstance(expr, GroupExpr):
            return self._interpret_group(expr)
        if isinstance(expr, Literal):
            return self._interpret_literal(expr)
        if isinstance(expr, UnaryExpr):
            return self._interpret_unary_expr(expr)
        raise RuntimeError

    def _interpret_binop(self, binop: BinaryExpr) -> LiteralValue:
        lval: LiteralValue = self._interpret_expr(binop.lexpr)
        rval: LiteralValue = self._interpret_expr(binop.rexpr)
        op: TokenType = binop.operator
        match op:
            case TokenType.DBLEQUAL:
                return lval == rval
            case TokenType.BANGEQUAL:
                return lval != rval
            case TokenType.GT:
                return lval > rval
            case TokenType.GTE:
                return lval >= rval
            case TokenType.LT:
                return lval < rval
            case TokenType.LTE:
                return lval <= rval
            case TokenType.PLUS:
                return lval + rval
            case TokenType.MINUS:
                return lval - rval
            case TokenType.STAR:
                return lval * rval
            case TokenType.SLASH:
                return lval / rval
            case TokenType.DBLSLASH:
                return lval // rval
            case TokenType.PERCENT:
                return lval % rval
            case TokenType.DBLSTAR:
                return lval**rval
        raise RuntimeError

    def _interpret_group(self, group: GroupExpr) -> LiteralValue:
        return self._interpret_expr(group.expr)

    def _interpret_unary_expr(self, unary: UnaryExpr) -> LiteralValue:
        value: LiteralValue = self._interpret_expr(unary.rexpr)
        op: TokenType = unary.operator
        match op:
            case TokenType.PLUS:
                return value
            case TokenType.MINUS:
                return -value
            case TokenType.BANG:
                return not value
        raise RuntimeError

    def _interpret_literal(self, literal: Literal) -> LiteralValue:
        return literal.value
