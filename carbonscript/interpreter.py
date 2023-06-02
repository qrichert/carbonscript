from decimal import Decimal

from .lexer import TokenType
from .parser import BinOp, Expr, Group, Literal, Stmt, Unary

LiteralValue = Decimal | str | bool | None


class Interpreter:
    def __init__(self) -> None:
        self.statements: list[Stmt] = []

    def interpret(self, statements: list[Stmt]) -> None:
        self.__init__()
        self.statements = statements

        stmt: Stmt
        for stmt in statements:
            self._interpret_expr(stmt.expression)

    def interpret_one(self, statement: Stmt) -> LiteralValue:
        return self._interpret_expr(statement.expression)

    def _interpret_expr(self, expr: Expr) -> LiteralValue:
        if isinstance(expr, BinOp):
            return self._interpret_binop(expr)
        if isinstance(expr, Unary):
            return self._interpret_unary_expr(expr)
        if isinstance(expr, Literal):
            return self._interpret_literal(expr)
        if isinstance(expr, Group):
            return self._interpret_group(expr)
        raise RuntimeError

    def _interpret_binop(self, binop: BinOp) -> LiteralValue:
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

    def _interpret_group(self, group: Group) -> LiteralValue:
        return self._interpret_expr(group.expr)

    def _interpret_unary_expr(self, unary: Unary) -> LiteralValue:
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
