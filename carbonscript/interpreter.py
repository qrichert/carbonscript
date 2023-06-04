from __future__ import annotations

from decimal import Decimal
from typing import Generator

from .lexer import TokenType
from .parser import (
    Assign,
    BinOp,
    ConstDecl,
    Expr,
    ExprStmt,
    Group,
    Literal,
    Stmt,
    Unary,
    VarDecl,
)

LiteralValue = Decimal | str | bool | None


class Scope:
    def __init__(self, parent: Scope | None) -> None:
        self._values: dict[str, tuple[LiteralValue, bool]] = {}
        self.parent: Scope | None = parent

    def to_dict(self) -> dict:
        values: dict = {}
        return self._values

    def _has_value(self, identifier: str) -> bool:
        return identifier in self._values

    def _get_value(self, identifier: str) -> LiteralValue:
        return self._values[identifier][0]

    def _declare_value(self, identifier: str, value: LiteralValue, const: bool) -> None:
        self._values[identifier] = (value, const)

    def _set_value(self, identifier: str, value: LiteralValue) -> None:
        const: bool = False  # Cannot be constant if updated.
        self._values[identifier] = (value, const)

    def _is_value_const(self, identifier: str) -> bool:
        return self._values[identifier][1]


class Environment(Scope):
    def __init__(self) -> None:
        super().__init__(None)
        self._current_scope: Scope = self
        self.scope_id: int = 0

    def push_scope(self) -> None:
        new_scope: Scope = Scope(self._current_scope)
        self._current_scope = new_scope
        self.scope_id += 1

    def pop_scope(self) -> None:
        assert self._current_scope is not self, "Cannot pop global scope."
        self._current_scope = self._current_scope.parent
        self.scope_id -= 1

    def _iter_scopes(self) -> Generator[Scope, None, None]:
        scope: Scope = self._current_scope
        while scope.parent:
            yield scope
            scope = scope.parent
        yield scope  # Global scope doesn't have a parent.

    def get(self, identifier: str) -> LiteralValue:
        scope: Scope
        for scope in self._iter_scopes():
            try:
                return scope._get_value(identifier)
            except KeyError:
                continue
        # TODO: Factor this out to Error class.
        raise RuntimeError(f"undefined identifier {identifier!r}")

    def declare(
        self, identifier: str, value: LiteralValue, const: bool = False
    ) -> None:
        if self._current_scope._has_value(identifier):
            raise RuntimeError(f"redefinition of identifier {identifier!r}")
        self._current_scope._declare_value(identifier, value, const)

    def set(self, identifier: str, value: LiteralValue) -> None:
        scope: Scope
        # If value exists in current scope or parent scope, set it.
        for scope in self._iter_scopes():
            if scope._has_value(identifier):
                if scope._is_value_const(identifier):
                    raise RuntimeError(f"trying to update constant {identifier!r}")
                scope._set_value(identifier, value)
                return
        # TODO: Factor this out to Error class.
        raise RuntimeError(f"undefined identifier {identifier!r}")


class Interpreter:
    def __init__(self) -> None:
        self.statements: list[Stmt] = []
        self.env: Environment = Environment()

    def interpret(self, statements: list[Stmt]) -> None:
        self.__init__()
        self.statements = statements

        stmt: Stmt
        for stmt in statements:
            self._interpret_declaration(stmt)

    # TODO: Differentiate between None (null) (legitimate value)
    #  and "no value" (e.g., var i = 12  # not an expression, no value).
    #  => Make LiteralValue (maybe rename Value?) a real class, so to
    #     differentiate -> is LiteralValue ? yes or no
    #     since LiteralValue(None) is still truthy.
    def interpret_one(self, stmt: Stmt) -> LiteralValue | None:
        return self._interpret_declaration(stmt)

    def _interpret_declaration(self, stmt: Stmt) -> LiteralValue | None:
        if isinstance(stmt, ExprStmt):
            return self._interpret_expr_stmt(stmt)
        if isinstance(stmt, VarDecl):
            if isinstance(stmt, ConstDecl):
                return self._interpret_var_decl(stmt, const=True)
            return self._interpret_var_decl(stmt)
        assert False, "Unmatched statement type."

    def _interpret_expr_stmt(self, stmt: ExprStmt) -> LiteralValue:
        return self._interpret_expr(stmt.expression)

    def _interpret_var_decl(self, stmt: VarDecl, const: bool = False) -> None:
        identifier: str = stmt.lidentifier.value
        value: LiteralValue = self._interpret_expr(stmt.rexpr)
        self.env.declare(identifier, value, const)

    def _interpret_expr(self, expr: Expr) -> LiteralValue:
        if isinstance(expr, BinOp):
            return self._interpret_binop(expr)
        if isinstance(expr, Unary):
            return self._interpret_unary_expr(expr)
        if isinstance(expr, Literal):
            return self._interpret_literal(expr)
        if isinstance(expr, Group):
            return self._interpret_group(expr)
        if isinstance(expr, Assign):
            return self._interpret_assignment(expr)
        assert False, "Unmatched expression type."

    def _interpret_binop(self, binop: BinOp) -> LiteralValue:
        lval: LiteralValue = self._interpret_expr(binop.lexpr)
        rval: LiteralValue = self._interpret_expr(binop.rexpr)
        op: TokenType = binop.operator
        # TODO: implement correct behaviour depending on type.
        #  see with LiteralValue class and maybe subclasses.
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
        assert False, "Unmatched binary operator."

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
        assert False, "Unmatched unary operator."

    def _interpret_literal(self, literal: Literal) -> LiteralValue:
        if literal.literal == TokenType.IDENTIFIER:
            return self.env.get(literal.value)
        return literal.value

    def _interpret_group(self, group: Group) -> LiteralValue:
        return self._interpret_expr(group.expr)

    def _interpret_assignment(self, assignment: Assign) -> LiteralValue:
        identifier: str = assignment.lidentifier.value
        value: LiteralValue = self._interpret_expr(assignment.rexpr)
        self.env.set(identifier, value)
        return value
