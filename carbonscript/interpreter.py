from .ast import (
    Assign,
    BinOp,
    Block,
    ConstDecl,
    Expr,
    ExprStmt,
    Group,
    IfStmt,
    Literal,
    Stmt,
    Unary,
    VarDecl,
)
from .environment import Environment
from .tokens import TokenType
from .values import LiteralValue


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
        if isinstance(stmt, IfStmt):
            return self._interpret_if_stmt(stmt)
        if isinstance(stmt, Block):
            return self._interpret_block(stmt)
        if isinstance(stmt, VarDecl):
            if isinstance(stmt, ConstDecl):
                return self._interpret_var_decl(stmt, const=True)
            return self._interpret_var_decl(stmt)
        assert False, f"Unmatched statement type {stmt.__class__.__name__!r}."

    def _interpret_expr_stmt(self, stmt: ExprStmt) -> LiteralValue:
        return self._interpret_expr(stmt.expression)

    def _interpret_if_stmt(self, stmt: IfStmt) -> None:
        # TODO: is_truthy, or bool on class
        condition: bool = bool(self._interpret_expr(stmt.condition))
        if condition:
            return self._interpret_block(stmt.then)
        elif isinstance(stmt.else_, IfStmt):
            return self._interpret_if_stmt(stmt.else_)
        elif isinstance(stmt.else_, Block):
            return self._interpret_block(stmt.else_)
        return None

    def _interpret_block(self, block: Block) -> None:
        self.env.push_scope()
        stmt: Stmt
        for stmt in block.statements:
            self._interpret_declaration(stmt)
        self.env.pop_scope()

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
        assert False, f"Unmatched expression type {expr.__class__.__name__!r}."

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
        assert False, f"Unmatched binary operator {op.__class__.__name__!r}."

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
        assert False, f"Unmatched unary operator {op.__class__.__name__!r}."

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
