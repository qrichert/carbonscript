from decimal import Decimal

from .ast import (
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
    ListIndex,
    Literal,
    LogicOp,
    Stmt,
    Unary,
    VarDecl,
    WhileStmt,
)
from .environment import Environment
from .tokens import TokenType
from .values import LiteralValue


class RequestNextLoopIteration(Exception):
    pass


class RequestLoopTermination(Exception):
    pass


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
        if isinstance(stmt, WhileStmt):
            return self._interpret_while_stmt(stmt)
        if isinstance(stmt, BreakStmt):
            return self._interpret_break_stmt(stmt)
        if isinstance(stmt, ContinueStmt):
            return self._interpret_continue_stmt(stmt)
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

    def _interpret_while_stmt(self, stmt: WhileStmt) -> None:
        while bool(self._interpret_expr(stmt.condition)):
            try:
                self._interpret_block(stmt.body)
            except RequestNextLoopIteration:
                continue
            except RequestLoopTermination:
                break
        return None

    def _interpret_break_stmt(self, stmt: BreakStmt) -> None:
        raise RequestLoopTermination

    def _interpret_continue_stmt(self, stmt: ContinueStmt) -> None:
        raise RequestNextLoopIteration

    def _interpret_block(self, block: Block) -> None:
        self.env.push_scope()
        stmt: Stmt
        for stmt in block.statements:
            self._interpret_declaration(stmt)
        self.env.pop_scope()

    def _interpret_var_decl(self, stmt: VarDecl, const: bool = False) -> None:
        identifier: str = stmt.lidentifier.value
        value: LiteralValue = self._interpret_expr(stmt.rexpr)
        # TODO, same for dict
        if isinstance(value, list):
            value: list = self._expand_list(value)
        self.env.declare(identifier, value, const)

    def _expand_list(self, list_: list) -> list:  # TODO: LiteralValue[list]
        new_list: list = []
        expr: Expr
        for expr in list_:
            # TODO: A bit clumsy, can't we make it cleaner once LiteralValues
            #  have their own types? (where we don't need to check for is list?)
            value: LiteralValue = self._interpret_expr(expr)
            if isinstance(value, list):
                value = self._expand_list(value)
            new_list.append(value)
        return new_list

    def _interpret_expr(self, expr: Expr) -> LiteralValue:
        if isinstance(expr, BinOp):
            return self._interpret_bin_op(expr)
        if isinstance(expr, LogicOp):
            return self._interpret_logic_op(expr)
        if isinstance(expr, Unary):
            return self._interpret_unary_expr(expr)
        if isinstance(expr, Literal):
            return self._interpret_literal(expr)
        if isinstance(expr, Group):
            return self._interpret_group(expr)
        if isinstance(expr, Assign):
            return self._interpret_assignment(expr)
        if isinstance(expr, ListIndex):
            return self._interpret_list_index(expr)
        assert False, f"Unmatched expression type {expr.__class__.__name__!r}."

    def _interpret_bin_op(self, bin_op: BinOp) -> LiteralValue:
        lval: LiteralValue
        rval: LiteralValue
        if isinstance(bin_op, BinOpRTL):  # Right-To-Left, rval first
            rval = self._interpret_expr(bin_op.rexpr)
            lval = self._interpret_expr(bin_op.lexpr)
        else:  # Left-To-Right, lval first
            lval = self._interpret_expr(bin_op.lexpr)
            rval = self._interpret_expr(bin_op.rexpr)
        op: TokenType = bin_op.operator
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

    def _interpret_logic_op(self, logic_op: LogicOp) -> LiteralValue:
        lval: LiteralValue = self._interpret_expr(logic_op.lexpr)
        op: TokenType = logic_op.operator
        match op:
            case TokenType.OR:
                # TODO: bool, should use the __bool__ once we've got real
                #  subclasses for LiteralValue (is_truthy()?).
                if bool(lval):
                    return lval
            case TokenType.AND:
                if not bool(lval):
                    return lval
            case _:
                assert False, f"Unmatched logic operator {op.__class__.__name__!r}."
        # Do NOT evaluate rexpr if not needed, it can have side effects.
        rval: LiteralValue = self._interpret_expr(logic_op.rexpr)
        return rval

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
        if literal.literal_type == TokenType.IDENTIFIER:
            return self.env.get(literal.value)
        return literal.value

    def _interpret_group(self, group: Group) -> LiteralValue:
        return self._interpret_expr(group.expr)

    def _interpret_assignment(self, assignment: Assign) -> LiteralValue:
        # TODO: Make sure all this is tested
        # TODO: factorize with _interpret_assignment_to_list() ?
        # TODO: test order of side effects from value to identifier
        value: LiteralValue = self._interpret_expr(assignment.rexpr)
        if isinstance(value, list):
            value: list = self._expand_list(value)

        lvalue: Expr = assignment.lvalue
        if isinstance(lvalue, Literal) and lvalue.literal_type == TokenType.IDENTIFIER:
            # TODO: It's strange that identifier names are called "value"
            #  although it makes sense since identifiers are Literals
            #  (same as int, string etc.). But there must be a better way.
            var_name: str = lvalue.value
            self.env.set(var_name, value)
            return value
        if isinstance(lvalue, ListIndex):
            return self._interpret_assignment_to_list(lvalue, value)

        raise RuntimeError("assigned to expression that is not an lvalue")

    def _interpret_assignment_to_list(
        self, list_index: ListIndex, value: LiteralValue
    ) -> LiteralValue:
        list_: Expr = self._interpret_expr(list_index.list_)
        if not isinstance(list_, list):
            raise RuntimeError("expression is not a list")
        index: LiteralValue = self._interpret_expr(list_index.index)
        if not isinstance(index, Decimal) or index != int(index):
            raise RuntimeError("non-integer list index")  # TODO: Add Token
        try:
            list_[int(index)] = value
        except IndexError:
            raise RuntimeError(
                f"list index out of range: {int(index)!r}"
            )  # TODO: Add Token
        return value

    def _interpret_list_index(self, list_index: ListIndex) -> LiteralValue:
        """#TODO: docstring"""
        # TODO: Make Callable a Literal value once we've got functions.
        list_: Expr = self._interpret_expr(list_index.list_)
        # TODO : why can it bo both a Literal and a [] ? Which should it be
        if not isinstance(list_, list):
            raise RuntimeError("expression is not a list")  # TODO: Add Token
        index: LiteralValue = self._interpret_expr(list_index.index)
        if not isinstance(index, Decimal) or index != int(index):
            raise RuntimeError("non-integer list index")  # TODO: Add Token
        try:
            value: LiteralValue = list_[int(index)]
        except IndexError:
            raise RuntimeError(
                f"list index out of range: {int(index)!r}"
            )  # TODO: Add Token
        return value
