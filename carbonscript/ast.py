from __future__ import annotations

from dataclasses import dataclass

from .tokens import TokenType
from .values import LiteralValue


@dataclass(repr=False)
class Expr:
    """Expression."""

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        return f"{class_name}()"


@dataclass
class LogicOp(Expr):
    """Operator and left/right values to operate on (e.g, `A and B`)."""

    lexpr: Expr
    operator: TokenType
    rexpr: Expr

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        operator: str = self.operator.value
        return f"{class_name}({self.lexpr!r}, {operator}, {self.rexpr!r})"


@dataclass
class BinOp(Expr):
    """Operator and left/right values to operate on (e.g, `A + B`)."""

    lexpr: Expr
    operator: TokenType
    rexpr: Expr

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        operator: str = self.operator.value
        return f"{class_name}({self.lexpr!r}, {operator}, {self.rexpr!r})"


@dataclass(repr=False)
class BinOpRTL(BinOp):
    """BinOp that is evaluated from Right-To-Left."""


@dataclass
class Unary(Expr):
    """Operator and right value to operate on (e.g. `-A`)"""

    operator: TokenType
    rexpr: Expr

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        operator: str = self.operator.value
        return f"{class_name}({operator}, {self.rexpr!r})"


# TODO[refactor]: Literals should have their own class.
#   Number, String, Boolean, Null, Identifier


@dataclass
class Literal(Expr):
    """Number, String, boolean, null, identifier. (e.g., `108`)."""

    literal: TokenType
    value: LiteralValue

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        name: str = self.literal.name
        return f"{class_name}({name}, {self.value!r})"


@dataclass
class Group(Expr):
    """Parentheses with an expression inside (e.g., `(A + B)`."""

    expr: Expr

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        return f"{class_name}({self.expr!r})"


@dataclass
class Assign(Expr):
    """Identifier and expression to assign to identifier."""

    lidentifier: Literal
    rexpr: Expr

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        return f"{class_name}({self.lidentifier}, {self.rexpr!r})"


@dataclass(repr=False)
class Stmt:
    """Statement."""


@dataclass
class ExprStmt(Stmt):
    """Expression Statement."""

    expression: Expr

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        return f"{class_name}({self.expression})"


@dataclass
class Block:
    """Block of statements."""

    statements: list[Stmt]

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        stmts: str = ", ".join([repr(stmt) for stmt in self.statements])
        return f"{class_name}({stmts})"


@dataclass
class IfStmt(Stmt):
    """If statement."""

    condition: Expr
    then: Block
    else_: Block | IfStmt | None

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        condition: str = "Cond(" + repr(self.condition) + ")"
        then: str = "Then(" + repr(self.then) + ")"
        else_: str = "Else(" + repr(self.else_) + ")" if self.else_ else ""
        return f"{class_name}({condition}, {then}{', ' + else_ if else_ else ''})"


@dataclass
class WhileStmt(Stmt):
    """While statement."""

    condition: Expr
    body: Block

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        condition: str = "Cond(" + repr(self.condition) + ")"
        body: str = repr(self.body)
        return f"{class_name}({condition}, {body})"


@dataclass
class BreakStmt(Stmt):
    """Break statement."""

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        return class_name


@dataclass
class ContinueStmt(Stmt):
    """Continue statement."""

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        return class_name


@dataclass(repr=False)
class Declaration(Stmt):
    """Declaration."""


@dataclass
class VarDecl(Declaration):
    """Variable declaration."""

    lidentifier: Literal
    rexpr: Expr

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        return f"{class_name}({self.lidentifier}, {self.rexpr!r})"


@dataclass
class ConstDecl(VarDecl):
    """Constant declaration"""

    def __repr__(self) -> str:
        return super().__repr__()
