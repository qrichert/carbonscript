import enum
from dataclasses import dataclass
from decimal import Decimal
from typing import Union

from .lexer import Token, TokenType


class ErrorType(enum.Enum):
    SYNTAX = "SYNTAX"


class ParseError(Exception):
    def __init__(self, type_: ErrorType, message: str, token: Token) -> None:
        self.message: str = message
        self.token: Token = token
        error_type: str = type_.value.lower()
        super().__init__(f"{error_type} error:{token.line}:{token.column}: {message}.")


@dataclass(repr=False)
class Expr:
    """Expression."""

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        return f"{class_name}()"


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
    value: Union[Decimal, str, bool, None]

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
        return f"ExprStmt({self.expression})"


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


@dataclass()
class ConstDecl(VarDecl):
    """Constant declaration"""

    def __repr__(self) -> str:
        return super().__repr__()


class Parser:
    """Build Abstract Syntax Tree (AST) from tokens.

    This implementation uses recursive descent.
    """

    def __init__(self) -> None:
        self.tokens: list[Token] = []
        self.statements: list[Stmt] = []
        self._pos: int = 0

    def parse(self, tokens: list[Token]) -> list[Stmt]:
        """Parse program.

        program → declaration* EOF
        """
        self.__init__()
        self.tokens = tokens
        while True:
            self._discard_empty_lines()
            if self._current().type == TokenType.EOF:
                break
            stmt: Stmt = self._parse_declaration()
            self.statements.append(stmt)
        return self.statements

    def _parse_declaration(self) -> Declaration | Stmt:
        """Parse declaration.

        declaration → var_decl
                    | stmt
        """
        if self._consume_token_if_matches(TokenType.DECLKEYWORD):
            match self._previous().value:
                case "var":
                    return self._var_decl()
                case "const":
                    return self._var_decl(const=True)
            assert False, "Unmatched declaration keyword."
        return self._parse_stmt()

    def _var_decl(self, const: bool = False) -> VarDecl | ConstDecl:
        """Parse variable declaration.

        var_decl → ("var" | "const") IDENTIFIER "=" expr "\n"
        """
        if self._consume_token_if_matches(TokenType.IDENTIFIER):
            lidentifier: Literal = Literal(TokenType.IDENTIFIER, self._previous().value)
            if self._consume_token_if_matches(TokenType.EQUAL):
                rexpr: Expr = self._parse_expr()
                self._match_end_of_statement()
                if const:
                    return ConstDecl(lidentifier, rexpr)
                return VarDecl(lidentifier, rexpr)
            raise ParseError(
                ErrorType.SYNTAX,
                "expected '=' sign after identifier",
                self._current(),
            )
        raise ParseError(
            ErrorType.SYNTAX,
            "assignment target is not an identifier",
            self._current(),
        )

    def _parse_stmt(self) -> Stmt:
        """Parse statement.

        stmt → expr_stmt
        """
        return self._parse_expr_stmt()

    def _parse_expr_stmt(self) -> ExprStmt:
        """Parse expression statement.

        expr_stmt → expr "\n"
        """
        expr: Expr = self._parse_expr()
        self._match_end_of_statement()
        return ExprStmt(expr)

    def _match_end_of_statement(self) -> None:
        if (
            not self._consume_token_if_matches(TokenType.NEWLINE)
            and self._current().type != TokenType.EOF
        ):
            raise ParseError(
                ErrorType.SYNTAX,
                "multiple statements on a single line",
                self._current(),
            )

    def _discard_empty_lines(self) -> None:
        def is_empty_line() -> bool:
            return self._current().type == TokenType.NEWLINE

        def is_whitespace_only_line() -> bool:
            return self._current().type == TokenType.WHITESPACE and (
                self._peek().type in (TokenType.NEWLINE, TokenType.EOF)
            )

        while is_empty_line() or is_whitespace_only_line():
            self._consume()

    def _parse_expr(self) -> Expr:
        """Parse expression.

        expr → assignment
        """
        return self._parse_assignment()

    def _parse_assignment(self) -> Expr:
        """Parse assignment.

        assignment → IDENTIFIER "=" assignment
                   | equality
        """
        expr: Expr = self._parse_equality()

        # "expr" is the lvalue of an assignment, if followed by "=".
        if self._consume_token_if_matches(TokenType.EQUAL):
            if not (isinstance(expr, Literal) and expr.literal == TokenType.IDENTIFIER):
                raise ParseError(
                    ErrorType.SYNTAX,
                    "assignment target is not an identifier",
                    self._previous(),  # "=" sign.
                )
            lidentifier: Literal = expr
            rexpr: Expr = self._parse_assignment()
            return Assign(lidentifier, rexpr)

        return expr

    def _parse_equality(self) -> Expr:
        """Parse equality.

        equality → comparison ( ( "==" | "!=" ) comparison )*
        """
        lexpr: Expr = self._parse_comparison()
        operator: TokenType
        while operator := self._consume_token_if_matches(
            TokenType.DBLEQUAL,
            TokenType.BANGEQUAL,
        ):
            rexpr: Expr = self._parse_comparison()
            if not rexpr:
                raise ParseError(
                    ErrorType.SYNTAX,
                    "missing right part of expression",
                    self._current(),
                )
            lexpr = BinOp(lexpr, operator, rexpr)
        return lexpr

    def _parse_comparison(self) -> Expr:
        """Parse comparison.

        comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
        """
        lexpr: Expr = self._parse_term()
        operator: TokenType
        while operator := self._consume_token_if_matches(
            TokenType.GT,
            TokenType.GTE,
            TokenType.LT,
            TokenType.LTE,
        ):
            rexpr: Expr = self._parse_term()
            if not rexpr:
                raise ParseError(
                    ErrorType.SYNTAX,
                    "missing right part of expression",
                    self._current(),
                )
            lexpr = BinOp(lexpr, operator, rexpr)
        return lexpr

    def _parse_term(self) -> Expr:
        """Parse term.

        term → factor ( ( "+" | "-" ) factor )*
        """
        lexpr: Expr = self._parse_factor()
        operator: TokenType
        while operator := self._consume_token_if_matches(
            TokenType.PLUS,
            TokenType.MINUS,
        ):
            rexpr: Expr = self._parse_factor()
            if not rexpr:
                raise ParseError(
                    ErrorType.SYNTAX,
                    "missing right part of expression",
                    self._current(),
                )
            lexpr = BinOp(lexpr, operator, rexpr)
        return lexpr

    def _parse_factor(self) -> Expr:
        """Parse factor.

        factor → power ( ( "*" | "/" | "//" | "%" ) power )*
        """
        lexpr: Expr = self._parse_power()
        operator: TokenType
        while operator := self._consume_token_if_matches(
            TokenType.STAR,
            TokenType.SLASH,
            TokenType.DBLSLASH,
            TokenType.PERCENT,
        ):
            rexpr: Expr = self._parse_power()
            if not rexpr:
                raise ParseError(
                    ErrorType.SYNTAX,
                    "missing right part of expression",
                    self._current(),
                )
            lexpr = BinOp(lexpr, operator, rexpr)
        return lexpr

    def _parse_power(self) -> Expr:
        """Parse power.

        power → unary ( "**" power )*
        """
        lexpr: Expr = self._parse_unary()
        operator: TokenType
        while operator := self._consume_token_if_matches(
            TokenType.DBLSTAR,
        ):
            rexpr: Expr = self._parse_power()
            if not rexpr:
                raise ParseError(
                    ErrorType.SYNTAX,
                    "missing right part of expression",
                    self._current(),
                )
            lexpr = BinOp(lexpr, operator, rexpr)
        return lexpr

    def _parse_unary(self) -> Expr:
        """Parse unary.

        unary → ( "+" | "-" | "!" ) unary
              | primary
        """
        operator: TokenType
        if operator := self._consume_token_if_matches(
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.BANG,
        ):
            rexpr: Expr = self._parse_unary()
            if not rexpr:
                raise ParseError(
                    ErrorType.SYNTAX,
                    "missing right part of expression",
                    self._current(),
                )
            return Unary(operator, rexpr)
        return self._parse_primary()

    def _parse_primary(self) -> Expr:
        """Parse primary.

        primary → NUMBER | STRING | BOOLEAN | NULL
                | "(" expr ")"
                | IDENTIFIER
        """
        # TODO[refactor]: These should be functions, each.
        literal: TokenType
        if self._consume_token_if_matches(TokenType.NUMBER):
            value: Decimal = Decimal(self._previous().value)
            return Literal(TokenType.NUMBER, value)
        if self._consume_token_if_matches(TokenType.STRING):
            string_token_type: TokenType = self._previous(2).type
            if string_token_type == TokenType.STRING:
                value: str = self._previous(2).value
                return Literal(TokenType.STRING, value)
            if string_token_type == TokenType.DBLQUOTE:  # empty string
                return Literal(TokenType.STRING, "")
        if self._consume_token_if_matches(TokenType.IDENTIFIER):
            value: str = self._previous().value
            return Literal(TokenType.IDENTIFIER, value)
        if literal := self._consume_token_if_matches(TokenType.LITKEYWORD):
            match self._previous().value:
                case "true":
                    return Literal(literal, True)
                case "false":
                    return Literal(literal, False)
                case "null":
                    return Literal(literal, None)
            assert False, "Unmatched literal keyword."
        if self._consume_token_if_matches(TokenType.LPAREN):
            expr: Expr = self._parse_expr()
            rparen: TokenType = self._consume_token_if_matches(TokenType.RPAREN)
            if not expr or not rparen:
                raise ParseError(
                    ErrorType.SYNTAX,
                    f"unterminated group expression, missing {')'!r}",
                    self._previous(),
                )
            return Group(expr)
        raise ParseError(
            ErrorType.SYNTAX,
            f"invalid symbol {self._current().value!r}",
            self._current(),
        )

    def _consume_token_if_matches(self, *token_types) -> TokenType | None:
        self._discard_whitespace()
        for token_type in token_types:
            if token_type == self._current().type:
                self._consume()
                return token_type
            if (
                token_type == TokenType.STRING
                and self._current().type == TokenType.DBLQUOTE
            ):
                if (
                    self._peek(1).type == TokenType.STRING
                    and self._peek(2).type == TokenType.DBLQUOTE
                ):
                    self._consume(3)
                    return TokenType.STRING
                if self._peek().type == TokenType.DBLQUOTE:  # empty string
                    self._consume(2)
                    return TokenType.STRING
                raise ParseError(
                    ErrorType.SYNTAX,
                    "unterminated string",
                    self._peek(),
                )
        return None

    def _discard_whitespace(self) -> None:
        while self._current().type == TokenType.WHITESPACE:
            self._consume()

    def _consume(self, nb_tokens: int = 1) -> None:
        self._pos += nb_tokens

    def _current(self) -> Token:
        return self.tokens[self._pos]

    def _peek(self, nb_tokens: int = 1) -> Token | None:
        try:
            return self.tokens[self._pos + nb_tokens]
        except IndexError:
            return None

    def _previous(self, nb_tokens: int = 1) -> Token | None:
        try:
            return self.tokens[self._pos - nb_tokens]
        except IndexError:
            return None
