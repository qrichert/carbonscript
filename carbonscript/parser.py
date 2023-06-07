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
        class_name: str = self.__class__.__name__
        return f"{class_name}({self.expression})"


@dataclass
class Block:
    statements: list[Stmt]

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        stmts: str = ", ".join([repr(stmt) for stmt in self.statements])
        return f"{class_name}({stmts})"


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


class Preprocessor:
    """Preprocess the tokens to make the parser's job easier.

    This is clearly not the most efficient way to do this, as it adds
    another pass, but it's one of the cleanest, most straightforward
    solution to the cleaning problem.

    The lexer is left to do one task: identify patterns. It does not
    need to know about the quirks of the language. And the parser too is
    left to do one task: generate an AST from tokens. It does not need
    to bother itself with dirty input.

    The preprocessor joins the two by preparing an input that's easy to
    parse, without requiring additional logic.
    """

    GARBAGE_TOKENS: set = {
        TokenType.WHITESPACE,
        TokenType.MLCOMMENT,
        TokenType.SLCOMMENT,
    }

    EOL_TOKENS: set = {
        TokenType.NEWLINE,
        TokenType.EOF,
    }

    def __init__(self) -> None:
        self.tokens: list[Token] = []
        self.preprocessed_tokens: list[Token] = []
        self._pos: int = 0
        self._current_indent: int = 0
        self._previous_indent: int = 0

    def preprocess(self, tokens: list[Token]) -> list[Token]:
        self.__init__()
        self.tokens = tokens
        while True:  # Line per line.
            was_line_discarded: bool = self._discard_line_if_only_garbage()
            if not was_line_discarded:
                self._process_line()
            if self._current().type == TokenType.NEWLINE:
                self._process_newline()
                continue
            if self._current().type == TokenType.EOF:
                self._process_eof()
                break
        return self.preprocessed_tokens

    def _discard_line_if_only_garbage(self) -> bool:
        look_ahead: int = 0  # = How many tokens are garbage.
        token: Token
        while (token := self._peek(look_ahead)).type not in self.EOL_TOKENS:
            if token.type not in self.GARBAGE_TOKENS:
                return False
            look_ahead += 1
        # Here, attained newline, saw only garbage.
        self._discard(look_ahead)
        return True

    def _process_line(self) -> None:
        self._process_indent()
        self._check_no_garbage_between_indent_and_statement()
        self._process_statement()

    def _process_indent(self) -> None:
        self._previous_indent = self._current_indent

        if self._current().type != TokenType.WHITESPACE:
            self._current_indent = 0
        else:
            whitespace: str = self._current().value
            self._discard()

            nb_chars: int = len(whitespace)
            if nb_chars % 4 != 0:
                raise ParseError(
                    ErrorType.SYNTAX,
                    "incorrect indent, expected a multiple of four spaces",
                    self._previous(),
                )
            self._current_indent = nb_chars // 4

        self._inject_block_markers()

    def _inject_block_markers(self) -> None:
        diff: int = self._current_indent - self._previous_indent
        if diff == 0:
            return
        elif diff > 0:
            if diff > 1:
                raise ParseError(
                    ErrorType.SYNTAX,
                    "indented too many blocks",
                    self._current(),
                )
            self._inject_n_tokens(TokenType.INDENT, diff)
        elif diff < 0:
            self._inject_n_tokens(TokenType.DEDENT, abs(diff))

    def _inject_n_tokens(self, token_type: TokenType, nb_tokens: int) -> None:
        line: int = self._current().line
        column: int = self._current().column
        for _ in range(nb_tokens):
            token: Token = Token(token_type, line=line, column=column)
            self.preprocessed_tokens.append(token)

    def _check_no_garbage_between_indent_and_statement(self) -> None:
        if self._current().type in self.GARBAGE_TOKENS:
            raise ParseError(
                ErrorType.SYNTAX,
                "statements can only be preceded by block indents",
                self._current(),
            )

    def _process_statement(self) -> None:
        token: Token
        while self._current().type not in self.EOL_TOKENS:
            while self._current().type in self.GARBAGE_TOKENS:
                self._discard()

            if self._current().type == TokenType.STRING:
                self._clean_string()

            if self._current().type not in self.EOL_TOKENS:
                self._keep()

    def _clean_string(self) -> None:
        value: str = self._current().value
        if len(value) < 2 or not value.endswith('"'):
            raise ParseError(
                ErrorType.SYNTAX,
                "unterminated string",
                self._current(),
            )
        self._current().value = value[1:-1]

    def _process_newline(self) -> None:
        last_token: Token | None = self._last_preprocessed()
        if not last_token or last_token.type == TokenType.NEWLINE:
            self._discard()
        else:
            self._keep()

    def _process_eof(self) -> None:
        self._ensure_ends_with_newline()
        self._inject_n_tokens(TokenType.DEDENT, self._current_indent)
        self._keep()

    def _ensure_ends_with_newline(self) -> None:
        last_token: Token | None = self._last_preprocessed()
        if last_token and last_token.type != TokenType.NEWLINE:
            self._inject_n_tokens(TokenType.NEWLINE, 1)

    def _keep(self) -> None:
        self.preprocessed_tokens.append(self._current())
        self._pos += 1

    def _discard(self, nb_tokens: int = 1) -> None:
        self._pos += nb_tokens

    def _backtrack(self) -> None:
        self._pos -= 1

    def _previous(self) -> Token | None:
        if self._pos == 0:  # /!\ Negative indexes start from the end.
            return None
        try:
            return self.tokens[self._pos - 1]
        except IndexError:
            return None

    def _current(self) -> Token:
        return self.tokens[self._pos]

    def _peek(self, nb_tokens: int = 1) -> Token | None:
        return self.tokens[self._pos + nb_tokens]

    def _last_preprocessed(self) -> Token | None:
        try:
            return self.preprocessed_tokens[-1]
        except IndexError:
            return None


class Parser:
    """Build Abstract Syntax Tree (AST) from tokens.

    This implementation uses recursive descent.
    """

    def __init__(self) -> None:
        self.tokens: list[Token] = []
        self._tokens: list[Token] = []
        self.statements: list[Stmt] = []
        self._pos: int = 0

    def parse(self, tokens: list[Token]) -> list[Stmt]:
        """Parse program.

        program → declaration* EOF
        """
        self.__init__()
        self.tokens = tokens
        self._tokens = Preprocessor().preprocess(tokens)
        while True:
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
                    return self._parse_var_decl()
                case "const":
                    return self._parse_var_decl(const=True)
            assert False, "Unmatched declaration keyword."
        return self._parse_stmt()

    def _parse_var_decl(self, const: bool = False) -> VarDecl | ConstDecl:
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

    def _parse_stmt(self) -> Stmt | Block:
        """Parse statement.

        stmt → expr_stmt
             | block
        """
        if block := self._parse_block():
            return block
        return self._parse_expr_stmt()

    def _parse_block(self) -> Block | None:
        """Parse block.

        block → INDENT declaration+ UNINDENT
        """
        if self._consume_token_if_matches(TokenType.INDENT):
            statements: list[Stmt] = []
            while self._current().type != TokenType.DEDENT:
                if self._current().type == TokenType.EOF:
                    break
                declaration: Declaration = self._parse_declaration()
                statements.append(declaration)
            if (
                not self._consume_token_if_matches(TokenType.DEDENT)
                and self._current().type != TokenType.EOF
            ):
                raise ParseError(
                    ErrorType.SYNTAX,
                    "unterminated block",
                    self._current(),
                )
            if not statements:
                ParseError(
                    ErrorType.SYNTAX,
                    "empty block",
                    self._current(),
                )
            return Block(statements)
        return None

    def _parse_expr_stmt(self) -> ExprStmt:
        """Parse expression statement.

        expr_stmt → expr "\n"
        """
        expr: Expr = self._parse_expr()
        self._match_end_of_statement()
        return ExprStmt(expr)

    def _match_end_of_statement(self) -> None:
        if not self._consume_token_if_matches(TokenType.NEWLINE):
            raise ParseError(
                ErrorType.SYNTAX,
                "multiple statements on a single line",
                self._current(),
            )

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
            value: str = self._previous().value
            return Literal(TokenType.STRING, value)
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
        if self._current().type in (TokenType.INDENT, TokenType.DEDENT):
            indent: str = (
                "indent" if self._current().type == TokenType.INDENT else "dedent"
            )
            raise ParseError(
                ErrorType.SYNTAX,
                f"unexpected {indent}",
                self._current(),
            )
        raise ParseError(
            ErrorType.SYNTAX,
            f"invalid symbol {self._current().value!r}",
            self._current(),
        )

    def _consume_token_if_matches(self, *token_types) -> TokenType | None:
        for token_type in token_types:
            if token_type == self._current().type:
                self._consume()
                return token_type
        return None

    def _consume(self) -> None:
        self._pos += 1

    def _previous(self) -> Token | None:
        if self._pos == 0:  # /!\ Negative indexes start from the end.
            return None
        try:
            return self._tokens[self._pos - 1]
        except IndexError:
            return None

    def _current(self) -> Token:
        return self._tokens[self._pos]
