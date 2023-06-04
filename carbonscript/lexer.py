import enum
import re


class TokenType(enum.Enum):
    DECLKEYWORD = "DECLKEYWORD"  # var, const
    LITKEYWORD = "LITKEYWORD"  # true, false, null, etc.
    IDENTIFIER = "IDENTIFIER"  # some_variable
    NUMBER = "NUMBER"  # 1.618
    STRING = "STRING"  # "hello, world"
    DBLSTAR = "DBLSTAR"  # **
    STAR = "STAR"  # *
    DBLSLASH = "DBLSLASH"  # //
    SLASH = "SLASH"  # /
    PERCENT = "PERCENT"  # %
    PLUS = "PLUS"  # +
    MINUS = "MINUS"  # -
    DBLEQUAL = "DBLEQUAL"  # ==
    BANGEQUAL = "BANGEQUAL"  # !=
    GTE = "GTE"  # >=
    GT = "GT"  # >
    LTE = "LTE"  # <=
    LT = "LT"  # <
    BANG = "BANG"  # !
    EQUAL = "EQUAL"  # =
    LPAREN = "LPAREN"  # (
    RPAREN = "RPAREN"  # )
    NEWLINE = "NEWLINE"  # \n
    WHITESPACE = "WHITESPACE"  # <space>, \t, etc. (but not \n)
    MLCOMMENT = "MLCOMMENT"  # ## Multi line comment ##
    SLCOMMENT = "SLCOMMENT"  # # Single line comment
    EOF = "EOF"  # End of file marker.
    UNKNOWN = "UNKNOWN"  # Matches no known type.
    GARBAGE = "GARBAGE"  # Shouldn't be used, will be garbage collected.


DECL_KEYWORDS: set[str] = {
    "var",
    "const",
}

LITERAL_KEYWORDS: set[str] = {
    "true",
    "false",
    "null",
}

PATTERNS: list[tuple[re.Pattern, TokenType]] = [
    (re.compile(r"[a-zA-Z_][a-zA-Z0-9_]*"), TokenType.IDENTIFIER),
    (re.compile(r"\d+(\.\d+)?"), TokenType.NUMBER),
    (re.compile(r'"'), TokenType.STRING),
    (re.compile(r"\*\*"), TokenType.DBLSTAR),
    (re.compile(r"\*"), TokenType.STAR),
    (re.compile(r"//"), TokenType.DBLSLASH),
    (re.compile(r"/"), TokenType.SLASH),
    (re.compile(r"%"), TokenType.PERCENT),
    (re.compile(r"\+"), TokenType.PLUS),
    (re.compile(r"-"), TokenType.MINUS),
    (re.compile(r"=="), TokenType.DBLEQUAL),
    (re.compile(r"!="), TokenType.BANGEQUAL),
    (re.compile(r">="), TokenType.GTE),
    (re.compile(r">"), TokenType.GT),
    (re.compile(r"<="), TokenType.LTE),
    (re.compile(r"<"), TokenType.LT),
    (re.compile(r"!"), TokenType.BANG),
    (re.compile(r"="), TokenType.EQUAL),
    (re.compile(r"\("), TokenType.LPAREN),
    (re.compile(r"\)"), TokenType.RPAREN),
    (re.compile(r"\n"), TokenType.NEWLINE),
    (re.compile(r"[ \t]+"), TokenType.WHITESPACE),
    (re.compile(r"##"), TokenType.MLCOMMENT),
    (re.compile(r"#"), TokenType.SLCOMMENT),
    (re.compile(r"$"), TokenType.EOF),
]

ENDS_WITH_ODD_NUMBER_OF_ESCAPE_BACKSLASHES: re.Pattern = re.compile(
    r"(?<!\\)(\\\\)*\\$"
)


class Token:
    def __init__(
        self, type_: TokenType, value: str = "", line: int = -1, column: int = -1
    ) -> None:
        self.type: TokenType = type_
        self.value: str = value
        self.line: int = line
        self.column: int = column

    def __eq__(self, other) -> bool:
        if not isinstance(other, Token):
            return False
        return all(
            [
                self.type == other.type,
                self.value == other.value,
            ]
        )

    def __repr__(self) -> str:
        name: str = self.type.name
        value: str = self.value
        if len(value) > 10:
            value = value[:9] + "…"
        value = f", {value!r}" if value else ""
        return f"Token({name}{value})"


class Context(enum.Enum):
    NONE = "NONE"
    STRING = "STRING"
    MLCOMMENT = "MLCOMMENT"
    SLCOMMENT = "SLCOMMENT"


class Lexer:
    """Tokenize input script."""

    def __init__(self) -> None:
        self.script: str = ""
        self.tokens: list[Token] = []
        self._pos: int = 0
        self._line: int = 1
        self._column: int = 1
        self._context: Context = Context.NONE

    @property
    def _last_token(self) -> Token | None:
        try:
            return self.tokens[-1]
        except IndexError:
            return Token(TokenType.GARBAGE)

    def lex(self, script: str) -> list[Token]:
        self.__init__()
        self.script = script

        while True:
            token: Token = self._find_next_token()
            if token.type != TokenType.GARBAGE:
                self.tokens.append(token)
            if token.type == TokenType.EOF:
                break

        return self.tokens

    def _find_next_token(self) -> Token:
        text_view: str = self.script[self._pos :]
        for pattern, token_type in PATTERNS:
            if match_ := pattern.match(text_view):
                value: str = match_.group(0)
                if token_type == TokenType.IDENTIFIER:
                    token_type = self._update_token_type_if_identifier_is_keyword(value)
                token: Token = self._consume_match(token_type, value)
                if token_type == TokenType.NEWLINE:
                    self._track_next_line()
                return token
        value: str = text_view[0]
        return self._consume_match(TokenType.UNKNOWN, value)

    @staticmethod
    def _update_token_type_if_identifier_is_keyword(value: str) -> TokenType:
        """Disambiguate between identifier and keyword.

        If we handle identifiers and keywords separately, a variable
        named "true_or_not" may be matched as:

            [Token(LITKEYWORD, 'true'), Token(IDENTIFIER, '_or_not')]

        This is an error, and the reason why we match everything as an
        identifier, and treat keywords as special case of identifiers.

        Everything is an identifier, unless it happens to be a keyword.
        This works because keywords match the identifier pattern regex.
        """
        if value in DECL_KEYWORDS:
            return TokenType.DECLKEYWORD
        if value in LITERAL_KEYWORDS:
            return TokenType.LITKEYWORD
        return TokenType.IDENTIFIER

    def _consume_match(self, token_type: TokenType, value: str) -> Token:
        """Extract token from script and advance playhead."""
        self._update_context(token_type)
        token_type = self._handle_context_specific_token_types(token_type, value)
        token: Token = self._create_token(token_type, value)
        self._consume(value)
        return token

    def _update_context(self, token_type: TokenType) -> None:
        if token_type == TokenType.EOF:
            self._context = Context.NONE
            return

        if token_type == TokenType.STRING:
            if self._context == Context.NONE:
                self._context = Context.STRING
            elif self._context == Context.STRING:
                if ENDS_WITH_ODD_NUMBER_OF_ESCAPE_BACKSLASHES.search(
                    self._last_token.value
                ):
                    return
                self._context = Context.NONE
            return

        if token_type == TokenType.MLCOMMENT:
            if self._context == Context.NONE:
                self._context = Context.MLCOMMENT
            elif self._context == Context.MLCOMMENT:
                self._context = Context.NONE
            return

        if token_type == TokenType.SLCOMMENT:
            if self._context == Context.NONE:
                self._context = Context.SLCOMMENT
            return
        if self._context == Context.SLCOMMENT:
            if token_type == TokenType.NEWLINE:
                self._context = Context.NONE
            return

    def _handle_context_specific_token_types(
        self, token_type: TokenType, value: str
    ) -> TokenType:
        if self._context == Context.STRING or (
            # Exiting STRING.
            token_type == TokenType.STRING
            and self._context == Context.NONE
            and self._last_token.type == token_type.STRING
        ):
            return self._append_to_existing_or_create(TokenType.STRING, value)
        if self._context == Context.MLCOMMENT or (
            # Exiting MLCOMMENT.
            token_type == TokenType.MLCOMMENT
            and self._context == Context.NONE
            and self._last_token.type == token_type.MLCOMMENT
        ):
            return self._append_to_existing_or_create(TokenType.MLCOMMENT, value)
        if self._context == Context.SLCOMMENT:
            return self._append_to_existing_or_create(TokenType.SLCOMMENT, value)
        return token_type

    def _append_to_existing_or_create(
        self, token_type: TokenType, value: str
    ) -> TokenType:
        if self._last_token.type == token_type:
            self._last_token.value += value
            return TokenType.GARBAGE
        return token_type

    def _create_token(self, token_type: TokenType, value: str) -> Token:
        return Token(
            type_=token_type,
            value=value,
            line=self._line,
            column=self._column,
        )

    def _consume(self, value: str) -> None:
        nb_chars_in_value: int = len(value)
        self._pos += nb_chars_in_value
        self._column += nb_chars_in_value

    def _track_next_line(self) -> None:
        self._line += 1
        self._column = 1
