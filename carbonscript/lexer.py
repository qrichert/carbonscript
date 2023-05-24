r"""Lexing module.

Lexing is the first step in the process. It consists of recognizing
known patterns (i.e., tokens) in a seemingly random soup of characters
(i.e., input string).

Note: The right word is "lexeme". A token is a lexeme with data attached
to it (type, line, column, etc.). In the implementation we use tokens,
so we'll stick with that word.

For example, if you consider the following input:

    "7 + 108 * 9"

The lexer will break it up into tokens:

    ["7", " ", "+", " ", "108", " ", "*", " ", "9"]

Namely, a number, whitespace, a plus symbol, whitespace, another number,
whitespace, the star symbol, whitespace, and a final number.

Note that neither the plus symbol (`+`) nor the star symbol (`*`) are
arithmetic operators yet. They are just tokens, or patterns, that are
used in the language and which the lexer recognizes.

In short, the lexer identifies language patterns in raw input, in
preparation for the next step: parsing.

One way to implement this is with regular expressions. (Doing the
matching by hand works too, but the regular expression engine happens
to be pretty good at matching a regular language, so why not leverage
that).

Start by defining patterns:

    NUMBER: \d+
    WHITESPACE: \s+
    PLUS: \+
    STAR: \*

Then consume the input match by match:

    input: "7 + 108 * 9"
    tokens: []

1. Input matches NUMBER? Yes. Extract token.

    input: " + 108 * 9"
    tokens: ["7"]

2. Input matches NUMBER? No. WHITESPACE? Yes. Extract token.

    input: "+ 108 * 9"
    tokens: ["7", " "]

2. Input matches NUMBER? No. WHITESPACE? No. PLUS? Yes. Extract token.

    input: " 108 * 9"
    tokens: ["7", " ", "+3]

And so on...
"""

import enum
import re


class TokenType(enum.Enum):
    KEYWORD = "KEYWORD"  # true, false, null, etc.
    STRING = "STRING"  # hello, world
    IDENTIFIER = "IDENTIFIER"  # some_variable
    NUMBER = "NUMBER"  # 1.618
    POWER = "POWER"  # **
    MULTIPLY = "MULTIPLY"  # *
    INT_DIVIDE = "INT_DIVIDE"  # //
    DIVIDE = "DIVIDE"  # /
    MODULUS = "MODULUS"  # %
    PLUS = "PLUS"  # +
    MINUS = "MINUS"  # -
    LPAREN = "LPAREN"  # (
    RPAREN = "RPAREN"  # )
    DBLQUOTE = "DBLQUOTE"  # "
    NEWLINE = "NEWLINE"  # \n
    WHITESPACE = "WHITESPACE"  # <space>, \t, etc. (but not \n)
    EOF = "EOF"  # End of file marker.
    UNKNOWN = "UNKNOWN"  # Matches no known type.
    GARBAGE = "GARBAGE"  # Shouldn't be used, will be garbage collected.


KEYWORDS: set[str] = {
    "true",
    "false",
    "null",
}

PATTERNS: list[tuple[re.Pattern, TokenType]] = [
    (re.compile(f"({'|'.join(KEYWORDS)})"), TokenType.KEYWORD),
    (re.compile(r"[a-zA-Z_][a-zA-Z0-9_]*"), TokenType.IDENTIFIER),
    (re.compile(r"\d+(\.\d+)?"), TokenType.NUMBER),
    (re.compile(r"\*\*"), TokenType.POWER),
    (re.compile(r"\*"), TokenType.MULTIPLY),
    (re.compile(r"//"), TokenType.INT_DIVIDE),
    (re.compile(r"/"), TokenType.DIVIDE),
    (re.compile(r"%"), TokenType.MODULUS),
    (re.compile(r"\+"), TokenType.PLUS),
    (re.compile(r"-"), TokenType.MINUS),
    (re.compile(r"\("), TokenType.LPAREN),
    (re.compile(r"\)"), TokenType.RPAREN),
    (re.compile(r'"'), TokenType.DBLQUOTE),
    (re.compile(r"\n"), TokenType.NEWLINE),
    (re.compile(r"\s+"), TokenType.WHITESPACE),
    (re.compile(r"$"), TokenType.EOF),
]


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
                token: Token = self._consume_match(token_type, value)
                if token_type == TokenType.NEWLINE:
                    self._track_next_line()
                return token
        value: str = text_view[0]
        return self._consume_match(TokenType.UNKNOWN, value)

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
        elif token_type == TokenType.DBLQUOTE:
            if self._context == Context.NONE:
                self._context = Context.STRING
            else:
                self._context = Context.NONE

    def _handle_context_specific_token_types(
        self, token_type: TokenType, value: str
    ) -> TokenType:
        if self._context == Context.STRING and token_type != TokenType.DBLQUOTE:
            # Append to existing string.
            if self._last_token.type == TokenType.STRING:
                self._last_token.value += value
                return TokenType.GARBAGE
            # New string.
            return TokenType.STRING
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
