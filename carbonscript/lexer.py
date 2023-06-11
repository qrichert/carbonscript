import enum
import re

from .tokens import Token, TokenType

KEYWORDS: dict[str, TokenType] = {
    "var": TokenType.DECLKW,
    "const": TokenType.DECLKW,
    "true": TokenType.LITKW,
    "false": TokenType.LITKW,
    "null": TokenType.LITKW,
    "if": TokenType.IF,
    "else": TokenType.ELSE,
    "while": TokenType.WHILE,
    "break": TokenType.BREAK,
    "continue": TokenType.CONTINUE,
    "or": TokenType.OR,
    "and": TokenType.AND,
}

PATTERNS: list[tuple[re.Pattern, TokenType]] = [
    (re.compile(r"[a-zA-Z_][a-zA-Z0-9_]*"), TokenType.IDENTIFIER),
    (re.compile(r"\d+(\.\d+)?"), TokenType.NUMBER),
    (re.compile(r'"'), TokenType.STRING),
    (re.compile(r"\*\*="), TokenType.DBLSTAREQ),
    (re.compile(r"\*="), TokenType.STAREQ),
    (re.compile(r"//="), TokenType.DBLSLASHEQ),
    (re.compile(r"/="), TokenType.SLASHEQ),
    (re.compile(r"%="), TokenType.PERCENTEQ),
    (re.compile(r"\+="), TokenType.PLUSEQ),
    (re.compile(r"-="), TokenType.MINUSEQ),
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
            match: re.Match
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

            [Token(LITKW, 'true'), Token(IDENTIFIER, '_or_not')]

        This is an error, and the reason why we match everything as an
        identifier, and treat keywords as a special case of identifiers.

        Everything is an identifier, unless it happens to be a keyword.
        This works because keywords match the identifier pattern regex.
        """
        try:
            return KEYWORDS[value]
        except KeyError:
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
