import enum


class TokenType(enum.Enum):
    # TODO[refactor]: Give each a separate TokenType (DECLKW, LITKW).
    DECLKW = "DECLKW"  # var, const
    LITKW = "LITKW"  # true, false, null, etc.
    IF = "IF"  # if
    ELSE = "ELSE"  # else
    WHILE = "WHILE"  # while
    BREAK = "BREAK"  # break
    CONTINUE = "CONTINUE"  # continue
    IDENTIFIER = "IDENTIFIER"  # some_variable
    NUMBER = "NUMBER"  # 1.618
    STRING = "STRING"  # "hello, world"
    DBLSTAREQ = "DBLSTAREQ"  # **=
    STAREQ = "STAREQ"  # *=
    DBLSLASHEQ = "DBLSLASHEQ"  # //=
    SLASHEQ = "SLASHEQ"  # /=
    PERCENTEQ = "PERCENTEQ"  # %=
    PLUSEQ = "PLUSEQ"  # +=
    MINUSEQ = "MINUSEQ"  # -=
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
    OR = "OR"  # or
    AND = "AND"  # and
    BANG = "BANG"  # !
    EQUAL = "EQUAL"  # =
    LPAREN = "LPAREN"  # (
    RPAREN = "RPAREN"  # )
    NEWLINE = "NEWLINE"  # \n
    INDENT = "INDENT"  # Indent marker (empty).
    DEDENT = "DEDENT"  # Dedent marker (empty).
    WHITESPACE = "WHITESPACE"  # <space>, \t, etc. (but not \n)
    MLCOMMENT = "MLCOMMENT"  # ## Multi line comment ##
    SLCOMMENT = "SLCOMMENT"  # # Single line comment
    EOF = "EOF"  # End of file marker.
    UNKNOWN = "UNKNOWN"  # Matches no known type.
    GARBAGE = "GARBAGE"  # Shouldn't be used, will be garbage collected.


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
