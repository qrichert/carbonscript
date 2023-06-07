import enum

from .tokens import Token


class ErrorType(enum.Enum):
    SYNTAX = "SYNTAX"


class ParseError(Exception):
    def __init__(self, type_: ErrorType, message: str, token: Token) -> None:
        self.message: str = message
        self.token: Token = token
        error_type: str = type_.value.lower()
        super().__init__(f"{error_type} error:{token.line}:{token.column}: {message}.")
