"""Parsing module.

Parsing is the second step of the process. This is where we associate
meaning to the tokens identified by the lexer.

The parsing process is based on grammar rules. In essence, grammar
dictates how tokens may be organized to form specific constructs that
have meaning.
=> Construct A is composed like this, construct B like that, etc.

The job of the parser is to detect these constructs in order to deduce
from them the intent of the programmer.
=> This arrangement of token is construct A, meaning we must do X.

Side note: Although it may seem like the boring part, defining grammar
rules is essential. The parser is simply a dumb translation of these
rules into code. The code is simple, the complexity lies in defining
coherent rules. Don't skip the grammar part.

In a very simplistic example, we could have the following grammar rule,
where the left of `→` can be rewritten as the right:

    addition → number "+" number

Now, if the parser came across the following sequence of tokens:

    ["7", "+", "108"]

It could say, "7" is a number, "+" is the `+` operator, "108" is another
number, and recognize the addition rule, where the intent is to add the
right number to the left number.

This can be represented as a tree:

      +
    /   \
    7  108

This type of tree is referred to as Abstract Syntax Tree, or AST.

If you take the previous example from the lexer:

    7 + 108 * 9

And the updated grammar:

    addition       → number "+" number
    multiplication → number "*" number

There is a problem. If you do the parsing in order, you end up with the
wrong tree:

        *
      /   \
      +   9
    /   \
    7  108

Or in other words:

    (7 + 108) * 9

To be correct, the grammar must take a concept called "precedence" into
account. Multiplication has precedence over addition.

The trick is to put operations into groups of precedence. For example,
you can put addition into a group called `group 1`, of precedence 1, and
multiplication into a group called `group 2`, of precedence 2:

    expression → group 1
    group 1    → group 2 "+" group 2
    group 2    → primary "*" primary
               | primary
    primary    → NUMBER

Each of these groups only matches (and includes) constructs of the same
precedence level or higher. `group 2` means to `group 1` "whatever
random stuff with higher precedence".

     ------------------------------------------------
    |             ---------------------------------  |
    |            |          ---------------------  | |
    |            |         |          ---------  | | |
    | expression | group 1 | group 2 | primary | | | |
    |            |         |          ---------  | | |
    |            |          ---------------------  | |
    |             ---------------------------------  |
     ------------------------------------------------

For example, you can read the `group 1` rule as "`group 1` is made of
anything that is a `group 2` or of higher precedence than `group 2`,
followed by a `+` sign, followed by anything that is a `group 2` or
higher".

    group 2    group 2
      ---     ---------
     | 7 | + | 108 * 9 |
      ---     ---------

`group 1` is blind to what's inside `group 2`. `group 1` only sees
`group 2`. `group 2` on the other hand sees `primary`:

    primary  primary
     -----     ---
    | 108 | * | 9 |
     -----     ---

Note the two other novelties:

- `expression` is our starting point. It only matches `group 1`, but
  `group 1` being the lowest precedence group, it matches all other
  (higher) groups as well.

- `primary` represents the highest level building blocks. Here these are
  the numbers, but more generally they would also include parentheses,
  keywords (true, false, null, etc.), and identifiers (variable names,
  function names, etc.).

Terminology note: The groups on the left are called "non-terminals",
which represent higher-level constructs. The sequences on the right
are made up of non-terminals and other symbols called "terminals", which
are the basic units or atomic elements of the language (numbers,
operators, parentheses, etc.). The algorithm we are using is called
"recursive descent parsing".

Now if we try and build the tree with the new rules, we end up with the
correct result:

      +
    /   \
    7   *
      /   \
     108  9

Let's break it down:

1. Start at `expression`, playhead at `7`.

    7 + 108 * 9
    ↑

2. `expression` is made up of `group 1`, go to `group 1`.
3. `group 1` is made of the construct `group 2 + group 2`, go to
   `group 2`.
4. `group 2` is either made of `primary * primary` or `primary`, let's
   try the first one, go to `primary`.
5. `primary` matches.
6. Peek ahead, is the next symbol a `*`? No. Try the next possible
   construct: `primary`.
7. `primary` matches return `7`.

    7

8. `group 2` matches (second option), return `7`.

    7

9. We are back at `group 1`, with a tree looking like:

    /
    7

10. Move the playhead to the next token, at `+`. Does it match the
    grammar? Yes. Remember, since `group 1` doesn't have alternate
    constructs (unlike `group 2`) it would be a syntax error if it
    didn't match! We can update the tree.

      +
    /
    7

11. Move the playhead to the next token, at `108`.

    7 + 108 * 9
         ↑

12. The syntax requires a `group 2`, go to `group 2`.
13. Try the first construct. It needs a `primary`, go to `primary`.
14. `primary` matches.

     /
    108

15. Peek ahead, is the next symbol a `*`? Yes.

       *
     /
    108

16. Peek ahead, is the next symbol a `primary`? Yes.

       *
     /   \
    108  9

17. `group 2` matches. Return the subtree and consume the tokens.

    7 + 108 * 9 (end)
                  ↑

18. `group 1` completes. Return the subtree.

      +
    /   \
    7   *
      /   \
     108  9

19. `expression` completes, we have reach the end.

This looks good. But there is a problem. Our grammar cannot parse
expressions like this one:

    7 + 9 + 3

Try it. `group 1` matches `7 + 9`. We are left with:

    group 1 + 3

There is no rule for this. The rule with the `+`, `group 1`, needs a
`group 2`, not a `group 1`. We could adjust the rule like this:

    group 1 → group 1 "+" group 2

Try it. `group 1` needs a `group 1`, a `+` and a `group 2`. So we go
to... `group 1` to look for a match. But `group 1` needs a `group 1`...
We have an infinite loop, and it is called "left-recursion", where the
first non-terminal is the non-terminal itself.

There are multiple ways one can solve this. One of them is to introduce
a `group 1'` (group 1 prime), the group 1 rule becomes:

    group 1  → group 2 group 1'
    group 1' → "+" group 2 group 1'
             |

1. `group 1` needs a `group 2`.
2. `group 2` matches `7` (though `primary`). OK.
3. `group 1` needs a `group 1'`
4. `group 1'` needs a `+`. OK.
5. `group 1'` needs a `group 2'`
6. `group 2` matches `9`. OK.
7. `group 1'` needs a `group 1' (bis)`
8. `group 1' (bis)` needs a `+`. OK.
8. `group 1' (bis)` needs a `group 2`.
6. `group 2` matches `3`. OK.
8. `group 1' (bis)` needs a `group 1' (bis) (bis)`.
9. `group 1' (bis) (bis)` can be empty. OK.

Another way it to rewrite it like this:

    group 1 → group 2 ( "+" group 2 )*

Which you can read "`group 2`, followed by: a `+` and a `group 2`,
an infinite number of times or not at all" (The `*` has the same meaning
as in regular expression).

Let's try it:

1. `group 1` needs a `group 2`.
2. `group 2` matches `7` (though `primary`). OK.
   (while matches: 1)
3. `group 1` needs a `+`. OK.
4. `group 1` needs a `group 2`. OK.
5. `group 2` matches `9`. OK.
   (while matches: 2)
6. `group 1` needs a `+`. OK.
7. `group 1` needs a `group 2`. OK.
8. `group 2` matches `3`. OK.
   (while matches: 3)
9. `group 1` needs a `+`. KO.
   Reason of KO: We have consumed all the tokens.
   (end while)

This second version prevents us from adding extra rules to the grammar.
This is the one this parser uses.

Let's just rename the groups to give them more meaningful names. We'll
call `group 1` `term` (which may later include `-`), and `group 2`
`factor` (which may later include `/`, `//`, `%`).

The final version of our grammar:

    expression → term
    term       → factor ( "+" factor )*
    factor     → primary "*" primary
               | primary
    primary    → NUMBER

The translation to code is pretty straightforward:
- Non-terminals (`group 1`, `group 2`, etc.) translate to functions.
  If you encounter a non-terminal, you call its function.
- Terminals (`+`, `NUMBER`, etc.) match and consume tokens.
- `|` translate to `if/else if/else` statements.
- `*` translate to loops.
"""

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


class LiteralType(enum.Enum):
    NUMBER = "NUMBER"
    STRING = "STRING"
    BOOLEAN = "BOOLEAN"
    NULL = "NULL"


@dataclass(repr=False)
class Expr:
    """Expression."""


@dataclass
class Literal(Expr):
    """Number, string, boolean, null. (e.g., `108`)."""

    literal: TokenType
    value: Union[Decimal, str, bool, None]

    def __repr__(self) -> str:
        class_name: str = self.__class__.__name__
        name: str = self.literal.name
        return f"{class_name}({name}, {self.value!r})"


@dataclass
class GroupExpr(Expr):
    """Parentheses with an expression inside (e.g., `(A + B)`."""

    expr: Expr


@dataclass
class BinaryExpr(Expr):
    """Operator and left/right values to operate on (e.g, `A + B`)."""

    lexpr: Expr
    operator: TokenType
    rexpr: Expr

    def __repr__(self) -> str:
        operator: str = self.operator.value
        return f"Expr({operator}, {self.lexpr!r}, {self.rexpr!r})"


@dataclass
class UnaryExpr(Expr):
    """Operator and right value to operate on (e.g. `-A`)"""

    operator: TokenType
    rexpr: Expr


class Parser:
    """Build Abstract Syntax Tree (AST) from tokens.

    This implementation uses recursive descent.
    """

    def __init__(self) -> None:
        self.tokens: list[Token] = []
        self.ast: list[Expr] = []
        self._pos: int = 0

    def parse(self, tokens: list[Token]) -> list[Expr]:
        self.__init__()
        self.tokens = tokens
        while self._current().type != TokenType.EOF:
            expr: Expr = self._parse_expr()
            self.ast.append(expr)
        return self.ast

    def _parse_expr(self) -> Expr:
        """Parse expression.

        expression → equality
        """
        return self._parse_equality()

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
            lexpr = BinaryExpr(lexpr, operator, rexpr)
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
            lexpr = BinaryExpr(lexpr, operator, rexpr)
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
            lexpr = BinaryExpr(lexpr, operator, rexpr)
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
            lexpr = BinaryExpr(lexpr, operator, rexpr)
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
            lexpr = BinaryExpr(lexpr, operator, rexpr)
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
            return UnaryExpr(operator, rexpr)
        return self._parse_primary()

    def _parse_primary(self) -> Expr:
        """Parse primary.

        primary → NUMBER | STRING | BOOLEAN | NULL
                | "(" expression ")"
        """
        literal: TokenType
        if literal := self._consume_token_if_matches(TokenType.NUMBER):
            value: Decimal = Decimal(self._previous().value)
            return Literal(literal, value)
        if literal := self._consume_token_if_matches(TokenType.STRING):
            string_token_type: TokenType = self._previous(2).type
            if string_token_type == TokenType.STRING:
                value: str = self._previous(2).value
                return Literal(literal, value)
            if string_token_type == TokenType.DBLQUOTE:  # empty string
                return Literal(literal, "")
        if literal := self._consume_token_if_matches(TokenType.LITKEYWORD):
            match self._previous().value:
                case "true":
                    return Literal(literal, True)
                case "false":
                    return Literal(literal, False)
                case "null":
                    return Literal(literal, None)
            raise ParseError(
                ErrorType.SYNTAX,
                f"invalid keyword {self._previous().value!r}",
                self._current(),
            )
        if self._consume_token_if_matches(TokenType.LPAREN):
            expr: Expr = self._parse_expr()
            rparen: TokenType = self._consume_token_if_matches(TokenType.RPAREN)
            if not expr or not rparen:
                raise ParseError(
                    ErrorType.SYNTAX,
                    f"unterminated group expression, missing {')'!r}",
                    self._previous(),
                )
            return GroupExpr(expr)
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
