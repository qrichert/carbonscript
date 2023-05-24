# CarbonScript

## Identifiers

Identifiers must start with an alphabetic character (`a-zA-Z`) or the
underscore character (`_`). This first character may be followed up by
one or more alphabetic characters (`a-zA-Z`), numeric characters (`0-9`)
or the underscore character (`_`).

**Correct:**

- `a`
- `abc`
- `AbC_42`
- `abc_42___`
- `_abc_42`

**Incorrect:**

- `42abc`

## Numbers

Numbers use correctly-rounded decimal floating-point arithmetic
(`0.1 + 0.2 = 0.3`).

There is only one type of number (`42` and `1.618` use the same
underlying data structure).

## Strings

Strings of text are delimiter by double quotes:

- `"hello, world\n"`

## Arithmetic Operators

- `**` Power
- `*` Multiply
- `//` Integer Divide
- `/` Divide
- `%` Modulus
- `+` Plus
- `-` Minus

## Parenthesis

Parenthesis can be used to change the precedence of expressions:
`1 + 2 * 3 = 7`, but `(1 + 2) * 3 = 9`.

## Natural Precedence

| rank | operator            |
| ---- | ------------------- |
| 1    | `(`, `)`            |
| 2    | `+x`, `-x`          |
| 3    | `**`                |
| 4    | `*`, `/`, `//`, `%` |
| 5    | `+`, `-`            |
