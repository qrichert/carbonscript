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

## Operator Precedence and Associativity

| precedence | name       | operator                     | associativity |
| ---------- | ---------- | ---------------------------- | ------------- |
| 1          | equality   | `==`, `!=`                   | left          |
| 2          | comparison | `>`, `>=`, `<`, `<=`         | left          |
| 3          | term       | `+`, `-`                     | left          |
| 4          | factor     | `*`, `/`, `//`, `%`          | left          |
| 5          | power      | `**`                         | right         |
| 6          | unary      | `+x`, `-x`, `!x`             | right         |
| 7          | primary    | literals, keywords, `(`, `)` | left          |

## Grammar

```
expression → equality
equality   → comparison ( ( "==" | "!=" ) comparison )*
comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
term       → factor ( ( "+" | "-" ) factor )*
factor     → power ( ( "*" | "/" | "//" | "%" ) power )*
power      → unary ( "**" power )*
unary      → ( "+" | "-" | "!" ) unary
           | primary
primary    → NUMBER | STRING | BOOLEAN | NULL
           | "(" expression ")"
```

```
function main() {
    a = 2
    b = 4
    return a + b ** a
}

while (true) {
    ...
    for (x in []) {
        ...
    }
}
```

```
function main()
    a = 2
    b = 4
    return a + b ** a

while (true)
    ...
    for (x in [])
        ...
```
