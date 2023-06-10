# Language Tour

- [Identifiers](#identifiers)
- [Numbers](#numbers)
- [Strings](#strings)
- [Arithmetic Operators](#arithmetic-operators)
- [Parentheses](#parentheses)
- [Operator Precedence and Associativity](#operator-precedence-and-associativity)
- [Variables and Constants](#variables-and-constants)
- [Scope](#scope)
- [Conditions](#conditions)
- [Loops](#loops)
- [Comments](#comments)
- [Complete Grammar](#complete-grammar)

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

Strings of text are delimited by double quotes:

- `"hello, world\n"`

Double quotes can be escaped:

- `"\"foo\""`

## Arithmetic Operators

- `**` Power
- `*` Multiply
- `//` Integer Divide
- `/` Divide
- `%` Modulus
- `+` Plus
- `-` Minus

## Parentheses

Parentheses can be used to change the precedence of expressions:
`1 + 2 * 3 = 7`, but `(1 + 2) * 3 = 9`.

## Operator Precedence and Associativity

| precedence | name       | operator                                  | associativity |
| ---------- | ---------- | ----------------------------------------- | ------------- |
| 1          | assignment | `=`                                       | right         |
| 2          | equality   | `==`, `!=`                                | left          |
| 3          | comparison | `>`, `>=`, `<`, `<=`                      | left          |
| 4          | term       | `+`, `-`                                  | left          |
| 5          | factor     | `*`, `/`, `//`, `%`                       | left          |
| 6          | power      | `**`                                      | right         |
| 7          | unary      | `+x`, `-x`, `!x`                          | right         |
| 8          | primary    | literals, keywords, `(`, `)`, identifiers | left          |

## Variables and Constants

To declare a variable or a constant, use the `var` and `const` keywords
respectively, with the assignment operator `=`.

To assign to a variable, use the assignment operator `=`, without the
`var` keyword. Assignments can be chained. Assignment is an expression
and has a value, which is the value of the left hand side after the
assignment.

Variables and constants have the exact same behaviour, except constants
can only be initialized, not reassigned.

```coffee
var foo = 3
const bar = foo * 10

# var a  # Error, variables must be initialized.

var a = 1
a = 2  # OK.

const b = 1
# b = 2  # Error, constants cannot be reassigned.

const c = foo = a = 42  # "c", "foo", and "a" now all equal 42.

print(foo = "bar")  # Mutates "foo" and prints "bar".
```

## Scope

Child scopes inherit and can mutate parent scopes. Local redefinition
of a parent scope variable is possible by redeclaring a local variable
with the same name. On the other hand it is not possible to redefine a
local variable.

```coffee
var foo = 42
var bar = 3

if (true)
    print(foo)  # 42
    print(bar)  # 3

    foo = 108  # Mutate "foo".
    var bar = 7  # Redefine "bar".

    var baz = 1.618  # Declare "baz".
    # var baz = 123.0  # Error, redefinition of "baz".

print(foo)  # 108
print(bar)  # 3
# print(baz)  # Error, out of scope.
```

This would be equivalent to the following C code:

```c
int foo = 42;
int bar = 3;

if (true) {
    printf("%d\n", foo); // 42
    printf("%d\n", bar); // 3

    foo = 108; // Mutate "foo".
    int bar = 7; // Redefine "bar".

    float baz = 1.618; // Declare "baz".
    // float baz = 123.0; // Error, redefinition of "baz".
}

printf("%d\n", foo); // 108
printf("%d\n", bar); // 3
// printf("%.3f\n", baz); // Error, out of scope.
```

## Conditions

Conditions use the `if` and `else` keywords. Like in many languages,
`if` can feed into an `else`, to create a synthetic `else if` statement.
The `else` part is optional.

```coffee
if (cond)
    foo = 1

if (cond)
    foo = 1
else
    foo = 2

if (cond)
    foo = 1
else if (cond)
    foo = 1.5
else
    foo = 2
```

## Loops

### `while` Loop

The `while` loop repeats instructions as long as the expression given
as condition evaluates is truthy. If the expression is never truthy, the
instructions will never execute.

The `contine` keyword can be used in a loop to jump to the next
iteration. The `break` jumps out of the loop, ending it completely.

```coffee
while (cond)
    foo = foo + 1

    if (foo == 10)
        continue  # Stop and jump to next iteration.

    if (foo == 20)
        break  # Stop and jump out of the loop.
```

## Comments

There are single line comments `# ...` and multiline comments
`## ... ###`.

```coffee
# This is a single line comment.
const foo = 1  # This one too.

## This is a multiline comment...
   ...which spans two lines. ##
```

## Complete Grammar

```
program       → declaration* EOF


declaration   → var_decl
              | stmt
var_decl      → ( "var" | "const" ) IDENTIFIER "=" expr "\n"


stmt          → expr_stmt
              | if_stmt
              | while_stmt
              | break_stmt
              | continue_stmt
              | block
expr_stmt     → expr "\n"
if_stmt       → "if" "(" expr ")" "\n" block
                ( "else" ( if_stmt | "\n" block ) )?
while_stmt    → "while" "(" expr ")" "\n" block
break_stmt    → "break" "\n"
continue_stmt → "continue" "\n"
block         → INDENT declaration+ DEDENT


expr          → assignment
assignment    → IDENTIFIER "=" assignment
              | equality
equality      → comparison ( ( "==" | "!=" ) comparison )*
comparison    → term ( ( ">" | ">=" | "<" | "<=" ) term )*
term          → factor ( ( "+" | "-" ) factor )*
factor        → power ( ( "*" | "/" | "//" | "%" ) power )*
power         → unary ( "**" power )*
unary         → ( "+" | "-" | "!" ) unary
              | primary
primary       → NUMBER | STRING | BOOLEAN | NULL
              | "(" expr ")"
              | IDENTIFIER
```