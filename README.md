# CarbonScript

CarbonScript is a high-level programming language.

It is primarily designed to be embedded in applications and to face
end-users.

Being a mix of Python and C-style/JavaScripty syntax, it should be quite
easy to pick up.

## First Time Looking at Interpreter Code?

Check out the
[Interpreter Crash Course](docs/interpreter-crash-course.md).

## Language Tour

See [Language Tour](docs/language-tour.md) for quick overview.

You may also read [The Big One](tests/fixtures/the_big_one.cbn).

If you're into that, here is the [Grammar](docs/grammar.md).

This is how CarbonScript looks and feels like:

```coffee
print("hello, world\n")


func hello()
    return "hello"


const foo = 42
var bar = 3

if (foo > bar)
    bar = 7
else if (foo < bar)
    bar = 1.618
else
    bar = 108


while (true)
    for (var i in [1..10:1])
        print(i)
    break
```

## Roadmap

- Make classes out of LiteralValues.
- Dicts `{a:b, c:d}`
- `for` loops
- Ranges `[1..10:1]` (auto-expanded to lists if indexed into).
- Replace unary `!` with `not`.
- Add global scope to Environment.
- Functions
- Classes
- Refactor error handling.
- Standard library (small).
- Python bindings and standard library overrides.
- Resource management/usage limits.
- Refactoring/cleanup
- Optimizations
