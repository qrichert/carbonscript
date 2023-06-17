# Interpreter Crash Course

- [Lexing](#lexing)
- [Parsing](#parsing)
- [Interpreting](#interpreting)

## Lexing

Lexing is the first step in the process. It consists of recognizing
known patterns (i.e., tokens) in a seemingly random soup of characters
(i.e., input string).

Note: The right word is "lexeme". A token is a lexeme with data attached
to it (type, line, column, etc.). In the implementation we use tokens,
so we'll stick with that word.

For example, if you consider the following input:

```
"7 + 108 * 9"
```

The lexer will break it up into tokens:

```
["7", " ", "+", " ", "108", " ", "*", " ", "9"]
```

Namely, a number, whitespace, a plus symbol, whitespace, another number,
whitespace, the star symbol, whitespace, and a final number.

Note that neither the plus symbol (`+`) nor the star symbol (`*`) are
arithmetic operators yet. They are just tokens, or patterns, that are
used in the language and which the lexer recognizes.

In short, the lexer identifies language patterns in raw input, in
preparation for the next step: parsing.

One way to implement this is with regular expressions. (Doing the
matching by hand works too, but the regular expression engine happens to
be pretty good at matching a regular language, so why not leverage
that).

Start by defining patterns:

```
NUMBER: \d+
WHITESPACE: \s+
PLUS: \+
STAR: \*
```

Then consume the input, match by match:

1. Initial state.

   ```
   input: "7 + 108 * 9"
   tokens: []
   ```

2. Input matches NUMBER? Yes. Extract token.

   ```
   input: " + 108 * 9"
   tokens: ["7"]
   ```

3. Input matches NUMBER? No. WHITESPACE? Yes. Extract token.

   ```
   input: "+ 108 * 9"
   tokens: ["7", " "]
   ```

4. Input matches NUMBER? No. WHITESPACE? No. PLUS? Yes. Extract token.

   ```
   input: " 108 * 9"
   tokens: ["7", " ", "+3]
   ```

And so on...

## Parsing

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

```
addition → number "+" number
```

Now, if the parser came across the following sequence of tokens:

```
["7", "+", "108"]
```

It could say, "7" is a number, "+" is the `+` operator, "108" is another
number, and recognize the addition rule, where the intent is to add the
right number to the left number.

This can be represented as a tree:

```
  +
/   \
7  108
```

This type of tree is referred to as Abstract Syntax Tree, or AST.

If you take the previous example from the lexer:

```
7 + 108 * 9
```

And the updated grammar:

```
addition       → number "+" number
multiplication → number "*" number
```

There is a problem. If you do the parsing in order, you end up with the
wrong tree:

```
    *
  /   \
  +   9
/   \
7  108
```

Or in other words:

```
(7 + 108) * 9
```

To be correct, the grammar must take a concept called "precedence" into
account. Multiplication has precedence over addition.

The trick is to put operations into groups of precedence. For example,
you can put addition into a group called `group 1`, of precedence 1, and
multiplication into a group called `group 2`, of precedence 2:

```
expression → group 1
group 1    → group 2 "+" group 2
group 2    → primary "*" primary
           | primary
primary    → NUMBER
```

Each of these groups only matches (and includes) constructs of the same
precedence level or higher. `group 2` means to `group 1` "whatever
random stuff with higher precedence".

```
 ------------------------------------------------
|             ---------------------------------  |
|            |          ---------------------  | |
|            |         |          ---------  | | |
| expression | group 1 | group 2 | primary | | | |
|            |         |          ---------  | | |
|            |          ---------------------  | |
|             ---------------------------------  |
 ------------------------------------------------
```

For example, you can read the `group 1` rule as "`group 1` is made of
anything that is a `group 2` or of higher precedence than `group 2`,
followed by a `+` sign, followed by anything that is a `group 2` or
higher".

```
group 2    group 2
  ---     ---------
 | 7 | + | 108 * 9 |
  ---     ---------
```

`group 1` is blind to what's inside `group 2`. `group 1` only sees
`group 2`. `group 2` on the other hand sees `primary`:

```
primary  primary
 -----     ---
| 108 | * | 9 |
 -----     ---
```

Note the two other novelties:

- `expression` is our starting point. It only matches `group 1`, but
  `group 1` being the lowest precedence group, it matches all other
  (higher) groups as well.

- `primary` represents the highest level building blocks. Here these are
  the numbers, but more generally they would also include parentheses,
  keywords (true, false, null, etc.), and identifiers (variable names,
  function names, etc.).

Terminology note: The groups on the left are called "non-terminals",
which represent higher-level constructs. The sequences on the right are
made up of non-terminals and other symbols called "terminals", which are
the basic units or atomic elements of the language (numbers, operators,
parentheses, etc.). The algorithm we are using is called "recursive
descent parsing".

Now if we try and build the tree with the new rules, we end up with the
correct result:

```
  +
/   \
7   *
  /   \
 108  9
```

Let's break it down:

1. Start at `expression`, playhead at `7`.

   ```
   7 + 108 * 9
   ↑
   ```

2. `expression` is made up of `group 1`, go to `group 1`.
3. `group 1` is made of the construct `group 2 + group 2`, go to
   `group 2`.
4. `group 2` is either made of `primary * primary` or `primary`, let's
   try the first one, go to `primary`.
5. `primary` matches.
6. Peek ahead, is the next symbol a `*`? No. Try the next possible
   construct: `primary`.
7. `primary` matches, return `7`.

   ```
   7
   ```

8. `group 2` matches (second option), return `7`.

   ```
   7
   ```

9. We are back at `group 1`, with a tree looking like:

   ```
   /
   7
   ```

10. Move the playhead to the next token, at `+`. Does it match the
    grammar? Yes. Remember, since `group 1` doesn't have alternate
    constructs (unlike `group 2`) it would be a syntax error if it
    didn't match! We can update the tree.

    ```
      +
    /
    7
    ```

11. Move the playhead to the next token, at `108`.

    ```
    7 + 108 * 9
         ↑
    ```

12. The syntax requires a `group 2`, go to `group 2`.
13. Try the first construct. It needs a `primary`, go to `primary`.
14. `primary` matches.

    ```
     /
    108
    ```

15. Peek ahead, is the next symbol a `*`? Yes.

    ```
       *
     /
    108
    ```

16. Peek ahead, is the next symbol a `primary`? Yes.

    ```
       *
     /   \
    108  9
    ```

17. `group 2` matches. Return the subtree and consume the tokens.

    ```
    7 + 108 * 9 (end)
                  ↑
    ```

18. `group 1` completes. Return the subtree.

    ```
      +
    /   \
    7   *
      /   \
     108  9
    ```

19. `expression` completes, we have reached the end.

This looks good. But there is a problem. Our grammar cannot parse
expressions like this one:

```
7 + 9 + 3
```

Try it. `group 1` matches `7 + 9`. We are left with:

```
group 1 + 3
```

There is no rule for this. The rule with the `+`, `group 1`, needs a
`group 2`, not a `group 1`. We could adjust the rule like this:

```
group 1 → group 1 "+" group 2
```

Try it. `group 1` needs a `group 1`, a `+` and a `group 2`. So we go
to... `group 1` to look for a match. But `group 1` needs a `group 1`...
We have an infinite loop, and it is called "left-recursion", where the
first non-terminal is the non-terminal itself.

There are multiple ways one can solve this. One of them is to introduce
a `group 1'` (group 1 prime), the group 1 rule becomes:

```
group 1  → group 2 group 1'
group 1' → "+" group 2 group 1'
         |
```

1. `group 1` needs a `group 2`.
2. `group 2` matches `7` (though `primary`). OK.
3. `group 1` needs a `group 1'`
4. `group 1'` needs a `+`. OK.
5. `group 1'` needs a `group 2'`
6. `group 2` matches `9`. OK.
7. `group 1'` needs a `group 1' (bis)`
8. `group 1' (bis)` needs a `+`. OK.
9. `group 1' (bis)` needs a `group 2`.
10. `group 2` matches `3`. OK.
11. `group 1' (bis)` needs a `group 1' (bis) (bis)`.
12. `group 1' (bis) (bis)` can be empty. OK.

Another way it to rewrite it like this:

```
group 1 → group 2 ( "+" group 2 )*
```

Which you can read "`group 2`, followed by: a `+` and a `group 2`, an
infinite number of times or not at all" (The `*` has the same meaning as
in regular expressions).

Let's try it:

1. `group 1` needs a `group 2`.
2. `group 2` matches `7` (though `primary`). OK.
3. (while matches: #1)
   1. `group 1` needs a `+`. OK.
   2. `group 1` needs a `group 2`. OK.
   3. `group 2` matches `9`. OK.
4. (while matches: #2)
   1. `group 1` needs a `+`. OK.
   2. `group 1` needs a `group 2`. OK.
   3. `group 2` matches `3`. OK.
5. (while matches: #3)
   1. `group 1` needs a `+`. KO. Reason of KO: We have consumed all the
      tokens.
6. (end while)

This second version prevents us from adding extra rules to the grammar.
This is the one we will use.

Let's just rename the groups to give them more meaningful names. We'll
call `group 1` `term` (which may later include `-`), and `group 2`
`factor` (which may later include `/`, `//`, `%`).

The final version of our grammar:

```
expression → term
term       → factor ( "+" factor )*
factor     → primary "*" primary
           | primary
primary    → NUMBER
```

The translation to code is pretty straightforward:

- Non-terminals (`group 1`, `group 2`, etc.) translate to functions. If
  you encounter a non-terminal, you call its parse function.
- Terminals (`+`, `NUMBER`, etc.) match and consume tokens.
- `|` translate to `if`/`else if`/`else` statements.
- `+` and `*` translate to `while` loops.
- `?` translate to `if` statements.

## Interpreting

Interpreting is the final step. This is where we traverse the AST, and
compute the nodes, until only the final result is left.

Let's use our freshly parsed tree as an example:

```
  +
/   \
7   *
  /   \
 108  9
```

1. The first node is a binary operation (meaning an operation that takes
   two arguments, not "binary" as in the base-2 numeral system). Call
   the binary operation routine.
2. There is a `+`, so it's an addition.

   ```
   ? + ?
   ```

3. Compute the left value. It is a literal, call the routine that
   returns the value of a literal. It returns `7`.

   ```
   7 + ?
   ```

4. Compute the right value. It is a binary operation. Call the binary
   operation routine (recursively!).
5. There is a `*`, so it's a multiplication.

   ```
   7 + (? * ?)
   ```

6. Compute the left value. It is a literal, call the routine that
   returns the value of a literal. It returns `108`.

   ```
   7 + (108 * ?)
   ```

7. Compute the right value. It is a literal, call the routine that
   returns the value of a literal. It returns `9`.

   ```
   7 + (108 * 9)
   ```

8. Multiply the left operand `108` by the right operand `9`. Return the
   result `972` to the calling routine ("+" binary operation).

   ```
   7 + 972
   ```

9. Add to the left operand `7`, the right operand `972`. Return the
   result `979` to the calling routine.

   ```
   979
   ```

10. We are at the root of the tree with nothing else left to do. The
    final result is `979`.
