# Grammar

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
assignment    → IDENTIFIER ( "=" | "+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "**=" ) assignment
              | logic_or
logic_or      → logic_and ( "or" logic_and )*
logic_and     → equality ( "and" equality )*
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
