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
assignment    → index ( "=" | "+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "**=" ) assignment
              | logic_or
logic_or      → logic_and ( "or" logic_and )*
logic_and     → equality ( "and" equality )*
equality      → comparison ( ( "==" | "!=" ) comparison )*
comparison    → term ( ( ">" | ">=" | "<" | "<=" ) term )*
term          → factor ( ( "+" | "-" ) factor )*
factor        → power ( ( "*" | "/" | "//" | "%" ) power )*
power         → unary ( "**" power )*
unary         → ( "+" | "-" | "!" ) unary
              | index
index         → primary ( list_index )*
primary       → NUMBER | STRING | BOOLEAN | NULL
              | iterable
              | IDENTIFIER
              | "(" expr ")"
iterable      → list
list          → "[" ( expr ( "," expr )* ","? )? "]"

list_index   → "[" expr "]"
```

<!--
TODO:

list_index:
lexer OK
parser OK
interpreter TODO

list:
lexer OK
parser TODO
interpreter TODO

index        → primary ( list_index | dict_index | func_call )*
dict_index   → "{" expr "}"
-->
