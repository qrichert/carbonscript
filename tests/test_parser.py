"""Test parsing module."""

import doctest
import unittest
from decimal import Decimal as D

import carbonscript.parser
from carbonscript.ast import (
    Assign,
    BinOp,
    BinOpRTL,
    Block,
    BreakStmt,
    ConstDecl,
    ContinueStmt,
    Expr,
    ExprStmt,
    Group,
    IfStmt,
    ListIndex,
    Literal,
    LogicOp,
    Stmt,
    Unary,
    VarDecl,
    WhileStmt,
)
from carbonscript.error import ParseError
from carbonscript.lexer import Lexer
from carbonscript.parser import Parser, Preprocessor
from carbonscript.tokens import Token, TokenType


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.parser))
    return tests


def parse_expression(expression: str) -> Expr | None:
    tokens: list[Token] = Lexer().lex(expression)
    statements: list[Stmt] = Parser().parse(tokens)
    if not statements:
        return None
    return statements[0].expression


def parse_script(script: str) -> list[Stmt]:
    tokens: list[Token] = Lexer().lex(script)
    return Parser().parse(tokens)


def preprocess_script(script: str) -> list[Token]:
    tokens: list[Token] = Lexer().lex(script)
    return Preprocessor().preprocess(tokens)


class TestPreprocessor(unittest.TestCase):
    def test_no_side_effects_on_input(self) -> None:
        tokens: list[Token] = [
            Token(TokenType.NUMBER, "3"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.NUMBER, "1"),
            Token(TokenType.EOF),
        ]
        preprocessor = Preprocessor()
        preprocessor.preprocess(tokens)
        self.assertIs(tokens, preprocessor.tokens)

    def test_eliminate_ml_comments(self) -> None:
        tokens = preprocess_script("## hello ##")
        self.assertListEqual(tokens, [Token(TokenType.EOF)])

    def test_eliminate_sl_comments(self) -> None:
        tokens = preprocess_script("# hello")
        self.assertListEqual(tokens, [Token(TokenType.EOF)])

    def test_eliminate_empty_lines(self) -> None:
        tokens = preprocess_script("\n\n\n")
        self.assertListEqual(tokens, [Token(TokenType.EOF)])

    def test_inject_newline_before_end_of_file_if_none(self) -> None:
        self.assertListEqual(
            preprocess_script("foo"),
            [
                Token(TokenType.IDENTIFIER, "foo"),
                Token(TokenType.NEWLINE),  # No value, it is injected.
                Token(TokenType.EOF),
            ],
        )

    def test_no_inject_newline_before_end_of_file_if_any(self) -> None:
        self.assertListEqual(
            preprocess_script("foo\n"),
            [
                Token(TokenType.IDENTIFIER, "foo"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.EOF),
            ],
        )

    def test_multiple_statements(self) -> None:
        self.assertListEqual(
            preprocess_script('"hello, world\n" \n    "123"\n"abc"'),
            [
                Token(TokenType.STRING, "hello, world\n"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.INDENT),
                Token(TokenType.STRING, "123"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.DEDENT),
                Token(TokenType.STRING, "abc"),
                Token(TokenType.NEWLINE),
                Token(TokenType.EOF),
            ],
        )

    def test_empty_lines(self) -> None:
        self.assertListEqual(
            preprocess_script('\n\n\n"hello, world\n" \n\n\n    "123"\n"abc"\n\n\n'),
            preprocess_script('"hello, world\n" \n    "123"\n"abc"\n'),
        )

    def test_multiple_blocks(self) -> None:
        self.assertListEqual(
            preprocess_script("0\n    1\n        2\n            3\n        2\n"),
            [
                Token(TokenType.NUMBER, "0"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.INDENT),
                Token(TokenType.NUMBER, "1"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.INDENT),
                Token(TokenType.NUMBER, "2"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.INDENT),
                Token(TokenType.NUMBER, "3"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.DEDENT),
                Token(TokenType.NUMBER, "2"),
                Token(TokenType.NEWLINE, "\n"),
                Token(TokenType.DEDENT),
                Token(TokenType.DEDENT),
                Token(TokenType.EOF),
            ],
        )

    def test_cannot_ident_more_than_one_block(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            preprocess_script("0\n        1\n"),
        self.assertEqual(ctx.exception.token, Token(TokenType.NUMBER, "1"))


class TestParser(unittest.TestCase):
    def test_no_side_effects_on_input(self) -> None:
        tokens: list[Token] = [
            Token(TokenType.NUMBER, "3"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.NUMBER, "1"),
            Token(TokenType.EOF),
        ]
        parser = Parser()
        parser.parse(tokens)
        self.assertIs(tokens, parser.tokens)

    def test_ml_comment(self) -> None:
        self.assertEqual(
            parse_expression("## foo \n bar ##"),
            None,
        )

    def test_ml_comment_with_newline_after(self) -> None:
        self.assertEqual(
            parse_expression("## foo \n bar ##\n"),
            None,
        )

    def test_ml_comment_with_whitespace_before(self) -> None:
        self.assertEqual(
            parse_expression("  ## foo \n bar ##"),
            None,
        )

    def test_ml_comment_with_block_before(self) -> None:
        self.assertEqual(
            parse_expression("    ## foo \n bar ##"),
            None,
        )

    def test_ml_comment_before_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("## foo ## 2+12")
        self.assertEqual(ctx.exception.token, Token(TokenType.MLCOMMENT, "## foo ##"))

    def test_ml_comment_inside_expression(self) -> None:
        self.assertEqual(
            parse_expression("2+ ## foo ## 12"),
            BinOp(
                Literal(TokenType.NUMBER, D("2")),
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("12")),
            ),
        )

    def test_ml_comment_after_expression(self) -> None:
        self.assertEqual(
            parse_expression("2+12 ## foo ##"),
            BinOp(
                Literal(TokenType.NUMBER, D("2")),
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("12")),
            ),
        )

    def test_sl_comment(self) -> None:
        self.assertEqual(
            parse_expression("## foo"),
            None,
        )

    def test_sl_comment_with_newline_after(self) -> None:
        self.assertEqual(
            parse_expression("## foo\n"),
            None,
        )

    def test_sl_comment_with_whitespace_before(self) -> None:
        self.assertEqual(
            parse_expression("  # foo"),
            None,
        )

    def test_sl_comment_with_block_before(self) -> None:
        self.assertEqual(
            parse_expression("    # foo"),
            None,
        )

    def test_sl_comment_after_expression(self) -> None:
        self.assertEqual(
            parse_expression("2+12 # foo"),
            BinOp(
                Literal(TokenType.NUMBER, D("2")),
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("12")),
            ),
        )

    def test_statements_can_only_be_preceded_by_whitespace(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("## comment ## var i = 42")
        self.assertEqual(
            ctx.exception.token, Token(TokenType.MLCOMMENT, "## comment ##")
        )

    def test_statements_can_only_be_preceded_by_spaces(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("fake_stmt\n\tvar i = 42")
        self.assertEqual(ctx.exception.token, Token(TokenType.WHITESPACE, "\t"))

    def test_block(self) -> None:
        self.assertListEqual(
            parse_script("if (true)\n    foo\nbar"),
            [
                IfStmt(
                    Literal(TokenType.LITKW, True),
                    Block([ExprStmt(Literal(TokenType.IDENTIFIER, "foo"))]),
                    None,
                ),
                ExprStmt(Literal(TokenType.IDENTIFIER, "bar")),
            ],
        )

    def test_first_statement_can_be_indented(self) -> None:
        self.assertEqual(
            parse_script("    var i = 42"),
            [
                Block(
                    [
                        VarDecl(
                            Literal(TokenType.IDENTIFIER, "i"),
                            Literal(TokenType.NUMBER, D("42")),
                        )
                    ]
                )
            ],
        )

    def test_indent_must_be_a_multiple_of_four_spaces(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("if (true)\n   var i = 42")
        self.assertEqual(ctx.exception.token, Token(TokenType.WHITESPACE, "   "))

        with self.assertRaises(ParseError) as ctx:
            parse_script("if (true)\n     var i = 42")
        self.assertEqual(ctx.exception.token, Token(TokenType.WHITESPACE, "     "))

        self.assertEqual(
            parse_script("if (true)\n    var i = 42"),
            [
                IfStmt(
                    Literal(TokenType.LITKW, True),
                    Block(
                        [
                            VarDecl(
                                Literal(TokenType.IDENTIFIER, "i"),
                                Literal(TokenType.NUMBER, D("42")),
                            )
                        ]
                    ),
                    None,
                ),
            ],
        )

    def test_cannot_indent_multiple_blocks_at_once(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("if (true)\n        const foo = bar")
        self.assertEqual(ctx.exception.token, Token(TokenType.DECLKW, "const"))


class TestStatements(unittest.TestCase):
    def test_empty(self) -> None:
        self.assertListEqual(
            parse_script(""),
            [],
        )

    def test_multiple_statements(self) -> None:
        self.assertListEqual(
            parse_script('"hello, world\n" \n    "123"\n"abc"'),
            [
                ExprStmt(Literal(TokenType.STRING, "hello, world\n")),
                Block([ExprStmt(Literal(TokenType.STRING, "123"))]),
                ExprStmt(Literal(TokenType.STRING, "abc")),
            ],
        )

    def test_multiple_statements_on_single_line(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1+2 3+4")
        self.assertEqual(ctx.exception.token, Token(TokenType.NUMBER, "3"))

    def test_empty_lines(self) -> None:
        self.assertListEqual(
            parse_script('\n\n\n"hello, world\n" \n    "123"\n"abc"\n\n\n'),
            [
                ExprStmt(Literal(TokenType.STRING, "hello, world\n")),
                Block([ExprStmt(Literal(TokenType.STRING, "123"))]),
                ExprStmt(Literal(TokenType.STRING, "abc")),
            ],
        )

    def test_newline_at_end_of_file(self) -> None:
        self.assertListEqual(
            parse_script('"newline ->"\n'),
            [
                ExprStmt(Literal(TokenType.STRING, "newline ->")),
            ],
        )

    def test_newline_at_end_of_file_no_newline(self) -> None:
        self.assertListEqual(
            parse_script('"no newline ->"'),
            [
                ExprStmt(Literal(TokenType.STRING, "no newline ->")),
            ],
        )

    def test_declaration_var(self) -> None:
        self.assertListEqual(
            parse_script("var a = 12 * 9"),
            [
                VarDecl(
                    Literal(TokenType.IDENTIFIER, "a"),
                    BinOp(
                        Literal(TokenType.NUMBER, D("12")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("9")),
                    ),
                )
            ],
        )

    def test_declaration_var_without_value(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("var foo"),
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_declaration_var_assigning_to_constant(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("var 2 = 3"),
        self.assertEqual(ctx.exception.token, Token(TokenType.NUMBER, "2"))

    def test_declaration_var_assigning_to_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("var a + b = c"),
        self.assertEqual(ctx.exception.token, Token(TokenType.PLUS, "+"))

    def test_declaration_const(self) -> None:
        self.assertListEqual(
            parse_script("const a = 12 * 9"),
            [
                ConstDecl(
                    Literal(TokenType.IDENTIFIER, "a"),
                    BinOp(
                        Literal(TokenType.NUMBER, D("12")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("9")),
                    ),
                )
            ],
        )

    def test_declaration_const_without_value(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const foo"),
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_declaration_const_assigning_to_constant(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const 2 = 3"),
        self.assertEqual(ctx.exception.token, Token(TokenType.NUMBER, "2"))

    def test_declaration_const_assigning_to_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const a + b = c"),
        self.assertEqual(ctx.exception.token, Token(TokenType.PLUS, "+"))

    def test_declaration_multiple_declarations_on_single_line(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const a = 2+2 var b = 3+3")
        self.assertEqual(ctx.exception.token, Token(TokenType.DECLKW, "var"))

    def test_declaration_assigning_multiple_expressions(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("const a = 2+2 (3+3)")
        self.assertEqual(ctx.exception.token, Token(TokenType.LPAREN, "("))

    def test_if_stmt_if_then(self) -> None:
        self.assertListEqual(
            parse_script("if (true)\n    2 + 2"),
            [
                IfStmt(
                    Literal(TokenType.LITKW, True),
                    Block(
                        [
                            ExprStmt(
                                BinOp(
                                    Literal(TokenType.NUMBER, D("2")),
                                    TokenType.PLUS,
                                    Literal(TokenType.NUMBER, D("2")),
                                )
                            ),
                        ]
                    ),
                    None,
                )
            ],
        )

    def test_if_stmt_if_then_else(self) -> None:
        self.assertListEqual(
            parse_script("if (true)\n    2 + 2\nelse\n    4 + 4"),
            [
                IfStmt(
                    Literal(TokenType.LITKW, True),
                    Block(
                        [
                            ExprStmt(
                                BinOp(
                                    Literal(TokenType.NUMBER, D("2")),
                                    TokenType.PLUS,
                                    Literal(TokenType.NUMBER, D("2")),
                                )
                            ),
                        ]
                    ),
                    Block(
                        [
                            ExprStmt(
                                BinOp(
                                    Literal(TokenType.NUMBER, D("4")),
                                    TokenType.PLUS,
                                    Literal(TokenType.NUMBER, D("4")),
                                )
                            ),
                        ]
                    ),
                )
            ],
        )

    def test_if_stmt_if_then_else_if_else(self) -> None:
        self.assertListEqual(
            parse_script(
                "if (true)\n    2 + 2\nelse if (false)\n    3 + 3\nelse\n    4 + 4"
            ),
            [
                IfStmt(
                    Literal(TokenType.LITKW, True),
                    Block(
                        [
                            ExprStmt(
                                BinOp(
                                    Literal(TokenType.NUMBER, D("2")),
                                    TokenType.PLUS,
                                    Literal(TokenType.NUMBER, D("2")),
                                )
                            ),
                        ]
                    ),
                    IfStmt(
                        Literal(TokenType.LITKW, False),
                        Block(
                            [
                                ExprStmt(
                                    BinOp(
                                        Literal(TokenType.NUMBER, D("3")),
                                        TokenType.PLUS,
                                        Literal(TokenType.NUMBER, D("3")),
                                    )
                                ),
                            ]
                        ),
                        Block(
                            [
                                ExprStmt(
                                    BinOp(
                                        Literal(TokenType.NUMBER, D("4")),
                                        TokenType.PLUS,
                                        Literal(TokenType.NUMBER, D("4")),
                                    )
                                ),
                            ]
                        ),
                    ),
                )
            ],
        )

    def test_if_stmt_nested(self) -> None:
        self.assertListEqual(
            parse_script(
                "if (true)\n    if (false)\n        foo\n    else\n        bar"
            ),
            [
                IfStmt(
                    Literal(TokenType.LITKW, True),
                    Block(
                        [
                            IfStmt(
                                Literal(TokenType.LITKW, False),
                                Block(
                                    [
                                        ExprStmt(Literal(TokenType.IDENTIFIER, "foo")),
                                    ]
                                ),
                                Block(
                                    [
                                        ExprStmt(Literal(TokenType.IDENTIFIER, "bar")),
                                    ]
                                ),
                            )
                        ]
                    ),
                    None,
                ),
            ],
        )

    def test_if_stmt_missing_opening_parenthesis(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("if true)\n    const foo = 1")
        self.assertEqual(ctx.exception.token, Token(TokenType.LITKW, "true"))

    def test_if_stmt_missing_closing_parenthesis(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("if (true\n    const foo = 1")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE, "\n"))

    def test_if_stmt_missing_newline(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("if (true)    const foo = 1")
        self.assertEqual(ctx.exception.token, Token(TokenType.DECLKW, "const"))

    def test_if_stmt_missing_block(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("if (true)\nconst foo = 1")
        self.assertEqual(ctx.exception.token, Token(TokenType.DECLKW, "const"))

    def test_if_stmt_missing_newline_after_else(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("if (true)\n    const foo = 1\nelse    var foo = 2")
        self.assertEqual(ctx.exception.token, Token(TokenType.DECLKW, "var"))

    def test_if_stmt_missing_block_after_else(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("if (true)\n    const foo = 1\nelse\nvar foo = 2")
        self.assertEqual(ctx.exception.token, Token(TokenType.DECLKW, "var"))

    def test_while_stmt(self) -> None:
        self.assertListEqual(
            parse_script("while (true)\n    bar"),
            [
                WhileStmt(
                    Literal(TokenType.LITKW, True),
                    Block([ExprStmt(Literal(TokenType.IDENTIFIER, "bar"))]),
                )
            ],
        )

    def test_while_stmt_missing_opening_parenthesis(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("while true)\n    const foo = 1")
        self.assertEqual(ctx.exception.token, Token(TokenType.LITKW, "true"))

    def test_while_stmt_missing_closing_parenthesis(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("while (true\n    const foo = 1")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE, "\n"))

    def test_while_stmt_missing_newline(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("while (true)    const foo = 1")
        self.assertEqual(ctx.exception.token, Token(TokenType.DECLKW, "const"))

    def test_while_stmt_missing_block(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("while (true)\nconst foo = 1")
        self.assertEqual(ctx.exception.token, Token(TokenType.DECLKW, "const"))

    def test_break_stmt(self) -> None:
        self.assertListEqual(
            parse_script("while (true)\n    break"),
            [
                WhileStmt(
                    Literal(TokenType.LITKW, True),
                    Block([BreakStmt()]),
                )
            ],
        )

    def test_break_stmt_outside_of_loop(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("break")
        self.assertEqual(ctx.exception.token, Token(TokenType.BREAK, "break"))

    def test_break_stmt_missing_newline(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("while (true)\n    break foo")
        self.assertEqual(ctx.exception.token, Token(TokenType.IDENTIFIER, "foo"))

    def test_continue_stmt(self) -> None:
        self.assertListEqual(
            parse_script("while (true)\n    continue"),
            [
                WhileStmt(
                    Literal(TokenType.LITKW, True),
                    Block([ContinueStmt()]),
                )
            ],
        )

    def test_continue_stmt_outside_of_loop(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("continue")
        self.assertEqual(ctx.exception.token, Token(TokenType.CONTINUE, "continue"))

    def test_continue_stmt_missing_newline(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_script("while (true)\n    continue foo")
        self.assertEqual(ctx.exception.token, Token(TokenType.IDENTIFIER, "foo"))


class TestLiterals(unittest.TestCase):
    def test_number_integer(self) -> None:
        self.assertEqual(
            parse_expression("3"),
            Literal(TokenType.NUMBER, D("3")),
        )

    def test_number_decimal(self) -> None:
        self.assertEqual(
            parse_expression("1.618"),
            Literal(TokenType.NUMBER, D("1.618")),
        )

    def test_string(self) -> None:
        self.assertEqual(
            parse_expression('"hello, world\n"'),
            Literal(TokenType.STRING, "hello, world\n"),
        )

    def test_string_empty(self) -> None:
        self.assertEqual(
            parse_expression('""'),
            Literal(TokenType.STRING, ""),
        )

    def test_string_unterminated(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression('"foo')
        self.assertEqual(ctx.exception.token, Token(TokenType.STRING, '"foo'))

    def test_string_unterminated_empty(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression('"')
        self.assertEqual(ctx.exception.token, Token(TokenType.STRING, '"'))

    def test_string_double_quote_escaped(self) -> None:
        self.assertEqual(
            parse_expression(r'"\"foo\""'),
            Literal(TokenType.STRING, r"\"foo\""),
        )

    def test_string_double_quote_escaped_multiple_odd(self) -> None:
        self.assertEqual(
            parse_expression(r'"\\\"foo\\\""'),
            Literal(TokenType.STRING, r"\\\"foo\\\""),
        )

    def test_string_double_quote_escaped_multiple_even(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression(r'"\\"foo\\""'),
        self.assertEqual(ctx.exception.token, Token(TokenType.IDENTIFIER, "foo"))

    def test_literal_keyword_true(self) -> None:
        self.assertEqual(
            parse_expression("true"),
            Literal(TokenType.LITKW, True),
        )

    def test_literal_keyword_false(self) -> None:
        self.assertEqual(
            parse_expression("false"),
            Literal(TokenType.LITKW, False),
        )

    def test_literal_keyword_null(self) -> None:
        self.assertEqual(
            parse_expression("null"),
            Literal(TokenType.LITKW, None),
        )


class TestIterables(unittest.TestCase):
    def test_list_empty(self) -> None:
        self.assertEqual(
            parse_expression("[]"),
            Literal(TokenType.LSQBRACKET, []),
        )

    def test_list_one_element(self) -> None:
        self.assertEqual(
            parse_expression("[1]"),
            Literal(
                TokenType.LSQBRACKET,
                [
                    Literal(TokenType.NUMBER, D("1")),
                ],
            ),
        )

    def test_list_one_element_trailing_comma(self) -> None:
        self.assertEqual(
            parse_expression("[1,]"),
            Literal(
                TokenType.LSQBRACKET,
                [
                    Literal(TokenType.NUMBER, D("1")),
                ],
            ),
        )

    def test_list_one_element_multiple_trailing_commas(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("[1,,]"),
        self.assertEqual(ctx.exception.token, Token(TokenType.COMMA, ","))

    def test_list_multiple_elements(self) -> None:
        self.assertEqual(
            parse_expression("[1,2]"),
            Literal(
                TokenType.LSQBRACKET,
                [
                    Literal(TokenType.NUMBER, D("1")),
                    Literal(TokenType.NUMBER, D("2")),
                ],
            ),
        )

    def test_list_multiple_elements_trailing_comma(self) -> None:
        self.assertEqual(
            parse_expression("[1,2,]"),
            Literal(
                TokenType.LSQBRACKET,
                [
                    Literal(TokenType.NUMBER, D("1")),
                    Literal(TokenType.NUMBER, D("2")),
                ],
            ),
        )

    def test_list_multiple_elements_multiple_trailing_commas(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("[1,2,,]"),
        self.assertEqual(ctx.exception.token, Token(TokenType.COMMA, ","))

    def test_list_with_expressions(self) -> None:
        self.assertEqual(
            parse_expression("[1+1, (2+2), foo]"),
            Literal(
                TokenType.LSQBRACKET,
                [
                    BinOp(
                        Literal(TokenType.NUMBER, D("1")),
                        TokenType.PLUS,
                        Literal(TokenType.NUMBER, D("1")),
                    ),
                    Group(
                        BinOp(
                            Literal(TokenType.NUMBER, D("2")),
                            TokenType.PLUS,
                            Literal(TokenType.NUMBER, D("2")),
                        ),
                    ),
                    Literal(TokenType.IDENTIFIER, "foo"),
                ],
            ),
        )

    def test_list_nested(self) -> None:
        self.assertEqual(
            parse_expression("[[1], [[2], 2]]"),
            Literal(
                TokenType.LSQBRACKET,
                [
                    Literal(TokenType.LSQBRACKET, [Literal(TokenType.NUMBER, D("1"))]),
                    Literal(
                        TokenType.LSQBRACKET,
                        [
                            Literal(
                                TokenType.LSQBRACKET,
                                [
                                    Literal(TokenType.NUMBER, D("2")),
                                ],
                            ),
                            Literal(TokenType.NUMBER, D("2")),
                        ],
                    ),
                ],
            ),
        )


class TestExpressions(unittest.TestCase):
    def test_with_whitespace(self) -> None:
        self.assertEqual(
            parse_expression("3   *   6   *   9 "),
            BinOp(
                BinOp(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("6")),
                ),
                TokenType.STAR,
                Literal(TokenType.NUMBER, D("9")),
            ),
        )

    def test_invalid_operator(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1°3")
        self.assertEqual(ctx.exception.token, Token(TokenType.UNKNOWN, "°"))

    def test_assignment(self) -> None:
        self.assertEqual(
            parse_expression("a=12*9"),
            Assign(
                Literal(TokenType.IDENTIFIER, "a"),
                BinOp(
                    Literal(TokenType.NUMBER, D("12")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("9")),
                ),
            ),
        )

    def test_assignment_chained(self) -> None:
        self.assertEqual(
            parse_expression("a=b=3"),
            Assign(
                Literal(TokenType.IDENTIFIER, "a"),
                Assign(
                    Literal(TokenType.IDENTIFIER, "b"),
                    Literal(TokenType.NUMBER, D("3")),
                ),
            ),
        )

    def test_assignment_assigning_to_constant(self) -> None:
        # Syntax error, but checked at runtime since expressions
        # evaluating to lvalues are allowed. Should be parsed just fine.
        self.assertEqual(
            parse_expression("2=3"),
            Assign(
                Literal(TokenType.NUMBER, D("2")),
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_assignment_assigning_to_expression(self) -> None:
        # Syntax error, but checked at runtime since expressions
        # evaluating to lvalues are allowed. Should be parsed just fine.
        self.assertEqual(
            parse_expression("a+b=c"),
            Assign(
                BinOp(
                    Literal(TokenType.IDENTIFIER, "a"),
                    TokenType.PLUS,
                    Literal(TokenType.IDENTIFIER, "b"),
                ),
                Literal(TokenType.IDENTIFIER, "c"),
            ),
        )

    def test_assignment_multiple_assignments_on_single_line(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("a = 2+2 b = 3+3")
        self.assertEqual(ctx.exception.token, Token(TokenType.IDENTIFIER, "b"))

    def test_assignment_assigning_multiple_expressions(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("a = 2+2 (3+3)")
        self.assertEqual(ctx.exception.token, Token(TokenType.LPAREN, "("))

    def test_assignment_to_list(self) -> None:
        self.assertEqual(
            parse_expression("a[12]=12*9"),
            Assign(
                ListIndex(
                    Literal(TokenType.IDENTIFIER, "a"),
                    Literal(TokenType.NUMBER, D("12")),
                ),
                BinOp(
                    Literal(TokenType.NUMBER, D("12")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("9")),
                ),
            ),
        )

    def test_in_place_operation_multiple_expressions(self) -> None:
        self.assertEqual(
            parse_expression("a += 2*2"),
            Assign(
                Literal(TokenType.IDENTIFIER, "a"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "a"),
                    TokenType.PLUS,
                    BinOp(
                        Literal(TokenType.NUMBER, D("2")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("2")),
                    ),
                ),
            ),
        )

    def test_in_place_operation_with_assignment(self) -> None:
        self.assertEqual(
            parse_expression("a += b = 2"),
            Assign(
                Literal(TokenType.IDENTIFIER, "a"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "a"),
                    TokenType.PLUS,
                    Assign(
                        Literal(TokenType.IDENTIFIER, "b"),
                        Literal(TokenType.NUMBER, D("2")),
                    ),
                ),
            ),
        )

    def test_in_place_operation_chained(self) -> None:
        self.assertEqual(
            parse_expression("a += b += b = 1"),
            Assign(
                Literal(TokenType.IDENTIFIER, "a"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "a"),
                    TokenType.PLUS,
                    Assign(
                        Literal(TokenType.IDENTIFIER, "b"),
                        BinOpRTL(
                            Literal(TokenType.IDENTIFIER, "b"),
                            TokenType.PLUS,
                            Assign(
                                Literal(TokenType.IDENTIFIER, "b"),
                                Literal(TokenType.NUMBER, D("1")),
                            ),
                        ),
                    ),
                ),
            ),
        )

    def test_logic_or(self) -> None:
        self.assertEqual(
            parse_expression("a or b"),
            LogicOp(
                Literal(TokenType.IDENTIFIER, "a"),
                TokenType.OR,
                Literal(TokenType.IDENTIFIER, "b"),
            ),
        )

    def test_logic_or_chained(self) -> None:
        self.assertEqual(
            parse_expression("a or b or 3"),
            LogicOp(
                LogicOp(
                    Literal(TokenType.IDENTIFIER, "a"),
                    TokenType.OR,
                    Literal(TokenType.IDENTIFIER, "b"),
                ),
                TokenType.OR,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_logic_or_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1or")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_logic_and(self) -> None:
        self.assertEqual(
            parse_expression("a and b"),
            LogicOp(
                Literal(TokenType.IDENTIFIER, "a"),
                TokenType.AND,
                Literal(TokenType.IDENTIFIER, "b"),
            ),
        )

    def test_logic_and_chained(self) -> None:
        self.assertEqual(
            parse_expression("a and b and 3"),
            LogicOp(
                LogicOp(
                    Literal(TokenType.IDENTIFIER, "a"),
                    TokenType.AND,
                    Literal(TokenType.IDENTIFIER, "b"),
                ),
                TokenType.AND,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_logic_and_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1and")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_logic_and_or_precedence(self) -> None:
        self.assertEqual(
            parse_expression("1+1 or 2-2 and 3*3"),
            LogicOp(
                BinOp(
                    Literal(TokenType.NUMBER, D("1")),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("1")),
                ),
                TokenType.OR,
                LogicOp(
                    BinOp(
                        Literal(TokenType.NUMBER, D("2")),
                        TokenType.MINUS,
                        Literal(TokenType.NUMBER, D("2")),
                    ),
                    TokenType.AND,
                    BinOp(
                        Literal(TokenType.NUMBER, D("3")),
                        TokenType.STAR,
                        Literal(TokenType.NUMBER, D("3")),
                    ),
                ),
            ),
        )

    def test_equality_equal(self) -> None:
        self.assertEqual(
            parse_expression("1==3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.DBLEQUAL,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_equality_not_equal(self) -> None:
        self.assertEqual(
            parse_expression("1!=3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.BANGEQUAL,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_equality_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1==")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_comparison_greater_than(self) -> None:
        self.assertEqual(
            parse_expression("1>3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.GT,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_comparison_greater_than_or_equal(self) -> None:
        self.assertEqual(
            parse_expression("1>=3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.GTE,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_comparison_lesser_than(self) -> None:
        self.assertEqual(
            parse_expression("1<3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.LT,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_comparison_lesser_than_or_equal(self) -> None:
        self.assertEqual(
            parse_expression("1<=3"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.LTE,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_comparison_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1>=")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_term_addition(self) -> None:
        self.assertEqual(
            parse_expression("3+1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_term_subtraction(self) -> None:
        self.assertEqual(
            parse_expression("3-1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.MINUS,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_term_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1+")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_factor_multiplication(self) -> None:
        self.assertEqual(
            parse_expression("3*1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.STAR,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_factor_division(self) -> None:
        self.assertEqual(
            parse_expression("3/1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.SLASH,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_factor_integer_division(self) -> None:
        self.assertEqual(
            parse_expression("3//1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.DBLSLASH,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_factor_modulo(self) -> None:
        self.assertEqual(
            parse_expression("3%1"),
            BinOp(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.PERCENT,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_factor_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1*")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_power(self) -> None:
        self.assertEqual(
            parse_expression("3**1"),
            BinOpRTL(
                Literal(TokenType.NUMBER, D("3")),
                TokenType.DBLSTAR,
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_power_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1**")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_in_place_term_addition(self) -> None:
        self.assertEqual(
            parse_expression("foo+=1"),
            Assign(
                Literal(TokenType.IDENTIFIER, "foo"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "foo"),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("1")),
                ),
            ),
        )

    def test_in_place_term_subtraction(self) -> None:
        self.assertEqual(
            parse_expression("foo-=1"),
            Assign(
                Literal(TokenType.IDENTIFIER, "foo"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "foo"),
                    TokenType.MINUS,
                    Literal(TokenType.NUMBER, D("1")),
                ),
            ),
        )

    def test_in_place_term_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1-=")
        self.assertEqual(ctx.exception.token, Token(TokenType.MINUSEQ, "-="))

    def test_in_place_factor_multiplication(self) -> None:
        self.assertEqual(
            parse_expression("foo*=1"),
            Assign(
                Literal(TokenType.IDENTIFIER, "foo"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "foo"),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("1")),
                ),
            ),
        )

    def test_in_place_factor_division(self) -> None:
        self.assertEqual(
            parse_expression("foo/=1"),
            Assign(
                Literal(TokenType.IDENTIFIER, "foo"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "foo"),
                    TokenType.SLASH,
                    Literal(TokenType.NUMBER, D("1")),
                ),
            ),
        )

    def test_in_place_factor_integer_division(self) -> None:
        self.assertEqual(
            parse_expression("foo//=1"),
            Assign(
                Literal(TokenType.IDENTIFIER, "foo"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "foo"),
                    TokenType.DBLSLASH,
                    Literal(TokenType.NUMBER, D("1")),
                ),
            ),
        )

    def test_in_place_factor_modulo(self) -> None:
        self.assertEqual(
            parse_expression("foo%=1"),
            Assign(
                Literal(TokenType.IDENTIFIER, "foo"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "foo"),
                    TokenType.PERCENT,
                    Literal(TokenType.NUMBER, D("1")),
                ),
            ),
        )

    def test_in_place_factor_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1/=")
        self.assertEqual(ctx.exception.token, Token(TokenType.SLASHEQ, "/="))

    def test_in_place_power(self) -> None:
        self.assertEqual(
            parse_expression("foo**=1"),
            Assign(
                Literal(TokenType.IDENTIFIER, "foo"),
                BinOpRTL(
                    Literal(TokenType.IDENTIFIER, "foo"),
                    TokenType.DBLSTAR,
                    Literal(TokenType.NUMBER, D("1")),
                ),
            ),
        )

    def test_in_place_power_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("1**=")
        self.assertEqual(ctx.exception.token, Token(TokenType.DBLSTAREQ, "**="))

    def test_unary_plus(self) -> None:
        self.assertEqual(
            parse_expression("+42"),
            Unary(
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("42")),
            ),
        )

    def test_unary_minus(self) -> None:
        self.assertEqual(
            parse_expression("-42"),
            Unary(
                TokenType.MINUS,
                Literal(TokenType.NUMBER, D("42")),
            ),
        )

    def test_unary_bang(self) -> None:
        self.assertEqual(
            parse_expression("!42"),
            Unary(
                TokenType.BANG,
                Literal(TokenType.NUMBER, D("42")),
            ),
        )

    def test_unary_missing_right_part_of_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("+")
        self.assertEqual(ctx.exception.token, Token(TokenType.NEWLINE))

    def test_list_index(self) -> None:
        self.assertEqual(
            parse_expression("foo[3]"),
            ListIndex(
                Literal(TokenType.IDENTIFIER, "foo"),
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_list_index_with_expression(self) -> None:
        self.assertEqual(
            parse_expression("(1+2)[3+4]"),
            ListIndex(
                Group(
                    BinOp(
                        Literal(TokenType.NUMBER, D("1")),
                        TokenType.PLUS,
                        Literal(TokenType.NUMBER, D("2")),
                    )
                ),
                BinOp(
                    Literal(TokenType.NUMBER, D("3")),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("4")),
                ),
            ),
        )

    def test_list_index_with_expression_precedence(self) -> None:
        self.assertEqual(
            parse_expression("1+2[3+4]"),
            BinOp(
                Literal(TokenType.NUMBER, D("1")),
                TokenType.PLUS,
                ListIndex(
                    Literal(TokenType.NUMBER, D("2")),
                    BinOp(
                        Literal(TokenType.NUMBER, D("3")),
                        TokenType.PLUS,
                        Literal(TokenType.NUMBER, D("4")),
                    ),
                ),
            ),
        )

    def test_list_index_chained(self) -> None:
        self.assertEqual(
            parse_expression("foo[3][2][1]"),
            ListIndex(
                ListIndex(
                    ListIndex(
                        Literal(TokenType.IDENTIFIER, "foo"),
                        Literal(TokenType.NUMBER, D("3")),
                    ),
                    Literal(TokenType.NUMBER, D("2")),
                ),
                Literal(TokenType.NUMBER, D("1")),
            ),
        )

    def test_list_index_empty(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("foo[]")
        self.assertEqual(ctx.exception.token, Token(TokenType.RSQBRACKET, "]"))

    def test_list_index_unterminated(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("foo[1")
        self.assertEqual(ctx.exception.token, Token(TokenType.NUMBER, "1"))

    def test_list_index_empty_and_unterminated(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("foo[")
        self.assertEqual(ctx.exception.token, Token(TokenType.LSQBRACKET, "["))

    def test_list_invalid_expression(self) -> None:
        with self.assertRaises(ParseError) as ctx:
            parse_expression("[°]")
        self.assertEqual(ctx.exception.token, Token(TokenType.UNKNOWN, "°"))

    def test_parenthesis(self) -> None:
        self.assertEqual(
            parse_expression("(7+108)"),
            Group(
                BinOp(
                    Literal(TokenType.NUMBER, D("7")),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("108")),
                )
            ),
        )

    def test_precedence(self) -> None:
        self.assertEqual(
            parse_expression("7+108*9"),
            BinOp(
                Literal(TokenType.NUMBER, D("7")),
                TokenType.PLUS,
                BinOp(
                    Literal(TokenType.NUMBER, D("108")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("9")),
                ),
            ),
        )

    def test_term_associativity(self) -> None:
        self.assertEqual(
            parse_expression("7+9+3"),
            BinOp(
                BinOp(
                    Literal(TokenType.NUMBER, D("7")),
                    TokenType.PLUS,
                    Literal(TokenType.NUMBER, D("9")),
                ),
                TokenType.PLUS,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_factor_associativity(self) -> None:
        self.assertEqual(
            parse_expression("7*9*3"),
            BinOp(
                BinOp(
                    Literal(TokenType.NUMBER, D("7")),
                    TokenType.STAR,
                    Literal(TokenType.NUMBER, D("9")),
                ),
                TokenType.STAR,
                Literal(TokenType.NUMBER, D("3")),
            ),
        )

    def test_power_associativity(self) -> None:
        self.assertEqual(
            parse_expression("7**9**3"),
            BinOpRTL(
                Literal(TokenType.NUMBER, D("7")),
                TokenType.DBLSTAR,
                BinOpRTL(
                    Literal(TokenType.NUMBER, D("9")),
                    TokenType.DBLSTAR,
                    Literal(TokenType.NUMBER, D("3")),
                ),
            ),
        )

    def test_unary_associativity_even(self) -> None:
        self.assertEqual(
            parse_expression("--3"),
            Unary(
                TokenType.MINUS,
                Unary(
                    TokenType.MINUS,
                    Literal(TokenType.NUMBER, D("3")),
                ),
            ),
        )

    def test_unary_associativity_even_odd(self) -> None:
        self.assertEqual(
            parse_expression("---3"),
            Unary(
                TokenType.MINUS,
                Unary(
                    TokenType.MINUS,
                    Unary(
                        TokenType.MINUS,
                        Literal(TokenType.NUMBER, D("3")),
                    ),
                ),
            ),
        )


if __name__ == "__main__":
    unittest.main()
