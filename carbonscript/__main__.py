import argparse
import cmd
import os
from pathlib import Path

from .interpreter import Interpreter, LiteralValue
from .lexer import Lexer, Token
from .parser import Expr, ParseError, Parser

CMD_HISTORY_FILE: Path = Path(__file__).parent.resolve() / ".carbonscript_history.txt"
CLEAR_CMD: str = "cls" if os.name == "nt" else "clear"


def color_error(string: str) -> str:
    return f"\033[0;91m{string}\033[0m"


def parse_args() -> argparse.Namespace:
    """Parse REPL arguments.

    Example: ``python -m carbonscript -c "1+1"``

    :return: Parsed arguments.
    """
    parser: argparse.ArgumentParser = argparse.ArgumentParser(prog="carbonscript")
    parser.add_argument(
        "-c",
        "--command",
        type=str,
        help="execute script",
        dest="command",
    )
    return parser.parse_args()


class CarbonScriptREPL(cmd.Cmd):
    intro = 'CarbonScript 0.0.1a\nType "exit()" quit.'
    prompt = ">>> "
    ruler = ""
    nohelp = color_error("No help on %r.")

    def cmdloop(self, *args, **kwargs) -> None:
        """Interrupt REPL gracefully."""
        try:
            super().cmdloop(*args, **kwargs)
        except KeyboardInterrupt:
            print("")
            return

    def preloop(self) -> None:
        """Preloop hook."""
        self._restore_command_history()

    @staticmethod
    def _restore_command_history() -> None:
        if CMD_HISTORY_FILE.is_file():
            try:
                import readline  # pylint: disable=import-outside-toplevel

                readline.read_history_file(CMD_HISTORY_FILE)
            except ImportError:
                pass

    def postloop(self) -> None:
        """Postloop hook."""
        self._save_command_history()

    @staticmethod
    def _save_command_history() -> None:
        try:
            import readline  # pylint: disable=import-outside-toplevel

            # Remove last command from history (= 'exit()').
            nb_items: int = readline.get_current_history_length()
            if nb_items > 0:
                readline.remove_history_item(nb_items - 1)

            readline.set_history_length(100)
            readline.write_history_file(CMD_HISTORY_FILE)
        except ImportError:
            pass

    def parseline(self, line: str):
        """Disable default help command."""
        if line in ("?", "help"):
            return None, None, line
        return super().parseline(line)

    def emptyline(self) -> None:
        """Ignore empty commands."""

    def default(self, line: str) -> None | bool:
        if line == "exit()":
            return True
        try:
            print(self._interpret_script(line))
        except ParseError as e:
            print(color_error(str(e)))
        return None

    @staticmethod
    def _interpret_script(script: str) -> LiteralValue:
        tokens: list[Token] = Lexer().lex(script)
        ast: list[Expr] = Parser().parse(tokens)
        return Interpreter().interpret(ast)


def main() -> None:
    args: argparse.Namespace = parse_args()

    repl: CarbonScriptREPL = CarbonScriptREPL()
    if args.command:
        repl.onecmd(args.command)
        return
    repl.cmdloop()


if __name__ == "__main__":
    main()
