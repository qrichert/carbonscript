import argparse
import cmd
import os
import sys
from pathlib import Path

from .ast import Stmt
from .error import ParseError
from .interpreter import Interpreter
from .lexer import Lexer
from .parser import Parser
from .tokens import Token
from .values import LiteralValue

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

    def __init__(self, *args, **kwargs) -> None:
        self.interpreter: Interpreter = Interpreter()
        super().__init__(*args, **kwargs)

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
            # TODO: here too with "None" being incorrect
            res: LiteralValue | None = self._interpret_as_expression(line)
            if res is not None:
                print(res)
        except KeyboardInterrupt:
            raise
        except (ParseError, Exception) as e:
            # TODO: Handle exception nicely.
            print(color_error(str(e)))
        return None

    def _interpret_as_expression(self, line: str) -> LiteralValue | None:
        tokens: list[Token] = Lexer().lex(line)
        statements: list[Stmt] = Parser().parse(tokens)
        return self.interpreter.interpret_one(statements[0])


def execute_file(file: Path) -> int:
    with open(file, "r") as f:
        script: str = f.read()
    try:
        tokens: list[Token] = Lexer().lex(script)
        statements: list[Stmt] = Parser().parse(tokens)
        Interpreter().interpret(statements)
    except KeyboardInterrupt:
        return 1
    except (ParseError, Exception) as e:
        # TODO: Handle exception nicely.
        print(color_error(str(e)))
        return 1
    return 0


def main(argc: int, argv: list[str]) -> int:
    if argc >= 2:
        first_arg: Path = Path(argv[1])
        if first_arg.is_file():
            return execute_file(first_arg)

    args: argparse.Namespace = parse_args()

    repl: CarbonScriptREPL = CarbonScriptREPL()
    if args.command:
        repl.onecmd(args.command)
        return
    repl.cmdloop()

    return 0


if __name__ == "__main__":
    sys.exit(main(len(sys.argv), sys.argv))
