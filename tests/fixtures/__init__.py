"""Constants for fixtures."""

from pathlib import Path

FIXTURES_PATH: Path = Path(__file__).resolve().parent

THE_BIG_ONE: str = str(FIXTURES_PATH / "the_big_one.cbn")
