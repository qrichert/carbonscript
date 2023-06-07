"""Test environment module."""

import doctest
import unittest

import carbonscript.environment
from carbonscript.environment import Environment


def load_tests(
    loader: unittest.TestLoader, tests: unittest.TestSuite, ignore: str
) -> unittest.TestSuite:
    # pylint: disable=unused-argument
    """Add module doctests."""
    tests.addTests(doctest.DocTestSuite(carbonscript.environment))
    return tests


class TestEnvironment(unittest.TestCase):
    def test_push_scope(self) -> None:
        env: Environment = Environment()
        env.push_scope()
        self.assertEqual(env.scope_id, 1)

    def test_pop_scope(self) -> None:
        env: Environment = Environment()
        env.push_scope()
        env.pop_scope()
        self.assertEqual(env.scope_id, 0)

    def test_pop_scope_beyond_global(self) -> None:
        env: Environment = Environment()
        with self.assertRaises(AssertionError):
            env.pop_scope()

    def test_scope_id(self) -> None:
        env: Environment = Environment()
        self.assertEqual(env.scope_id, 0)
        env.push_scope()
        self.assertEqual(env.scope_id, 1)
        env.push_scope()
        self.assertEqual(env.scope_id, 2)
        env.pop_scope()
        self.assertEqual(env.scope_id, 1)
        env.pop_scope()
        self.assertEqual(env.scope_id, 0)
        env.push_scope()
        self.assertEqual(env.scope_id, 1)

    def test_get(self) -> None:
        env: Environment = Environment()
        env.declare("foo", "bar")
        self.assertEqual(env.get("foo"), "bar")

    def test_get_undefined(self) -> None:
        env: Environment = Environment()
        with self.assertRaises(RuntimeError):
            env.get("foo")

    def test_declare(self) -> None:
        env: Environment = Environment()
        env.declare("foo", "bar")
        self.assertEqual(env.get("foo"), "bar")

    def test_set(self) -> None:
        env: Environment = Environment()
        env.declare("foo", "bar")
        env.set("foo", "baz")
        self.assertEqual(env.get("foo"), "baz")

    def test_set_undefined(self) -> None:
        env: Environment = Environment()
        with self.assertRaises(RuntimeError):
            env.set("foo", "bar")

    def test_set_constant(self) -> None:
        env: Environment = Environment()
        env.declare("foo", "bar", const=True)
        with self.assertRaises(RuntimeError):
            env.set("foo", "baz")

    def test_get_from_parent_scope(self) -> None:
        env: Environment = Environment()
        env.declare("global", "foo")
        env.push_scope()
        self.assertEqual(env.get("global"), "foo")

    def test_set_in_parent_scope(self) -> None:
        env: Environment = Environment()
        env.declare("global", "foo")
        env.push_scope()
        env.set("global", "bar")
        self.assertEqual(env.get("global"), "bar")

    def test_set_in_parent_scope_then_pop(self) -> None:
        env: Environment = Environment()
        env.declare("global", "foo")
        env.push_scope()
        env.set("global", "bar")
        env.pop_scope()
        self.assertEqual(env.get("global"), "bar")

    def test_redefine_over_parent_scope(self) -> None:
        env: Environment = Environment()
        env.declare("global", "foo")
        env.push_scope()
        env.declare("global", "bar")
        self.assertEqual(env.get("global"), "bar")

    def test_redefine_over_parent_scope_then_pop(self) -> None:
        env: Environment = Environment()
        env.declare("global", "foo")
        env.push_scope()
        env.declare("global", "bar")
        env.pop_scope()
        self.assertEqual(env.get("global"), "foo")

    def test_redefine_in_same_scope(self) -> None:
        env: Environment = Environment()
        env.declare("global", "foo")
        with self.assertRaises(RuntimeError):
            env.declare("global", "bar")

    def test_out_of_scope(self) -> None:
        env: Environment = Environment()
        env.push_scope()
        env.declare("scope1", "bar")
        env.pop_scope()
        with self.assertRaises(RuntimeError):
            env.get("scope1")  # Out of scope.


if __name__ == "__main__":
    unittest.main()
