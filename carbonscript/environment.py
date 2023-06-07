from __future__ import annotations

from typing import Generator

from .values import LiteralValue


class Scope:
    def __init__(self, parent: Scope | None) -> None:
        self._values: dict[str, tuple[LiteralValue, bool]] = {}
        self.parent: Scope | None = parent

    def to_dict(self) -> dict:
        return self._values

    def _has_value(self, identifier: str) -> bool:
        return identifier in self._values

    def _get_value(self, identifier: str) -> LiteralValue:
        return self._values[identifier][0]

    def _declare_value(self, identifier: str, value: LiteralValue, const: bool) -> None:
        self._values[identifier] = (value, const)

    def _set_value(self, identifier: str, value: LiteralValue) -> None:
        const: bool = False  # Cannot be constant if updated.
        self._values[identifier] = (value, const)

    def _is_value_const(self, identifier: str) -> bool:
        return self._values[identifier][1]


class Environment(Scope):
    def __init__(self) -> None:
        super().__init__(None)
        self._current_scope: Scope = self
        self.scope_id: int = 0

    def push_scope(self) -> None:
        new_scope: Scope = Scope(self._current_scope)
        self._current_scope = new_scope
        self.scope_id += 1

    def pop_scope(self) -> None:
        assert self._current_scope is not self, "Cannot pop global scope."
        self._current_scope = self._current_scope.parent
        self.scope_id -= 1

    def _iter_scopes(self) -> Generator[Scope, None, None]:
        scope: Scope = self._current_scope
        while scope.parent:
            yield scope
            scope = scope.parent
        yield scope  # Global scope doesn't have a parent.

    def get(self, identifier: str) -> LiteralValue:
        scope: Scope
        for scope in self._iter_scopes():
            try:
                return scope._get_value(identifier)
            except KeyError:
                continue
        # TODO: Factor this out to Error class.
        raise RuntimeError(f"undefined identifier {identifier!r}")

    def declare(
        self, identifier: str, value: LiteralValue, const: bool = False
    ) -> None:
        if self._current_scope._has_value(identifier):
            raise RuntimeError(f"redefinition of identifier {identifier!r}")
        self._current_scope._declare_value(identifier, value, const)

    def set(self, identifier: str, value: LiteralValue) -> None:
        scope: Scope
        # If value exists in current scope or parent scope, set it.
        for scope in self._iter_scopes():
            if scope._has_value(identifier):
                if scope._is_value_const(identifier):
                    raise RuntimeError(f"trying to update constant {identifier!r}")
                scope._set_value(identifier, value)
                return
        # TODO: Factor this out to Error class.
        raise RuntimeError(f"undefined identifier {identifier!r}")
