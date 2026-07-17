#!/usr/bin/env python3
"""Derive the semantic and runtime identity of the resource wrapper."""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import pathlib
import platform
import sys
from typing import Any


def _sha256(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return "sha256:" + digest.hexdigest()


def _resolve_local(name: str, roots: tuple[pathlib.Path, ...]) -> pathlib.Path | None:
    relative = pathlib.Path(*name.split("."))
    for root in roots:
        module = root / relative.with_suffix(".py")
        package = root / relative / "__init__.py"
        if module.is_file():
            return module.resolve()
        if package.is_file():
            return package.resolve()
    return None


def discover_local_import_closure(
    entry: pathlib.Path, roots: tuple[pathlib.Path, ...]
) -> tuple[pathlib.Path, ...]:
    roots = tuple(root.resolve() for root in roots)
    pending = [entry.resolve()]
    visited: set[pathlib.Path] = set()
    while pending:
        current = pending.pop()
        if current in visited:
            continue
        visited.add(current)
        tree = ast.parse(current.read_text(encoding="utf-8"), filename=str(current))
        for node in ast.walk(tree):
            names: list[str] = []
            if isinstance(node, ast.Import):
                names = [alias.name for alias in node.names]
            elif isinstance(node, ast.ImportFrom):
                if node.level:
                    raise ValueError(f"relative import is not supported: {current}")
                if node.module:
                    names = [node.module]
            elif (
                isinstance(node, ast.Call)
                and isinstance(node.func, ast.Name)
                and node.func.id == "__import__"
            ):
                raise ValueError(f"dynamic import is not supported: {current}")
            pending.extend(path for name in names if (path := _resolve_local(name, roots)))
    return tuple(sorted(visited))


def build_identity(
    entry: pathlib.Path, python: pathlib.Path, nix_derivation: str
) -> dict[str, Any]:
    entry = entry.resolve()
    closure = discover_local_import_closure(entry, (entry.parent,))
    value: dict[str, Any] = {
        "schema_version": "parser-rq-resource-identity-v1",
        "runtime": {
            "implementation": sys.implementation.name,
            "cache_tag": sys.implementation.cache_tag,
            "version": platform.python_version(),
            "executable_sha256": _sha256(python.resolve()),
            "nix_derivation": nix_derivation,
        },
        "sources": [{"path": path.name, "sha256": _sha256(path)} for path in closure],
    }
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":")).encode()
    value["wrapper_identity_hash"] = "sha256:" + hashlib.sha256(encoded).hexdigest()
    return value


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--entry", required=True, type=pathlib.Path)
    parser.add_argument("--out", required=True, type=pathlib.Path)
    parser.add_argument("--nix-derivation", default="development")
    args = parser.parse_args()
    value = build_identity(args.entry, pathlib.Path(sys.executable), args.nix_derivation)
    args.out.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
