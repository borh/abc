#!/usr/bin/env python3
"""Generate the closed semantic identity of the publication validator."""

from __future__ import annotations

import argparse
import ast
import hashlib
import importlib.metadata
import json
import pathlib
import sys
from typing import Any


REVIEWED_SOURCE_PATHS = (
    pathlib.Path("reports/parser-ir/publication-bundle-validate.py"),
    pathlib.Path("reports/lib/evidence.py"),
    pathlib.Path("reports/lib/hashing.py"),
    pathlib.Path("reports/lib/io.py"),
    pathlib.Path("reports/lib/paths.py"),
    pathlib.Path("reports/lib/source_region.py"),
)
VALIDATOR_ID = "ab-validator/publication-bundle-validate/v2"


def _module_path(name: str, reports_root: pathlib.Path) -> pathlib.Path | None:
    if not name.startswith("reports."):
        return None
    relative = pathlib.Path(*name.split(".")[1:])
    module = reports_root / relative.with_suffix(".py")
    if module.is_file():
        return module.resolve()
    package = reports_root / relative / "__init__.py"
    return package.resolve() if package.is_file() else None


def _local_imports(path: pathlib.Path, reports_root: pathlib.Path) -> set[pathlib.Path]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    found: set[pathlib.Path] = set()
    for node in ast.walk(tree):
        names: list[str] = []
        if isinstance(node, ast.Import):
            names.extend(alias.name for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module:
            names.append(node.module)
        for name in names:
            if resolved := _module_path(name, reports_root):
                found.add(resolved)
    return found


def transitive_local_imports(
    entrypoint: pathlib.Path, reports_root: pathlib.Path
) -> set[pathlib.Path]:
    pending = [entrypoint.resolve()]
    visited: set[pathlib.Path] = set()
    while pending:
        current = pending.pop()
        if current in visited:
            continue
        visited.add(current)
        pending.extend(_local_imports(current, reports_root) - visited)
    return visited


def _sha256(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def _canonical(value: Any) -> bytes:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")).encode(
        "utf-8"
    )


def projected_hash(manifest: dict[str, Any]) -> str:
    projected = dict(manifest)
    projected.pop("validator_semantics_hash", None)
    return _sha256(_canonical(projected))


def build_manifest(repo_root: pathlib.Path) -> dict[str, Any]:
    repo_root = repo_root.resolve()
    entrypoint = repo_root / REVIEWED_SOURCE_PATHS[0]
    actual = transitive_local_imports(entrypoint, repo_root / "reports")
    reviewed = {(repo_root / path).resolve() for path in REVIEWED_SOURCE_PATHS}
    if actual != reviewed:
        missing = sorted(str(path.relative_to(repo_root)) for path in actual - reviewed)
        stale = sorted(str(path.relative_to(repo_root)) for path in reviewed - actual)
        raise ValueError(f"reviewed semantic closure differs: unreviewed={missing}, stale={stale}")
    manifest: dict[str, Any] = {
        "schema_version": "parser-rq-publication-validator-identity-v1",
        "validator_id": VALIDATOR_ID,
        "runtime": {
            "python": ".".join(str(part) for part in sys.version_info[:3]),
            "jsonschema": importlib.metadata.version("jsonschema"),
        },
        "sources": [
            {"path": path.as_posix(), "sha256": _sha256((repo_root / path).read_bytes())}
            for path in REVIEWED_SOURCE_PATHS
        ],
    }
    manifest["validator_semantics_hash"] = projected_hash(manifest)
    return manifest


def check_manifest(expected: dict[str, Any], actual: dict[str, Any]) -> list[str]:
    errors = []
    if expected.get("validator_semantics_hash") != projected_hash(expected):
        errors.append("stored validator_semantics_hash does not authenticate manifest")
    if expected != actual:
        errors.append("validator semantic identity has drifted")
    return errors


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--write", action="store_true")
    mode.add_argument("--check", action="store_true")
    parser.add_argument("--repo-root", required=True, type=pathlib.Path)
    parser.add_argument("--out", required=True, type=pathlib.Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    actual = build_manifest(args.repo_root)
    if args.write:
        args.out.parent.mkdir(parents=True, exist_ok=True)
        args.out.write_text(
            json.dumps(actual, ensure_ascii=False, indent=2) + "\n", encoding="utf-8"
        )
        return 0
    expected = json.loads(args.out.read_text(encoding="utf-8"))
    errors = check_manifest(expected, actual)
    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
