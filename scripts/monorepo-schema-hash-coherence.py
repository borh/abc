#!/usr/bin/env python3
"""Recheck every recorded soranoha schema hash against the schema it names.

A record that carries ``<name>_schema_id`` and ``<name>_schema_hash`` is
asserting which schema it was written against. Nothing recomputed those
assertions, so both the metadata-record and person-record pins drifted across
two schema edits before this gate existed: the importer would have rejected
every real export, and the fixtures claimed a digest no schema ever had.

The check walks live JSON and recomputes each pin from the schema itself, so a
schema edit fails here rather than at the first import of real data.
"""

from __future__ import annotations

import hashlib
import json
import pathlib
import re
import sys
from typing import Any

# Frozen evidence of what was published. Its recorded hashes name the schema
# as it stood, so recomputing them from today's schema is the wrong question.
SKIPPED = (
    ".beads",
    ".git",
    "archive",
    "ab-validator/docs",
)

# A pin held in source rather than in a record. The constant is deliberate:
# it states which schema the surrounding field mapping was written against, so
# a schema change has to be reviewed alongside the mapping. What it cannot do
# by itself is notice that the schema moved. No source holds one at present.
SOURCE_PINS: tuple[tuple[str, str, str], ...] = ()

ID_SUFFIX = "_schema_id"
HASH_SUFFIX = "_schema_hash"


def canonical_bytes(value: Any) -> bytes:
    """Encode under rfc8785-safe-integer-json-bytes-v1, the one canonicalizer.

    Floats are rejected rather than encoded: the Clojure producer admits only
    safe integers, and a silent float here would produce a digest no producer
    can reproduce.
    """

    def reject_floats(node: Any) -> None:
        if isinstance(node, float):
            raise ValueError(f"non-integer number is outside the canonicalizer: {node!r}")
        if isinstance(node, dict):
            for child in node.values():
                reject_floats(child)
        elif isinstance(node, list):
            for child in node:
                reject_floats(child)

    reject_floats(value)
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode(
        "utf-8"
    )


def schema_hashes(repo_root: pathlib.Path) -> dict[str, str]:
    """Map each published schema's ``$id`` to its current canonical digest."""
    hashes: dict[str, str] = {}
    for path in sorted((repo_root / "soranoha" / "schemas").glob("*.schema.json")):
        schema = json.loads(path.read_text(encoding="utf-8"))
        schema_id = schema.get("$id")
        if not isinstance(schema_id, str):
            raise ValueError(f"{path} has no string $id")
        hashes[schema_id] = "sha256:" + hashlib.sha256(canonical_bytes(schema)).hexdigest()
    return hashes


def record_pins(node: Any) -> list[tuple[str, str]]:
    """Collect every ``(schema_id, schema_hash)`` pair stated in one document."""
    found: list[tuple[str, str]] = []
    if isinstance(node, dict):
        for key, value in node.items():
            if key.endswith(ID_SUFFIX) and isinstance(value, str):
                paired = node.get(key[: -len(ID_SUFFIX)] + HASH_SUFFIX)
                if isinstance(paired, str):
                    found.append((value, paired))
            found.extend(record_pins(value))
    elif isinstance(node, list):
        for child in node:
            found.extend(record_pins(child))
    return found


def walk_json(repo_root: pathlib.Path) -> list[pathlib.Path]:
    skipped = {repo_root / part for part in SKIPPED}
    paths = []
    for path in repo_root.rglob("*.json"):
        if any(parent in skipped for parent in path.parents):
            continue
        paths.append(path)
    return sorted(paths)


def check_records(repo_root: pathlib.Path, hashes: dict[str, str]) -> list[str]:
    errors = []
    for path in walk_json(repo_root):
        try:
            document = json.loads(path.read_text(encoding="utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError):
            continue
        for schema_id, recorded in record_pins(document):
            expected = hashes.get(schema_id)
            if expected is None or recorded == expected:
                continue
            relative = path.relative_to(repo_root)
            errors.append(f"{relative}: {schema_id} recorded {recorded}, schema hashes {expected}")
    return errors


def check_source_pins(repo_root: pathlib.Path, hashes: dict[str, str]) -> list[str]:
    errors = []
    for relative, constant, schema_id in SOURCE_PINS:
        text = (repo_root / relative).read_text(encoding="utf-8")
        match = re.search(rf"{re.escape(constant)}[^=]*=\s*\n?\s*\"(sha256:[0-9a-f]{{64}})\"", text)
        if match is None:
            errors.append(f"{relative}: {constant} is no longer a pinned sha256 literal")
            continue
        expected = hashes[schema_id]
        if match.group(1) != expected:
            errors.append(
                f"{relative}: {constant} pins {match.group(1)}, {schema_id} hashes {expected}"
            )
    return errors


def main(argv: list[str]) -> int:
    repo_root = pathlib.Path(argv[1] if len(argv) > 1 else ".").resolve()
    hashes = schema_hashes(repo_root)
    errors = check_records(repo_root, hashes) + check_source_pins(repo_root, hashes)
    for error in errors:
        print(error, file=sys.stderr)
    if errors:
        return 1
    print(f"schema hash coherence passed: {len(hashes)} published schemas")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
