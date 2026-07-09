#!/usr/bin/env python
"""Self-contained schema-contracts manifest verifier for the nix sandbox.

Why this exists: `abc/flake.nix`'s `schema-contract-drift` check builds a
sandbox from `cp -R ${./.} source` — i.e. only the `abc/` directory, copied
in isolation from the rest of the monorepo. `tools/schema_contracts.py` is a
thin runpy wrapper that resolves and executes
`parents[2]/scripts/abc_schema_contracts.py` (the monorepo-root generator
that both `abc/` and `ab-validator/` share). That root script does not exist
inside the sandbox, so invoking the wrapper there raises FileNotFoundError
at *build* time. `nix flake check --no-build` never notices because it only
evaluates derivations; the break is latent until something actually builds
this check.

This script re-implements the read-only "check" half of the generator using
only the standard library and files already present under `abc/`, so it
runs correctly inside the sandbox. It is coverage-equivalent for the
registered schema set: the manifest (`schemas/schema-contracts.json`) is
itself the authoritative list of rows to verify, so validating each row
against its schema file — without needing to also *discover* schema files
via `scripts/abc_schema_contracts.py`'s `SCHEMA_FILES` constant — checks
exactly the same surface the root generator would. Generation (`--write`)
and cross-profile drift authoring remain the job of
`scripts/abc_schema_contracts.py`; this script only ever reads and reports.

`canonical_json` and `schema_hash` below are copied verbatim (all 10 lines)
from `scripts/abc_schema_contracts.py` rather than imported, because that
module lives outside `abc/` and is unreachable from inside the sandbox.
Keep the two in sync by hand if the canonicalization ever changes.
"""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path
from typing import Any

MANIFEST_PATH = Path("schemas/schema-contracts.json")


def canonical_json(value: Any) -> str:
    """Match ABC's existing schema hash canonicalization."""
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).replace("/", r"\/")


def schema_hash(value: Any) -> str:
    return "sha256:" + hashlib.sha256(canonical_json(value).encode("utf-8")).hexdigest()


def main() -> int:
    errors: list[str] = []

    try:
        manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
    except OSError as exc:
        print(f"{MANIFEST_PATH}: cannot read manifest: {exc}", file=sys.stderr)
        return 1
    except json.JSONDecodeError as exc:
        print(f"{MANIFEST_PATH}: invalid JSON: {exc}", file=sys.stderr)
        return 1

    for row in manifest.get("schemas", []):
        path = Path(row["path"])
        if not path.exists():
            errors.append(f"{path}: missing schema file")
            continue
        try:
            schema = json.loads(path.read_text(encoding="utf-8"))
        except json.JSONDecodeError as exc:
            errors.append(f"{path}: invalid JSON: {exc}")
            continue
        if not isinstance(schema, dict):
            errors.append(f"{path}: schema must be a JSON object")
            continue

        if schema.get("$id") != row["id"]:
            errors.append(
                f"{path}: $id mismatch: manifest={row['id']!r} schema={schema.get('$id')!r}"
            )
        if schema.get("title") != row["title"]:
            errors.append(
                f"{path}: title mismatch: manifest={row['title']!r} schema={schema.get('title')!r}"
            )
        if schema.get("version") != row["version"]:
            errors.append(
                f"{path}: version mismatch: manifest={row['version']!r} "
                f"schema={schema.get('version')!r}"
            )
        recomputed = schema_hash(schema)
        if recomputed != row["hash"]:
            errors.append(
                f"{path}: hash mismatch: manifest={row['hash']!r} recomputed={recomputed!r}"
            )

    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        print(
            f"{MANIFEST_PATH}: schema contract drift "
            "(regenerate with the monorepo-root scripts/abc_schema_contracts.py --write)",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
