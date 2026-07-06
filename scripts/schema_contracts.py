#!/usr/bin/env python3
"""Generate or check the vendored ABC schema contract manifest."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from pathlib import Path
from typing import Any

CONTRACT_VERSION = "abc-schema-contracts-v1"
CANONICALIZATION = "abc-legacy-json-c14n-v0"
VERSION_RE = re.compile(r"^\d+\.\d+\.\d+$")
SCHEMA_FILES = (
    "aat-parser-ir-divergence.schema.json",
    "aat-parser-ir-mapping.schema.json",
    "manifest.schema.json",
    "parser-ir-publication-preservation.schema.json",
    "parser-ir.schema.json",
    "source-region-coverage.schema.json",
)


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


def load_schema(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"{path}: schema must be a JSON object")
    return value


def build_manifest(schema_dir: Path, path_prefix: str) -> dict[str, Any]:
    rows: list[dict[str, str]] = []
    errors: list[str] = []
    for name in SCHEMA_FILES:
        path = schema_dir / name
        if not path.exists():
            errors.append(f"{path}: missing schema file")
            continue
        schema = load_schema(path)
        schema_id = schema.get("$id")
        title = schema.get("title")
        version = schema.get("version")
        if not isinstance(schema_id, str) or not schema_id:
            errors.append(f"{path}: missing non-empty $id")
        if not isinstance(title, str) or not title:
            errors.append(f"{path}: missing non-empty title")
        if not isinstance(version, str) or not VERSION_RE.match(version):
            errors.append(f"{path}: missing semver version")
        rows.append(
            {
                "path": f"{path_prefix.rstrip('/')}/{name}" if path_prefix else name,
                "id": str(schema_id),
                "title": str(title),
                "version": str(version),
                "hash": schema_hash(schema),
            }
        )
    if errors:
        raise ValueError("\n".join(errors))
    return {
        "schema_contract_version": CONTRACT_VERSION,
        "canonicalization": CANONICALIZATION,
        "schemas": rows,
    }


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(value, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--schema-dir", type=Path, default=Path("data/abc-schemas/schemas"))
    parser.add_argument(
        "--manifest",
        type=Path,
        default=Path("data/abc-schemas/schema-contracts.json"),
    )
    parser.add_argument("--path-prefix", default="data/abc-schemas/schemas")
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()

    try:
        generated = build_manifest(args.schema_dir, args.path_prefix)
        if args.write:
            write_json(args.manifest, generated)
            return 0
        expected = json.loads(args.manifest.read_text(encoding="utf-8"))
        if expected != generated:
            print(f"{args.manifest}: schema contract drift", file=sys.stderr)
            print("Run scripts/schema_contracts.py --write", file=sys.stderr)
            return 1
        return 0
    except Exception as exc:
        print(str(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
