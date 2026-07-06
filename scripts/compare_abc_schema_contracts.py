#!/usr/bin/env python3
"""Compare vendored ABC schema contracts against an explicit ABC checkout.

This is an operator-side pre-monorepo guard. It intentionally requires an
explicit ABC path instead of baking a sibling checkout into normal build gates.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any


def read_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"{path}: expected JSON object")
    return value


def normalized_contract(path: Path) -> dict[str, Any]:
    manifest = read_json(path)
    schemas = manifest.get("schemas")
    if not isinstance(schemas, list):
        raise ValueError(f"{path}: schemas must be a list")

    rows: dict[str, dict[str, str]] = {}
    for row in schemas:
        if not isinstance(row, dict):
            raise ValueError(f"{path}: schema row must be an object")
        schema_id = row.get("id")
        if not isinstance(schema_id, str) or not schema_id:
            raise ValueError(f"{path}: schema row missing non-empty id")
        if schema_id in rows:
            raise ValueError(f"{path}: duplicate schema id {schema_id}")
        rows[schema_id] = {
            key: require_string(path, schema_id, row, key)
            for key in ("id", "title", "version", "hash")
        }

    return {
        "schema_contract_version": require_top_string(path, manifest, "schema_contract_version"),
        "canonicalization": require_top_string(path, manifest, "canonicalization"),
        "schemas": rows,
    }


def require_top_string(path: Path, manifest: dict[str, Any], key: str) -> str:
    value = manifest.get(key)
    if not isinstance(value, str) or not value:
        raise ValueError(f"{path}: missing non-empty {key}")
    return value


def require_string(path: Path, schema_id: str, row: dict[str, Any], key: str) -> str:
    value = row.get(key)
    if not isinstance(value, str) or not value:
        raise ValueError(f"{path}: schema {schema_id} missing non-empty {key}")
    return value


def compare_contracts(local_path: Path, abc_path: Path) -> list[str]:
    local = normalized_contract(local_path)
    upstream = normalized_contract(abc_path)
    errors: list[str] = []

    for key in ("schema_contract_version", "canonicalization"):
        if local[key] != upstream[key]:
            errors.append(f"{key}: local={local[key]} abc={upstream[key]}")

    local_schemas = local["schemas"]
    upstream_schemas = upstream["schemas"]
    local_ids = set(local_schemas)
    upstream_ids = set(upstream_schemas)
    for schema_id in sorted(local_ids - upstream_ids):
        errors.append(f"{schema_id}: present locally but missing from ABC")
    for schema_id in sorted(upstream_ids - local_ids):
        errors.append(f"{schema_id}: present in ABC but missing locally")

    for schema_id in sorted(local_ids & upstream_ids):
        local_row = local_schemas[schema_id]
        upstream_row = upstream_schemas[schema_id]
        for key in ("title", "version", "hash"):
            if local_row[key] != upstream_row[key]:
                errors.append(
                    f"{schema_id} {key}: local={local_row[key]} abc={upstream_row[key]}"
                )
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--local",
        type=Path,
        default=Path("data/abc-schemas/schema-contracts.json"),
        help="ab-validator vendored schema contract manifest",
    )
    parser.add_argument(
        "--abc",
        type=Path,
        required=True,
        help="ABC checkout root containing schemas/schema-contracts.json",
    )
    args = parser.parse_args()

    local_path = args.local
    abc_path = args.abc / "schemas/schema-contracts.json"
    try:
        errors = compare_contracts(local_path, abc_path)
    except Exception as exc:
        print(str(exc), file=sys.stderr)
        return 1

    if errors:
        print("schema contract drift:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
