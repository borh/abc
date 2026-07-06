#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

schema_link="$repo_root/ab-validator/data/abc-schemas/schemas"
expected_schema_dir="$repo_root/abc/schemas"

if [[ ! -L "$schema_link" ]]; then
  echo "expected $schema_link to be a symlink to abc/schemas" >&2
  exit 1
fi

actual_schema_dir="$(readlink -f "$schema_link")"
expected_schema_dir="$(readlink -f "$expected_schema_dir")"
if [[ "$actual_schema_dir" != "$expected_schema_dir" ]]; then
  echo "schema symlink points at $actual_schema_dir, expected $expected_schema_dir" >&2
  exit 1
fi

python3 "$repo_root/ab-validator/scripts/compare_abc_schema_contracts.py" \
  --local "$repo_root/ab-validator/data/abc-schemas/schema-contracts.json" \
  --abc "$repo_root/abc"

echo "monorepo schema drift check ok"
