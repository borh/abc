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

python "$repo_root/ab-validator/scripts/compare_abc_schema_contracts.py" \
  --local "$repo_root/ab-validator/data/abc-schemas/schema-contracts.json" \
  --abc "$repo_root/abc"

python - "$expected_schema_dir" "$repo_root/ab-validator/data/abc-schemas/nix-schemas" \
  "$repo_root/abc/schemas/schema-contracts.json" <<'PY'
import json
import sys
from pathlib import Path

abc_schema_dir = Path(sys.argv[1])
mirror_dir = Path(sys.argv[2])
manifest_path = Path(sys.argv[3])

manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
expected_names = {Path(row["path"]).name for row in manifest["schemas"]}
actual_names = {path.name for path in mirror_dir.glob("*.schema.json")}

errors = []
for name in sorted(expected_names - actual_names):
    errors.append(f"missing isolated Nix schema mirror file: {name}")
for name in sorted(actual_names - expected_names):
    errors.append(f"extra isolated Nix schema mirror file: {name}")
for name in sorted(expected_names & actual_names):
    if (abc_schema_dir / name).read_bytes() != (mirror_dir / name).read_bytes():
        errors.append(f"isolated Nix schema mirror drift: {name}")

if errors:
    for error in errors:
        print(error, file=sys.stderr)
    sys.exit(1)
PY

policy_link="$repo_root/ab-validator/data/abc-schemas/data/source-region-publication-policy-v0.json"
expected_policy="$repo_root/abc/data/source-region-publication-policy-v0.json"

if [[ ! -L "$policy_link" ]]; then
  echo "expected $policy_link to be a symlink to abc/data/source-region-publication-policy-v0.json" >&2
  exit 1
fi

actual_policy="$(readlink -f "$policy_link")"
expected_policy="$(readlink -f "$expected_policy")"
if [[ "$actual_policy" != "$expected_policy" ]]; then
  echo "source-region policy symlink points at $actual_policy, expected $expected_policy" >&2
  exit 1
fi

echo "monorepo schema drift check ok"
