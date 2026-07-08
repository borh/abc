#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_schema_dir="$repo_root/abc/schemas"
mirror_dir="$repo_root/ab-validator/data/abc-schemas/nix-schemas"
manifest_path="$abc_schema_dir/schema-contracts.json"

python "$repo_root/scripts/sync-schema-mirror.py" \
  "$manifest_path" \
  "$abc_schema_dir" \
  "$mirror_dir"

bash "$repo_root/scripts/monorepo-schema-drift.sh"
