#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aat-parser-ir-mapping-smoke"
aat_dir="$repo_root/scratch/morph-full-corpus/aats/aozora-rs-adapter"
abc_root="$repo_root/../abc"

rm -rf "$out_dir"
mkdir -p "$out_dir"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$abc_root" \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json" \
  --assert-zero-unsupported

jq -e '.files_scanned == 17894' "$out_dir/summary.json"
jq -e '.files_with_unsupported == 0' "$out_dir/summary.json"
jq -e '.generated_mapping_rules == 25' "$out_dir/summary.json"
jq -e '.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' "$out_dir/summary.json"
jq -e '.target_parser_ir_schema_hash == "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"' "$out_dir/summary.json"
jq -e '([.transform_rule_descriptions[].category] | index("UNSUPPORTED") | not)' "$out_dir/mapping.json"
