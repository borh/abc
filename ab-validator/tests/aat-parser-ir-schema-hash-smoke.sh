#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"

out="$(python "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py" \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json" \
  "$abc_root/schemas/parser-ir.schema.json")"

printf '%s\n' "$out"

for schema_name in aat-parser-ir-mapping.schema.json parser-ir.schema.json; do
  expected_hash="$(jq -er --arg id "https://w3id.org/abc/schemas/$schema_name" \
    '.schemas[] | select(.id == $id) | .hash' "$abc_root/schemas/schema-contracts.json")"
  printf '%s\n' "$out" | rg -F "$abc_root/schemas/$schema_name"$'\t'"$expected_hash"
done
