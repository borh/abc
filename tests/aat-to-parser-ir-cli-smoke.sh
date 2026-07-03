#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"
out_dir="${TMPDIR:-/tmp}/ab-validator-aat-to-parser-ir-smoke"
rm -rf "$out_dir"
mkdir -p "$out_dir"

aat="$out_dir/input.aat.json"
cat > "$aat" <<'JSON'
{
  "version": 1,
  "work_id": "smoke",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:7777777777777777777777777777777777777777777777777777777777777777",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "A" },
        { "kind": "ruby", "base": "B", "reading": "bee", "direction": "right" }
      ]
    }
  ]
}
JSON

convert_args=(
  convert
  --aat "$aat"
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json"
  --parser-ir-out "$out_dir/parser-ir.json"
  --divergence-out "$out_dir/divergence.json"
  --abc-root "$abc_root"
)

if [ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]; then
  "$AB_AAT_TO_PARSER_IR_BIN" "${convert_args[@]}"
else
  cargo_args=()
  if [ -n "${CARGO_ARGS:-}" ]; then
    # shellcheck disable=SC2206
    cargo_args=(${CARGO_ARGS})
  fi
  "${CARGO:-cargo}" "${cargo_args[@]}" run --package ab-aat-to-parser-ir -- "${convert_args[@]}"
fi

jq -e '.schema_id == "https://w3id.org/abc/schemas/parser-ir.schema.json"' "$out_dir/parser-ir.json"
jq -e '.mapping.mapping_version == "0.1.1"' "$out_dir/divergence.json"
jq -e 'all(.records[]; .rule_id != null and .message != null and .count >= 1)' "$out_dir/divergence.json"

python3 - "$abc_root/schemas/parser-ir.schema.json" \
  "$abc_root/schemas/aat-parser-ir-divergence.schema.json" \
  "$repo_root/data/aat-parser-ir-divergence-bundle-v1.schema.json" \
  "$out_dir/parser-ir.json" \
  "$out_dir/divergence.json" <<'PY'
import json
import sys

import jsonschema

parser_schema_path, record_schema_path, bundle_schema_path, parser_ir_path, bundle_path = sys.argv[1:]
with open(parser_schema_path, encoding="utf-8") as handle:
    parser_schema = json.load(handle)
with open(record_schema_path, encoding="utf-8") as handle:
    record_schema = json.load(handle)
with open(bundle_schema_path, encoding="utf-8") as handle:
    bundle_schema = json.load(handle)
with open(parser_ir_path, encoding="utf-8") as handle:
    parser_ir = json.load(handle)
with open(bundle_path, encoding="utf-8") as handle:
    bundle = json.load(handle)

jsonschema.validate(parser_ir, parser_schema)
jsonschema.validate(bundle, bundle_schema)
for record in bundle["records"]:
    jsonschema.validate(record, record_schema)
PY

audit_dir="$out_dir/audit-corpus"
mkdir -p "$audit_dir"
cp "$aat" "$audit_dir/pass.aat.json"

audit_args=(
  audit-corpus
  --aat-dir "$audit_dir"
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json"
  --summary-json "$out_dir/audit-summary.json"
  --report-md "$out_dir/audit-report.md"
  --jobs 2
  --abc-root "$abc_root"
)

if [ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]; then
  "$AB_AAT_TO_PARSER_IR_BIN" "${audit_args[@]}"
else
  "${CARGO:-cargo}" "${cargo_args[@]}" run --package ab-aat-to-parser-ir -- "${audit_args[@]}"
fi

jq -e '.totals.files_attempted == 1' "$out_dir/audit-summary.json"
jq -e '.totals.files_succeeded == 1' "$out_dir/audit-summary.json"
jq -e '.totals.files_failed == 0' "$out_dir/audit-summary.json"
grep -n 'Full-Corpus AAT Parser-IR Conversion Audit' "$out_dir/audit-report.md"
