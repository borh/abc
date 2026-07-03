#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aat-parser-ir-mapping-policy-smoke"
aat_dir="$out_dir/aat"

rm -rf "$out_dir"
mkdir -p "$aat_dir"

cat > "$aat_dir/policy.json" <<'JSON'
{
  "version": 1,
  "work_id": "policy",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "windows-31j-lossy",
    "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "parse_complete": true,
    "warnings": [
      { "message": "fixture warning" }
    ]
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "gaiji",
          "description": "U+4E00",
          "resolved": "一",
          "jis_code": null,
          "unresolved_reason": null
        },
        {
          "kind": "warigaki",
          "upper": [{ "kind": "text", "value": "上" }],
          "lower": [{ "kind": "text", "value": "下" }]
        },
        {
          "kind": "style",
          "style_type": "kaeriten",
          "content": [{ "kind": "text", "value": "レ" }]
        }
      ]
    }
  ]
}
JSON

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$repo_root/../abc" \
  --mapping-version 0.1.1 \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json"

jq -e '.mapping_version == "0.1.1"' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' "$out_dir/mapping.json"

python3 - <<PY
import json
from pathlib import Path
import sys

repo_root = Path("$repo_root")
sys.path.insert(0, str(repo_root / "reports/aat-fidelity/aat_parser_ir_mapping"))
import generate

aat = json.loads((Path("$aat_dir") / "policy.json").read_text())
_ledger, nodes, _block_kinds, _inline_kinds, _has_warigaki = generate.map_aat_document(aat)
spans = [node["span"] for node in nodes]
assert spans, "fixture should emit parser-IR nodes"
assert any(span["end"] > 0 for span in spans), spans
for previous, current in zip(spans, spans[1:]):
    assert current["start"] >= previous["start"], spans
    assert current["end"] >= current["start"], spans
PY
