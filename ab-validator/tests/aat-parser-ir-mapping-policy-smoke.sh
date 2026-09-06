#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"
out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/aat-parser-ir-mapping-policy-smoke"
aat_dir="$out_dir/aat"

run_mapping_generator() {
  if [[ "${AB_MAPPING_USE_SYSTEM_PYTHON:-0}" == "1" ]]; then
    python "$@"
  else
    uv run --isolated --no-project --with 'jsonschema>=4.0' "$@"
  fi
}

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
        },
        {
          "kind": "ruby",
          "base": "漢",
          "reading": "かん",
          "base_content": [{ "kind": "text", "value": "漢" }],
          "reading_content": [{ "kind": "text", "value": "かん" }]
        }
      ]
    }
  ]
}
JSON

cat > "$aat_dir/nested-warigaki.json" <<'JSON'
{
  "version": 1,
  "work_id": "nested-warigaki",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "jisage_block",
      "children": [
        {
          "kind": "paragraph",
          "content": [
            {
              "kind": "style",
              "style_type": "kaeriten",
              "content": [
                {
                  "kind": "warigaki",
                  "upper": [{ "kind": "text", "value": "甲" }],
                  "lower": [{ "kind": "text", "value": "乙" }]
                }
              ]
            }
          ]
        },
        {
          "kind": "heading",
          "level": 1,
          "style": "fixture",
          "content": [
            {
              "kind": "warigaki",
              "upper": [{ "kind": "text", "value": "丙" }],
              "lower": [{ "kind": "text", "value": "丁" }]
            }
          ]
        }
      ]
    }
  ]
}
JSON

run_mapping_generator \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.4.0 \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json"

jq -e '.mapping_version == "0.4.0"' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; (.category != "STRUCTURAL") or ((.aat_pointer // "") | contains("paragraph") | not))' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "LOSS" and .aat_pointer == "meta.parse_complete")' "$out_dir/mapping.json"
jq -e '.identity_projection.parser_ir_pointer == "derived_from"' "$out_dir/summary.json"
jq -e '.identity_projection.aat_pointers == ["version", "meta.adapter", "meta.adapter_version"]' "$out_dir/summary.json"
jq -e '.files_with_warigaki == 2' "$out_dir/summary.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].children[].content[].content[].warigaki")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].children[].heading.content[].warigaki")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; (.category != "LOSS") or ((.description | contains("warigaki")) | not))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .category != "LOSS" or .aat_pointer != "blocks[].content[].ruby.base_content")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "LOSS" and .aat_pointer == "blocks[].content[].ruby.reading_content")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; (.aat_pointer // "") | contains("/") | not)' "$out_dir/mapping.json"

python - <<PY
import json
from pathlib import Path
import sys

repo_root = Path("$repo_root")
sys.path.insert(0, str(repo_root / "reports/aat-fidelity/aat_parser_ir_mapping"))
import validate_contract

mapping = json.loads(Path("$out_dir/mapping.json").read_text())
schema = json.loads((repo_root / "data/aat-schema.json").read_text())
validate_contract.validate_mapping_contract(mapping, schema)
validate_contract.validate_mapping_contract(
    {
        "transform_rule_descriptions": [
            {
                "rule_id": "A-99",
                "aat_pointer": "blocks[].children[].content[].content[].content[].gaiji.resolved",
            }
        ]
    },
    schema,
)
for pointer in [
    "blocks.content.gaiji.description",
    "blocks[].content.gaiji.description",
    "meta.warnings.line",
]:
    try:
        validate_contract.validate_mapping_contract(
            {"transform_rule_descriptions": [{"rule_id": "A-98", "aat_pointer": pointer}]},
            schema,
        )
    except validate_contract.MappingContractError:
        pass
    else:
        raise AssertionError(f"invalid pointer accepted: {pointer}")
PY

python - <<PY
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
    assert current["start"] >= previous["end"], spans
    assert current["end"] >= current["start"], spans

nested = json.loads((Path("$aat_dir") / "nested-warigaki.json").read_text())
ledger, nested_nodes, _block_kinds, _inline_kinds, has_warigaki = generate.map_aat_document(nested)
assert has_warigaki, ledger
assert any(
    entry["category"] == "UNSUPPORTED" and "warigaki" in entry["aat"]
    for entry in ledger
), ledger
assert any(
    node.get("type") == "emphasis" and node.get("text") == "甲乙"
    for node in nested_nodes
), nested_nodes
assert any(
    node.get("type") == "heading" and node.get("text") == "丙丁"
    for node in nested_nodes
), nested_nodes
nested_spans = [node["span"] for node in nested_nodes]
assert any(span["end"] > span["start"] for span in nested_spans), nested_spans
PY

layout_dir="$out_dir/layout-only-aat"
mkdir -p "$layout_dir"
cat > "$layout_dir/layout.json" <<'JSON'
{
  "version": 1,
  "work_id": "layout-only",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "jisage_block",
      "x-indent": 4,
      "children": [
        {
          "kind": "paragraph",
          "content": [
            {
              "kind": "style",
              "style_type": "burasage",
              "x-indent-first": 0,
              "x-indent-rest": 1,
              "content": [{ "kind": "text", "value": "甲" }]
            }
          ]
        }
      ]
    }
  ]
}
JSON

run_mapping_generator \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$layout_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.4.0 \
  --out "$out_dir/layout-only.mapping.json" \
  --summary-json "$out_dir/layout-only.summary.json"

jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].jisage_block")' "$out_dir/layout-only.mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .parser_ir_pointer != "indentation")' "$out_dir/layout-only.mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .parser_ir_pointer != "emphasis")' "$out_dir/layout-only.mapping.json"
