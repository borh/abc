#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-ir-publication-coverage.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

parser_schema="$out_dir/parser-ir.schema.json"
mapping="$out_dir/mapping.json"
source_summary="$out_dir/source-summary.json"
matrix_summary="$out_dir/matrix-summary.json"
source_delta="$out_dir/source-delta.summary.json"
summary_json="$out_dir/coverage.summary.json"
report_md="$out_dir/coverage.md"

cat > "$parser_schema" <<'JSON'
{
  "$id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "properties": {"schema_hash": {"const": "sha256:fixture-parser-ir"}},
  "$defs": {
    "node": {
      "oneOf": [
        {"$ref": "#/$defs/textNode"},
        {"$ref": "#/$defs/rubyNode"},
        {"$ref": "#/$defs/warigakiNode"},
        {"$ref": "#/$defs/rawSourceNode"}
      ]
    },
    "textNode": {"properties": {"type": {"const": "text"}}},
    "rubyNode": {"properties": {"type": {"const": "ruby"}}},
    "warigakiNode": {"properties": {"type": {"const": "warigaki"}}},
    "rawSourceNode": {"properties": {"type": {"const": "raw-source"}}}
  }
}
JSON

cat > "$mapping" <<'JSON'
{
  "mapping_id": "https://w3id.org/abc/mappings/fixture",
  "mapping_version": "0.2.3",
  "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:fixture-parser-ir",
  "transform_rule_descriptions": [
    {
      "rule_id": "A-01",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].ruby.direction",
      "parser_ir_pointer": "ruby.direction",
      "description": "ruby direction projects to parser-IR"
    },
    {
      "rule_id": "U-01",
      "category": "UNSUPPORTED",
      "aat_pointer": "blocks[].content[].raw",
      "parser_ir_pointer": "(none)",
      "description": "fixture unsupported raw source"
    },
    {
      "rule_id": "S-10",
      "category": "STRUCTURAL",
      "aat_pointer": "blocks[].content[].warigaki",
      "parser_ir_pointer": "warigaki",
      "description": "warigaki requires custom preservation"
    }
  ]
}
JSON

cat > "$source_summary" <<'JSON'
{
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 17894,
  "unallowlisted_unknown_markers_total": 0
}
JSON

cat > "$matrix_summary" <<'JSON'
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "adapter_preference": ["aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora"],
    "mapping": "mapping.json"
  },
  "totals": {
    "rows_attempted": 5,
    "materialization_succeeded": 5,
    "materialization_failed": 0,
    "rows_skipped": 0
  },
  "rows": [
    {"work_id": "1", "selected_aat": {"adapter": "aozora2html"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}},
    {"work_id": "1", "selected_aat": {"adapter": "aozora-epub3"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}},
    {"work_id": "1", "selected_aat": {"adapter": "aozora-rs"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}},
    {"work_id": "1", "selected_aat": {"adapter": "aozora2"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}},
    {"work_id": "1", "selected_aat": {"adapter": "aozora"}, "parser_ir": {"schema_hash": "sha256:fixture-parser-ir", "nodes": 3, "paragraph_count": 1}}
  ]
}
JSON

cat > "$source_delta" <<'JSON'
{
  "schema_version": "plain-prose-source-delta-v1",
  "parser_evidence_coverage": {
    "verdict": "FIVE_PARSER_EVIDENCE_COMPLETE",
    "observed_rows_by_parser": {
      "aozora2html": 1,
      "aozora-epub3": 1,
      "aozora-rs": 1,
      "aozora2": 1,
      "aozora": 1
    },
    "missing_parsers": []
  }
}
JSON

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "ir-publication-coverage-v1"' "$summary_json" >/dev/null
jq -e '.source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' "$summary_json" >/dev/null
jq -e '.parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_COMPLETE"' "$summary_json" >/dev/null
jq -e '.plaintext_policy.metadata_policy == "exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance"' "$summary_json" >/dev/null
jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_MISSING"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.text.class == "tei_exact"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.ruby.class == "tei_exact"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.warigaki.class == "tei_plus_abc_extension"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type."raw-source".class == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.node_coverage.counts_by_class.tei_exact == 2' "$summary_json" >/dev/null
jq -e '.node_coverage.counts_by_class.tei_plus_abc_extension == 1' "$summary_json" >/dev/null
jq -e '.node_coverage.counts_by_class.tei_policy_projection == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.tei_exact == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.tei_plus_abc_extension == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.unsupported_gap == 1' "$summary_json" >/dev/null
jq -e '.unsupported_gaps.count == 1' "$summary_json" >/dev/null
jq -e '.unsupported_gaps.items[0].aat_pointer == "blocks[].content[].raw"' "$summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"' "$summary_json" >/dev/null
rg -n "IR Publication Coverage" "$report_md" >/dev/null
rg -n "Unsupported gaps" "$report_md" >/dev/null
