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
supported_mapping="$out_dir/mapping-supported.json"
supported_summary_json="$out_dir/coverage-supported.summary.json"
supported_report_md="$out_dir/coverage-supported.md"
candidate_summary_json="$out_dir/coverage-candidate.summary.json"
candidate_report_md="$out_dir/coverage-candidate.md"
unknown_mapping="$out_dir/mapping-unknown-pointer.json"
unknown_summary_json="$out_dir/coverage-unknown-pointer.summary.json"
unknown_report_md="$out_dir/coverage-unknown-pointer.md"
owner_mapping="$out_dir/mapping-owner-categories.json"
owner_summary_json="$out_dir/coverage-owner-categories.summary.json"
owner_report_md="$out_dir/coverage-owner-categories.md"
valid_custom_contract="$out_dir/custom-contract.valid.json"
spoofed_custom_contract="$out_dir/custom-contract.spoofed.json"
invalid_custom_contract="$out_dir/custom-contract.invalid.json"
invalid_contract_summary_json="$out_dir/coverage-invalid-contract.summary.json"
invalid_contract_report_md="$out_dir/coverage-invalid-contract.md"
incomplete_matrix_summary="$out_dir/matrix-summary-incomplete.json"
incomplete_source_delta="$out_dir/source-delta-incomplete.summary.json"
incomplete_summary_json="$out_dir/coverage-incomplete.summary.json"
incomplete_report_md="$out_dir/coverage-incomplete.md"
unknown_node_schema="$out_dir/parser-ir-unknown-node.schema.json"
unknown_node_summary_json="$out_dir/coverage-unknown-node.summary.json"
unknown_node_report_md="$out_dir/coverage-unknown-node.md"

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
        {"$ref": "#/$defs/layoutSpanNode"},
        {"$ref": "#/$defs/rawSourceNode"}
      ]
    },
    "textNode": {"properties": {"type": {"const": "text"}}},
    "rubyNode": {"properties": {"type": {"const": "ruby"}}},
    "warigakiNode": {"properties": {"type": {"const": "warigaki"}}},
    "layoutSpanNode": {"properties": {"type": {"const": "layout-span"}}},
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
      "rule_id": "I-02",
      "category": "INVENTION",
      "aat_pointer": "blocks[].content[].figure.filename",
      "parser_ir_pointer": "image.src",
      "description": "figure filename projects to parser-IR image source"
    },
    {
      "rule_id": "L-03",
      "category": "LOSS",
      "aat_pointer": "blocks[].content[].figure.caption",
      "parser_ir_pointer": null,
      "description": "figure caption can be emitted as caption policy projection"
    },
    {
      "rule_id": "A-04",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].span",
      "parser_ir_pointer": "span",
      "description": "Observed 10 occurrences; span coordinates need preservation"
    },
    {
      "rule_id": "A-05",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].style",
      "parser_ir_pointer": "emphasis",
      "description": "Observed 11 occurrences; style projects through TEI policy"
    },
    {
      "rule_id": "L-04",
      "category": "LOSS",
      "aat_pointer": "blocks[].content[].figure.width",
      "parser_ir_pointer": null,
      "description": "Observed 12 occurrences; figure width projects through TEI policy"
    },
    {
      "rule_id": "L-05",
      "category": "LOSS",
      "aat_pointer": "blocks[].heading.content[].tcy",
      "parser_ir_pointer": null,
      "description": "Observed 13 occurrences; tcy needs schema and profile admission"
    },
    {
      "rule_id": "S-02",
      "category": "STRUCTURAL",
      "aat_pointer": "blocks[].heading",
      "parser_ir_pointer": null,
      "description": "Observed 14 occurrences; heading structure is represented by parser-IR heading and TEI head policy"
    },
    {
      "rule_id": "S-03",
      "category": "STRUCTURAL",
      "aat_pointer": "blocks[].jisage_block",
      "parser_ir_pointer": null,
      "description": "Observed 15 occurrences; jisage structure is represented by paragraph layout or indentation policy"
    },
    {
      "rule_id": "L-06",
      "category": "LOSS",
      "aat_pointer": "blocks[].heading.content[].gaiji",
      "parser_ir_pointer": null,
      "description": "Observed 16 occurrences; heading inline gaiji needs heading inline-content schema support"
    },
    {
      "rule_id": "U-01",
      "category": "UNSUPPORTED",
      "aat_pointer": "blocks[].content[].raw_material",
      "parser_ir_pointer": "(none)",
      "description": "Observed 288 occurrences; fixture unsupported raw source"
    },
    {
      "rule_id": "A-02",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].heading.level",
      "parser_ir_pointer": "heading.level",
      "description": "heading level projects to parser-IR policy field"
    },
    {
      "rule_id": "A-03",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].gaiji.raw_marker",
      "parser_ir_pointer": "gaiji.raw_marker",
      "description": "gaiji raw marker projects to parser-IR policy field"
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

cat > "$valid_custom_contract" <<'JSON'
{
  "$id": "https://example.org/abc/custom-contract-candidate.json",
  "schema_version": "candidate-0.0.1",
  "records": []
}
JSON

cat > "$spoofed_custom_contract" <<'JSON'
{
  "$id": "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json",
  "schema_version": "0.2.0",
  "title": "ABC Parser-IR Publication Preservation Sidecar",
  "properties": {
    "coverage": {
      "properties": {
        "classes": {
          "items": {
            "enum": ["custom_sidecar", "tei_profile_projection"]
          }
        }
      }
    }
  },
  "$defs": {
    "record": {
      "properties": {
        "class": {
          "enum": ["custom_sidecar", "tei_profile_projection"]
        },
        "construct": {
          "enum": [
            "accent",
            "diagnostic",
            "figure_metadata",
            "gaiji_resolution",
            "heading_jisage_structure",
            "mapping_identity",
            "paragraph.node_range",
            "paragraph.source_pointer",
            "producer_metrics",
            "source_identity",
            "source_note.classification",
            "span_coordinates",
            "style_rendition"
          ]
        }
      }
    }
  }
}
JSON

printf '%s\n' '{ invalid json' > "$invalid_custom_contract"

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "ir-publication-coverage-v1"' "$summary_json" >/dev/null
jq -e '.scope.kind == "ir_publication_coverage"' "$summary_json" >/dev/null
jq -e '.scope.required_parsers == ["aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora"]' "$summary_json" >/dev/null
jq -e '.source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' "$summary_json" >/dev/null
jq -e '.source_region_contract.verdict == "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"' "$summary_json" >/dev/null
jq -e '.source_region_contract.schema_id == "https://w3id.org/abc/schemas/source-region-coverage.schema.json"' "$summary_json" >/dev/null
jq -e '.source_region_contract.schema_version == "aozora-source-region-coverage-v1"' "$summary_json" >/dev/null
jq -e '.source_region_contract.policy_id == "https://w3id.org/abc/policies/source-region-publication-v0"' "$summary_json" >/dev/null
jq -e '.source_region_contract.manifest_sidecar_role_present == true' "$summary_json" >/dev/null
jq -e '.parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_COMPLETE"' "$summary_json" >/dev/null
jq -e '.plaintext_policy.metadata_policy == "exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance"' "$summary_json" >/dev/null
jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_MISSING"' "$summary_json" >/dev/null
jq -e '.tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_MISSING"' "$summary_json" >/dev/null
jq -e '.field_coverage.by_field."gaiji.raw_marker".class == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.field_coverage.by_field."paragraph.layout".class == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.field_coverage.by_field."mapping.identity".class == "custom_sidecar"' "$summary_json" >/dev/null
jq -e '.field_coverage.by_field."source.pointer".class == "custom_sidecar"' "$summary_json" >/dev/null
jq -e '.diagnostic_coverage.by_field."warnings[].code".class == "custom_sidecar"' "$summary_json" >/dev/null
jq -e '.diagnostic_coverage.by_field."errors[].span".class == "custom_sidecar"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.text.class == "tei_exact"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.ruby.class == "tei_exact"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type."layout-span".class == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type.warigaki.class == "tei_plus_abc_extension"' "$summary_json" >/dev/null
jq -e '.node_coverage.by_node_type."raw-source".class == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.node_coverage.counts_by_class.tei_exact == 2' "$summary_json" >/dev/null
jq -e '.node_coverage.counts_by_class.tei_plus_abc_extension == 1' "$summary_json" >/dev/null
jq -e '.node_coverage.counts_by_class.tei_policy_projection == 2' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.by_construct.image.class == "tei_exact"' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.by_construct.caption.class == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.tei_exact == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.tei_policy_projection == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.tei_plus_abc_extension == 1' "$summary_json" >/dev/null
jq -e '.source_construct_coverage.counts_by_class.unsupported_gap == 8' "$summary_json" >/dev/null
jq -e '.unsupported_gaps.count == 8' "$summary_json" >/dev/null
jq -e '.unsupported_gaps.items[] | select(.aat_pointer == "blocks[].content[].raw_material") | .observed_occurrences == 288' "$summary_json" >/dev/null
jq -e '.unsupported_gaps.items[] | select(.aat_pointer == "blocks[].content[].raw_material") | .prevalence_source == "rule_description"' "$summary_json" >/dev/null
jq -e '([.unsupported_gaps.items[] | select(.aat_pointer == "blocks[].content[].figure.caption" or .aat_pointer == "blocks[].content[].figure.filename")] | length) == 0' "$summary_json" >/dev/null
jq -e '([.unsupported_gaps.items[] | select(.parser_ir_pointer == "heading.level" or .parser_ir_pointer == "gaiji.raw_marker")] | length) == 0' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.count == 7' "$summary_json" >/dev/null
jq -e '.closure_gaps.true_unsupported_gaps.count == 1' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.closure_family == "figure_metadata") | .closure_lane == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.closure_family == "style_rendition") | .closure_lane == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.closure_family == "span_coordinates") | .closure_lane == "custom_sidecar"' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.aat_pointer == "blocks[].heading.content[].tcy") | .closure_family == "font_tcy" and .closure_lane == "parser_ir_schema_delta"' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.aat_pointer == "blocks[].heading") | .closure_family == "heading_jisage_structure" and .closure_lane == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.aat_pointer == "blocks[].jisage_block") | .closure_family == "heading_jisage_structure" and .closure_lane == "tei_policy_projection"' "$summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.items[] | select(.aat_pointer == "blocks[].heading.content[].gaiji") | .closure_family == "heading_inline_content" and .closure_lane == "parser_ir_schema_delta"' "$summary_json" >/dev/null
jq -e '.closure_gaps.true_unsupported_gaps.items[0].closure_family == null' "$summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"' "$summary_json" >/dev/null
rg -n "IR Publication Coverage" "$report_md" >/dev/null
rg -n "Field Coverage" "$report_md" >/dev/null
rg -n "Raw Unsupported-Derived Mapping Rows" "$report_md" >/dev/null

jq 'del(.transform_rule_descriptions[] | select(.category == "UNSUPPORTED"))' "$mapping" > "$supported_mapping"

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$supported_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --summary-json "$supported_summary_json" \
  --report-md "$supported_report_md"

jq -e '.unsupported_gaps.count == 7' "$supported_summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.count == 7' "$supported_summary_json" >/dev/null
jq -e '.closure_gaps.true_unsupported_gaps.count == 0' "$supported_summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"' "$supported_summary_json" >/dev/null

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$supported_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --custom-contract-schema "$valid_custom_contract" \
  --summary-json "$candidate_summary_json" \
  --report-md "$candidate_report_md"

jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_CANDIDATE_PROVIDED"' "$candidate_summary_json" >/dev/null
jq -e '.custom_contract.schema_id == "https://example.org/abc/custom-contract-candidate.json"' "$candidate_summary_json" >/dev/null
jq -e '.tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_MISSING"' "$candidate_summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"' "$candidate_summary_json" >/dev/null

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$supported_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --custom-contract-schema "$spoofed_custom_contract" \
  --summary-json "$candidate_summary_json" \
  --report-md "$candidate_report_md"

jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_CANDIDATE_PROVIDED"' "$candidate_summary_json" >/dev/null
jq -e '.tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_MISSING"' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_custom_contract.count == 0' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_tei_profile.count == 0' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.count == 7' "$candidate_summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"' "$candidate_summary_json" >/dev/null

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$supported_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --custom-contract-schema "$repo_root/data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json" \
  --summary-json "$candidate_summary_json" \
  --report-md "$candidate_report_md"

jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"' "$candidate_summary_json" >/dev/null
jq -e '.source_region_contract.verdict == "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"' "$candidate_summary_json" >/dev/null
jq -e '.tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_custom_contract.count == 1' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_custom_contract.counts_by_family.span_coordinates == 1' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_tei_profile.count == 4' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_tei_profile.counts_by_family.style_rendition == 1' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_tei_profile.counts_by_family.figure_metadata == 1' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_tei_profile.counts_by_family.heading_jisage_structure == 2' "$candidate_summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.count == 2' "$candidate_summary_json" >/dev/null
jq -e '([.closure_gaps.classified_but_not_admitted.items[] | select(.closure_family == "span_coordinates")] | length) == 0' "$candidate_summary_json" >/dev/null
jq -e '([.closure_gaps.classified_but_not_admitted.items[] | select(.closure_family == "style_rendition" or .closure_family == "figure_metadata" or .closure_family == "heading_jisage_structure")] | length) == 0' "$candidate_summary_json" >/dev/null
jq -e '.unsupported_derived_closure_coverage.counts_by_status.admitted_by_custom_contract == 1' "$candidate_summary_json" >/dev/null
jq -e '.unsupported_derived_closure_coverage.counts_by_status.admitted_by_tei_profile == 4' "$candidate_summary_json" >/dev/null
jq -e '.unsupported_derived_closure_coverage.counts_by_status.classified_but_not_admitted == 2' "$candidate_summary_json" >/dev/null
jq -e '.unsupported_derived_closure_coverage.counts_by_status.true_unsupported_gap == 0' "$candidate_summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS"' "$candidate_summary_json" >/dev/null

REPO_ROOT="$repo_root" python3 - <<'PY'
import importlib.util
import os
import pathlib

script = pathlib.Path(os.environ["REPO_ROOT"]) / "reports/parser-ir/publication-coverage.py"
spec = importlib.util.spec_from_file_location("publication_coverage", script)
module = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(module)

coverage_only = {
    "verdict": "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION",
    "schema_id": module.ABC_PRESERVATION_SCHEMA_ID,
    "schema_version": module.ABC_PRESERVATION_SCHEMA_VERSION,
    "record_classes": ["custom_sidecar"],
    "coverage_classes": ["custom_sidecar", "tei_profile_projection"],
    "constructs": sorted(module.TEI_PROFILE_CONSTRUCTS),
}
result = module.tei_profile_contract_block(coverage_only)
assert result["verdict"] == "TEI_PROFILE_CONTRACT_INCOMPLETE", result
assert result["missing_record_classes"] == ["tei_profile_projection"], result

missing_construct = {
    "verdict": "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION",
    "schema_id": module.ABC_PRESERVATION_SCHEMA_ID,
    "schema_version": module.ABC_PRESERVATION_SCHEMA_VERSION,
    "record_classes": ["custom_sidecar", "tei_profile_projection"],
    "coverage_classes": ["custom_sidecar", "tei_profile_projection"],
    "constructs": ["accent", "figure_metadata", "style_rendition"],
}
result = module.tei_profile_contract_block(missing_construct)
assert result["verdict"] == "TEI_PROFILE_CONTRACT_INCOMPLETE", result
assert result["missing_constructs"] == ["heading_jisage_structure"], result
PY

cat > "$unknown_mapping" <<'JSON'
{
  "mapping_id": "https://w3id.org/abc/mappings/fixture-unknown",
  "mapping_version": "0.2.3",
  "mapping_schema_hash": "sha256:38e7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:fixture-parser-ir",
  "transform_rule_descriptions": [
    {
      "rule_id": "X-01",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].mystery_marker",
      "parser_ir_pointer": "parser.unknown",
      "description": "fixture unknown source construct"
    }
  ]
}
JSON

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$unknown_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --custom-contract-schema "$valid_custom_contract" \
  --summary-json "$unknown_summary_json" \
  --report-md "$unknown_report_md"

jq -e '.unsupported_gaps.count == 1' "$unknown_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[0].aat_pointer == "blocks[].content[].mystery_marker"' "$unknown_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[0].owner == "policy"' "$unknown_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[0].observed_occurrences == null' "$unknown_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[0].prevalence_source == "unavailable"' "$unknown_summary_json" >/dev/null
jq -e '.closure_gaps.true_unsupported_gaps.count == 1' "$unknown_summary_json" >/dev/null
jq -e '.closure_gaps.true_unsupported_gaps.items[0].closure_family == null' "$unknown_summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS"' "$unknown_summary_json" >/dev/null

cat > "$owner_mapping" <<'JSON'
{
  "mapping_id": "https://w3id.org/abc/mappings/fixture-owner-categories",
  "mapping_version": "0.2.4",
  "mapping_schema_hash": "sha256:owner-category-fixture",
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:fixture-parser-ir",
  "transform_rule_descriptions": [
    {
      "rule_id": "U-99",
      "category": "UNSUPPORTED",
      "aat_pointer": "blocks[].content[].owner_unsupported",
      "parser_ir_pointer": "unknown_unsupported",
      "description": "unsupported category owner fixture"
    },
    {
      "rule_id": "L-99",
      "category": "LOSS",
      "aat_pointer": "blocks[].content[].owner_loss",
      "parser_ir_pointer": "unknown_loss",
      "description": "loss category owner fixture"
    },
    {
      "rule_id": "S-99",
      "category": "STRUCTURAL",
      "aat_pointer": "blocks[].content[].owner_structural",
      "parser_ir_pointer": "unknown_structural",
      "description": "structural category owner fixture"
    },
    {
      "rule_id": "A-99",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].owner_ambiguity",
      "parser_ir_pointer": "unknown_ambiguity",
      "description": "ambiguity category owner fixture"
    },
    {
      "rule_id": "X-99",
      "category": "SOMETHING_NEW",
      "aat_pointer": "blocks[].content[].owner_unknown",
      "parser_ir_pointer": "unknown_other",
      "description": "unknown category owner fixture"
    }
  ]
}
JSON

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$owner_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --custom-contract-schema "$valid_custom_contract" \
  --summary-json "$owner_summary_json" \
  --report-md "$owner_report_md"

jq -e '.unsupported_gaps.count == 5' "$owner_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[] | select(.rule_id == "U-99") | .owner == "parser_ir_schema"' "$owner_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[] | select(.rule_id == "L-99") | .owner == "custom_schema"' "$owner_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[] | select(.rule_id == "S-99") | .owner == "aat_to_parser_ir_converter"' "$owner_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[] | select(.rule_id == "A-99") | .owner == "policy"' "$owner_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[] | select(.rule_id == "X-99") | .owner == "policy"' "$owner_summary_json" >/dev/null
jq -e '([.unsupported_gaps.items[] | select(.owner == "evidence")] | length) == 0' "$owner_summary_json" >/dev/null

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$supported_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --custom-contract-schema "$invalid_custom_contract" \
  --summary-json "$invalid_contract_summary_json" \
  --report-md "$invalid_contract_report_md"

jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_INVALID"' "$invalid_contract_summary_json" >/dev/null
jq -e '.verdict != "IR_PUBLICATION_COVERAGE_COMPLETE"' "$invalid_contract_summary_json" >/dev/null

jq '
  .rows |= map(
    if .selected_aat.adapter == "aozora"
    then .parser_ir.nodes = "not-numeric" | .materialization.status = "failed"
    else .
    end
  )
' "$matrix_summary" > "$incomplete_matrix_summary"

cat > "$incomplete_source_delta" <<'JSON'
{
  "schema_version": "plain-prose-source-delta-v1",
  "parser_evidence_coverage": {
    "verdict": "FIVE_PARSER_EVIDENCE_INCOMPLETE",
    "observed_rows_by_parser": {
      "aozora2html": 1,
      "aozora-epub3": 1,
      "aozora-rs": 1,
      "aozora2": 1,
      "aozora": 0
    },
    "missing_parsers": ["aozora"]
  }
}
JSON

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$parser_schema" \
  --mapping "$supported_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$incomplete_matrix_summary" \
  --source-delta-summary "$incomplete_source_delta" \
  --summary-json "$incomplete_summary_json" \
  --report-md "$incomplete_report_md"

jq -e '.parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_INCOMPLETE"' "$incomplete_summary_json" >/dev/null
jq -e '.parser_evidence_coverage.observed_rows_by_parser.aozora == 0' "$incomplete_summary_json" >/dev/null
jq -e '.parser_evidence_coverage.missing_parsers == ["aozora"]' "$incomplete_summary_json" >/dev/null
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_BLOCKED_INCOMPLETE_PARSER_EVIDENCE"' "$incomplete_summary_json" >/dev/null

cat > "$unknown_node_schema" <<'JSON'
{
  "$id": "https://w3id.org/abc/schemas/parser-ir-unknown-node.schema.json",
  "properties": {"schema_hash": {"const": "sha256:fixture-parser-ir-unknown-node"}},
  "$defs": {
    "node": {
      "oneOf": [
        {"$ref": "#/$defs/textNode"},
        {"$ref": "#/$defs/mysteryNode"}
      ]
    },
    "textNode": {"properties": {"type": {"const": "text"}}},
    "mysteryNode": {"properties": {"type": {"const": "mystery-node"}}}
  }
}
JSON

python3 "$repo_root/reports/parser-ir/publication-coverage.py" \
  --parser-ir-schema "$unknown_node_schema" \
  --mapping "$supported_mapping" \
  --source-summary "$source_summary" \
  --matrix-summary "$matrix_summary" \
  --source-delta-summary "$source_delta" \
  --custom-contract-schema "$valid_custom_contract" \
  --summary-json "$unknown_node_summary_json" \
  --report-md "$unknown_node_report_md"

jq -e '.node_coverage.by_node_type."mystery-node".class == "unsupported_gap"' "$unknown_node_summary_json" >/dev/null
jq -e '.node_coverage.unsupported[0].owner == "custom_schema"' "$unknown_node_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[0].observed_occurrences == null' "$unknown_node_summary_json" >/dev/null
jq -e '.unsupported_gaps.items[0].prevalence_source == "unavailable"' "$unknown_node_summary_json" >/dev/null
