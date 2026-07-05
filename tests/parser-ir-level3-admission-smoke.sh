#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-level3-admission-smoke.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

source_summary="$out_dir/source-summary.json"
failing_source_summary="$out_dir/source-summary-failing.json"
matrix_summary="$out_dir/matrix-summary.json"
mapping_file="$out_dir/mapping.json"
invalid_mapping_file="$out_dir/mapping-invalid.json"
summary_json="$out_dir/admission.summary.json"
report_md="$out_dir/admission.md"
failing_summary_json="$out_dir/admission-source-fail.summary.json"
failing_report_md="$out_dir/admission-source-fail.md"
invalid_summary_json="$out_dir/admission-invalid-mapping.summary.json"
invalid_report_md="$out_dir/admission-invalid-mapping.md"
invalid_stderr="$out_dir/admission-invalid-mapping.stderr"
foreign_checkout_root="$out_dir/foreign-checkout"
foreign_mapping_file="$foreign_checkout_root/data/aat-to-parser-ir-mapping-v1.json"
foreign_matrix_summary="$out_dir/matrix-summary-foreign-mapping.json"
foreign_summary_json="$out_dir/admission-foreign-mapping.summary.json"
foreign_report_md="$out_dir/admission-foreign-mapping.md"
unmapped_checkout_root="$out_dir/unmapped-checkout"
unmapped_mapping_file="$unmapped_checkout_root/external/mapping.json"
unmapped_matrix_summary="$out_dir/matrix-summary-unmapped-mapping.json"
unmapped_summary_json="$out_dir/admission-unmapped-mapping.summary.json"
unmapped_report_md="$out_dir/admission-unmapped-mapping.md"
unmapped_stderr="$out_dir/admission-unmapped-mapping.stderr"

cat > "$source_summary" <<'JSON'
{
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 17894,
  "unallowlisted_unknown_markers_total": 0
}
JSON

cat > "$failing_source_summary" <<'JSON'
{
  "gate_status": "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED",
  "works_scanned": 17894,
  "unallowlisted_unknown_markers_total": 2
}
JSON

cat > "$mapping_file" <<'JSON'
{
  "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
  "mapping_version": "0.2.3",
  "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
  "source_aat_version": 1,
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:a1fcd348bf396d8d4e6f30ffb928b76b3802b594ea773ed6fa9e1dac52edf712",
  "transform_rule_descriptions": [
    {
      "rule_id": "A-01",
      "category": "AMBIGUITY",
      "aat_pointer": "blocks[].content[].ruby.direction",
      "parser_ir_pointer": "ruby.placement",
      "action": "project",
      "description": "fixture rule"
    }
  ],
  "loss_taxonomy": {
    "AMBIGUITY": {"description": "fixture", "default_action": "drop-sidecar", "records_sidecar": true}
  }
}
JSON

cat > "$matrix_summary" <<JSON
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "candidate_mode": "all",
    "structural_summary": "fixture-structural-summary.json",
    "mapping": "mapping.json"
  },
  "totals": {
    "rows_attempted": 8,
    "tei_eaj_rows_attempted": 7,
    "materialization_succeeded": 8,
    "materialization_failed": 0,
    "rows_skipped": 1
  },
  "skipped": [
    {"tei_eaj_file": "data/etc/no-work-id.xml", "reason": "no_materializable_aat"}
  ],
  "rows": [
    {
      "work_id": "plain-pass",
      "title": "Plain pass",
      "tei_eaj_file": "data/plain-pass.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "plain-note",
      "title": "Plain source note",
      "tei_eaj_file": "data/plain-note.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "source_note_back_routing"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "plain-over",
      "title": "Plain over segmented",
      "tei_eaj_file": "data/plain-over.xml",
      "selected_aat": {"adapter": "aozora-rs", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "plain-text-policy",
      "title": "Plain text policy",
      "tei_eaj_file": "data/plain-text-policy.xml",
      "selected_aat": {"adapter": "aozora-epub3", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned"},
      "text": {"body_text_match_bucket": "ruby_expanded_equal"}
    },
    {
      "work_id": "plain-page-break",
      "title": "Plain page break",
      "tei_eaj_file": "data/plain-page-break.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "page_break_projection"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "drama",
      "title": "Drama",
      "tei_eaj_file": "data/drama.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "drama"},
      "classification": {"paragraph_origin_bucket": "adapter_under_segmented"},
      "text": {"body_text_match_bucket": "different"}
    },
    {
      "work_id": "verse",
      "title": "Verse",
      "tei_eaj_file": "data/verse.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "verse"},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented"},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "lv4",
      "title": "Level 4 enrichment",
      "tei_eaj_file": "data/lv4.xml",
      "selected_aat": {"adapter": "aozora2html", "adapter_version": "fixture"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "lv4_enrichment"},
      "classification": {"paragraph_origin_bucket": "aligned"},
      "text": {"body_text_match_bucket": "base_equal"}
    }
  ]
}
JSON

cat > "$invalid_mapping_file" <<'JSON'
{
  "mapping_id": "",
  "mapping_version": "0.2.3",
  "mapping_schema_hash": "",
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:a1fcd348bf396d8d4e6f30ffb928b76b3802b594ea773ed6fa9e1dac52edf712",
  "transform_rule_descriptions": []
}
JSON

mkdir -p "$(dirname "$foreign_mapping_file")"
cat > "$foreign_mapping_file" <<'JSON'
{
  "mapping_id": "https://example.invalid/not-the-current-checkout",
  "mapping_version": "999.0.0",
  "mapping_schema_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
  "target_parser_ir_schema_id": "https://example.invalid/schemas/foreign-parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
  "transform_rule_descriptions": [
    {
      "rule_id": "foreign-rule",
      "description": "should not be loaded"
    }
  ]
}
JSON

python3 "$repo_root/reports/parser-ir/level3-admission.py" \
  --matrix-summary "$matrix_summary" \
  --source-summary "$source_summary" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "profile-aware-level3-tei-admission-v1"' "$summary_json"
jq -e '.source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' "$summary_json"
jq -e '.parser_ir_infrastructure_verdict == "LEVEL3_IR_INFRASTRUCTURE_READY"' "$summary_json"
jq -e '.mapping.mapping_id == "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"' "$summary_json"
jq -e '.mapping.mapping_version == "0.2.3"' "$summary_json"
jq -e '.mapping.mapping_hash | test("^sha256:[0-9a-f]{64}$")' "$summary_json"
jq -e '.mapping.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' "$summary_json"
jq -e '.mapping.target_parser_ir_schema_id == "https://w3id.org/abc/schemas/parser-ir.schema.json"' "$summary_json"
jq -e '.mapping.target_parser_ir_schema_hash == "sha256:a1fcd348bf396d8d4e6f30ffb928b76b3802b594ea773ed6fa9e1dac52edf712"' "$summary_json"
jq -e '.mapping.generated_mapping_rules == 1' "$summary_json"
jq -e '.plain_prose_admission.rows_total == 5' "$summary_json"
jq -e '.plain_prose_admission.rows_passed == 2' "$summary_json"
jq -e '.plain_prose_admission.rows_failed == 3' "$summary_json"
jq -e '.plain_prose_admission.verdict == "LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY"' "$summary_json"
jq -e '.plain_prose_admission.blocking_owners == ["adapter", "policy"]' "$summary_json"
jq -e '.plain_prose_admission.failures_by_owner.adapter == 1' "$summary_json"
jq -e '.plain_prose_admission.failures_by_owner.policy == 2' "$summary_json"
jq -e '.plain_prose_admission.failures_by_adapter["aozora-rs"] == 1' "$summary_json"
jq -e '.plain_prose_admission.paragraph_origin_buckets.source_note_back_routing == 1' "$summary_json"
jq -e '.plain_prose_admission.paragraph_origin_buckets.page_break_projection == 1' "$summary_json"
jq -e '.plain_prose_admission.plaintext_policy.plaintext_surface == "body_base_text"' "$summary_json"
jq -e '.plain_prose_admission.plaintext_policy.ruby_expanded_surfaces == "diagnostic_only"' "$summary_json"
jq -e '.plain_prose_admission.plaintext_policy.metadata_policy == "exclude_typed_metadata_from_plaintext"' "$summary_json"
jq -e '.profile_lanes.drama.verdict == "LANE_POLICY_REQUIRED"' "$summary_json"
jq -e '.profile_lanes.verse.verdict == "LANE_POLICY_REQUIRED"' "$summary_json"
jq -e '.profile_lanes.lv4_enrichment.verdict == "LANE_OUT_OF_SCOPE_FOR_LEVEL3"' "$summary_json"
jq -e '.evidence_gaps.rows == 1' "$summary_json"
rg -n 'LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY' "$report_md"
rg -n 'Drama' "$report_md"
rg -n 'Level 4 enrichment' "$report_md"

python3 "$repo_root/reports/parser-ir/level3-admission.py" \
  --matrix-summary "$matrix_summary" \
  --source-summary "$failing_source_summary" \
  --summary-json "$failing_summary_json" \
  --report-md "$failing_report_md"

jq -e '.source_authority_gate.gate_status == "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED"' "$failing_summary_json"
jq -e '.plain_prose_admission.rows_total == 5' "$failing_summary_json"
jq -e '.plain_prose_admission.rows_passed == 0' "$failing_summary_json"
jq -e '.plain_prose_admission.rows_failed == 5' "$failing_summary_json"
jq -e '.plain_prose_admission.verdict == "LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY_AND_EVIDENCE"' "$failing_summary_json"
jq -e '.plain_prose_admission.blocking_owners == ["adapter", "policy", "evidence"]' "$failing_summary_json"
jq -e '.plain_prose_admission.failures_by_owner.evidence == 5' "$failing_summary_json"

cat > "$foreign_matrix_summary" <<JSON
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "candidate_mode": "all",
    "structural_summary": "fixture-structural-summary.json",
    "mapping": "$foreign_mapping_file"
  },
  "totals": {
    "rows_attempted": 0,
    "tei_eaj_rows_attempted": 0,
    "materialization_succeeded": 0,
    "materialization_failed": 0,
    "rows_skipped": 0
  },
  "skipped": [],
  "rows": []
}
JSON

python3 "$repo_root/reports/parser-ir/level3-admission.py" \
  --matrix-summary "$foreign_matrix_summary" \
  --source-summary "$source_summary" \
  --summary-json "$foreign_summary_json" \
  --report-md "$foreign_report_md"

jq -e --arg expected "$(jq -r '.mapping_id' "$repo_root/data/aat-to-parser-ir-mapping-v1.json")" \
  '.mapping.mapping_id == $expected' "$foreign_summary_json"
jq -e --arg unexpected "https://example.invalid/not-the-current-checkout" \
  '.mapping.mapping_id != $unexpected' "$foreign_summary_json"

mkdir -p "$(dirname "$unmapped_mapping_file")"
cat > "$unmapped_mapping_file" <<'JSON'
{
  "mapping_id": "https://example.invalid/unmapped-external-path",
  "mapping_version": "1.0.0",
  "mapping_schema_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222",
  "target_parser_ir_schema_id": "https://example.invalid/schemas/unmapped-parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:3333333333333333333333333333333333333333333333333333333333333333",
  "transform_rule_descriptions": [
    {
      "rule_id": "unmapped-rule",
      "description": "should fail before load"
    }
  ]
}
JSON

cat > "$unmapped_matrix_summary" <<JSON
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "candidate_mode": "all",
    "structural_summary": "fixture-structural-summary.json",
    "mapping": "$unmapped_mapping_file"
  },
  "totals": {
    "rows_attempted": 0,
    "tei_eaj_rows_attempted": 0,
    "materialization_succeeded": 0,
    "materialization_failed": 0,
    "rows_skipped": 0
  },
  "skipped": [],
  "rows": []
}
JSON

if python3 "$repo_root/reports/parser-ir/level3-admission.py" \
  --matrix-summary "$unmapped_matrix_summary" \
  --source-summary "$source_summary" \
  --summary-json "$unmapped_summary_json" \
  --report-md "$unmapped_report_md" \
  2>"$unmapped_stderr"; then
  echo "expected unrewriteable external mapping path to fail" >&2
  exit 1
fi

rg -n 'current checkout|cannot remap|mapping path' "$unmapped_stderr"

invalid_matrix_summary="$out_dir/matrix-summary-invalid-mapping.json"
cat > "$invalid_matrix_summary" <<JSON
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "candidate_mode": "all",
    "structural_summary": "fixture-structural-summary.json",
    "mapping": "mapping-invalid.json"
  },
  "totals": {
    "rows_attempted": 0,
    "tei_eaj_rows_attempted": 0,
    "materialization_succeeded": 0,
    "materialization_failed": 0,
    "rows_skipped": 0
  },
  "skipped": [],
  "rows": []
}
JSON

if python3 "$repo_root/reports/parser-ir/level3-admission.py" \
  --matrix-summary "$invalid_matrix_summary" \
  --source-summary "$source_summary" \
  --summary-json "$invalid_summary_json" \
  --report-md "$invalid_report_md" \
  2>"$invalid_stderr"; then
  echo "expected invalid mapping fixture to fail" >&2
  exit 1
fi

rg -n 'missing or empty mapping fields|transform_rule_descriptions' "$invalid_stderr"
