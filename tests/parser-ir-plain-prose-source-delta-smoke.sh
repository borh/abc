#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-plain-prose-delta-smoke.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

admission_summary="$out_dir/admission.summary.json"
matrix_summary="$out_dir/matrix.summary.json"
structural_summary="$out_dir/structural.summary.json"
source_summary="$out_dir/source.summary.json"
mapping_file="$out_dir/mapping.json"
summary_json="$out_dir/plain-prose-delta.summary.json"
report_md="$out_dir/plain-prose-delta.md"
strict_summary_json="$out_dir/plain-prose-delta-strict.summary.json"
strict_report_md="$out_dir/plain-prose-delta-strict.md"
strict_stderr="$out_dir/plain-prose-delta-strict.stderr"

cat > "$source_summary" <<'JSON'
{
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 17894,
  "unallowlisted_unknown_markers_total": 0
}
JSON

cat > "$mapping_file" <<'JSON'
{
  "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
  "mapping_version": "0.2.3",
  "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d",
  "transform_rule_descriptions": [
    {"rule_id": "S-01", "category": "STRUCTURAL"}
  ]
}
JSON

cat > "$admission_summary" <<'JSON'
{
  "schema_version": "profile-aware-level3-tei-admission-v1",
  "source_authority_gate": {
    "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
    "works_scanned": 17894,
    "unallowlisted_unknown_markers_total": 0
  },
  "parser_ir_infrastructure_verdict": "LEVEL3_IR_INFRASTRUCTURE_READY",
  "plain_prose_admission": {
    "rows_total": 5,
    "rows_passed": 0,
    "rows_failed": 5,
    "verdict": "LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY",
    "blocking_owners": ["adapter", "policy"]
  },
  "mapping": {
    "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
    "mapping_version": "0.2.3",
    "mapping_hash": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4",
    "target_parser_ir_schema_hash": "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d",
    "generated_mapping_rules": 1
  }
}
JSON

cat > "$structural_summary" <<'JSON'
{
  "schema_version": "tei-eaj-structural-expansion-v1",
  "rows": []
}
JSON

cat > "$matrix_summary" <<'JSON'
{
  "schema_version": "tei-eaj-generated-comparison-v1",
  "inputs": {
    "adapter_preference": ["aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora"],
    "mapping": "mapping.json",
    "candidate_mode": "all"
  },
  "rows": [
    {
      "work_id": "w1",
      "title": "Adapter over segmented",
      "tei_eaj_file": "data/plain/w1.xml",
      "selected_aat": {"adapter": "aozora2html"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented", "source_note_body_excluded": false},
      "text": {"body_text_match_bucket": "base_equal"}
    },
    {
      "work_id": "w1",
      "title": "Adapter over segmented",
      "tei_eaj_file": "data/plain/w1.xml",
      "selected_aat": {"adapter": "aozora-epub3"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false},
      "text": {"body_text_match_bucket": "ruby_expanded_equal"}
    },
    {
      "work_id": "w1",
      "title": "Adapter over segmented",
      "tei_eaj_file": "data/plain/w1.xml",
      "selected_aat": {"adapter": "aozora-rs"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "source_note_back_routing", "source_note_body_excluded": true},
      "text": {"body_text_match_bucket": "base_drop_parentheticals_equal"}
    },
    {
      "work_id": "w1",
      "title": "Adapter over segmented",
      "tei_eaj_file": "data/plain/w1.xml",
      "selected_aat": {"adapter": "aozora2"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false},
      "text": {"body_text_match_bucket": "different"}
    },
    {
      "work_id": "w2",
      "title": "Missing parser evidence",
      "tei_eaj_file": "data/plain/w2.xml",
      "selected_aat": {"adapter": "aozora2html"},
      "materialization": {"status": "passed"},
      "tei_eaj": {"structure_profile": "plain_prose"},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false},
      "text": {"body_text_match_bucket": "base_equal"}
    }
  ],
  "skipped": [
    {"tei_eaj_file": "data/plain/no-id.xml", "reason": "no_materializable_aat"}
  ]
}
JSON

set +e
python3 "$repo_root/reports/parser-ir/plain-prose-source-delta.py" \
  --admission-summary "$admission_summary" \
  --matrix-summary "$matrix_summary" \
  --structural-summary "$structural_summary" \
  --source-summary "$source_summary" \
  --mapping "$mapping_file" \
  --summary-json "$strict_summary_json" \
  --report-md "$strict_report_md" \
  2>"$strict_stderr"
strict_status=$?
set -e

test "$strict_status" -ne 0
rg -n "missing parser evidence" "$strict_stderr"

python3 "$repo_root/reports/parser-ir/plain-prose-source-delta.py" \
  --admission-summary "$admission_summary" \
  --matrix-summary "$matrix_summary" \
  --structural-summary "$structural_summary" \
  --source-summary "$source_summary" \
  --mapping "$mapping_file" \
  --summary-json "$summary_json" \
  --report-md "$report_md" \
  --allow-missing-parser-evidence

jq -e '.schema_version == "plain-prose-source-delta-probe-v1"' "$summary_json"
jq -e '.required_parsers == ["aozora2html", "aozora-epub3", "aozora-rs", "aozora2", "aozora"]' "$summary_json"
jq -e '.mapping.mapping_id == "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"' "$summary_json"
jq -e '.parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_INCOMPLETE"' "$summary_json"
jq -e '.parser_evidence_coverage.missing_parsers == ["aozora"]' "$summary_json"
jq -e '.parser_evidence_coverage.rows_missing_required_parser_evidence == 2' "$summary_json"
jq -e '.classification_counts.adapter_paragraph_bug == 1' "$summary_json"
jq -e '.classification_counts.ruby_metadata_not_plaintext == 1' "$summary_json"
jq -e '.classification_counts.source_note_metadata_excluded == 1' "$summary_json"
jq -e '.classification_counts.source_text_policy_required == 1' "$summary_json"
jq -e '.classification_counts.missing_parser_evidence == 2' "$summary_json"
jq -e '.blocking_owners == ["adapter", "evidence", "policy"]' "$summary_json"
jq -e '[.rows[] | select((.blocking_owners | index("adapter")) and (.blocking_owners | index("evidence")))] | length >= 1' "$summary_json"
jq -e '[.rows[] | select(.classifications | index("ruby_metadata_not_plaintext"))] | length == 1' "$summary_json"
jq -e '[.rows[] | select(.classifications | index("source_note_metadata_excluded"))] | length == 1' "$summary_json"
jq -e '[.rows[] | select(.blocking_owners == ["evidence"])] | length == 1' "$summary_json"
jq -e '[.rows[] | select((.classifications | index("adapter_paragraph_bug")) and (.classifications | index("missing_parser_evidence")))] | length == 1' "$summary_json"

rg -n 'FIVE_PARSER_EVIDENCE_INCOMPLETE' "$report_md"
rg -n 'ruby_metadata_not_plaintext' "$report_md"
rg -n 'source_note_metadata_excluded' "$report_md"
