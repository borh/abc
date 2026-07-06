#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-publication-next-work.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

source_summary="$out_dir/source.json"
coverage_summary="$out_dir/coverage.json"
matrix_summary="$out_dir/matrix.json"
conversion_summary="$out_dir/conversion.json"
reference_summary="$out_dir/reference.json"
performance_md="$out_dir/performance.md"
summary_json="$out_dir/next-work.summary.json"
report_md="$out_dir/next-work.md"

cat > "$source_summary" <<'JSON'
{
  "schema_version": "aozora-source-region-coverage-v1",
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 2,
  "unknown_region_occurrences": 999,
  "source_region_coverage": {
    "unsupported_body_markup_occurrences": 0,
    "unknown_region_occurrences": 0,
    "unknown_unreviewed_occurrences": 0,
    "terminal_provenance_occurrences": 3,
    "colophon_metadata_occurrences": 7,
    "malformed_source_occurrences": 1
  }
}
JSON

cat > "$coverage_summary" <<'JSON'
{
  "verdict": "IR_PUBLICATION_COVERAGE_COMPLETE",
  "source_region_contract": {
    "verdict": "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"
  },
  "publication_bundle_contract": {
    "verdict": "PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION",
    "rows_validated": 285,
    "rows_failed": 0
  },
  "closure_gaps": {
    "classified_but_not_admitted": {"count": 0},
    "true_unsupported_gaps": {"count": 0}
  }
}
JSON

cat > "$matrix_summary" <<'JSON'
{
  "totals": {
    "rows_attempted": 10,
    "materialization_failed": 0,
    "rows_skipped": 1
  },
  "paragraph_origin_buckets": {
    "aligned": 2,
    "adapter_over_segmented": 4,
    "adapter_collapsed": 3,
    "adapter_under_segmented": 1,
    "converter_paragraph_mismatch": 1,
    "adapter_raw_only": 1,
    "source_note_back_routing": 5,
    "page_break_projection": 6
  },
  "body_text_relation_buckets": {
    "equal": 1,
    "different": 9
  },
  "adapter_paragraph_origin_buckets": {
    "aozora2html": {"adapter_over_segmented": 2},
    "aozora-epub3": {"adapter_over_segmented": 2},
    "aozora-rs": {"adapter_collapsed": 2},
    "aozora2": {"converter_paragraph_mismatch": 1},
    "aozora": {"adapter_collapsed": 1}
  }
}
JSON

cat > "$conversion_summary" <<'JSON'
{
  "totals": {"files_attempted": 10, "files_succeeded": 10, "files_failed": 0},
  "compatibility_candidates": [
    {"aat_adapter": "aozora2html", "evidence_scope": {"files_scanned": 2}},
    {"aat_adapter": "aozora-epub3", "evidence_scope": {"files_scanned": 2}},
    {"aat_adapter": "aozora-rs", "evidence_scope": {"files_scanned": 2}},
    {"aat_adapter": "aozora2", "evidence_scope": {"files_scanned": 2}},
    {"aat_adapter": "aozora", "evidence_scope": {"files_scanned": 2}}
  ]
}
JSON

cat > "$reference_summary" <<'JSON'
{
  "verdict": "SOURCE_REFERENCE_RECONCILIATION_COMPLETE",
  "totals": {
    "observed_without_syntax_row": 0,
    "p4suta_feature_unmapped": 0
  }
}
JSON

cat > "$performance_md" <<'MD'
# Parser Performance Measurement

DNF is useful signal.
MD

python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "aozora-publication-next-work-v1"' "$summary_json" >/dev/null
jq -e '.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' "$summary_json" >/dev/null
jq -e '.completed_gates[] | select(.id == "publication_bundle_contract" and .status == "complete")' "$summary_json" >/dev/null
jq -e '.completed_gates[] | select(.id == "source_authority" and .status == "complete")' "$summary_json" >/dev/null
jq -e '.completed_gates[] | select(.id == "source_authority" and .evidence.counter_source == "source_region_coverage")' "$summary_json" >/dev/null
jq -e '([.parser_lanes[].adapter] | sort) == (["aozora", "aozora-epub3", "aozora-rs", "aozora2", "aozora2html"] | sort)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "source_region_disposition_samples" and .owner == "ab-validator+abc")' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "text_policy_calibration" and .evidence.different_rows == 9)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and .evidence.adapter_distortion_rows == 10)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and (.evidence.included_buckets | sort) == (["adapter_collapsed", "adapter_over_segmented", "adapter_raw_only", "adapter_under_segmented", "converter_paragraph_mismatch"] | sort))' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and (.evidence.excluded_buckets | sort) == (["aligned", "page_break_projection", "source_note_back_routing"] | sort))' "$summary_json" >/dev/null
jq -e '.calibration_only_items[] | select(.id == "tei_eaj_editorial_enrichment")' "$summary_json" >/dev/null
rg -n "Aozora Publication Next Work" "$report_md" >/dev/null
