#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/smoke-env.sh"
repo_root="$AB_VALIDATOR_ROOT"
out_dir="$(smoke_tmp_dir ab-publication-next-work)"
trap 'smoke_cleanup "$out_dir"' EXIT

source_summary="$out_dir/source.json"
coverage_summary="$out_dir/coverage.json"
matrix_summary="$out_dir/matrix.json"
conversion_summary="$out_dir/conversion.json"
reference_summary="$out_dir/reference.json"
performance_md="$out_dir/performance.md"
source_disposition_summary="$out_dir/source-disposition.json"
text_policy_summary="$out_dir/text-policy.json"
complete_text_policy_summary="$out_dir/text-policy.complete.json"
adapter_worksets_summary="$out_dir/adapter-worksets.json"
complete_adapter_worksets_summary="$out_dir/adapter-worksets.complete.json"
dossier_dir="$out_dir/dossiers"
parser_acceptance_spec="$out_dir/parser-acceptance.md"
tei_p5_root="$out_dir/tei-p5"
summary_json="$out_dir/next-work.summary.json"
complete_source_summary_json="$out_dir/next-work.complete-source.summary.json"
complete_text_summary_json="$out_dir/next-work.complete-text.summary.json"
complete_adapter_summary_json="$out_dir/next-work.complete-adapter.summary.json"
complete_all_summary_json="$out_dir/next-work.complete-all.summary.json"
missing_root_summary_json="$out_dir/next-work.missing-root.summary.json"
env_tei_root_summary_json="$out_dir/next-work.env-tei-root.summary.json"
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
    "body_end_boundary_occurrences": 5,
    "terminal_provenance_occurrences": 3,
    "colophon_metadata_occurrences": 7,
    "letter_address_origin_occurrences": 2,
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

cat > "$source_disposition_summary" <<'JSON'
{
  "schema_version": "source-region-disposition-samples-v1",
  "verdict": "SOURCE_REGION_DISPOSITION_SAMPLES_OPEN",
  "classes": [
    {
      "source_class": "terminal_provenance",
      "status": "admitted",
      "measured_occurrences": 3
    },
    {
      "source_class": "letter_address_origin",
      "status": "policy_needed",
      "measurement_status": "evidence_needed"
    }
  ]
}
JSON

cat > "$text_policy_summary" <<'JSON'
{
  "schema_version": "parser-ir-text-policy-delta-v1",
  "total_different_rows": 9,
  "counts_by_cause": {
    "ruby_or_parenthetical_policy": 2,
    "front_back_source_region_policy": 3,
    "body_visible_layout_policy": 1,
    "adapter_text_loss": 2,
    "tei_eaj_editorial_or_enrichment": 1,
    "unknown_text_delta": 0
  },
  "workset_files": {
    "ruby_or_parenthetical_policy": {
      "path": "docs/reports/text-policy/ruby_or_parenthetical_policy.json",
      "hash": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      "count": 2,
      "source_markup_backed": true
    },
    "front_back_source_region_policy": {
      "path": "docs/reports/text-policy/front_back_source_region_policy.json",
      "hash": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      "count": 3,
      "source_markup_backed": true
    },
    "tei_eaj_editorial_or_enrichment": {
      "path": "docs/reports/text-policy/tei_eaj_editorial_or_enrichment.json",
      "hash": "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
      "count": 1,
      "source_markup_backed": false
    },
    "unknown_text_delta": {
      "path": "docs/reports/text-policy/unknown_text_delta.json",
      "hash": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
      "count": 1,
      "source_markup_backed": false,
      "requires_manual_classification": true
    }
  },
  "source_markup_backed_workset_files": {
    "ruby_or_parenthetical_policy": {
      "path": "docs/reports/text-policy/ruby_or_parenthetical_policy.json",
      "hash": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      "count": 2,
      "source_markup_backed": true
    },
    "front_back_source_region_policy": {
      "path": "docs/reports/text-policy/front_back_source_region_policy.json",
      "hash": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      "count": 3,
      "source_markup_backed": true
    }
  },
  "manual_classification_workset_files": {
    "unknown_text_delta": {
      "path": "docs/reports/text-policy/unknown_text_delta.json",
      "hash": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
      "count": 1,
      "source_markup_backed": false,
      "requires_manual_classification": true
    }
  },
  "source_markup_backed_blockers": [{"row_id": "r1"}, {"row_id": "r2"}],
  "calibration_only_rows": [{"row_id": "r3"}]
}
JSON

cat > "$adapter_worksets_summary" <<'JSON'
{
  "schema_version": "adapter-fidelity-worksets-v1",
  "worksets": {
    "adapter_over_segmented": {"count": 4},
    "adapter_collapsed": {"count": 3},
    "adapter_under_segmented": {"count": 1},
    "converter_paragraph_mismatch": {"count": 1},
    "adapter_raw_only": {"count": 1}
  },
  "workset_files": {
    "adapter_collapsed": {
      "all": {"path": "docs/reports/worksets/adapter_collapsed/all.json", "hash": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "count": 2},
      "by_adapter": {
        "aozora-rs": {"path": "docs/reports/worksets/adapter_collapsed/aozora-rs.json", "hash": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", "count": 1}
      }
    }
  },
  "excluded_counts": {
    "aligned": 2,
    "source_note_back_routing": 5,
    "page_break_projection": 6
  }
}
JSON

mkdir -p "$dossier_dir"
mkdir -p "$tei_p5_root/Source/Specs"
printf '<elementSpec ident="ruby"/>\n' > "$tei_p5_root/Source/Specs/ruby.xml"
printf '<elementSpec ident="hi"/>\n' > "$tei_p5_root/Source/Specs/hi.xml"
cat > "$dossier_dir/ruby.md" <<'MD'
# Ruby

Status: admitted

## Source Inventory

Ruby source rows are measured.

## Parser-IR Representation

Parser-IR carries ruby nodes.

## TEI P5 Target

Use TEI ruby policy. See `abc/references/TEI/P5/Source/Specs/ruby.xml`.

## ABC Extension Or Sidecar

No sidecar is required for ordinary ruby.

## Plaintext Projection

Plaintext excludes ruby readings.

## Current Evidence

Evidence is fixture-local.

## Open Decisions

No fixture decision.
MD
cat > "$dossier_dir/warigaki.md" <<'MD'
# Warigaki

Status: schema-needed

## Source Inventory

Warigaki source rows are measured.

## Parser-IR Representation

Parser-IR needs schema work.

## TEI P5 Target

TEI target remains under review.

## ABC Extension Or Sidecar

Sidecar policy may be required.

## Plaintext Projection

Plaintext excludes warigaki metadata.

## Current Evidence

Evidence is fixture-local.

## Open Decisions

Schema decision remains open.
MD
cat > "$dossier_dir/layout-indentation.md" <<'MD'
# Layout And Indentation

Status: policy-needed

## Source Inventory

Layout source rows are measured.

## Parser-IR Representation

Parser-IR carries layout facts.

## TEI P5 Target

Use TEI rendition policy. See `abc/references/TEI/P5/Source/Specs/hi.xml`.

## ABC Extension Or Sidecar

ABC extension policy may preserve source-exact layout kind.

## Plaintext Projection

Plaintext excludes layout metadata.

## Current Evidence

Evidence is fixture-local.

## Open Decisions

Policy decision remains open.
MD

mkdir -p "$out_dir/evidence"
printf '{"ok": true}\n' > "$out_dir/evidence/existing.json"
cat > "$parser_acceptance_spec" <<'MD'
# Comprehensive Parser Acceptance Criteria

Status: Draft

## Required Evidence Inputs

- `__EXISTING_EVIDENCE__`
- `__MISSING_EVIDENCE__`
- non-path parser lane evidence
MD
python3 - "$parser_acceptance_spec" "$out_dir/evidence/existing.json" "$out_dir/evidence/missing.json" <<'PY'
import pathlib
import sys

path = pathlib.Path(sys.argv[1])
text = path.read_text()
text = text.replace("__EXISTING_EVIDENCE__", sys.argv[2])
text = text.replace("__MISSING_EVIDENCE__", sys.argv[3])
path.write_text(text)
PY

python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --source-disposition-summary "$source_disposition_summary" \
  --text-policy-summary "$text_policy_summary" \
  --adapter-worksets-summary "$adapter_worksets_summary" \
  --dossier-dir "$dossier_dir" \
  --tei-p5-root "$tei_p5_root" \
  --parser-acceptance-spec "$parser_acceptance_spec" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "aozora-publication-next-work-v1"' "$summary_json" >/dev/null
jq -e '.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' "$summary_json" >/dev/null
jq -e '.completed_gates[] | select(.id == "publication_bundle_contract" and .status == "complete")' "$summary_json" >/dev/null
jq -e '.completed_gates[] | select(.id == "source_authority" and .status == "complete")' "$summary_json" >/dev/null
jq -e '.completed_gates[] | select(.id == "source_authority" and .evidence.counter_source == "source_region_coverage")' "$summary_json" >/dev/null
jq -e '([.parser_lanes[].adapter] | sort) == (["aozora", "aozora-epub3", "aozora-rs", "aozora2", "aozora2html"] | sort)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "source_region_disposition_samples" and .owner == "ab-validator+abc")' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "source_region_disposition_samples" and .status == "open")' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "source_region_disposition_samples" and .evidence.classes_total == 2 and .evidence.policy_needed_classes == ["letter_address_origin"])' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "text_policy_calibration" and .evidence.different_rows == 9)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "text_policy_calibration" and .status == "open")' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "text_policy_calibration" and .evidence.counts_by_cause.front_back_source_region_policy == 3 and .evidence.source_markup_backed_blockers == 2)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "text_policy_calibration" and .evidence.workset_files.ruby_or_parenthetical_policy.count == 2 and .evidence.source_markup_backed_workset_files.front_back_source_region_policy.count == 3)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "text_policy_calibration" and .evidence.manual_classification_workset_files.unknown_text_delta.count == 1 and .evidence.manual_classification_workset_files.unknown_text_delta.requires_manual_classification == true)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and .evidence.adapter_distortion_rows == 10)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and .status == "open")' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and .evidence.worksets.adapter_collapsed == 3 and .evidence.excluded_counts.page_break_projection == 6)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and .evidence.workset_files.adapter_collapsed.all.count == 2 and .evidence.workset_files.adapter_collapsed.by_adapter."aozora-rs".count == 1)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and (.evidence.included_buckets | sort) == (["adapter_collapsed", "adapter_over_segmented", "adapter_raw_only", "adapter_under_segmented", "converter_paragraph_mismatch"] | sort))' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and (.evidence.excluded_buckets | sort) == (["aligned", "page_break_projection", "source_note_back_routing"] | sort))' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "tei_p5_mapping_dossiers" and .evidence.dossier_count == 3 and .evidence.status_counts."schema-needed" == 1)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "tei_p5_mapping_dossiers" and .status == "complete")' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "tei_p5_mapping_dossiers" and .evidence.complete_section_count == 3 and .evidence.incomplete_section_dossiers == [])' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "tei_p5_mapping_dossiers" and .evidence.tei_p5_reference_count == 2 and (.evidence.dossiers[] | select(.name == "ruby" and .tei_p5_references == ["abc/references/TEI/P5/Source/Specs/ruby.xml"])))' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "tei_p5_mapping_dossiers" and .evidence.tei_p5_reference_file_count == 2 and .evidence.tei_p5_reference_directory_count == 0 and .evidence.missing_tei_p5_references == [])' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "tei_p5_mapping_dossiers" and .evidence.tei_p5_reference_root_exists == true and .evidence.unverified_tei_p5_references == [])' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "parser_acceptance_criteria" and .evidence.spec_status == "Draft" and .evidence.required_evidence_inputs == 3)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "parser_acceptance_criteria" and .evidence.required_evidence_paths_total == 2 and .evidence.required_evidence_paths_existing == 1 and (.evidence.missing_required_evidence_paths | length) == 1)' "$summary_json" >/dev/null
jq -e '.calibration_only_items[] | select(.id == "tei_eaj_editorial_enrichment")' "$summary_json" >/dev/null
rg -n "Aozora Publication Next Work" "$report_md" >/dev/null
rg -n "letter_address_origin|front_back_source_region_policy|ruby_or_parenthetical_policy.json|unknown_text_delta.json|adapter_collapsed|schema-needed|Comprehensive Parser Acceptance Criteria" "$report_md" >/dev/null

cat > "$source_disposition_summary" <<'JSON'
{
  "schema_version": "source-region-disposition-samples-v1",
  "verdict": "SOURCE_REGION_DISPOSITION_SAMPLES_READY",
  "classes": [
    {
      "source_class": "terminal_provenance",
      "status": "admitted",
      "measured_occurrences": 3
    },
    {
      "source_class": "letter_address_origin",
      "status": "admitted",
      "measured_occurrences": 2
    }
  ]
}
JSON

python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --source-disposition-summary "$source_disposition_summary" \
  --text-policy-summary "$text_policy_summary" \
  --adapter-worksets-summary "$adapter_worksets_summary" \
  --dossier-dir "$dossier_dir" \
  --tei-p5-root "$tei_p5_root" \
  --parser-acceptance-spec "$parser_acceptance_spec" \
  --summary-json "$complete_source_summary_json" \
  --report-md "$out_dir/next-work.complete-source.md"

jq -e '.next_work_items[] | select(.id == "source_region_disposition_samples" and .status == "complete" and .evidence.policy_needed_count == 0 and .evidence.evidence_needed_classes == [])' "$complete_source_summary_json" >/dev/null
rg -n -F '`source_region_disposition_samples` (ab-validator+abc): `complete`' "$out_dir/next-work.complete-source.md" >/dev/null

cat > "$complete_text_policy_summary" <<'JSON'
{
  "schema_version": "parser-ir-text-policy-delta-v1",
  "total_different_rows": 9,
  "counts_by_cause": {
    "ruby_or_parenthetical_policy": 2,
    "front_back_source_region_policy": 3,
    "body_visible_layout_policy": 1,
    "adapter_text_loss": 2,
    "tei_eaj_editorial_or_enrichment": 1,
    "unknown_text_delta": 0
  },
  "workset_files": {
    "ruby_or_parenthetical_policy": {
      "path": "docs/reports/text-policy/ruby_or_parenthetical_policy.json",
      "hash": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      "count": 2,
      "source_markup_backed": true
    },
    "front_back_source_region_policy": {
      "path": "docs/reports/text-policy/front_back_source_region_policy.json",
      "hash": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      "count": 3,
      "source_markup_backed": true
    },
    "body_visible_layout_policy": {
      "path": "docs/reports/text-policy/body_visible_layout_policy.json",
      "hash": "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
      "count": 1,
      "source_markup_backed": true
    },
    "adapter_text_loss": {
      "path": "docs/reports/text-policy/adapter_text_loss.json",
      "hash": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
      "count": 2,
      "source_markup_backed": true
    },
    "tei_eaj_editorial_or_enrichment": {
      "path": "docs/reports/text-policy/tei_eaj_editorial_or_enrichment.json",
      "hash": "sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
      "count": 1,
      "source_markup_backed": false
    },
    "unknown_text_delta": {
      "path": null,
      "hash": null,
      "count": 0,
      "source_markup_backed": false,
      "requires_manual_classification": true
    }
  },
  "manual_classification_workset_files": {
    "unknown_text_delta": {
      "path": null,
      "hash": null,
      "count": 0,
      "source_markup_backed": false,
      "requires_manual_classification": true
    }
  },
  "source_markup_backed_blockers": [{"row_id": "r1"}, {"row_id": "r2"}],
  "calibration_only_rows": [{"row_id": "r3"}]
}
JSON

python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --source-disposition-summary "$source_disposition_summary" \
  --text-policy-summary "$complete_text_policy_summary" \
  --adapter-worksets-summary "$adapter_worksets_summary" \
  --dossier-dir "$dossier_dir" \
  --tei-p5-root "$tei_p5_root" \
  --parser-acceptance-spec "$parser_acceptance_spec" \
  --summary-json "$complete_text_summary_json" \
  --report-md "$out_dir/next-work.complete-text.md"

jq -e '.next_work_items[] | select(.id == "text_policy_calibration" and .status == "complete" and .evidence.different_rows == 9 and .evidence.source_markup_backed_blockers == 2)' "$complete_text_summary_json" >/dev/null
rg -n -F '`text_policy_calibration` (ab-validator+abc): `complete`' "$out_dir/next-work.complete-text.md" >/dev/null

cat > "$complete_adapter_worksets_summary" <<'JSON'
{
  "schema_version": "adapter-fidelity-worksets-v1",
  "worksets": {
    "adapter_over_segmented": {"count": 4},
    "adapter_collapsed": {"count": 3},
    "adapter_under_segmented": {"count": 1},
    "converter_paragraph_mismatch": {"count": 1},
    "adapter_raw_only": {"count": 1}
  },
  "workset_files": {
    "adapter_over_segmented": {
      "all": {"path": "docs/reports/worksets/adapter_over_segmented/all.json", "hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111", "count": 4},
      "by_adapter": {}
    },
    "adapter_collapsed": {
      "all": {"path": "docs/reports/worksets/adapter_collapsed/all.json", "hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222", "count": 3},
      "by_adapter": {}
    },
    "adapter_under_segmented": {
      "all": {"path": "docs/reports/worksets/adapter_under_segmented/all.json", "hash": "sha256:3333333333333333333333333333333333333333333333333333333333333333", "count": 1},
      "by_adapter": {}
    },
    "converter_paragraph_mismatch": {
      "all": {"path": "docs/reports/worksets/converter_paragraph_mismatch/all.json", "hash": "sha256:4444444444444444444444444444444444444444444444444444444444444444", "count": 1},
      "by_adapter": {}
    },
    "adapter_raw_only": {
      "all": {"path": "docs/reports/worksets/adapter_raw_only/all.json", "hash": "sha256:5555555555555555555555555555555555555555555555555555555555555555", "count": 1},
      "by_adapter": {}
    }
  },
  "excluded_counts": {
    "aligned": 2,
    "source_note_back_routing": 5,
    "page_break_projection": 6
  }
}
JSON

python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --source-disposition-summary "$source_disposition_summary" \
  --text-policy-summary "$text_policy_summary" \
  --adapter-worksets-summary "$complete_adapter_worksets_summary" \
  --dossier-dir "$dossier_dir" \
  --tei-p5-root "$tei_p5_root" \
  --parser-acceptance-spec "$parser_acceptance_spec" \
  --summary-json "$complete_adapter_summary_json" \
  --report-md "$out_dir/next-work.complete-adapter.md"

jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and .status == "complete" and .evidence.adapter_distortion_rows == 10)' "$complete_adapter_summary_json" >/dev/null
rg -n -F '`adapter_fidelity_worksets` (ab-validator): `complete`' "$out_dir/next-work.complete-adapter.md" >/dev/null

cat > "$parser_acceptance_spec" <<MD
# Comprehensive Parser Acceptance Criteria

Status: Accepted

## Required Evidence Inputs

- \`$out_dir/evidence/existing.json\`
- non-path parser lane evidence
MD

python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --source-disposition-summary "$source_disposition_summary" \
  --text-policy-summary "$complete_text_policy_summary" \
  --adapter-worksets-summary "$complete_adapter_worksets_summary" \
  --dossier-dir "$dossier_dir" \
  --tei-p5-root "$tei_p5_root" \
  --parser-acceptance-spec "$parser_acceptance_spec" \
  --summary-json "$complete_all_summary_json" \
  --report-md "$out_dir/next-work.complete-all.md"

jq -e '.verdict == "AOZORA_PUBLICATION_NEXT_WORK_COMPLETE"' "$complete_all_summary_json" >/dev/null
jq -e '([.next_work_items[].status] | all(. == "complete"))' "$complete_all_summary_json" >/dev/null
rg -n -F 'Verdict: `AOZORA_PUBLICATION_NEXT_WORK_COMPLETE`' "$out_dir/next-work.complete-all.md" >/dev/null

python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --source-disposition-summary "$source_disposition_summary" \
  --text-policy-summary "$text_policy_summary" \
  --adapter-worksets-summary "$adapter_worksets_summary" \
  --dossier-dir "$dossier_dir" \
  --tei-p5-root "$out_dir/missing-tei-p5" \
  --parser-acceptance-spec "$parser_acceptance_spec" \
  --summary-json "$missing_root_summary_json" \
  --report-md "$out_dir/next-work.missing-root.md"

jq -e '.next_work_items[] | select(.id == "tei_p5_mapping_dossiers" and .evidence.tei_p5_reference_root_exists == false and .evidence.tei_p5_reference_file_count == 2 and (.evidence.unverified_tei_p5_references | length) == 2 and .evidence.missing_tei_p5_references == [])' "$missing_root_summary_json" >/dev/null

AB_TEI_P5_ROOT="$tei_p5_root" python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --source-disposition-summary "$source_disposition_summary" \
  --text-policy-summary "$text_policy_summary" \
  --adapter-worksets-summary "$adapter_worksets_summary" \
  --dossier-dir "$dossier_dir" \
  --parser-acceptance-spec "$parser_acceptance_spec" \
  --summary-json "$env_tei_root_summary_json" \
  --report-md "$out_dir/next-work.env-tei-root.md"

jq -e '.evidence_inputs.tei_p5_root.exists == true' "$env_tei_root_summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "tei_p5_mapping_dossiers" and .evidence.tei_p5_reference_root_exists == true and .evidence.tei_p5_reference_file_count == 2 and .evidence.unverified_tei_p5_references == [])' "$env_tei_root_summary_json" >/dev/null
