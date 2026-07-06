#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/smoke-env.sh"
repo_root="$AB_VALIDATOR_ROOT"
out_dir="$(smoke_tmp_dir ab-source-region-samples)"
trap 'smoke_cleanup "$out_dir"' EXIT

source_summary="$out_dir/source.json"
source_md="$out_dir/source.md"
policy="$out_dir/policy.json"
policy_with_letter="$out_dir/policy-with-letter.json"
summary_json="$out_dir/source-region-samples.summary.json"
summary_json_with_letter="$out_dir/source-region-samples-with-letter.summary.json"
report_md="$out_dir/source-region-samples.md"
report_md_with_letter="$out_dir/source-region-samples-with-letter.md"
repo_policy_summary_json="$out_dir/source-region-samples-repo-policy.summary.json"
repo_policy_report_md="$out_dir/source-region-samples-repo-policy.md"

cat > "$source_summary" <<'JSON'
{
  "schema_version": "aozora-source-region-coverage-v1",
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 2,
  "source_region_coverage": {
    "source_apparatus_occurrences": 11,
    "front_matter_occurrences": 13,
    "back_matter_occurrences": 17,
    "body_end_boundary_occurrences": 5,
    "terminal_provenance_occurrences": 3,
    "colophon_metadata_occurrences": 7,
    "letter_address_origin_occurrences": 2,
    "malformed_source_occurrences": 1
  }
}
JSON

cat > "$source_md" <<'MD'
# Source Region Coverage
MD

cat > "$policy" <<'JSON'
{
  "policy_id": "https://w3id.org/abc/policies/source-region-publication-v0",
  "policy_version": "0.1.0",
  "dispositions": [
    {
      "source_class": "notation_legend",
      "target_class": "tei_policy_projection",
      "tei_target": "encodingDesc/editorialDecl",
      "custom_sidecar": true,
      "plaintext_projection": "omit",
      "measurement_status": "measured"
    },
    {
      "source_class": "notation_placeholder",
      "target_class": "custom_sidecar",
      "tei_target": null,
      "custom_sidecar": true,
      "plaintext_projection": "omit",
      "measurement_status": "measured"
    },
    {
      "source_class": "body_end_boundary",
      "target_class": "custom_sidecar",
      "tei_target": null,
      "custom_sidecar": true,
      "plaintext_projection": "omit",
      "measurement_status": "measured"
    },
    {
      "source_class": "terminal_provenance",
      "target_class": "tei_policy_projection",
      "tei_target": "text/back/div[@type='source']",
      "custom_sidecar": true,
      "plaintext_projection": "omit",
      "measurement_status": "measured"
    },
    {
      "source_class": "colophon_metadata",
      "target_class": "tei_policy_projection",
      "tei_target": "teiHeader/sourceDesc",
      "custom_sidecar": true,
      "plaintext_projection": "omit",
      "measurement_status": "measured"
    },
    {
      "source_class": "malformed_source",
      "target_class": "diagnostic",
      "tei_target": null,
      "custom_sidecar": true,
      "plaintext_projection": "omit",
      "measurement_status": "measured"
    }
  ]
}
JSON

jq '.dispositions += [{
  "source_class": "letter_address_origin",
  "target_class": "tei_policy_projection",
  "tei_target": "teiHeader/profileDesc/correspDesc",
  "custom_sidecar": true,
  "plaintext_projection": "omit",
  "measurement_status": "measured"
}]' "$policy" > "$policy_with_letter"

python3 "$repo_root/reports/source-regions/source-region-disposition-samples.py" \
  --source-summary "$source_summary" \
  --source-report-md "$source_md" \
  --policy "$policy" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

python3 "$repo_root/reports/source-regions/source-region-disposition-samples.py" \
  --source-summary "$source_summary" \
  --source-report-md "$source_md" \
  --policy "$policy_with_letter" \
  --summary-json "$summary_json_with_letter" \
  --report-md "$report_md_with_letter"

jq -e '.verdict == "SOURCE_REGION_DISPOSITION_SAMPLES_READY"' "$summary_json" >/dev/null
jq -e '.classes[] | select(.source_class == "terminal_provenance" and .plaintext_projection == "omit")' "$summary_json" >/dev/null
jq -e '.classes[] | select(.source_class == "colophon_metadata" and .tei_target == "teiHeader/sourceDesc")' "$summary_json" >/dev/null
jq -e '.classes[] | select(.source_class == "body_end_boundary" and .measured_counter == "body_end_boundary_occurrences" and .measured_occurrences == 5)' "$summary_json" >/dev/null
jq -e '.classes[] | select(.source_class == "letter_address_origin" and .status == "policy_needed" and .measurement_status == "measured" and .measured_counter == "letter_address_origin_occurrences" and .measured_occurrences == 2 and .policy_class_present == false)' "$summary_json" >/dev/null
jq -e '.classes[] | select(.source_class == "letter_address_origin" and .status == "admitted" and .policy_class_present == true and .tei_target == "teiHeader/profileDesc/correspDesc")' "$summary_json_with_letter" >/dev/null
jq -e '.classes[] | select(.source_class == "letter_address_origin" and .measured_counter == "letter_address_origin_occurrences" and .measured_occurrences == 2)' "$summary_json_with_letter" >/dev/null
rg -n "Source-Region Disposition Samples" "$report_md" >/dev/null

python3 "$repo_root/reports/source-regions/source-region-disposition-samples.py" \
  --source-summary "$repo_root/docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json" \
  --source-report-md "$repo_root/docs/superpowers/reports/2026-07-04-source-authority-representability.md" \
  --policy "$repo_root/data/abc-schemas/data/source-region-publication-policy-v0.json" \
  --summary-json "$repo_policy_summary_json" \
  --report-md "$repo_policy_report_md"

jq -e '.classes[] | select(.source_class == "letter_address_origin" and .status == "admitted" and .policy_class_present == true and .tei_target == "teiHeader/profileDesc/correspDesc")' "$repo_policy_summary_json" >/dev/null
