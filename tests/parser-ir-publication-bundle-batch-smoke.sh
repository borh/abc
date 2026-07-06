#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-parser-ir-publication-bundle-batch.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

source_region="$out_dir/source-region-coverage.json"
summary_json="$out_dir/batch.summary.json"
report_md="$out_dir/batch.md"
failed_summary_json="$out_dir/batch-failed.summary.json"
failed_report_md="$out_dir/batch-failed.md"

hash_file() {
  local path="$1"
  printf 'sha256:%s' "$(sha256sum "$path" | awk '{print $1}')"
}

cat > "$source_region" <<'JSON'
{
  "schema_version": "aozora-source-region-coverage-v1",
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 2,
  "unallowlisted_unknown_markers_total": 0,
  "source_region_coverage": {
    "body_typed_occurrences": 2,
    "body_raw_preserved_occurrences": 0,
    "source_apparatus_occurrences": 1,
    "front_matter_occurrences": 1,
    "back_matter_occurrences": 0,
    "terminal_provenance_occurrences": 0,
    "colophon_metadata_occurrences": 0,
    "malformed_source_occurrences": 0,
    "unsupported_body_markup_occurrences": 0,
    "unknown_region_occurrences": 0,
    "unknown_unreviewed_occurrences": 0
  },
  "representability": {
    "typed_occurrences": 2,
    "raw_preserved_occurrences": 0,
    "out_of_body_occurrences": 1,
    "malformed_noise_occurrences": 1,
    "unsupported_occurrences": 0,
    "needs_research_occurrences": 0
  }
}
JSON

write_row_bundle() {
  local row_dir="$1"
  local reading="$2"
  local source_note="$3"
  local plaintext="$4"
  local publication_dir="$row_dir/publication"

  mkdir -p "$publication_dir"

  cat > "$row_dir/parser-ir.json" <<JSON
{
  "schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "schema_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222",
  "source": {
    "work_content_hash": "sha256:7777777777777777777777777777777777777777777777777777777777777777",
    "source_path": "cards/000000/files/example.txt",
    "encoding": "Shift_JIS",
    "normalization": "source"
  },
  "nodes": [
    {"type": "text", "text": "吾輩", "span": {"start": 0, "end": 2}, "source_pointer": "blocks[0]"},
    {
      "type": "ruby",
      "span": {"start": 2, "end": 8},
      "source_pointer": "blocks[0]",
      "ruby": {"base": "猫", "reading": "$reading"}
    },
    {
      "type": "source-note",
      "span": {"start": 8, "end": 28},
      "text": "$source_note",
      "placement": "back",
      "classification": "heuristic",
      "source_pointer": "blocks[1]"
    }
  ],
  "paragraphs": [
    {"id": "p000000", "node_range": {"start": 0, "end": 2}, "role": "body", "classification": "direct", "source_pointer": "blocks[0]"},
    {"id": "p000001", "node_range": {"start": 2, "end": 3}, "role": "source-note", "classification": "heuristic", "source_pointer": "blocks[1]"}
  ],
  "warnings": [],
  "errors": []
}
JSON

  printf '%s\n' "$plaintext" > "$publication_dir/plain.txt"

  cat > "$publication_dir/tei.xml" <<XML
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0" xmlns:abc="https://w3id.org/abc/ns/tei" abc:vocab-version="0">
  <text>
    <body>
      <p>吾輩<ruby xml:id="tei-r000001" abc:preservation-record="r000001"><rb>猫</rb><rt>$reading</rt></ruby></p>
    </body>
    <back>
      <div type="source"><note type="source-attribution">$source_note</note></div>
    </back>
  </text>
</TEI>
XML

  cat > "$publication_dir/tei-validation-result.json" <<'JSON'
{"status": "passed", "findings": []}
JSON

  cat > "$publication_dir/preservation.json" <<'JSON'
{
  "schema_id": "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json",
  "schema_version": "0.2.0",
  "schema_hash": "sha256:3333333333333333333333333333333333333333333333333333333333333333",
  "coverage": {"record_count": 2, "classes": ["custom_sidecar", "tei_profile_projection"]},
  "records": [
    {
      "record_id": "r000001",
      "ir_pointer": "/nodes/1/ruby",
      "tei_pointer": "#tei-r000001",
      "class": "tei_profile_projection",
      "construct": "style_rendition",
      "source_pointer": "blocks[0]",
      "source_inventory_row": "ruby.basic",
      "message": "Fixture TEI projection record.",
      "count": 1,
      "first_path": "/nodes/1/ruby"
    },
    {
      "record_id": "r000002",
      "ir_pointer": "/paragraphs/1/classification",
      "tei_pointer": null,
      "class": "custom_sidecar",
      "construct": "source_note.classification",
      "source_pointer": "blocks[1]",
      "source_inventory_row": null,
      "message": "Fixture source-note classification record.",
      "count": 1,
      "first_path": "/paragraphs/1/classification"
    }
  ]
}
JSON

  local tei_validation_hash
  local preservation_hash
  local tei_hash
  local plain_hash
  tei_validation_hash="$(hash_file "$publication_dir/tei-validation-result.json")"
  preservation_hash="$(hash_file "$publication_dir/preservation.json")"
  tei_hash="$(hash_file "$publication_dir/tei.xml")"
  plain_hash="$(hash_file "$publication_dir/plain.txt")"

  cat > "$publication_dir/tei.manifest.json" <<JSON
{
  "artifact_kind": "tei",
  "validation_status": "passed",
  "content": {"content_hash": "$tei_hash", "media_type": "application/tei+xml", "path_hint": "tei.xml"},
  "sidecars": [
    {"role": "validation-result", "hash": "$tei_validation_hash", "media_type": "application/json", "path_hint": "tei-validation-result.json"},
    {"role": "preservation", "hash": "$preservation_hash", "media_type": "application/json", "path_hint": "preservation.json"}
  ]
}
JSON

  cat > "$publication_dir/plaintext.manifest.json" <<JSON
{
  "artifact_kind": "plaintext",
  "validation_status": "passed",
  "content": {"content_hash": "$plain_hash", "media_type": "text/plain; charset=UTF-8", "path_hint": "plain.txt"},
  "sidecars": []
}
JSON
}

write_row_bundle "$out_dir/rows/row-a" "ねこ" "（古伝説と、シルレルの詩から。）" "吾輩猫"
write_row_bundle "$out_dir/rows/row-b" "ねこ" "底本：「fixture」" "吾輩猫"

python3 "$repo_root/reports/parser-ir/publication-bundle-validate.py" \
  --batch-root "$out_dir" \
  --source-region-summary "$source_region" \
  --abc-commit "abc1234" \
  --command "fixture batch materialization" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "publication-bundle-batch-validation-evidence-v1"' "$summary_json" >/dev/null
jq -e '.verdict == "PUBLICATION_BUNDLE_BATCH_VALIDATION_PASSED"' "$summary_json" >/dev/null
jq -e '.scope.rows_discovered == 2 and .scope.rows_validated == 2 and .scope.rows_failed == 0' "$summary_json" >/dev/null
jq -e '.checks.plaintext_body_only == true' "$summary_json" >/dev/null
jq -e '.rows | length == 2' "$summary_json" >/dev/null
jq -e '.rows[] | select(.row_id == "row-a" and .verdict == "PUBLICATION_BUNDLE_VALIDATION_PASSED")' "$summary_json" >/dev/null
rg -n "Publication Bundle Batch Validation" "$report_md" >/dev/null

write_row_bundle "$out_dir/rows/row-c" "ねこ" "底本：「fixture」" "吾輩猫ねこ"

python3 "$repo_root/reports/parser-ir/publication-bundle-validate.py" \
  --batch-root "$out_dir" \
  --source-region-summary "$source_region" \
  --abc-commit "abc1234" \
  --command "fixture batch materialization" \
  --summary-json "$failed_summary_json" \
  --report-md "$failed_report_md"

jq -e '.verdict == "PUBLICATION_BUNDLE_BATCH_VALIDATION_FAILED"' "$failed_summary_json" >/dev/null
jq -e '.scope.rows_discovered == 3 and .scope.rows_failed == 1' "$failed_summary_json" >/dev/null
jq -e '.checks.plaintext_body_only == false' "$failed_summary_json" >/dev/null
jq -e '.failures[] | select(.row_id == "row-c" and .check == "plaintext_body_only")' "$failed_summary_json" >/dev/null
jq -e '.failures[] | select(.row_id == "row-c" and .check == "plaintext_body_only" and .details.kind == "plaintext_mismatch" and .details.expected_length == 3 and .details.actual_length == 5)' "$failed_summary_json" >/dev/null
