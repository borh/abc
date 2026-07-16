#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-parser-ir-publication-bundle.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

bundle_dir="$out_dir/publication"
mkdir -p "$bundle_dir"

parser_ir="$out_dir/parser-ir.json"
source_region="$out_dir/source-region-coverage.json"
summary_json="$out_dir/bundle.summary.json"
report_md="$out_dir/bundle.md"
failed_summary_json="$out_dir/bundle-failed.summary.json"
failed_report_md="$out_dir/bundle-failed.md"

hash_file() {
  local path="$1"
  printf 'sha256:%s' "$(sha256sum "$path" | awk '{print $1}')"
}

cat > "$parser_ir" <<'JSON'
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
    {
      "type": "text",
      "text": "吾輩",
      "span": {"start": 0, "end": 2}
    },
    {
      "type": "ruby",
      "span": {"start": 2, "end": 8},
      "ruby": {"base": "猫", "reading": "ねこ", "scope": "explicit"}
    },
    {
      "type": "layout-span",
      "span": {"start": 8, "end": 10},
      "text": "10",
      "layout": {"kind": "tcy", "source": "aat-inline"}
    },
    {
      "type": "source-note",
      "span": {"start": 10, "end": 30},
      "text": "（古伝説と、シルレルの詩から。）",
      "note_type": "source-attribution",
      "placement": "back",
      "classification": "heuristic",
      "source_pointer": "blocks[1]"
    }
  ],
  "paragraphs": [
    {
      "id": "p000000",
      "span": {"start": 0, "end": 10},
      "span_source": "direct",
      "node_range": {"start": 0, "end": 3},
      "role": "body",
      "classification": "direct",
      "source_pointer": "blocks[0]"
    },
    {
      "id": "p000001",
      "span": {"start": 10, "end": 30},
      "span_source": "direct",
      "node_range": {"start": 3, "end": 4},
      "role": "source-note",
      "classification": "heuristic",
      "source_pointer": "blocks[1]"
    }
  ],
  "warnings": [],
  "errors": []
}
JSON

cat > "$source_region" <<'JSON'
{
  "schema_version": "aozora-source-region-coverage-v1",
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 1,
  "unallowlisted_unknown_markers_total": 0,
  "source_region_coverage": {
    "body_typed_occurrences": 1,
    "body_raw_preserved_occurrences": 0,
    "source_apparatus_occurrences": 1,
    "front_matter_occurrences": 1,
    "back_matter_occurrences": 0,
    "body_end_boundary_occurrences": 0,
    "terminal_provenance_occurrences": 0,
    "colophon_metadata_occurrences": 0,
    "letter_address_origin_occurrences": 0,
    "malformed_source_occurrences": 0,
    "unsupported_body_markup_occurrences": 0,
    "unknown_region_occurrences": 0,
    "unknown_unreviewed_occurrences": 0
  },
  "representability": {
    "typed_occurrences": 1,
    "raw_preserved_occurrences": 0,
    "out_of_body_occurrences": 1,
    "malformed_noise_occurrences": 1,
    "unsupported_occurrences": 0,
    "needs_research_occurrences": 0
  }
}
JSON

cat > "$bundle_dir/plain.txt" <<'TXT'
吾輩猫10
TXT

cat > "$bundle_dir/tei.xml" <<'XML'
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0" xmlns:abc="https://w3id.org/abc/ns/tei" abc:vocab-version="0">
  <text>
    <body>
      <p>吾輩<ruby xml:id="tei-r000001" abc:preservation-record="r000001"><rb>猫</rb><rt>ねこ</rt></ruby><hi rend="text-combine-upright" abc:layout-kind="tcy">10</hi></p>
    </body>
    <back>
      <div type="source"><note type="source-attribution">（古伝説と、シルレルの詩から。）</note></div>
    </back>
  </text>
</TEI>
XML

cat > "$bundle_dir/tei-validation-result.json" <<'JSON'
{"status": "passed", "findings": []}
JSON

cat > "$bundle_dir/preservation.json" <<'JSON'
{
  "schema_id": "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json",
  "schema_version": "0.3.0",
  "schema_hash": "sha256:3333333333333333333333333333333333333333333333333333333333333333",
  "parser_ir": {"schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json", "schema_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222", "work_id": "fixture"},
  "tei": {"profile_id": "tei-eaj-v0", "profile_hash": "sha256:4444444444444444444444444444444444444444444444444444444444444444"},
  "source": {"corpus_snapshot_hash": "sha256:5555555555555555555555555555555555555555555555555555555555555555", "work_content_hash": "sha256:7777777777777777777777777777777777777777777777777777777777777777", "source_path": "cards/000000/files/example.txt", "encoding": "Shift_JIS", "normalization": "source"},
  "producer": {"agent": "publication-bundle-smoke", "generated_at": "2026-07-17T00:00:00Z"},
  "mapping": null,
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

tei_validation_hash="$(hash_file "$bundle_dir/tei-validation-result.json")"
preservation_hash="$(hash_file "$bundle_dir/preservation.json")"
tei_hash="$(hash_file "$bundle_dir/tei.xml")"
plain_hash="$(hash_file "$bundle_dir/plain.txt")"

cat > "$bundle_dir/tei.manifest.json" <<JSON
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

cat > "$bundle_dir/plaintext.manifest.json" <<JSON
{
  "artifact_kind": "plaintext",
  "validation_status": "passed",
  "content": {"content_hash": "$plain_hash", "media_type": "text/plain; charset=UTF-8", "path_hint": "plain.txt"},
  "sidecars": []
}
JSON

python "$repo_root/reports/parser-ir/publication-bundle-validate.py" \
  --parser-ir "$parser_ir" \
  --source-region-summary "$source_region" \
  --parser-ir-schema "$repo_root/../abc/schemas/parser-ir.schema.json" \
  --preservation-schema "$repo_root/../abc/schemas/parser-ir-publication-preservation.schema.json" \
  --validator-identity "$repo_root/data/parser-rq-publication-validator-v1.json" \
  --publication-dir "$bundle_dir" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "publication-bundle-validation-evidence-v2"' "$summary_json" >/dev/null
jq -e '.verdict == "PUBLICATION_BUNDLE_VALIDATION_PASSED"' "$summary_json" >/dev/null
jq -e '.validated_bundle.tei.hash == "'"$tei_hash"'"' "$summary_json" >/dev/null
jq -e '.validated_bundle.plaintext.hash == "'"$plain_hash"'"' "$summary_json" >/dev/null
jq -e '.structure_check_candidates.tei_manifest_references_preservation == true' "$summary_json" >/dev/null
jq -e '.structure_check_candidates.tei_manifest_references_validation_result == true' "$summary_json" >/dev/null
jq -e '.structure_check_candidates.tei_abc_projection_resolves_to_sidecar == true' "$summary_json" >/dev/null
jq -e '.structure_check_candidates.preservation_tei_pointers_resolve == true' "$summary_json" >/dev/null
jq -e '.structure_check_candidates.preservation_source_pointers_resolve == true' "$summary_json" >/dev/null
jq -e '.structure_check_candidates.plaintext_body_only == true' "$summary_json" >/dev/null
jq -e '.supporting_preconditions.parser_ir_schema_valid == true' "$summary_json" >/dev/null
jq -e '.join_input.status == "valid" and .counts.preservation_records > 0' "$summary_json" >/dev/null
rg -n "Publication Bundle Validation" "$report_md" >/dev/null

printf '吾輩猫ねこ\n（古伝説と、シルレルの詩から。）\n' > "$bundle_dir/plain.txt"

python "$repo_root/reports/parser-ir/publication-bundle-validate.py" \
  --parser-ir "$parser_ir" \
  --source-region-summary "$source_region" \
  --parser-ir-schema "$repo_root/../abc/schemas/parser-ir.schema.json" \
  --preservation-schema "$repo_root/../abc/schemas/parser-ir-publication-preservation.schema.json" \
  --validator-identity "$repo_root/data/parser-rq-publication-validator-v1.json" \
  --publication-dir "$bundle_dir" \
  --summary-json "$failed_summary_json" \
  --report-md "$failed_report_md"

jq -e '.verdict == "PUBLICATION_BUNDLE_VALIDATION_FAILED"' "$failed_summary_json" >/dev/null
jq -e '.structure_check_candidates.plaintext_body_only == false' "$failed_summary_json" >/dev/null
jq -e '.failures[] | select(.check == "plaintext_body_only")' "$failed_summary_json" >/dev/null
