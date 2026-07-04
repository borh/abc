#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-"$repo_root/../abc"}"

if [[ ! -d "$abc_root" ]]; then
  echo "missing ABC repo at $abc_root; set AB_ABC_ROOT=/path/to/abc" >&2
  exit 2
fi

out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-parser-ir-level3-generated-audit-smoke.XXXXXX")"
tei_root="$out_dir/tei-eaj"
mkdir -p "$tei_root/data/complete/tei_lib_lv3"

cat > "$tei_root/data/complete/tei_lib_lv3/fixture_tei.xml" <<'XML'
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <text>
    <body>
      <p>一段落。</p>
      <p>二段落。</p>
    </body>
  </text>
</TEI>
XML

cat > "$out_dir/workset.json" <<JSON
{
  "schema_version": "tei-eaj-aozora-workset-export-v1",
  "tei_eaj_source": {
    "revision": "fixture",
    "root": "$tei_root"
  },
  "files": [
    {
      "work_id": "fixture",
      "title": "Fixture",
      "tei_eaj_file": "data/complete/tei_lib_lv3/fixture_tei.xml",
      "level": "Level 3",
      "state": "complete",
      "comparison_status": "missing_abc_counterpart",
      "tei_eaj_p_count": 2,
      "tei_eaj_note_count": 0
    }
  ]
}
JSON

cat > "$out_dir/structural-summary.json" <<JSON
{
  "rows": [
    {
      "tei": {
        "work_id": "fixture",
        "title": "Fixture",
        "tei_eaj_file": "data/complete/tei_lib_lv3/fixture_tei.xml",
        "level": "Level 3",
        "tei_eaj_p_count": 2,
        "tei_eaj_note_count": 0
      },
      "classification": {
        "kind": "parser_ir_level3_representable"
      },
      "aat_inputs": [
        {
          "label": "aozora2html:fixture",
          "path": "$repo_root/tests/fixtures/aat-parser-ir/real-aozora2html-sample.aat.json",
          "aat": {
            "adapter": "aozora2html",
            "adapter_version": "fixture",
            "paragraph_blocks": 4,
            "final_source_attribution_candidate": false
          },
          "conversion": {
            "success": true,
            "error": null
          },
          "parser_ir": {
            "paragraph_count": 4,
            "paragraphs_represented": true,
            "source_attribution_represented": false
          },
          "verdict": {
            "residual_free": true
          }
        }
      ]
    }
  ]
}
JSON

(cd "$repo_root" && cargo build -p ab-aat-to-parser-ir >/dev/null)

python3 "$repo_root/reports/parser-ir/tei-eaj-generated-compare.py" \
  --workset "$out_dir/workset.json" \
  --structural-summary "$out_dir/structural-summary.json" \
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
  --converter-bin "${AB_AAT_TO_PARSER_IR_BIN:-"$repo_root/target/debug/ab-aat-to-parser-ir"}" \
  --abc-root "$abc_root" \
  --abc-schema-root "$repo_root/data/abc-schemas" \
  --out-dir "$out_dir/audit" \
  --max-rows 1

jq -e '.totals.rows_attempted == 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '.totals.materialization_succeeded == 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].generated_tei.body_p_count >= 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].tei_eaj.body_p_count == 2' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].classification.paragraph_delta_bucket != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.body_text_relation_buckets | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].text.body_base_text_relation != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].text.generated_body_base_text_length > 0' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].text.tei_eaj_body_base_text_length > 0' "$out_dir/audit/summary.json" >/dev/null
grep -n "Generated Parser-IR TEI vs TEI-EAJ Workset Audit" "$out_dir/audit/report.md"
grep -n "Body Text Relation Buckets" "$out_dir/audit/report.md"

echo "parser-IR Level 3 generated workset audit smoke ok: $out_dir/audit"
