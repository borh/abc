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

cat > "$tei_root/data/complete/tei_lib_lv3/empty_tei.xml" <<'XML'
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <text>
    <body>
      <p>欠落していない本文。</p>
      <p>二段落。</p>
    </body>
  </text>
</TEI>
XML

cat > "$tei_root/data/complete/tei_lib_lv3/lb_verse_tei.xml" <<'XML'
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <text>
    <body>
      <p>一行目<lb/>二行目<lb/>三行目<lb/>四行目</p>
    </body>
  </text>
</TEI>
XML

cat > "$out_dir/empty-raw.aat.json" <<'JSON'
{
  "version": 1,
  "work_id": "empty-raw",
  "meta": {
    "adapter": "aozora",
    "adapter_version": "fixture",
    "parse_complete": true,
    "source_encoding": "windows-31j",
    "source_hash": "sha256:9999999999999999999999999999999999999999999999999999999999999999",
    "warnings": []
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "raw",
          "source": "",
          "x-provenance": "parser-derived"
        }
      ]
    }
  ]
}
JSON

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
    },
    {
      "work_id": "empty",
      "title": "Empty Fixture",
      "tei_eaj_file": "data/complete/tei_lib_lv3/empty_tei.xml",
      "level": "Level 3",
      "state": "complete",
      "comparison_status": "missing_abc_counterpart",
      "tei_eaj_p_count": 2,
      "tei_eaj_note_count": 0
    },
    {
      "work_id": "lb-verse",
      "title": "Line Break Verse Fixture",
      "tei_eaj_file": "data/complete/tei_lib_lv3/lb_verse_tei.xml",
      "level": "Level 3",
      "state": "complete",
      "comparison_status": "missing_abc_counterpart",
      "tei_eaj_p_count": 1,
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
        },
        {
          "label": "aozora-rs:fixture",
          "path": "$repo_root/tests/fixtures/aat-parser-ir/real-aozora-rs-sample.aat.json",
          "aat": {
            "adapter": "aozora-rs",
            "adapter_version": "fixture",
            "paragraph_blocks": 1,
            "final_source_attribution_candidate": false
          },
          "conversion": {
            "success": true,
            "error": null
          },
          "parser_ir": {
            "paragraph_count": 1,
            "paragraphs_represented": true,
            "source_attribution_represented": false
          },
          "verdict": {
            "residual_free": true
          }
        }
      ]
    },
    {
      "tei": {
        "work_id": "empty",
        "title": "Empty Fixture",
        "tei_eaj_file": "data/complete/tei_lib_lv3/empty_tei.xml",
        "level": "Level 3",
        "tei_eaj_p_count": 2,
        "tei_eaj_note_count": 0
      },
      "classification": {
        "kind": "parser_ir_level3_representable"
      },
      "aat_inputs": [
        {
          "label": "aozora:empty",
          "path": "$out_dir/empty-raw.aat.json",
          "aat": {
            "adapter": "aozora",
            "adapter_version": "fixture",
            "paragraph_blocks": 1,
            "final_source_attribution_candidate": false
          },
          "conversion": {
            "success": true,
            "error": null
          },
          "parser_ir": {
            "paragraph_count": 0,
            "paragraphs_represented": false,
            "source_attribution_represented": false
          },
          "verdict": {
            "residual_free": true
          }
        }
      ]
    },
    {
      "tei": {
        "work_id": "lb-verse",
        "title": "Line Break Verse Fixture",
        "tei_eaj_file": "data/complete/tei_lib_lv3/lb_verse_tei.xml",
        "level": "Level 3",
        "tei_eaj_p_count": 1,
        "tei_eaj_note_count": 0
      },
      "classification": {
        "kind": "parser_ir_level3_representable"
      },
      "aat_inputs": [
        {
          "label": "aozora-rs:lb-verse",
          "path": "$repo_root/tests/fixtures/aat-parser-ir/real-aozora-rs-sample.aat.json",
          "aat": {
            "adapter": "aozora-rs",
            "adapter_version": "fixture",
            "paragraph_blocks": 1,
            "final_source_attribution_candidate": false
          },
          "conversion": {
            "success": true,
            "error": null
          },
          "parser_ir": {
            "paragraph_count": 1,
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
  --candidate-mode all \
  --jobs 2 \
  --max-rows 0

jq -e '.inputs.candidate_mode == "all"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.inputs.jobs == 2' "$out_dir/audit/summary.json" >/dev/null
jq -e '.totals.rows_attempted == 4' "$out_dir/audit/summary.json" >/dev/null
jq -e '.totals.tei_eaj_rows_attempted == 3' "$out_dir/audit/summary.json" >/dev/null
jq -e '.totals.materialization_succeeded == 4' "$out_dir/audit/summary.json" >/dev/null
jq -e '([.rows[].selected_aat.adapter] | unique | length) == 3' "$out_dir/audit/summary.json" >/dev/null
jq -e '.inputs.materialization_mode == "batch"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.jobs_total == 4 and .jobs_succeeded == 4 and .jobs_concurrency == 2' "$out_dir/audit/materialization-summary.json" >/dev/null
test -s "$out_dir/audit/materialization-batch.json"
jq -e '[.rows[] | select(.selected_aat.adapter == "aozora" and .aat.raw_nodes_total == 1 and .aat.raw_only_parser_residue_blocks == 1 and .parser_ir.nodes == 0 and .parser_ir.paragraph_count == 0 and .generated_tei.body_missing_gap_p_count == 1 and .generated_tei.body_p_count == 0 and .materialization.status == "passed" and .classification.paragraph_origin_bucket == "adapter_raw_only")] | length == 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].generated_tei.body_p_count >= 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].tei_eaj.body_p_count == 2' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].tei_eaj.structure_profile == "plain_prose"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].tei_eaj.structure_profiles == ["plain_prose"]' "$out_dir/audit/summary.json" >/dev/null
jq -e '[.rows[] | select((.generated_tei.document_tag_counts.ruby // 0) > 0 and (.generated_tei.document_tag_counts.rb // 0) > 0 and (.generated_tei.document_tag_counts.rt // 0) > 0)] | length >= 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].tei_eaj.body_tag_counts.p == 2' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].aat.paragraph_blocks >= 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].classification.paragraph_delta_bucket != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].classification.paragraph_origin_bucket != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].paragraph_rendering.body_ranges != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].paragraph_rendering.empty_body_ranges != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].paragraph_rendering.page_break_nodes != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].paragraph_rendering.source_note_back_ranges != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].deltas.generated_vs_parser_ir_body_p_count != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].deltas.aat_paragraph_blocks_vs_tei_eaj_body_p_count != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.paragraph_origin_buckets | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.tei_eaj_structure_profile_buckets.plain_prose == 3' "$out_dir/audit/summary.json" >/dev/null
jq -e '.tei_eaj_structure_profile_buckets.lineated_text == 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '[.rows[] | select(.work_id == "lb-verse" and .tei_eaj.structure_profile == "lineated_text" and (.tei_eaj.structure_profiles == ["lineated_text"]))] | length == 1' "$out_dir/audit/summary.json" >/dev/null
jq -e '.paragraph_origin_by_tei_eaj_profile.plain_prose | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_paragraph_origin_buckets.aozora2html | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_paragraph_origin_buckets["aozora-rs"] | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_paragraph_origin_buckets.aozora | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_paragraph_delta_buckets.aozora2html | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_paragraph_delta_buckets["aozora-rs"] | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_paragraph_delta_buckets.aozora | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_body_text_match_buckets.aozora2html | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_body_text_match_buckets["aozora-rs"] | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.adapter_body_text_match_buckets.aozora | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.body_text_relation_buckets | type == "object"' "$out_dir/audit/summary.json" >/dev/null
! grep -q '\.worktrees/' "$out_dir/audit/summary.json"
jq -e '.rows[0].text.body_base_text_relation != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].text.generated_body_base_text_length > 0' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].text.tei_eaj_body_base_text_length > 0' "$out_dir/audit/summary.json" >/dev/null
jq -e '.body_text_match_buckets | type == "object"' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].text.body_text_match_bucket != null' "$out_dir/audit/summary.json" >/dev/null
jq -e '.rows[0].text.surface_relations.ruby_expanded_parenless.relation != null' "$out_dir/audit/summary.json" >/dev/null
grep -n "Generated Parser-IR TEI vs TEI-EAJ Workset Audit" "$out_dir/audit/report.md"
grep -n "Body Text Relation Buckets" "$out_dir/audit/report.md"
grep -n "Body Text Match Buckets" "$out_dir/audit/report.md"
grep -n "TEI-EAJ Structure Profile Buckets" "$out_dir/audit/report.md"
grep -n "Paragraph Origin by TEI-EAJ Profile" "$out_dir/audit/report.md"
grep -n "Adapter Paragraph Delta Buckets" "$out_dir/audit/report.md"
grep -n "Paragraph Origin Buckets" "$out_dir/audit/report.md"
grep -n "Adapter Paragraph Origin Buckets" "$out_dir/audit/report.md"
grep -n "Adapter Body Text Match Buckets" "$out_dir/audit/report.md"

echo "parser-IR Level 3 generated workset audit smoke ok: $out_dir/audit"
