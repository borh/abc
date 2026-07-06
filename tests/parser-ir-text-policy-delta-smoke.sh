#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-text-policy-delta.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

matrix_summary="$out_dir/matrix.json"
summary_json="$out_dir/text-policy-delta.summary.json"
report_md="$out_dir/text-policy-delta.md"
worksets_dir="$out_dir/text-policy-worksets"

cat > "$matrix_summary" <<'JSON'
{
  "body_text_relation_buckets": {
    "different": 15,
    "equal": 1,
    "generated_contains_tei_eaj": 1
  },
  "rows": [
    {
      "work_id": "ruby-1",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {
        "body_base_text_relation": "different",
        "surface_relations": {
          "ruby_expanded_parenless": {"relation": "equal"},
          "base_drop_parentheticals": {"relation": "different"}
        }
      },
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "ruby-2",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {
        "body_base_text_relation": "different",
        "surface_relations": {
          "ruby_expanded_parenless": {"relation": "different"},
          "base_drop_parentheticals": {"relation": "equal"}
        }
      },
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "front-1",
      "selected_aat": {"adapter": "aozora-rs"},
      "text": {"body_base_text_relation": "different", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["front_back_matter"]},
      "classification": {"paragraph_origin_bucket": "source_note_back_routing", "source_note_body_excluded": true}
    },
    {
      "work_id": "front-2",
      "selected_aat": {"adapter": "aozora-rs"},
      "text": {"body_base_text_relation": "different", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "source_note_back_routing", "source_note_body_excluded": false}
    },
    {
      "work_id": "layout-1",
      "selected_aat": {"adapter": "aozora2"},
      "text": {"body_base_text_relation": "different", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["lineated_text"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "layout-2",
      "selected_aat": {"adapter": "aozora2"},
      "text": {"body_base_text_relation": "different", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["verse"]},
      "classification": {"paragraph_origin_bucket": "page_break_projection", "source_note_body_excluded": false}
    },
    {
      "work_id": "adapter-1",
      "selected_aat": {"adapter": "aozora-epub3"},
      "text": {"body_base_text_relation": "different", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented", "source_note_body_excluded": false}
    },
    {
      "work_id": "adapter-2",
      "selected_aat": {"adapter": "aozora-epub3"},
      "text": {"body_base_text_relation": "different", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "adapter_collapsed", "source_note_body_excluded": false}
    },
    {
      "work_id": "lv4-1",
      "selected_aat": {"adapter": "aozora"},
      "text": {"body_base_text_relation": "different", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["lv4_enrichment"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "ruby-near-1",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {
        "body_base_text_relation": "different",
        "body_base_text_first_diff": {
          "generated_preview": "していることは事実である。",
          "tei_eaj_preview": "（ようえい）していることは事実である。"
        },
        "surface_relations": {
          "ruby_expanded_parenless": {"relation": "different", "length_delta": -2},
          "base_drop_parentheticals": {"relation": "different", "length_delta": -2}
        }
      },
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "notes-ruby-1",
      "selected_aat": {"adapter": "aozora-epub3"},
      "text": {
        "body_base_text_relation": "different",
        "body_base_text_first_diff": {
          "generated_preview": "なく降らす時などは、",
          "tei_eaj_preview": "（とめど）なく降らす時などは、"
        },
        "surface_relations": {
          "ruby_expanded_parenless": {"relation": "different", "length_delta": 0},
          "base_drop_parentheticals": {"relation": "different", "length_delta": 0}
        }
      },
      "tei_eaj": {"structure_profiles": ["notes"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "letter-region-1",
      "selected_aat": {"adapter": "aozora2"},
      "text": {
        "body_base_text_relation": "different",
        "body_base_text_first_diff": {
          "generated_preview": "宛先東京市麹町区三番町六四第一福四萬館発信地千葉県",
          "tei_eaj_preview": "雑誌ありがたう御座いました。"
        },
        "surface_relations": {}
      },
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "front-parenthetical-precedence",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {
        "body_base_text_relation": "different",
        "body_base_text_first_diff": {
          "generated_preview": "していることは事実である。",
          "tei_eaj_preview": "（ようえい）していることは事実である。"
        },
        "surface_relations": {}
      },
      "tei_eaj": {"structure_profiles": ["front_back_matter"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "layout-parenthetical-precedence",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {
        "body_base_text_relation": "different",
        "body_base_text_first_diff": {
          "generated_preview": "していることは事実である。",
          "tei_eaj_preview": "（ようえい）していることは事実である。"
        },
        "surface_relations": {}
      },
      "tei_eaj": {"structure_profiles": ["lineated_text"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "adapter-parenthetical-precedence",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {
        "body_base_text_relation": "different",
        "body_base_text_first_diff": {
          "generated_preview": "していることは事実である。",
          "tei_eaj_preview": "（ようえい）していることは事実である。"
        },
        "surface_relations": {}
      },
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented", "source_note_body_excluded": false}
    },
    {
      "work_id": "calibration-1",
      "selected_aat": {"adapter": "aozora"},
      "text": {"body_base_text_relation": "generated_contains_tei_eaj", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    },
    {
      "work_id": "equal-1",
      "selected_aat": {"adapter": "aozora"},
      "text": {"body_base_text_relation": "equal", "surface_relations": {}},
      "tei_eaj": {"structure_profiles": ["plain_prose"]},
      "classification": {"paragraph_origin_bucket": "aligned", "source_note_body_excluded": false}
    }
  ]
}
JSON

mkdir -p "$worksets_dir"
printf '[{"stale": true}]\n' > "$worksets_dir/ruby_or_parenthetical_policy.json"
printf 'keep\n' > "$worksets_dir/unrelated.txt"

python3 "$repo_root/reports/parser-ir/text-policy-delta.py" \
  --matrix-summary "$matrix_summary" \
  --summary-json "$summary_json" \
  --report-md "$report_md" \
  --worksets-dir "$worksets_dir"

jq -e '.schema_version == "parser-ir-text-policy-delta-v1"' "$summary_json" >/dev/null
jq -e '.total_different_rows == 15' "$summary_json" >/dev/null
jq -e '.total_different_rows == (.counts_by_cause | to_entries | map(.value) | add)' "$summary_json" >/dev/null
jq -e '.counts_by_cause.ruby_or_parenthetical_policy == 4' "$summary_json" >/dev/null
jq -e '.counts_by_cause.front_back_source_region_policy == 4' "$summary_json" >/dev/null
jq -e '.counts_by_cause.body_visible_layout_policy == 3' "$summary_json" >/dev/null
jq -e '.counts_by_cause.adapter_text_loss == 3' "$summary_json" >/dev/null
jq -e '.counts_by_cause.tei_eaj_editorial_or_enrichment == 1' "$summary_json" >/dev/null
jq -e '.counts_by_cause.unknown_text_delta == 0' "$summary_json" >/dev/null
jq -e '.already_equal_rows == 1' "$summary_json" >/dev/null
jq -e '.calibration_only_rows[] | select(.body_text_relation == "generated_contains_tei_eaj")' "$summary_json" >/dev/null
jq -e '([.source_markup_backed_blockers[].cause] | index("tei_eaj_editorial_or_enrichment") | not)' "$summary_json" >/dev/null
jq -e '.workset_files.ruby_or_parenthetical_policy.count == 4 and .workset_files.ruby_or_parenthetical_policy.source_markup_backed == true and (.workset_files.ruby_or_parenthetical_policy.hash | test("^sha256:[0-9a-f]{64}$"))' "$summary_json" >/dev/null
jq -e '.workset_files.front_back_source_region_policy.count == 4 and .workset_files.body_visible_layout_policy.count == 3 and .workset_files.adapter_text_loss.count == 3' "$summary_json" >/dev/null
jq -e '.workset_files.tei_eaj_editorial_or_enrichment.count == 1 and .workset_files.tei_eaj_editorial_or_enrichment.source_markup_backed == false' "$summary_json" >/dev/null
jq -e '.workset_files.unknown_text_delta.count == 0 and .workset_files.unknown_text_delta.path == null and .workset_files.unknown_text_delta.source_markup_backed == false and .workset_files.unknown_text_delta.requires_manual_classification == true' "$summary_json" >/dev/null
jq -e '(.source_markup_backed_workset_files | keys | sort) == (["adapter_text_loss", "body_visible_layout_policy", "front_back_source_region_policy", "ruby_or_parenthetical_policy"] | sort)' "$summary_json" >/dev/null
jq -e '.manual_classification_workset_files.unknown_text_delta.requires_manual_classification == true and .manual_classification_workset_files.unknown_text_delta.source_markup_backed == false' "$summary_json" >/dev/null
test -e "$worksets_dir/unrelated.txt"
jq -e 'length == 4 and .[0].cause == "ruby_or_parenthetical_policy"' "$worksets_dir/ruby_or_parenthetical_policy.json" >/dev/null
jq -e 'length == 4 and all(.[]; .cause == "front_back_source_region_policy")' "$worksets_dir/front_back_source_region_policy.json" >/dev/null
jq -e 'length == 1 and .[0].cause == "tei_eaj_editorial_or_enrichment"' "$worksets_dir/tei_eaj_editorial_or_enrichment.json" >/dev/null
test ! -e "$worksets_dir/unknown_text_delta.json"
rg -n "Text Policy Delta" "$report_md" >/dev/null
rg -n "Workset Files" "$report_md" >/dev/null
