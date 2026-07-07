#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/smoke-env.sh"
repo_root="$AB_VALIDATOR_ROOT"
out_dir="$(smoke_tmp_dir ab-adapter-fidelity)"
trap 'smoke_cleanup "$out_dir"' EXIT

matrix_summary="$out_dir/matrix.json"
summary_json="$out_dir/adapter-fidelity.summary.json"
report_md="$out_dir/adapter-fidelity.md"
worksets_dir="$out_dir/worksets"

cat > "$matrix_summary" <<'JSON'
{
  "rows": [
    {
      "work_id": "over",
      "tei_eaj_file": "tei/over.xml",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {"body_base_text_relation": "different"},
      "classification": {"paragraph_origin_bucket": "adapter_over_segmented"},
      "parser_ir": {"nodes": 10, "paragraph_count": 2, "source_note_count": 0}
    },
    {
      "work_id": "collapsed",
      "tei_eaj_file": "tei/collapsed.xml",
      "selected_aat": {"adapter": "aozora-rs"},
      "text": {"body_base_text_relation": "different"},
      "classification": {"paragraph_origin_bucket": "adapter_collapsed"},
      "parser_ir": {"nodes": 11, "paragraph_count": 1, "source_note_count": 0}
    },
    {
      "work_id": "under",
      "tei_eaj_file": "tei/under.xml",
      "selected_aat": {"adapter": "aozora2"},
      "text": {"body_base_text_relation": "different"},
      "classification": {"paragraph_origin_bucket": "adapter_under_segmented"},
      "parser_ir": {"nodes": 12, "paragraph_count": 1, "source_note_count": 0}
    },
    {
      "work_id": "converter",
      "tei_eaj_file": "tei/converter.xml",
      "selected_aat": {"adapter": "aozora"},
      "text": {"body_base_text_relation": "different"},
      "classification": {"paragraph_origin_bucket": "converter_paragraph_mismatch"},
      "parser_ir": {"nodes": 13, "paragraph_count": 3, "source_note_count": 1}
    },
    {
      "work_id": "raw",
      "tei_eaj_file": "tei/raw.xml",
      "selected_aat": {"adapter": "aozora-epub3"},
      "text": {"body_base_text_relation": "different"},
      "classification": {"paragraph_origin_bucket": "adapter_raw_only"},
      "parser_ir": {"nodes": 14, "paragraph_count": 0, "source_note_count": 0}
    },
    {
      "work_id": "aligned",
      "tei_eaj_file": "tei/aligned.xml",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {"body_base_text_relation": "equal"},
      "classification": {"paragraph_origin_bucket": "aligned"},
      "parser_ir": {"nodes": 15, "paragraph_count": 4, "source_note_count": 0}
    },
    {
      "work_id": "source-note",
      "tei_eaj_file": "tei/source-note.xml",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {"body_base_text_relation": "different"},
      "classification": {"paragraph_origin_bucket": "source_note_back_routing"},
      "parser_ir": {"nodes": 16, "paragraph_count": 4, "source_note_count": 1}
    },
    {
      "work_id": "page-break",
      "tei_eaj_file": "tei/page-break.xml",
      "selected_aat": {"adapter": "aozora2html"},
      "text": {"body_base_text_relation": "different"},
      "classification": {"paragraph_origin_bucket": "page_break_projection"},
      "parser_ir": {"nodes": 17, "paragraph_count": 4, "source_note_count": 0}
    }
  ]
}
JSON

mkdir -p "$worksets_dir/stale_bucket"
printf '["stale"]\n' > "$worksets_dir/stale_bucket/stale-adapter.json"

python "$repo_root/reports/parser-ir/adapter-fidelity-worksets.py" \
  --matrix-summary "$matrix_summary" \
  --summary-json "$summary_json" \
  --report-md "$report_md" \
  --worksets-dir "$worksets_dir"

jq -e '.schema_version == "adapter-fidelity-worksets-v1"' "$summary_json" >/dev/null
jq -e '(.included_buckets | sort) == (["adapter_collapsed", "adapter_over_segmented", "adapter_raw_only", "adapter_under_segmented", "converter_paragraph_mismatch"] | sort)' "$summary_json" >/dev/null
jq -e '(.excluded_buckets | sort) == (["aligned", "page_break_projection", "source_note_back_routing"] | sort)' "$summary_json" >/dev/null
jq -e '.worksets.adapter_over_segmented.rows[] | select(.work_id == "over" and .adapter == "aozora2html" and .parser_ir_nodes == 10)' "$summary_json" >/dev/null
jq -e '.worksets.adapter_collapsed.rows[] | select(.work_id == "collapsed" and .adapter == "aozora-rs")' "$summary_json" >/dev/null
jq -e '.worksets.adapter_under_segmented.rows[] | select(.work_id == "under" and .adapter == "aozora2")' "$summary_json" >/dev/null
jq -e '.worksets.converter_paragraph_mismatch.rows[] | select(.work_id == "converter" and .source_note_count == 1)' "$summary_json" >/dev/null
jq -e '.worksets.adapter_raw_only.rows[] | select(.work_id == "raw" and .paragraph_count == 0)' "$summary_json" >/dev/null
jq -e '.workset_files.adapter_over_segmented.all.count == 1 and .workset_files.adapter_over_segmented.by_adapter.aozora2html.count == 1' "$summary_json" >/dev/null
jq -e '.workset_files.adapter_collapsed.by_adapter."aozora-rs".count == 1' "$summary_json" >/dev/null
jq -e '.workset_files.adapter_under_segmented.by_adapter.aozora2.count == 1' "$summary_json" >/dev/null
jq -e '. == ["over"]' "$worksets_dir/adapter_over_segmented/all.json" >/dev/null
jq -e '. == ["over"]' "$worksets_dir/adapter_over_segmented/aozora2html.json" >/dev/null
jq -e '. == ["collapsed"]' "$worksets_dir/adapter_collapsed/aozora-rs.json" >/dev/null
jq -e '. == ["under"]' "$worksets_dir/adapter_under_segmented/aozora2.json" >/dev/null
test ! -e "$worksets_dir/stale_bucket/stale-adapter.json"
jq -e '([.worksets[].rows[].work_id] | index("aligned") | not)' "$summary_json" >/dev/null
jq -e '([.worksets[].rows[].work_id] | index("source-note") | not)' "$summary_json" >/dev/null
jq -e '([.worksets[].rows[].work_id] | index("page-break") | not)' "$summary_json" >/dev/null
rg -n "Adapter Fidelity Worksets" "$report_md" >/dev/null
