#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"
smoke_trace_failures
research_root="${AB_RESEARCH_ROOT:-$repo_root/research}"
out_dir="${TMPDIR:-/tmp}/ab-validator-aat-to-parser-ir-smoke"
rm -rf "$out_dir"
mkdir -p "$out_dir"

aat="$out_dir/input.aat.json"
cat > "$aat" <<'JSON'
{
  "version": 1,
  "work_id": "smoke",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "primary_text_hash": "sha256:7777777777777777777777777777777777777777777777777777777777777777",
    "source_hash": "sha256:7777777777777777777777777777777777777777777777777777777777777777",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "A" },
        { "kind": "ruby", "base": "B", "reading": "bee", "direction": "right" }
      ]
    }
  ]
}
JSON

convert_args=(
  convert
  --aat "$aat"
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json"
  --work-content-hash "sha256:6666666666666666666666666666666666666666666666666666666666666666"
  --parser-ir-out "$out_dir/parser-ir.json"
  --divergence-out "$out_dir/divergence.json"
  --research-root "$research_root"
)

if [ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]; then
  "$AB_AAT_TO_PARSER_IR_BIN" "${convert_args[@]}"
else
  cargo_args=()
  if [ -n "${CARGO_ARGS:-}" ]; then
    # shellcheck disable=SC2206
    cargo_args=(${CARGO_ARGS})
  fi
  "${CARGO:-cargo}" "${cargo_args[@]}" run --package ab-aat-to-parser-ir -- "${convert_args[@]}"
fi

smoke_jq '.schema_id == "https://w3id.org/soranoha/schemas/parser-ir.schema.json"' "$out_dir/parser-ir.json"
smoke_jq '.derived_from.aat_version == 1' "$out_dir/parser-ir.json"
smoke_jq '.derived_from.aat_adapter == "fixture"' "$out_dir/parser-ir.json"
smoke_jq '.derived_from.aat_adapter_version == "fixture 0.1.0"' "$out_dir/parser-ir.json"
smoke_jq '.derived_from.mapping_id == "https://w3id.org/soranoha/mappings/aat-v1-to-parser-ir-v1/generated-probe"' "$out_dir/parser-ir.json"
smoke_jq --slurpfile mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" '.derived_from.mapping_version == $mapping[0].mapping_version' "$out_dir/parser-ir.json"
smoke_jq '.derived_from.mapping_schema_hash == "sha256:c75eaa2e3f4f42002e25ff6cccf725c019c98d653360ca4e648c0f96bec2958a"' "$out_dir/parser-ir.json"
smoke_jq '.paragraphs[0].node_range == {"start":0,"end":2}' "$out_dir/parser-ir.json"
smoke_jq '.paragraphs[0].role == "body"' "$out_dir/parser-ir.json"
smoke_jq '.source.work_content_hash == "sha256:6666666666666666666666666666666666666666666666666666666666666666"' "$out_dir/parser-ir.json"
smoke_jq '.source.primary_text_hash == "sha256:7777777777777777777777777777777777777777777777777777777777777777"' "$out_dir/parser-ir.json"
smoke_jq --slurpfile mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" '.mapping.mapping_version == $mapping[0].mapping_version' "$out_dir/divergence.json"
smoke_jq 'all(.records[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/divergence.json"
# parse_complete used to be dropped and reported as LOSS rule L-466. Since
# c38b661b the converter requires it and carries it in parser-IR derived_from,
# so it must appear there and must not be reported as a divergence.
smoke_jq '.derived_from.parse_complete == true' "$out_dir/parser-ir.json"
smoke_jq 'all(.records[]; .aat_pointer != "meta.parse_complete")' "$out_dir/divergence.json"
smoke_jq '.aat.parse_complete == true' "$out_dir/divergence.json"
smoke_jq 'all(.records[]; .rule_id != null and .message != null and .count >= 1)' "$out_dir/divergence.json"

python_jsonschema=(python)
if ! python - <<'PY' >/dev/null 2>&1
import jsonschema
PY
then
  if command -v uv >/dev/null 2>&1; then
    python_jsonschema=(uv run --isolated --no-project --with 'jsonschema>=4.0' python)
  else
    echo "python jsonschema module is required; install it or run through nix" >&2
    exit 1
  fi
fi

"${python_jsonschema[@]}" - "$research_root/schemas/parser-ir.schema.json" \
  "$research_root/schemas/aat-parser-ir-divergence.schema.json" \
  "$repo_root/data/aat-parser-ir-divergence-bundle-v1.schema.json" \
  "$out_dir/parser-ir.json" \
  "$out_dir/divergence.json" <<'PY'
import json
import sys

import jsonschema

parser_schema_path, record_schema_path, bundle_schema_path, parser_ir_path, bundle_path = sys.argv[1:]
with open(parser_schema_path, encoding="utf-8") as handle:
    parser_schema = json.load(handle)
with open(record_schema_path, encoding="utf-8") as handle:
    record_schema = json.load(handle)
with open(bundle_schema_path, encoding="utf-8") as handle:
    bundle_schema = json.load(handle)
with open(parser_ir_path, encoding="utf-8") as handle:
    parser_ir = json.load(handle)
with open(bundle_path, encoding="utf-8") as handle:
    bundle = json.load(handle)

jsonschema.validate(parser_ir, parser_schema)
jsonschema.validate(bundle, bundle_schema)
for record in bundle["records"]:
    jsonschema.validate(record, record_schema)
PY

audit_dir="$out_dir/audit-corpus"
mkdir -p "$audit_dir"
cp "$aat" "$audit_dir/pass.aat.json"

audit_args=(
  audit-corpus
  --aat-dir "$audit_dir"
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json"
  --summary-json "$out_dir/audit-summary.json"
  --report-md "$out_dir/audit-report.md"
  --compat-edn-out "$out_dir/compatibility-candidates.edn"
  --jobs 2
  --research-root "$research_root"
)

if [ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]; then
  "$AB_AAT_TO_PARSER_IR_BIN" "${audit_args[@]}"
else
  "${CARGO:-cargo}" "${cargo_args[@]}" run --package ab-aat-to-parser-ir -- "${audit_args[@]}"
fi

smoke_jq '.totals.files_attempted == 1' "$out_dir/audit-summary.json"
smoke_jq '.totals.files_succeeded == 1' "$out_dir/audit-summary.json"
smoke_jq '.totals.files_failed == 0' "$out_dir/audit-summary.json"
smoke_jq '.mapping.mapping_hash | test("^sha256:[0-9a-f]{64}$")' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates | length == 1' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates[0].aat_adapter == "fixture"' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates[0].aat_adapter_version == "fixture 0.1.0"' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates[0].mapping_hash == .mapping.mapping_hash' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates[0].evidence_scope.evidence_type == "conversion-audit"' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates[0].evidence_scope.files_scanned == 1' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates[0].evidence_scope.files_succeeded == 1' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates[0].evidence_scope.files_failed == 0' "$out_dir/audit-summary.json"
smoke_jq '.compatibility_candidates[0].evidence_scope.unsupported_occurrences == 0' "$out_dir/audit-summary.json"
grep -n '^# AAT Parser-IR Conversion Audit$' "$out_dir/audit-report.md"
grep -n ':evidence_type :conversion-audit' "$out_dir/compatibility-candidates.edn"
grep -n ':aat_adapter "fixture"' "$out_dir/compatibility-candidates.edn"
edn_parse_expr='(require '"'"'[clojure.edn :as edn]) (edn/read-string (slurp (System/getenv "COMPAT_EDN")))'
if command -v bb >/dev/null 2>&1; then
  COMPAT_EDN="$out_dir/compatibility-candidates.edn" bb -e "$edn_parse_expr"
else
  COMPAT_EDN="$out_dir/compatibility-candidates.edn" clojure -M -e "$edn_parse_expr"
fi

structural_aat="$out_dir/structural.aat.json"
cat > "$structural_aat" <<'JSON'
{
  "version": 1,
  "work_id": "000035_1567",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:8888888888888888888888888888888888888888888888888888888888888888",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "メロスは激怒した。" }
      ]
    },
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "（古伝説と、シルレルの詩から。）" }
      ]
    }
  ]
}
JSON

structural_args=(
  structural-probe
  --aat "fixture=$structural_aat"
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json"
  --summary-json "$out_dir/structural-summary.json"
  --report-md "$out_dir/structural-report.md"
  --research-root "$research_root"
)

if [ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]; then
  "$AB_AAT_TO_PARSER_IR_BIN" "${structural_args[@]}"
else
  "${CARGO:-cargo}" "${cargo_args[@]}" run --package ab-aat-to-parser-ir -- "${structural_args[@]}"
fi

smoke_jq '.totals.inputs == 1' "$out_dir/structural-summary.json"
smoke_jq '.totals.conversions_succeeded == 1' "$out_dir/structural-summary.json"
smoke_jq '.inputs[0].aat.paragraph_blocks == 2' "$out_dir/structural-summary.json"
smoke_jq '.inputs[0].aat.final_source_attribution_candidate == true' "$out_dir/structural-summary.json"
smoke_jq '.inputs[0].parser_ir.paragraph_count == 2' "$out_dir/structural-summary.json"
smoke_jq '.inputs[0].parser_ir.paragraphs_represented == true' "$out_dir/structural-summary.json"
smoke_jq '.inputs[0].parser_ir.source_attribution_represented == true' "$out_dir/structural-summary.json"
smoke_jq '.inputs[0].verdict.residual_free == true' "$out_dir/structural-summary.json"
smoke_jq '.inputs[0].divergence.paragraph_structural_records == []' "$out_dir/structural-summary.json"
grep -n 'AAT Structural Probe' "$out_dir/structural-report.md"

tei_aat_dir="$out_dir/tei-aat"
mkdir -p "$tei_aat_dir"
cp "$structural_aat" "$tei_aat_dir/000035_1567-fixture.json"
tei_workset="$out_dir/tei-eaj-workset.json"
cat > "$tei_workset" <<'JSON'
{
  "schema_version": "tei-eaj-aozora-workset-export-v1",
  "summary": {
    "tei_eaj_file_count": 1,
    "tei_eaj_work_id_count": 1,
    "abc_counterpart_count": 1,
    "compared_file_count": 1,
    "missing_counterpart_count": 0,
    "no_work_id_count": 0,
    "base_text_equal_count": 0,
    "base_text_mismatch_count": 1,
    "uncompared_file_count": 0
  },
  "tei_eaj_source": {"revision": "fixture", "root": "/fixture"},
  "abc_inputs": {"counterparts": [], "tei_dirs": [], "tei_specs": []},
  "candidate_work_ids": ["1567"],
  "missing_abc_counterpart_work_ids": [],
  "no_work_id_files": [],
  "files": [{
    "abc_body_base_text_length": 9806,
    "abc_note_count": 1,
    "abc_p_count": 1,
    "abc_tei": "crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml",
    "base_text_equal": false,
    "comparison_status": "compared",
    "first_difference": null,
    "level": "Level 4",
    "state": "complete",
    "tei_eaj_body_base_text_length": 9790,
    "tei_eaj_file": "data/complete/tei_lib_lv4/1567_tei.xml",
    "tei_eaj_note_count": 0,
    "tei_eaj_p_count": 19,
    "title": "走れメロス",
    "work_id": "1567"
  }]
}
JSON

tei_args=(
  tei-eaj-structural-expansion
  --workset "$tei_workset"
  --aat-dir "fixture=$tei_aat_dir"
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json"
  --summary-json "$out_dir/tei-eaj-structural-summary.json"
  --report-md "$out_dir/tei-eaj-structural-report.md"
  --research-root "$research_root"
)

if [ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]; then
  "$AB_AAT_TO_PARSER_IR_BIN" "${tei_args[@]}"
else
  "${CARGO:-cargo}" "${cargo_args[@]}" run --package ab-aat-to-parser-ir -- "${tei_args[@]}"
fi

smoke_jq '.totals.tei_eaj_files == 1' "$out_dir/tei-eaj-structural-summary.json"
smoke_jq '.totals.rows_with_aat_evidence == 1' "$out_dir/tei-eaj-structural-summary.json"
smoke_jq '.totals.parser_ir_gap_rows == 0' "$out_dir/tei-eaj-structural-summary.json"
smoke_jq '.totals.source_attribution_gap_rows == 0' "$out_dir/tei-eaj-structural-summary.json"
smoke_jq '.rows[0].tei.tei_eaj_p_count == 19' "$out_dir/tei-eaj-structural-summary.json"
grep -n 'TEI-EAJ Structural Expansion' "$out_dir/tei-eaj-structural-report.md"
