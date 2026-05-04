#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aat-batch-triage-smoke"
db_path="$out_dir/fidelity.duckdb"
reports_dir="$out_dir/check-reports/fixture-adapter"
aat_dir="$out_dir/aat/fixture-adapter"
report_dir="$out_dir/report"

rm -rf "$out_dir"
mkdir -p "$reports_dir" "$aat_dir"

cat > "$reports_dir/work-ok.json" <<'JSON'
{
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0",
  "work_id": "work-ok",
  "results": {
    "schema_valid": {"pass": true, "confidence": "strict"},
    "parse_completeness": {"pass": false, "message": "parse incomplete", "confidence": "strict"}
  }
}
JSON

cat > "$reports_dir/work-error.json" <<'JSON'
{
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0",
  "work_id": "work-error",
  "results": {
    "fatal_error": {"pass": false, "message": "boom", "confidence": "strict"}
  }
}
JSON

cat > "$reports_dir/work-ok-alt.json" <<'JSON'
{
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0",
  "work_id": "work-ok",
  "results": {
    "schema_valid": {"pass": true, "confidence": "strict"},
    "parse_completeness": {"pass": true, "confidence": "strict"}
  }
}
JSON

cat > "$aat_dir/work-ok.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-ok",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {"kind": "text", "value": "A"},
        {
          "kind": "style",
          "style": "font_size",
          "content": [{"kind": "text", "value": "B"}],
          "x-provenance": "source-derived",
          "x-aozora-syntax-id": "decoration.font_size"
        }
      ]
    }
  ],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": false,
    "semantic_summary": {
      "syntax": {
        "decoration.font_size": [
          {"kind": "style", "provenance": "source-derived", "value": {"size": "large"}},
          {"kind": "style", "provenance": "parser", "value": {"size": "large"}}
        ]
      }
    }
  }
}
JSON

duckdb_bin="${DUCKDB:-duckdb}"
if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
  duckdb_bin=/etc/profiles/per-user/bor/bin/duckdb
fi
libstdcxx_path="$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')"
libstdcxx_dir="$(dirname "$libstdcxx_path")"

LD_LIBRARY_PATH="${libstdcxx_dir}:${LD_LIBRARY_PATH:-}" \
  uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$out_dir/check-reports" \
  --aat-dir "$out_dir/aat" \
  --db "$db_path" \
  --report-id "aat-batch-smoke" \
  --out-dir "$report_dir"

"$duckdb_bin" -csv -header "$db_path" \
  "select total_reports, reports_with_failures, total_failures, reports_with_source_derived_nodes from aat_batch_report_summary where report_id = 'aat-batch-smoke'" \
  > "$out_dir/summary.csv"

rg -n "3,2,2,1" "$out_dir/summary.csv"
rg -n "parse_completeness" "$report_dir/outputs/property_failures.csv"
rg -n "work-ok.json" "$report_dir/outputs/failure_examples.csv"
rg -n "work-ok.json" "$report_dir/outputs/source_derived_examples.csv"
rg -n "decoration.font_size" "$report_dir/outputs/source_derived_syntax.csv"
rg -n "source_derived_nodes" "$report_dir/index.md"
