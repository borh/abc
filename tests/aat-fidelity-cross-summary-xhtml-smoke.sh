#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/cross-summary-xhtml-smoke"
db_path="$out_dir/fidelity.duckdb"
report_json="$out_dir/report.json"
summary_md="$out_dir/summary.md"

rm -rf "$out_dir"
mkdir -p "$out_dir"

duckdb_bin="${DUCKDB:-duckdb}"
if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
  duckdb_bin=/etc/profiles/per-user/bor/bin/duckdb
fi
libstdcxx_dir="$(dirname "$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')")"
export LD_LIBRARY_PATH="$libstdcxx_dir:${LD_LIBRARY_PATH:-}"

cat > "$report_json" <<'JSON'
{
  "rows": [
    {
      "adapter": "aozora2html",
      "case_id": "gaiji.jis.2-13-47",
      "schema_status": "pass",
      "upstream_status": "faithful",
      "oracle_status": "fail",
      "oracle_review_status": "reviewed",
      "oracle_evidence_strength": "reference",
      "failures": ["fixture failure"]
    }
  ]
}
JSON

cat > "$out_dir/upstream.xhtml" <<'XHTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body><div class="main_text">本文</div></body></html>
XHTML
cp "$out_dir/upstream.xhtml" "$out_dir/local.xhtml"

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id smoke.xhtml.equal \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --local-xhtml "$out_dir/local.xhtml"

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/cross_adapter_summary.py" \
  --report "$report_json" \
  --oracle "$repo_root/data/aat-oracle-cases.toml" \
  --xhtml-db "$db_path" \
  --xhtml-report-id xhtml-smoke \
  > "$summary_md"

rg -n 'XHTML Source Evidence' "$summary_md"
rg -n 'xhtml-smoke' "$summary_md"
rg -n 'rendered-body proxy eligible | 1' "$summary_md"
rg -n 'aozora2html.*source-level oracle failures should be interpreted beside rendered-XHTML evidence' "$summary_md"

echo "aat fidelity cross summary xhtml smoke ok: $summary_md"
