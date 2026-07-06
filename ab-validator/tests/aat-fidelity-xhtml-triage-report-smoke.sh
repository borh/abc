#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/xhtml-triage-smoke"
db_path="$out_dir/fidelity.duckdb"

rm -rf "$out_dir"
mkdir -p "$out_dir"

cat > "$out_dir/upstream.xhtml" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body><div class="main_text">吾輩猫である。</div></body></html>
HTML

cat > "$out_dir/local.xhtml" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body><div class="main_text">吾輩犬である。</div></body></html>
HTML

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
libstdcxx_dir="$(dirname "$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')")"
export LD_LIBRARY_PATH="$libstdcxx_dir:${LD_LIBRARY_PATH:-}"

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id smoke-xhtml \
  --case-id smoke.case \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --local-xhtml "$out_dir/local.xhtml" \
  --feature-tags "ruby;gaiji"

"$repo_root/reports/aat-fidelity/build-xhtml-triage-report.sh" \
  --db "$db_path" \
  --report-id smoke-xhtml \
  --out-dir "$out_dir/report" \
  --limit 10

test -s "$out_dir/report/index.md"
test -s "$out_dir/report/status-summary.csv"
test -s "$out_dir/report/feature-status-summary.csv"
test -s "$out_dir/report/mismatch-shapes.csv"
test -s "$out_dir/report/top-mismatches.csv"

rg -n '^main_text_mismatch,1$' "$out_dir/report/status-summary.csv"
rg -n '^gaiji,main_text_mismatch,1$' "$out_dir/report/feature-status-summary.csv"
rg -n '^ruby,main_text_mismatch,1$' "$out_dir/report/feature-status-summary.csv"
rg -n 'main_text_mismatch' "$out_dir/report/index.md"

echo "aat fidelity xhtml triage report smoke ok: $out_dir/report"
