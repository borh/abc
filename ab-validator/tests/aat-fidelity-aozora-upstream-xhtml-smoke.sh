#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/upstream-xhtml-smoke"
db_path="$out_dir/fidelity.duckdb"

rm -rf "$out_dir"
mkdir -p "$out_dir"

printf '吾輩《わがはい》は猫である。' > "$out_dir/source.txt"
cat > "$out_dir/upstream.xhtml" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
  <body>
    <div class="main_text"><ruby><rb>吾輩</rb><rp>（</rp><rt>わがはい</rt><rp>）</rp></ruby>は猫である。<br /></div>
  </body>
</html>
HTML

"$repo_root/reports/aat-fidelity/compare-aozora-upstream-xhtml.sh" \
  --case-id upstream.fixture.ruby \
  --source "$out_dir/source.txt" \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --out-dir "$out_dir" \
  --db "$db_path" \
  --report-id upstream-smoke

test -s "$out_dir/upstream.fixture.ruby.local.xhtml"
test -s "$db_path"

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, raw_equal, main_text_equal, comparison_status, upstream_main_text, local_main_text from fidelity_xhtml_observations" \
  | tee "$out_dir/query.csv"

rg -n 'upstream.fixture.ruby,false,true,main_text_equal,"吾輩（わがはい）は猫である。","吾輩（わがはい）は猫である。"' "$out_dir/query.csv"

echo "aat fidelity upstream xhtml smoke ok: $out_dir"
