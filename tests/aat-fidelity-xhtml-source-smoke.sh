#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/xhtml-source-smoke"
db_path="$out_dir/fidelity.duckdb"

rm -rf "$out_dir"
mkdir -p "$out_dir"

cat > "$out_dir/upstream.xhtml" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
  <body>
    <div class="main_text">吾輩<ruby><rb>猫</rb><rt>ねこ</rt></ruby>である。<br /></div>
  </body>
</html>
HTML

cat > "$out_dir/local.xhtml" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body>
<div class="main_text">
吾輩<ruby><rb>猫</rb><rt>ねこ</rt></ruby>である。<br/>
</div>
</body></html>
HTML

duckdb_bin="${DUCKDB:-duckdb}"
if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
  duckdb_bin=/etc/profiles/per-user/bor/bin/duckdb
fi
libstdcxx_dir="$(dirname "$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')")"
export LD_LIBRARY_PATH="$libstdcxx_dir:${LD_LIBRARY_PATH:-}"

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id fixture.xhtml.same-main-text \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --local-xhtml "$out_dir/local.xhtml"

test -s "$db_path"

"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, raw_equal, main_text_equal, comparison_status, upstream_main_text, local_main_text from fidelity_xhtml_observations" \
  | tee "$out_dir/query.csv"

rg -n 'fixture.xhtml.same-main-text,false,true,main_text_equal,"吾輩猫ねこである。","吾輩猫ねこである。"' "$out_dir/query.csv"

: > "$out_dir/empty-local.xhtml"
uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id fixture.xhtml.empty-local \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --local-xhtml "$out_dir/empty-local.xhtml"

"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, main_text_equal, comparison_status, local_main_text from fidelity_xhtml_observations where case_id = 'fixture.xhtml.empty-local'" \
  | tee "$out_dir/empty-local-query.csv"

rg -n '^fixture.xhtml.empty-local,false,local_parse_error,$' "$out_dir/empty-local-query.csv"

echo "aat fidelity xhtml source smoke ok: $out_dir"
