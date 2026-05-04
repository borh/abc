#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/xhtml-adapter-errors-smoke"
db_path="$out_dir/fidelity.duckdb"

rm -rf "$out_dir"
mkdir -p "$out_dir"

duckdb_bin="${DUCKDB:-duckdb}"
if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
  duckdb_bin=/etc/profiles/per-user/bor/bin/duckdb
fi
libstdcxx_dir="$(dirname "$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')")"
export LD_LIBRARY_PATH="$libstdcxx_dir:${LD_LIBRARY_PATH:-}"

cat > "$out_dir/upstream.xhtml" <<'XHTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body><div class="main_text">本文</div></body></html>
XHTML

cat > "$out_dir/local-jisage.json" <<'JSON'
{"version":1,"blocks":[],"meta":{"parse_complete":false,"warnings":[{"message":"aozora2html parser aborted: エラー(158行目):字下げを閉じようとしましたが、字下げ中ではありません. \n処理を停止します"}]}}
JSON

cat > "$out_dir/local-close-tag.json" <<'JSON'
{"version":1,"blocks":[],"meta":{"parse_complete":false,"warnings":[{"message":"aozora2html parser aborted: /db/ab-validator/gems/aozora2html-3.0.1/lib/aozora2html.rb:846:in 'Aozora2Html#push_block_tag': undefined method 'close_tag' for nil (NoMethodError)"}]}}
JSON

for case_id in smoke.jisage smoke.close_tag; do
  local_json="$out_dir/local-jisage.json"
  if [[ "$case_id" == "smoke.close_tag" ]]; then
    local_json="$out_dir/local-close-tag.json"
  fi
  uv run --isolated --no-project \
    --with 'duckdb>=1.1' \
    --with 'lxml>=5' \
    "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
    --db "$db_path" \
    --report-id adapter-error-smoke \
    --case-id "$case_id" \
    --upstream-xhtml "$out_dir/upstream.xhtml" \
    --local-xhtml "$local_json"
done

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/classify-xhtml-adapter-errors.py" \
  --db "$db_path" \
  --report-id adapter-error-smoke \
  --out-dir "$out_dir/report"

test -s "$out_dir/report/adapter-error-classes.csv"
test -s "$out_dir/report/adapter-error-examples.csv"
test -s "$out_dir/report/index.md"

rg -n '^jisage_close_without_open,1$' "$out_dir/report/adapter-error-classes.csv"
rg -n '^ruby_close_tag_no_method,1$' "$out_dir/report/adapter-error-classes.csv"
rg -n 'smoke.jisage,jisage_close_without_open' "$out_dir/report/adapter-error-examples.csv"
rg -n 'XHTML Adapter Error Classification' "$out_dir/report/index.md"

echo "aat fidelity xhtml adapter errors smoke ok: $out_dir/report"
