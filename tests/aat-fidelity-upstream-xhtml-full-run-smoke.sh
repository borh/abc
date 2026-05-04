#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/upstream-xhtml-full-run-smoke"
aozora_root="$out_dir/aozorabunko"
card_dir="$aozora_root/cards/000001"
files_dir="$card_dir/files"
db_path="$out_dir/fidelity.duckdb"

rm -rf "$out_dir"
mkdir -p "$files_dir"

cat > "$card_dir/card1.html" <<HTML
<!doctype html>
<html><body>
  <a href="files/1_ruby_1.txt">テキストファイル</a>
  <a href="files/1_1.html">XHTMLファイル</a>
</body></html>
HTML

printf '吾輩《わがはい》は猫である。' > "$files_dir/1_ruby_1.txt"
cat > "$files_dir/1_1.html" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
  <body>
    <div class="main_text"><ruby><rb>吾輩</rb><rp>（</rp><rt>わがはい</rt><rp>）</rp></ruby>は猫である。<br /></div>
  </body>
</html>
HTML

"$repo_root/reports/aat-fidelity/run-upstream-xhtml-full.sh" \
  --aozora-root "$aozora_root" \
  --out-dir "$out_dir/run" \
  --db "$db_path" \
  --report-id full-smoke \
  --jobs 2 \
  --max-cards 1 \
  --triage-limit 5

test -s "$out_dir/run/card-urls.txt"
test -s "$out_dir/run/manifest.tsv"
test -s "$out_dir/run/manifest.valid.tsv"
test -s "$out_dir/run/metadata.csv"
test -s "$out_dir/run/observations/000001_1.local.xhtml"
test -s "$out_dir/run/observations/summary.csv"
test -s "$out_dir/run/observations/status-summary.csv"
test -s "$out_dir/run/triage-report/index.md"
test -s "$out_dir/run/adapter-error-report/index.md"
test -s "$out_dir/run/adapter-error-report/adapter-error-classes.csv"
test -s "$db_path"

duckdb_bin="${DUCKDB:-duckdb}"
if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
  duckdb_bin=/etc/profiles/per-user/bor/bin/duckdb
fi
"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, comparison_status, rendered_body_proxy_eligible, proxy_basis, feature_tags from fidelity_xhtml_observations where report_id = 'full-smoke'" \
  | tee "$out_dir/query.csv"

rg -n '^000001_1,main_text_equal,true,normalized_main_text_equal,ruby$' "$out_dir/query.csv"
rg -n '^main_text_equal,1$' "$out_dir/run/observations/status-summary.csv"
rg -n 'Report id: `full-smoke`' "$out_dir/run/triage-report/index.md"
rg -n 'XHTML Adapter Error Classification' "$out_dir/run/adapter-error-report/index.md"

echo "aat fidelity upstream xhtml full run smoke ok: $out_dir"
