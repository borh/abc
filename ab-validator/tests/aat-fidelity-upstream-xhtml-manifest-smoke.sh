#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/upstream-xhtml-manifest-smoke"
manifest="$out_dir/manifest.tsv"
metadata="$out_dir/metadata.csv"
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

cat > "$manifest" <<TSV
# case_id	source	upstream_xhtml
fixture.manifest.ruby	$out_dir/source.txt	$out_dir/upstream.xhtml
TSV

cat > "$metadata" <<CSV
case_id,feature_tags,card_url,source,upstream_xhtml,status
fixture.manifest.ruby,ruby;inline_annotation,https://example.test/cards/000001/card1.html,$out_dir/source.txt,$out_dir/upstream.xhtml,paired
CSV

"$repo_root/reports/aat-fidelity/run-upstream-xhtml-observations.sh" \
  --manifest "$manifest" \
  --metadata "$metadata" \
  --out-dir "$out_dir/observations" \
  --db "$db_path" \
  --report-id manifest-smoke

test -s "$db_path"
test -s "$out_dir/observations/summary.csv"
test -s "$out_dir/observations/status-summary.csv"
test -s "$out_dir/observations/fixture.manifest.ruby.local.xhtml"

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, raw_equal, main_text_equal, feature_tags, card_url from fidelity_xhtml_observations" \
  | tee "$out_dir/query.csv"

rg -n 'fixture.manifest.ruby,false,true,ruby;inline_annotation,https://example.test/cards/000001/card1.html' "$out_dir/query.csv"
rg -n 'total,raw_equal,main_text_equal' "$out_dir/observations/summary.csv"
rg -n '^1,0,1$' "$out_dir/observations/summary.csv"
rg -n 'comparison_status,rows' "$out_dir/observations/status-summary.csv"
rg -n '^main_text_equal,1$' "$out_dir/observations/status-summary.csv"

echo "aat fidelity upstream xhtml manifest smoke ok: $out_dir"
