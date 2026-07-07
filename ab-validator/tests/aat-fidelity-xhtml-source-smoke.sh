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
  --report-id xhtml-smoke \
  --case-id fixture.xhtml.same-main-text \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --local-xhtml "$out_dir/local.xhtml"

test -s "$db_path"

"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, raw_equal, main_text_equal, rendered_body_proxy_eligible, proxy_basis, comparison_status, first_diff_index, upstream_diff_context, local_diff_context, upstream_main_text, local_main_text from fidelity_xhtml_observations where case_id = 'fixture.xhtml.same-main-text'" \
  | tee "$out_dir/query.csv"

rg -n 'fixture.xhtml.same-main-text,false,true,true,normalized_main_text_equal,main_text_equal,-1,,,\"吾輩猫ねこである。\",\"吾輩猫ねこである。\"' "$out_dir/query.csv"

python - <<PY
from pathlib import Path

body = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja"><head>
  <meta http-equiv="Content-Type" content="text/html;charset=Shift_JIS" />
</head><body><div class="main_text">元日を御目出（おめで）たい</div></body></html>'''
Path("$out_dir/upstream-shiftjis-meta.xhtml").write_bytes(body.encode("cp932"))
Path("$out_dir/local-shiftjis-meta.xhtml").write_text(
    '''<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body><div class="main_text">元日を御目出（おめで）たい</div></body></html>''',
    encoding="utf-8",
)
PY

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id fixture.xhtml.shiftjis-meta \
  --upstream-xhtml "$out_dir/upstream-shiftjis-meta.xhtml" \
  --local-xhtml "$out_dir/local-shiftjis-meta.xhtml"

"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, main_text_equal, comparison_status, upstream_main_text from fidelity_xhtml_observations where case_id = 'fixture.xhtml.shiftjis-meta'" \
  | tee "$out_dir/shiftjis-meta-query.csv"

rg -n '^fixture.xhtml.shiftjis-meta,true,main_text_equal,\"元日を御目出（おめで）たい\"$' "$out_dir/shiftjis-meta-query.csv"

cat > "$out_dir/upstream-nested-biblio.xhtml" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body>
<div class="main_text">本文
<div class="bibliographical_information">底本：fixture</div>
<div class="notation_notes">注記説明</div>
</div>
</body></html>
HTML

cat > "$out_dir/local-nested-biblio.xhtml" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body>
<div class="main_text">本文</div>
</body></html>
HTML

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id fixture.xhtml.nested-biblio \
  --upstream-xhtml "$out_dir/upstream-nested-biblio.xhtml" \
  --local-xhtml "$out_dir/local-nested-biblio.xhtml"

"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, main_text_equal, comparison_status, upstream_main_text from fidelity_xhtml_observations where case_id = 'fixture.xhtml.nested-biblio'" \
  | tee "$out_dir/nested-biblio-query.csv"

rg -n '^fixture.xhtml.nested-biblio,true,main_text_equal,\"本文\"$' "$out_dir/nested-biblio-query.csv"

cat > "$out_dir/local-mismatch.xhtml" <<'HTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body>
<div class="main_text">吾輩<ruby><rb>犬</rb><rt>いぬ</rt></ruby>である。<br/></div>
</body></html>
HTML

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id fixture.xhtml.mismatch \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --local-xhtml "$out_dir/local-mismatch.xhtml"

"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, main_text_equal, rendered_body_proxy_eligible, proxy_basis, comparison_status, first_diff_index, upstream_diff_context, local_diff_context from fidelity_xhtml_observations where case_id = 'fixture.xhtml.mismatch'" \
  | tee "$out_dir/mismatch-query.csv"

rg -n '^fixture.xhtml.mismatch,false,false,not_eligible,main_text_mismatch,2,\"吾輩猫ねこである。\",\"吾輩犬いぬである。\"$' "$out_dir/mismatch-query.csv"

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

cat > "$out_dir/local-adapter-error.json" <<'JSON'
{"version":1,"work_id":"stdin","blocks":[],"meta":{"adapter":"aozora2html","adapter_version":"fixture","source_encoding":"windows-31j","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":false,"warnings":[{"message":"aozora2html parser aborted: fixture"}]}}
JSON

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id fixture.xhtml.adapter-error \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --local-xhtml "$out_dir/local-adapter-error.json"

"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, main_text_equal, comparison_status, local_main_text from fidelity_xhtml_observations where case_id = 'fixture.xhtml.adapter-error'" \
  | tee "$out_dir/adapter-error-query.csv"

rg -n '^fixture.xhtml.adapter-error,false,local_adapter_error,$' "$out_dir/adapter-error-query.csv"

cat > "$out_dir/upstream-no-main-text.xhtml" <<'XHTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body><div class="bibliographical_information">本文なし</div></body></html>
XHTML

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id fixture.xhtml.adapter-error-empty-body \
  --upstream-xhtml "$out_dir/upstream-no-main-text.xhtml" \
  --local-xhtml "$out_dir/local-adapter-error.json"

"$duckdb_bin" -csv -header "$db_path" \
  "select case_id, main_text_equal, rendered_body_proxy_eligible, proxy_basis, comparison_status from fidelity_xhtml_observations where case_id = 'fixture.xhtml.adapter-error-empty-body'" \
  | tee "$out_dir/adapter-error-empty-body-query.csv"

rg -n '^fixture.xhtml.adapter-error-empty-body,true,false,not_eligible,local_adapter_error$' "$out_dir/adapter-error-empty-body-query.csv"

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  python - "$repo_root" "$db_path" "$out_dir/upstream.xhtml" "$out_dir/local.xhtml" "$out_dir/local-mismatch.xhtml" <<'PY'
import importlib.util
import sys
from pathlib import Path

import duckdb

repo_root = Path(sys.argv[1])
db_path = Path(sys.argv[2])
upstream = Path(sys.argv[3])
local_equal = Path(sys.argv[4])
local_mismatch = Path(sys.argv[5])

spec = importlib.util.spec_from_file_location(
    "compare_xhtml_sources",
    repo_root / "reports" / "aat-fidelity" / "compare-xhtml-sources.py",
)
module = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(module)

conn = duckdb.connect(str(db_path))
module.create_tables(conn)
module.load_observation_with_conn(
    conn=conn,
    report_id="xhtml-shared-loader",
    case_id="shared.equal",
    upstream_xhtml=upstream,
    local_xhtml=local_equal,
)
module.load_observation_with_conn(
    conn=conn,
    report_id="xhtml-shared-loader",
    case_id="shared.mismatch",
    upstream_xhtml=upstream,
    local_xhtml=local_mismatch,
)
rows = conn.execute(
    "select comparison_status, count(*) from fidelity_xhtml_observations "
    "where report_id = 'xhtml-shared-loader' group by 1 order by 1"
).fetchall()
assert rows == [("main_text_equal", 1), ("main_text_mismatch", 1)], rows
print("shared loader ok")
PY

echo "aat fidelity xhtml source smoke ok: $out_dir"
