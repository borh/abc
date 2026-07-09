#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [[ -n "${AB_AAT_TRIAGE_PYTHON:-}" ]]; then
  triage_py=("$AB_AAT_TRIAGE_PYTHON")
else
  triage_py=(uv run --isolated --no-project --with 'duckdb>=1.1')
fi

out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/duckdb-smoke"
db_path="$out_dir/fidelity.duckdb"
report_path="$repo_root/reports/aat-fidelity/fixtures/report.json"

rm -rf "$out_dir"
mkdir -p "$out_dir"

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
libstdcxx_dir="$(dirname "$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')")"
export LD_LIBRARY_PATH="$libstdcxx_dir:${LD_LIBRARY_PATH:-}"

"${triage_py[@]}" \
  "$repo_root/reports/aat-fidelity/load-report-duckdb.py" \
  --report "$report_path" \
  --oracle "$repo_root/data/aat-oracle-cases.toml" \
  --db "$db_path" \
  --report-id fixture

test -s "$db_path"

tmp_py="$(mktemp --suffix=.py)"
trap 'rm -f "$tmp_py"' EXIT

cat > "$tmp_py" <<'PY'
import sys
import duckdb

db_path = sys.argv[1]
conn = duckdb.connect(db_path, read_only=True)

assert conn.sql("select count(*) from fidelity_rows").fetchone()[0] == 1
assert conn.sql("select count(*) from fidelity_failures").fetchone()[0] == 0
assert conn.sql("select count(*) from fidelity_syntax_rows").fetchone()[0] >= 1

row = conn.sql(
    "select adapter, case_id, schema_status, upstream_status, oracle_status "
    "from fidelity_rows"
).fetchone()
assert row == ("aozora2", "gaiji.jis.2-13-47", "pass", "faithful", "pass")

print("aat fidelity duckdb smoke ok")
PY

"${triage_py[@]}" "$tmp_py" "$db_path"
