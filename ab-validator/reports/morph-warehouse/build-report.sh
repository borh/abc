#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat >&2 <<'USAGE'
usage: reports/morph-warehouse/build-report.sh RUN_DIR OUTPUT_DIR [LIMIT]

Build a reproducible DuckDB SQL report from a morph warehouse run.
Use an OUTPUT_DIR under /db for real corpus runs.
USAGE
}

if [[ $# -lt 2 || $# -gt 3 ]]; then
  usage
  exit 2
fi

run_dir="$1"
out_dir="$2"
limit="${3:-50}"

if [[ ! "$limit" =~ ^[0-9]+$ || "$limit" == "0" ]]; then
  echo "LIMIT must be a positive integer" >&2
  exit 2
fi

if [[ ! -f "$run_dir/runs.parquet" ]]; then
  echo "missing warehouse runs.parquet: $run_dir/runs.parquet" >&2
  exit 2
fi

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
query_dir="$script_dir/queries"
outputs_dir="$out_dir/outputs"
snapshots_dir="$out_dir/queries"
tmp_dir="$out_dir/tmp"

rm -rf "$out_dir"
mkdir -p "$outputs_dir" "$snapshots_dir" "$tmp_dir"

workflow_lib="$script_dir/../../../scripts/workflow-run-lib.sh"
if [[ -f "$workflow_lib" ]]; then
  # shellcheck source=/dev/null
  source "$workflow_lib"
  workflow_init "$out_dir/workflow-run.json" "morph-warehouse.build-report.v1" "local"
  workflow_finished=false
  trap 'status=$?; if [[ $status -ne 0 && "${workflow_finished:-false}" == "false" ]]; then workflow_step_fail "script" "script failed with exit $status"; workflow_finish "failed"; fi' EXIT
else
  workflow_init() { :; }
  workflow_step_pass() { :; }
  workflow_step_fail() { :; }
  workflow_finish() { :; }
fi

sql_quote() {
  printf "%s" "$1" | sed "s/'/''/g"
}

sed_escape() {
  printf "%s" "$1" | sed -e 's/[\/&]/\\&/g'
}

run_dir_sql="$(sql_quote "$run_dir")"
tmp_dir_sql="$(sql_quote "$tmp_dir")"
run_dir_sed="$(sed_escape "$run_dir_sql")"
limit_sed="$(sed_escape "$limit")"

index="$out_dir/index.md"
cat > "$index" <<EOF
# Morph Warehouse SQL Report

- Run directory: \`$run_dir\`
- Row limit: \`$limit\`
- Built at UTC: \`$(date -u +%Y-%m-%dT%H:%M:%SZ)\`

Each section links the exact SQL snapshot and generated TSV output.

| Section | SQL | TSV |
| --- | --- | --- |
EOF

for template in "$query_dir"/*.sql; do
  name="$(basename "$template" .sql)"
  snapshot="$snapshots_dir/$name.sql"
  output="$outputs_dir/$name.tsv"
  statement="$tmp_dir/$name.copy.sql"
  output_sql="$(sql_quote "$output")"

  sed \
    -e "s/__RUN_DIR__/$run_dir_sed/g" \
    -e "s/__LIMIT__/$limit_sed/g" \
    "$template" > "$snapshot"

  title="$(sed -n 's/^-- title: //p' "$template" | head -n 1)"
  if [[ -z "$title" ]]; then
    title="$name"
  fi

  {
    printf "SET temp_directory='%s';\n" "$tmp_dir_sql"
    # Full-corpus runs (e.g. dict-comparison over the whole Aozora set) exceed a
    # small cap on the nway_feature_diffs aggregation; make it tunable. The 8GB
    # default stays CI-safe for triage-sized runs.
    printf "SET memory_limit='%s';\n" "${AB_DUCKDB_MEMORY_LIMIT:-8GB}"
    printf "SET preserve_insertion_order=false;\n"
    printf "SET threads=%s;\n" "${AB_DUCKDB_THREADS:-4}"
    printf "COPY (\n"
    cat "$snapshot"
    printf "\n) TO '%s' (HEADER, DELIMITER '\\t');\n" "$output_sql"
  } > "$statement"

  "$duckdb_bin" < "$statement"
  workflow_step_pass "$name" "outputs/$name.tsv"
  printf '| %s | [`%s`](queries/%s.sql) | [`%s`](outputs/%s.tsv) |\n' \
    "$title" "$name.sql" "$name" "$name.tsv" "$name" >> "$index"
done

rm -rf "$tmp_dir"

cat >> "$index" <<'EOF'

## Notes

This report is generated from triage-safe tables. Lemma, reading, normalization, and conjugation drill-downs require a full-profile warehouse or targeted full reruns.
EOF

workflow_finished=true
workflow_finish "passed"

echo "wrote morph warehouse report to $out_dir"
