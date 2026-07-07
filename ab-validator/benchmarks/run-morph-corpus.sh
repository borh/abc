#!/usr/bin/env bash
# Measure the ab-morph-run analyze-aat pipeline over a stable persisted AAT
# corpus. Captures wall time, peak RSS, Parquet part counts, row/row-group
# stats, and a jobs scaling sweep for §3.15.
#
# Does NOT tune anything. Output feeds the §3.5/§3.12/§3.15 decision doc.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workspace_root="$(cd "$repo_root/.." && pwd)"
if [[ -f "$workspace_root/scripts/soranoha-runtime-env.sh" ]]; then
  # shellcheck source=/dev/null
  source "$workspace_root/scripts/soranoha-runtime-env.sh"
fi
aat_dir="${AB_MORPH_AAT_DIR:-${AB_AOZORA2HTML_AAT_DIR:-$repo_root/scratch/state/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}}"
warehouse_dir="${AB_MORPH_WAREHOUSE_DIR:-$repo_root/scratch/state/morph-warehouse-bench}"
jobs_list="${AB_MORPH_JOBS:-1 $(nproc)}"
analyzers="${AB_MORPH_ANALYZERS:-vibrato sudachi-a sudachi-c}"
out_dir="${AB_BENCH_OUT:-/tmp/ab-validator-morph-bench-$(date -u +%Y%m%dT%H%M%SZ)}"
cleanup_tmp=false
if [[ -z "${AB_BENCH_OUT+x}" ]]; then cleanup_tmp=true; fi

if [[ ! -d "$aat_dir" ]]; then
  echo "AAT corpus not found: $aat_dir" >&2
  exit 2
fi

mkdir -p "$out_dir" "$warehouse_dir"
if "$cleanup_tmp"; then trap 'rm -rf -- "$out_dir" "$warehouse_dir/runs/bench-*"' EXIT; fi
cd "$repo_root"

# Resolve GNU time (avoid the bash `time` keyword, which ignores -v).
time_bin="$(type -P time || true)"
if [[ -z "$time_bin" ]]; then
  echo "GNU time not found; peak RSS will be unavailable" >&2
  time_bin="command time"
fi

echo "building release ab-morph-run"
cargo build --release -p ab-morph-run

sudachi_dict="${AB_SUDACHI_DICT:-}"
if [[ -z "$sudachi_dict" ]]; then
  sudachi_dict="$(nix path-info .#sudachi-dictionary-full 2>/dev/null)/share/sudachi/system.dic"
fi
export TMPDIR="${AB_DB_ROOT:-$repo_root/scratch/state}/tmp"
export TMP="$TMPDIR"; export TEMP="$TMPDIR"

# Parse GNU time's "Elapsed (wall clock) time (h:mm:ss or m:ss): 1:23.45" into
# seconds as a decimal. Handles both m:ss.hs and h:mm:ss.hs.
elapsed_to_seconds() {
  local raw="$1"   # e.g. "1:23.45"
  # GNU time never prints hours for <1h runs; split into mm and ss.hs.
  if [[ "$raw" == *:*:* ]]; then
    # h:mm:ss.hs
    awk -F: -v s="$raw" 'BEGIN{split(s,a,":"); print a[1]*3600+a[2]*60+a[3]}'
  else
    awk -F: -v s="$raw" 'BEGIN{split(s,a,":"); print a[1]*60+a[2]}'
  fi
}

run_one() {
  local j="$1"
  local run_id="bench-j${j}"
  rm -rf "$warehouse_dir/runs/$run_id"
  local args=()
  for a in $analyzers; do args+=(--analyzer "$a"); done
  local timing="$out_dir/time-j${j}.txt"
  AB_SUDACHI_DICT="$sudachi_dict" \
    "$time_bin" -v \
    target/release/ab-morph-run analyze-aat \
      --aat-dir "$aat_dir" \
      "${args[@]}" \
      --warehouse-dir "$warehouse_dir" \
      --run-id "$run_id" \
      --warehouse-profile full \
      --jobs "$j" \
    >"$out_dir/j${j}.stdout" 2>"$timing"
  local wall_raw peak_kb
  wall_raw="$(awk -F': ' '/Elapsed \(wall clock\) time/ {print $2; exit}' "$timing" | tr -d ' ')"
  local wall
  wall="$(elapsed_to_seconds "$wall_raw")"
  peak_kb="$(awk -F': ' '/Maximum resident set size/ {print $2; exit}' "$timing" | tr -d ' ')"
  echo "{\"jobs\":$j,\"wall_seconds\":$wall,\"peak_rss_kb\":$peak_kb,\"run_id\":\"$run_id\"}"
}

# §3.5 / §3.12 signals: part counts + per-table row and row-group stats from the
# jobs=$(nproc) run (representative full run).
primary_j="$(echo "$jobs_list" | awk '{print $NF}')"
echo "running primary measurement at jobs=$primary_j"
primary_json="$(run_one "$primary_j")"

warehouse_run_dir="$warehouse_dir/runs/bench-j${primary_j}"
parts_json="$out_dir/parts.json"
{
  echo "{"
  echo "\"total_parquet_parts\":$(find "$warehouse_run_dir" -name '*.parquet' 2>/dev/null | wc -l | tr -d ' '),"
  echo "\"total_parquet_bytes\":$(du -sb "$warehouse_run_dir" 2>/dev/null | awk '{print $1}'),"
  echo "\"tables\":["
  first=1
  # Warehouse layout is MIXED: a table may be a flat `<table>.parquet` file
  # OR a `<table>.parquet/` directory containing `part-NNNNN.parquet` shards.
  # Aggregate per table across either shape.
  shopt -s nullglob
  # Collect distinct table names from both files and directories.
  declare -A seen
  for entry in "$warehouse_run_dir"/*.parquet "$warehouse_run_dir"/*.parquet/; do
    [[ -e "$entry" ]] || continue
    name="$(basename "$entry" .parquet)"
    [[ -n "${seen[$name]:-}" ]] && continue
    seen[$name]=1
    if [[ -d "$warehouse_run_dir/$name.parquet" ]]; then
      glob="$warehouse_run_dir/$name.parquet/*.parquet"
      part_count="$(find "$warehouse_run_dir/$name.parquet" -name '*.parquet' | wc -l | tr -d ' ')"
    else
      glob="$warehouse_run_dir/$name.parquet"
      part_count=1
    fi
    stats="$(duckdb -noheader -list -c "
      SELECT
        COUNT(*) AS row_groups,
        COALESCE(SUM(row_group_num_rows),0) AS num_rows,
        COALESCE(SUM(total_compressed_size),0) AS compressed_bytes
      FROM parquet_metadata('$glob');" 2>/dev/null || echo "0|0|0")"
    row_groups="$(echo "$stats" | cut -d'|' -f1)"
    rows="$(echo "$stats" | cut -d'|' -f2)"
    bytes="$(echo "$stats" | cut -d'|' -f3)"
    median_part_bytes="$(awk -v b="$bytes" -v p="$part_count" 'BEGIN{if(p>0){printf "%d",b/p}else{print 0}}')"
    [[ $first -eq 1 ]] || echo ","
    printf '  {"table":"%s","parts":%s,"row_groups":%s,"compressed_bytes":%s,"num_rows":%s,"median_part_bytes":%s}' \
      "$name" "$part_count" "$row_groups" "$bytes" "$rows" "$median_part_bytes"
    first=0
  done
  shopt -u nullglob
  echo ""
  echo "]"
  echo "}"
} >"$parts_json"

# §3.15 signal: jobs scaling sweep (jobs=1 vs jobs=primary_j).
echo "running jobs scaling sweep over: $jobs_list"
scaling_json="["
first=1
for j in $jobs_list; do
  if [[ "$j" -eq "$primary_j" ]]; then sample="$primary_json"; else sample="$(run_one "$j")"; fi
  [[ $first -eq 1 ]] || scaling_json+=","
  scaling_json+="$sample"
  first=0
done
scaling_json+="]"

speedup="$(jq -r --argjson scaling "$scaling_json" \
  --argjson primary_j "$primary_j" \
  -n '( ($scaling | map(select(.jobs == 1)) | first | .wall_seconds) as $one |
        ( ($scaling | map(select(.jobs == $primary_j)) | first | .wall_seconds) as $peak |
        (if ($one // null) == null or ($peak // 0) == 0 then null else ($one / $peak) end) ) )')"
if [[ -z "$speedup" || "$speedup" == "null" ]]; then
  speedup="null"
fi

jq -n \
  --arg generated_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  --arg aat_dir "$aat_dir" \
  --arg analyzers "$analyzers" \
  --argjson primary_j "$primary_j" \
  --argjson primary "$primary_json" \
  --slurpfile parts "$parts_json" \
  --argjson scaling "$scaling_json" \
  --argjson speedup "$speedup" \
  '{
    generated_at: $generated_at,
    aat_dir: $aat_dir,
    analyzers: $analyzers,
    primary_jobs: $primary_j,
    primary: $primary,
    parts: $parts[0],
    scaling: $scaling,
    scaling_speedup_primary_over_single: $speedup
  }' | tee "$out_dir/summary.json"

echo "summary: $out_dir/summary.json"
echo "parts:   $out_dir/parts.json"
