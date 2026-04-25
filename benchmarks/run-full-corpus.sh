#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
corpus="${AB_CORPUS:-$repo_root/references/aozorabunko}"
jobs="${AB_BENCH_JOBS:-$(nproc)}"
timeout="${AB_BENCH_TIMEOUT:-30s}"
out_dir="${AB_BENCH_OUT:-/tmp/ab-validator-bench-$(date -u +%Y%m%dT%H%M%SZ)}"

index_path="$out_dir/index.json"
reports_dir="$out_dir/reports"
summary_path="$out_dir/summary.json"
adapter="$repo_root/adapters/aozora2/target/release/aozora2-adapter"

elapsed_seconds() {
  local start_ns="$1"
  local end_ns="$2"
  awk -v start="$start_ns" -v end="$end_ns" 'BEGIN { printf "%.6f", (end - start) / 1000000000 }'
}

json_number() {
  local value="$1"
  awk -v value="$value" 'BEGIN { printf "%.6f", value }'
}

if [[ ! -d "$corpus" ]]; then
  echo "AB corpus not found: $corpus" >&2
  exit 2
fi

mkdir -p "$out_dir" "$reports_dir"
cd "$repo_root"

echo "building release workspace"
cargo build --release --workspace

echo "building release aozora2 adapter"
cargo build --release --manifest-path adapters/aozora2/Cargo.toml

echo "indexing corpus: $corpus"
index_start_ns="$(date +%s%N)"
target/release/ab-index \
  --corpus "$corpus" \
  --output "$index_path" \
  2> "$out_dir/index.stderr"
index_end_ns="$(date +%s%N)"
index_seconds="$(elapsed_seconds "$index_start_ns" "$index_end_ns")"

works_count="$(jq '.works_count' "$index_path")"
corpus_hash="$(jq -r '.corpus_hash' "$index_path")"
index_bytes="$(wc -c < "$index_path" | tr -d ' ')"

echo "validating $works_count indexed works with $jobs jobs"
check_start_ns="$(date +%s%N)"
target/release/ab-check \
  --index "$index_path" \
  --corpus "$corpus" \
  --adapter "$adapter" \
  --output "$reports_dir" \
  --jobs "$jobs" \
  --per-work-timeout "$timeout" \
  2> "$out_dir/check.stderr"
check_end_ns="$(date +%s%N)"
check_seconds="$(elapsed_seconds "$check_start_ns" "$check_end_ns")"

report_count="$(find "$reports_dir" -type f -name '*.json' | wc -l | tr -d ' ')"
report_bytes="$(du -sb "$reports_dir" | awk '{print $1}')"
failure_count="$(
  find "$reports_dir" -type f -name '*.json' -print0 |
    xargs -0 jq -r '.results | to_entries[] | select(.value.pass == false) | .key' |
    wc -l |
    tr -d ' '
)"
index_works_per_second="$(json_number "$(awk -v works="$works_count" -v seconds="$index_seconds" 'BEGIN { print works / seconds }')")"
check_works_per_second="$(json_number "$(awk -v works="$report_count" -v seconds="$check_seconds" 'BEGIN { print works / seconds }')")"

jq -n \
  --arg generated_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  --arg corpus "$corpus" \
  --arg corpus_hash "$corpus_hash" \
  --arg adapter "$adapter" \
  --arg timeout "$timeout" \
  --argjson jobs "$jobs" \
  --argjson works_count "$works_count" \
  --argjson report_count "$report_count" \
  --argjson failure_count "$failure_count" \
  --argjson index_bytes "$index_bytes" \
  --argjson report_bytes "$report_bytes" \
  --argjson index_seconds "$index_seconds" \
  --argjson check_seconds "$check_seconds" \
  --argjson index_works_per_second "$index_works_per_second" \
  --argjson check_works_per_second "$check_works_per_second" \
  '{
    generated_at: $generated_at,
    corpus: $corpus,
    corpus_hash: $corpus_hash,
    adapter: $adapter,
    jobs: $jobs,
    per_work_timeout: $timeout,
    works_count: $works_count,
    report_count: $report_count,
    failure_count: $failure_count,
    index: {
      seconds: $index_seconds,
      works_per_second: $index_works_per_second,
      bytes: $index_bytes
    },
    check: {
      seconds: $check_seconds,
      works_per_second: $check_works_per_second,
      report_bytes: $report_bytes
    }
  }' | tee "$summary_path"

echo "summary: $summary_path"
