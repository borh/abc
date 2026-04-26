#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
corpus="${AB_CORPUS:-$repo_root/references/aozorabunko}"
jobs="${AB_BENCH_JOBS:-$(nproc)}"
timeout="${AB_BENCH_TIMEOUT:-600s}"
aat_diff_limit="${AB_AAT_DIFF_LIMIT:-50}"
out_dir="${AB_BENCH_OUT:-/tmp/ab-validator-compare-$(date -u +%Y%m%dT%H%M%SZ)}"

mkdir -p "$out_dir"
cd "$repo_root"

cargo build --release --workspace
cargo build --release --manifest-path adapters/aozora2/Cargo.toml
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml

target/release/ab-index \
  --corpus "$corpus" \
  --output "$out_dir/index.json" \
  2> "$out_dir/index.stderr"

count_json_files() {
  find "$1" -type f -name '*.json' | wc -l | tr -d ' '
}

count_failures() {
  find "$1" -type f -name '*.json' -print0 |
    xargs -0 jq -r '.results | to_entries[] | select(.value.pass == false) | .key' |
    wc -l |
    tr -d ' '
}

run_adapter() {
  local name="$1"
  local adapter="$2"
  local reports="$out_dir/reports/$name"
  local aats="$out_dir/aats/$name"
  local start end seconds
  start="$(date +%s%N)"
  target/release/ab-check \
    --index "$out_dir/index.json" \
    --corpus "$corpus" \
    --adapter "$adapter" \
    --output "$reports" \
    --aat-output "$aats" \
    --jobs "$jobs" \
    --per-work-timeout "$timeout" \
    2> "$out_dir/$name.stderr"
  end="$(date +%s%N)"
  seconds="$(awk -v start="$start" -v end="$end" 'BEGIN { printf "%.6f", (end - start) / 1000000000 }')"
  jq -n \
    --arg name "$name" \
    --argjson seconds "$seconds" \
    --argjson reports "$(count_json_files "$reports")" \
    --argjson failures "$(count_failures "$reports")" \
    '{name: $name, seconds: $seconds, reports: $reports, failures: $failures}'
}

run_adapter aozora2 "$repo_root/adapters/aozora2/target/release/aozora2-adapter" \
  > "$out_dir/aozora2-summary.json"
run_adapter aozora-rs "$repo_root/adapters/aozora-rs/target/release/aozora-rs-adapter" \
  > "$out_dir/aozora-rs-summary.json"

target/release/ab-compare \
  --reports-a "$out_dir/reports/aozora2/aozora2-adapter" \
  --reports-b "$out_dir/reports/aozora-rs/aozora-rs-adapter" \
  --aats-a "$out_dir/aats/aozora2/aozora2-adapter" \
  --aats-b "$out_dir/aats/aozora-rs/aozora-rs-adapter" \
  --aat-diff-output "$out_dir/aat-structure-comparison.json" \
  --aat-diff-limit "$aat_diff_limit" \
  --metrics-root "$out_dir/aats/aozora-rs/aozora-rs-adapter" \
  --metrics-output "$out_dir/aozora-rs-metrics-summary.json" \
  --output "$out_dir/comparison.json"

jq -n \
  --slurpfile a "$out_dir/aozora2-summary.json" \
  --slurpfile b "$out_dir/aozora-rs-summary.json" \
  --slurpfile c "$out_dir/comparison.json" \
  --slurpfile metrics "$out_dir/aozora-rs-metrics-summary.json" \
  --slurpfile aatdiff "$out_dir/aat-structure-comparison.json" \
  --arg corpus_hash "$(jq -r '.corpus_hash' "$out_dir/index.json")" \
  '{
    generated_at: now | todate,
    corpus_hash: $corpus_hash,
    aozora2: $a[0],
    aozora_rs: $b[0],
    comparison: $c[0],
    aozora_rs_metrics: $metrics[0],
    aat_structure_comparison: $aatdiff[0]
  }' > "$out_dir/summary.json"

cat "$out_dir/summary.json"
echo "summary: $out_dir/summary.json"
