#!/usr/bin/env bash
# Smoke test for benchmarks/run-morph-corpus.sh: runs it against a tiny
# synthetic AAT corpus and asserts the shape of summary.json + parts.json.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf -- "$tmp"' EXIT
aat_dir="$tmp/aat"
mkdir -p "$aat_dir"
# Two minimal AAT documents matching the shape the morph-run reader expects
# (verified against crates/ab-morph-run/src/lib.rs:1098 TINY_AAT fixture).
cat >"$aat_dir/000001-aaa.json" <<'JSON'
{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"私は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}
JSON
cat >"$aat_dir/000002-bbb.json" <<'JSON'
{"version":1,"work_id":"source-b","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"今日は良い天気だ。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}
JSON

out_dir="$tmp/out"
AB_MORPH_AAT_DIR="$aat_dir" \
AB_MORPH_WAREHOUSE_DIR="$tmp/warehouse" \
AB_MORPH_JOBS="1 1" \
AB_MORPH_ANALYZERS="vibrato" \
AB_BENCH_OUT="$out_dir" \
bash "$repo_root/benchmarks/run-morph-corpus.sh"

# Assert the harness produced both outputs with the required top-level keys.
jq -e '.primary.jobs == 1' "$out_dir/summary.json"
jq -e '.primary.wall_seconds | type == "number"' "$out_dir/summary.json"
jq -e '.parts.total_parquet_parts | type == "number"' "$out_dir/summary.json"
jq -e '(.scaling | length) == 2' "$out_dir/summary.json"
# speedup may be null or a number on the tiny synthetic corpus (sub-second runs).
# Accept either; the shape assertion is the point, not the value.
jq -e '.scaling_speedup_primary_over_single | (. == null) or (type == "number")' "$out_dir/summary.json"
echo "smoke ok"
