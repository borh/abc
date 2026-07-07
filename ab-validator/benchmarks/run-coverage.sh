#!/usr/bin/env bash
# Whole-corpus syntax-coverage prevalence runner.
#
# Builds release binaries (ab-index, ab-coverage, all adapters), produces an
# ab-index, then runs the prevalence pipeline against every work in the
# index using the parser-cache at $cache_root.
#
# Outputs land under "$out_dir" (default: scratch/ab-coverage-<ts>/).
# Set AB_COV_MERGE=1 to merge the resulting prevalence numbers back into
# data/aozora-syntax-coverage.toml.
#
# Env knobs:
#   AB_CORPUS         corpus root override (default: flake-pinned corpus)
#   AB_COV_PARSERS    comma-separated parser ids (default: aozora2,aozora-rs,aozora2html)
#   AB_COV_JOBS       rayon thread count (default: nproc)
#   AB_COV_TIMEOUT    per-work adapter timeout in seconds (default: 180)
#   AB_COV_CACHE      cache root (default: $repo/target/parser-cache)
#   AB_COV_OUT        output dir (default: $repo/scratch/ab-coverage-<ts>)
#   AB_COV_WORK_IDS   optional JSON array of work_ids to restrict the run
#   AB_COV_NO_CACHE   set to 1 to disable cache reads/writes
#   AB_COV_MERGE      set to 1 to merge prevalence into the matrix toml

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
corpus="$("$repo_root/scripts/resolve-aozorabunko-corpus.sh")"
parsers="${AB_COV_PARSERS:-aozora2,aozora-rs,aozora2html}"
jobs="${AB_COV_JOBS:-$(nproc)}"
timeout="${AB_COV_TIMEOUT:-180}"
cache_root="${AB_COV_CACHE:-$repo_root/target/parser-cache}"
out_dir="${AB_COV_OUT:-$repo_root/scratch/ab-coverage-$(date -u +%Y%m%dT%H%M%SZ)}"

mkdir -p "$out_dir"
cd "$repo_root"

cargo build --release --workspace
cargo build --release --manifest-path adapters/aozora2/Cargo.toml
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml

target/release/ab-index \
  --corpus "$corpus" \
  --output "$out_dir/index.json" \
  2> "$out_dir/index.stderr"

cmd=(target/release/ab-coverage
  --matrix data/aozora-syntax-coverage.toml
  --index "$out_dir/index.json"
  --corpus "$corpus"
  --parsers "$parsers"
  --cache-root "$cache_root"
  --jobs "$jobs"
  --timeout-secs "$timeout"
  --output "$out_dir/summary.json")

if [[ -n "${AB_COV_WORK_IDS:-}" ]]; then
  cmd+=(--work-ids "$AB_COV_WORK_IDS")
fi
if [[ "${AB_COV_NO_CACHE:-0}" == "1" ]]; then
  cmd+=(--no-cache)
fi
if [[ "${AB_COV_MERGE:-0}" == "1" ]]; then
  cmd+=(--merge)
fi

"${cmd[@]}" 2> "$out_dir/coverage.stderr"

echo "summary: $out_dir/summary.json"
