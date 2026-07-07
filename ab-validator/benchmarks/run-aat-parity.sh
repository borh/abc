#!/usr/bin/env bash
# Three-way AAT parity check across aozora2, aozora-rs, and aozora2html.
#
# Replaces the prior pandoc-based parity script, which compared
# pandoc-of-text on both sides because no Rust adapter rendered HTML.
# This script compares structured AAT plus parser-emitted semantic
# summaries — the original harness contract.
#
# Usage:
#   run-aat-parity.sh --index PATH --work-ids PATH [--sample N]
#                     [--corpus PATH] [--jobs N] [--out-dir PATH]
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

index=""
work_ids=""
sample=""
corpus="$repo_root/references/aozorabunko"
jobs="${AB_BENCH_JOBS:-$(nproc)}"
timeout="${AB_BENCH_TIMEOUT:-180s}"
out_dir=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --index) index="$2"; shift 2 ;;
    --work-ids) work_ids="$2"; shift 2 ;;
    --sample) sample="$2"; shift 2 ;;
    --corpus) corpus="$2"; shift 2 ;;
    --jobs) jobs="$2"; shift 2 ;;
    --out-dir) out_dir="$2"; shift 2 ;;
    -h|--help)
      sed -n '2,11p' "$0"; exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

if [[ -z "$index" || -z "$work_ids" ]]; then
  echo "error: --index and --work-ids are required" >&2
  exit 2
fi

if [[ -z "$out_dir" ]]; then
  out_dir="/tmp/ab-aat-parity-$(date -u +%Y%m%dT%H%M%SZ)"
fi
mkdir -p "$out_dir"

echo "=== preparing work-id sample ==="
trimmed_ids="$out_dir/work-ids.json"
if [[ -n "$sample" ]]; then
  python -c "
import json, sys
ids = json.load(open('$work_ids'))[:int('$sample')]
json.dump(ids, open('$trimmed_ids','w'))
print(f'using {len(ids)} of {len(json.load(open(\"$work_ids\")))} work ids', file=sys.stderr)
"
else
  cp "$work_ids" "$trimmed_ids"
fi

echo "=== building adapters ==="
cargo build --release -p ab-check -p ab-compare >/dev/null
cargo build --release --manifest-path adapters/aozora2/Cargo.toml >/dev/null
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml >/dev/null

declare -A adapter
adapter[aozora2]="$repo_root/adapters/aozora2/target/release/aozora2-adapter"
adapter[aozora-rs]="$repo_root/adapters/aozora-rs/target/release/aozora-rs-adapter"
adapter[aozora2html]="$repo_root/adapters/aozora2html/aozora2html-adapter"

echo "=== running ab-check for each adapter ==="
for name in aozora2 aozora-rs aozora2html; do
  reports="$out_dir/$name/reports"
  aats="$out_dir/$name/aats"
  mkdir -p "$reports" "$aats"
  echo "  $name → $reports"
  cargo run --release -q -p ab-check -- \
    --index "$index" \
    --work-ids "$trimmed_ids" \
    --corpus "$corpus" \
    --adapter "${adapter[$name]}" \
    --output "$reports" \
    --aat-output "$aats" \
    --jobs "$jobs" \
    --per-work-timeout "$timeout"
done

echo "=== running ab-compare for each pair ==="
declare -A pair_diff
for pair in "aozora2html:aozora-rs" "aozora2html:aozora2" "aozora-rs:aozora2"; do
  a="${pair%%:*}"
  b="${pair##*:}"
  diff_path="$out_dir/diff-${a}-vs-${b}.json"
  cmp_path="$out_dir/cmp-${a}-vs-${b}.json"
  pair_diff[$pair]="$diff_path"
  echo "  $a vs $b"
  cargo run --release -q -p ab-compare -- \
    --reports-a "$out_dir/$a/reports/${a}-adapter" \
    --reports-b "$out_dir/$b/reports/${b}-adapter" \
    --aats-a    "$out_dir/$a/aats/${a}-adapter" \
    --aats-b    "$out_dir/$b/aats/${b}-adapter" \
    --aat-diff-output "$diff_path" \
    --output "$cmp_path"
done

echo "=== aggregating summary.json ==="
summary="$out_dir/summary.json"
python - "$out_dir" "$summary" <<'PY'
import glob, json, os, sys
out_dir, summary_path = sys.argv[1], sys.argv[2]

def count_failures(reports_dir, prop):
    n = 0
    for p in glob.glob(os.path.join(reports_dir, "*.json")):
        d = json.load(open(p))
        v = d.get("results", {}).get(prop)
        if isinstance(v, dict) and v.get("pass") is False:
            n += 1
    return n

adapters = ["aozora2", "aozora-rs", "aozora2html"]
schema_fail = {}
parse_fail = {}
total_works = {}
for a in adapters:
    rd = os.path.join(out_dir, a, "reports", f"{a}-adapter")
    schema_fail[a] = count_failures(rd, "schema_valid")
    parse_fail[a] = count_failures(rd, "parse_completeness")
    total_works[a] = len(glob.glob(os.path.join(rd, "*.json")))

pairs = ["aozora2html-vs-aozora-rs", "aozora2html-vs-aozora2", "aozora-rs-vs-aozora2"]
block_count_match = {}
block_count_mismatch = {}
summary_hash_mismatch = {}
for pair in pairs:
    diff_path = os.path.join(out_dir, f"diff-{pair}.json")
    if not os.path.exists(diff_path):
        continue
    diff = json.load(open(diff_path))
    common = diff.get("common_aat", 0)
    structural_diff = diff.get("structural_difference_count", 0)
    block_count_mismatch[pair] = structural_diff
    block_count_match[pair] = max(common - structural_diff, 0)
    summary_hash_mismatch[pair] = diff.get(
        "semantic_summary_hash_difference_counts", {}
    )

agg = {
    "total_works": total_works,
    "schema_valid_failures": schema_fail,
    "parse_complete_failures": parse_fail,
    "aat_block_count_match": block_count_match,
    "aat_block_count_mismatch": block_count_mismatch,
    "semantic_summary_hash_mismatch": summary_hash_mismatch,
}
json.dump(agg, open(summary_path, "w"), indent=2, ensure_ascii=False)
print(f"wrote {summary_path}")
PY

echo
echo "=== summary ==="
cat "$summary"
echo
echo "out_dir: $out_dir"
