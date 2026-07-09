#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
corpus="$("$repo_root/scripts/resolve-aozorabunko-corpus.sh")"
jobs="${AB_BENCH_JOBS:-$(nproc)}"
timeout="${AB_BENCH_TIMEOUT:-600s}"
aat_diff_limit="${AB_AAT_DIFF_LIMIT:-50}"
out_dir="${AB_BENCH_OUT:-/tmp/ab-validator-compare-$(date -u +%Y%m%dT%H%M%SZ)}"
cleanup_tmp=false
if [[ -z "${AB_BENCH_OUT+x}" ]]; then
  cleanup_tmp=true
fi

mkdir -p "$out_dir"
if "$cleanup_tmp"; then
  trap 'rm -rf -- "$out_dir"' EXIT
fi
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

# Reproducible run descriptor for one adapter's AAT dump. Mirrors run-aat-full.sh's
# metadata.json (schema_version 2, SOURCE_DATE_EPOCH-honouring timestamp, repo_head,
# repo_dirty boolean, corpus, adapter_id, adapter_version) so the aozora2/aozora-rs
# dumps this benchmark produces are self-describing like the other adapters'. It also
# folds in `content_hash` (via reports/lib/aat_hash.hash_aat_dir over the dump) so the
# descriptor self-certifies its own bytes. Placed at $out_dir/aats/<name>/metadata.json:
# its parent dir contains the <name>-adapter AAT tree, matching the run-set's coherence
# rule that run_descriptor sits at the dump root above aat_dir. Additive only — this does
# not touch the comparison logic below.
write_run_descriptor() {
  local name="$1"
  local adapter="$2"
  python3 - "$repo_root" "$corpus" "$out_dir" "$name" "$adapter" "$jobs" "$timeout" <<'PY'
import json
import os
import pathlib
import subprocess
import sys
from datetime import datetime, timezone

repo_root, corpus, out_dir, name, adapter, jobs, timeout = sys.argv[1:]
repo = pathlib.Path(repo_root)
out = pathlib.Path(out_dir)
sys.path.insert(0, str(repo / "reports" / "lib"))
from aat_hash import hash_aat_dir  # noqa: E402


def run(args):
    return subprocess.check_output(args, cwd=repo, text=True).strip()


_sde = os.environ.get("SOURCE_DATE_EPOCH")
generated_at = (
    datetime.fromtimestamp(int(_sde), timezone.utc).isoformat()
    if _sde
    else datetime.now(timezone.utc).isoformat()
)

aat_dir = out / "aats" / name / f"{name}-adapter"
reports_dir = out / "reports" / name / f"{name}-adapter"

metadata = {
    "schema_version": 2,
    "generated_at_utc": generated_at,
    "repo_head": run(["git", "rev-parse", "HEAD"]),
    "repo_dirty": bool(run(["git", "status", "--short"])),
    "corpus": str(pathlib.Path(corpus).resolve()),
    "report_id": f"parser-comparison-{name}",
    "jobs": int(jobs),
    "timeout": timeout,
    "adapter_id": name,
    "adapter": adapter,
    "adapter_version": run([adapter, "--version"]),
    "index_path": str(out / "index.json"),
    "reports_dir": str(reports_dir),
    "aat_dir": str(aat_dir),
    "content_hash": hash_aat_dir(aat_dir),
}
(out / "aats" / name / "metadata.json").write_text(
    json.dumps(metadata, indent=2, ensure_ascii=False) + "\n"
)
PY
}

run_adapter aozora2 "$repo_root/adapters/aozora2/target/release/aozora2-adapter" \
  > "$out_dir/aozora2-summary.json"
run_adapter aozora-rs "$repo_root/adapters/aozora-rs/target/release/aozora-rs-adapter" \
  > "$out_dir/aozora-rs-summary.json"

# Self-describing descriptors next to each AAT dump (aozora2 + aozora-rs), matching the
# metadata.json the other adapters get from run-aat-full.sh. See write_run_descriptor.
write_run_descriptor aozora2 "$repo_root/adapters/aozora2/target/release/aozora2-adapter"
write_run_descriptor aozora-rs "$repo_root/adapters/aozora-rs/target/release/aozora-rs-adapter"

target/release/ab-compare \
  --reports-a "$out_dir/reports/aozora2/aozora2-adapter" \
  --reports-b "$out_dir/reports/aozora-rs/aozora-rs-adapter" \
  --aats-a "$out_dir/aats/aozora2/aozora2-adapter" \
  --aats-b "$out_dir/aats/aozora-rs/aozora-rs-adapter" \
  --aat-diff-output "$out_dir/aat-structure-comparison.json" \
  --aat-diff-limit "$aat_diff_limit" \
  --metrics-root "$out_dir/aats/aozora-rs/aozora-rs-adapter" \
  --metrics-output "$out_dir/aozora-rs-metrics-summary.json" \
  --index "$out_dir/index.json" \
  --triage-output "$out_dir/triage.json" \
  --output "$out_dir/comparison.json"

jq -n \
  --slurpfile a "$out_dir/aozora2-summary.json" \
  --slurpfile b "$out_dir/aozora-rs-summary.json" \
  --slurpfile c "$out_dir/comparison.json" \
  --slurpfile metrics "$out_dir/aozora-rs-metrics-summary.json" \
  --slurpfile aatdiff "$out_dir/aat-structure-comparison.json" \
  --slurpfile triage "$out_dir/triage.json" \
  --arg corpus_hash "$(jq -r '.corpus_hash' "$out_dir/index.json")" \
  '{
    generated_at: now | todate,
    corpus_hash: $corpus_hash,
    aozora2: $a[0],
    aozora_rs: $b[0],
    comparison: $c[0],
    aozora_rs_metrics: $metrics[0],
    aat_structure_comparison: $aatdiff[0],
    triage: $triage[0]
  }' > "$out_dir/summary.json"

cat "$out_dir/summary.json"
echo "summary: $out_dir/summary.json"
