#!/usr/bin/env bash
# Single entry point for the fidelity-coverage reports (§4.7 / §4.8 / §4.9).
#
# Phase 2 shape: RESOLVE the run-set manifest into a lock (the one impure step —
# validates coherence + fails closed on empty dirs, ignoring ambient AB_*_AAT_DIR),
# then run the three COMPUTE scripts against that lock as their sole dump-selection
# input. Compute reads only the lock (+ the summary). See
# docs/superpowers/specs/2026-07-09-fidelity-phase2-resolve-compute-lock.md.
#
# Usage:
#   run-coverage-report.sh <fidelity-summary.json> [out_dir]
# Env:
#   AB_AAT_RUN_SET   optional run-set manifest path (else the checked-in current.json)
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ab_root="$(cd "$here/../.." && pwd)"
summary="${1:?usage: run-coverage-report.sh <fidelity-summary.json> [out_dir]}"
# Default output goes to gitignored scratch/, never into the tracked reports/ tree.
out_dir="${2:-$ab_root/scratch/coverage-report}"
mkdir -p "$out_dir"
lock="$out_dir/fidelity.lock.json"

# 1. resolve: manifest -> lock. Validates + fails closed; emits the reproducibility record.
echo "== resolve run-set -> lock ==" >&2
python3 "$here/resolve-run-set.py" --repo-root "$ab_root" --out "$lock"

# 2. compute: run the three coverage scripts against the lock + summary.
echo "== normalized-corpus-coverage (§4.7) ==" >&2
python3 "$here/normalized-corpus-coverage.py" "$summary" --lock "$lock" > "$out_dir/normalized-corpus-coverage.json"
echo "== parity-support-audit (§4.9) ==" >&2
python3 "$here/parity-support-audit.py" "$summary" --lock "$lock" > "$out_dir/parity-support-audit.json" 2> "$out_dir/parity.matrix.txt"
echo "== fidelity-robustness-split (§4.8) ==" >&2
python3 "$here/fidelity-robustness-split.py" "$out_dir/normalized-corpus-coverage.json" --lock "$lock" > "$out_dir/fidelity-robustness-split.json"

echo "wrote coverage reports to $out_dir (lock: $lock)" >&2
