#!/usr/bin/env bash
# Single entry point for the fidelity-coverage reports (§4.7 / §4.8 / §4.9).
#
# The AAT run-set (run-sets/current.json) is the SOLE dump-selection authority:
# resolution ignores ambient `AB_*_AAT_DIR` entirely (aat_runs, F1/F2 of the
# fidelity-run idempotency ADR), so no env-neutralization is needed here. This
# script validates the run-set, checks the resolved dirs exist, then runs the
# three coverage scripts against a denominator summary.
#
# Usage:
#   run-coverage-report.sh <fidelity-summary.json> [out_dir]
# Env:
#   AB_AAT_RUN_SET   optional run-set path (else the checked-in current.json)
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ab_root="$(cd "$here/../.." && pwd)"
summary="${1:?usage: run-coverage-report.sh <fidelity-summary.json> [out_dir]}"
# Default output goes to gitignored scratch/, never into the tracked reports/ tree.
out_dir="${2:-$ab_root/scratch/coverage-report}"
mkdir -p "$out_dir"

# 1. validate run-set coherence (adapter_id + flake.lock rev/narHash).
echo "== validate run-set ==" >&2
python3 "$here/validate-aat-run-set.py" --repo-root "$ab_root" | tee "$out_dir/run-set-validation.json" >&2

# 2. verify the resolved AAT dirs actually exist (descriptors may not yet; dirs must).
echo "== resolved AAT dirs ==" >&2
python3 - <<PY
import sys
sys.path.insert(0, "$ab_root/reports/lib")
from aat_runs import load_run_set, adapter_aat_dirs
dirs = adapter_aat_dirs(load_run_set())
missing = []
for label, d in dirs.items():
    import glob
    n = len(glob.glob(d + "/*.json"))
    print(f"  {label:14} {n:>6} files  {d}", file=sys.stderr)
    if n == 0:
        missing.append((label, d))
if missing:
    print("ERROR: run-set resolves to empty AAT dir(s): " + ", ".join(f"{l} -> {d}" for l, d in missing), file=sys.stderr)
    sys.exit(3)
PY

# 3. run the three coverage scripts against the run-set + summary.
echo "== normalized-corpus-coverage (§4.7) ==" >&2
python3 "$here/normalized-corpus-coverage.py" "$summary" > "$out_dir/normalized-corpus-coverage.json"
echo "== parity-support-audit (§4.9) ==" >&2
python3 "$here/parity-support-audit.py" "$summary" > "$out_dir/parity-support-audit.json" 2> "$out_dir/parity.matrix.txt"
echo "== fidelity-robustness-split (§4.8) ==" >&2
python3 "$here/fidelity-robustness-split.py" "$out_dir/normalized-corpus-coverage.json" > "$out_dir/fidelity-robustness-split.json"

echo "wrote coverage reports to $out_dir" >&2
