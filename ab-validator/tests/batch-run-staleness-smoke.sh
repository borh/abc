#!/usr/bin/env bash
# CI gate for the batch-run staleness / skip-recompute mechanism.
#
# Runs the pure Python test suites that guard input-set identity, the
# by-input index + output re-verification, the warehouse gate, and the AAT
# generator identity / freshness check. These prove the invariants the whole
# feature rests on: identity is deterministic and content-sensitive, a fresh
# indexed run is skipped, and any changed/broken input recomputes (fail toward
# correctness). Pure stdlib unittest: no network, no nix, no /db.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

suites=(
  reports/lib/tests
  reports/morph-warehouse/tests
  reports/aat-fidelity/tests
)

fail=0
for suite in "${suites[@]}"; do
  echo "== $suite =="
  if ! python3 -m unittest discover -s "$suite" -p 'test_*.py' -t "$suite"; then
    fail=1
  fi
done

if [[ "$fail" -ne 0 ]]; then
  echo "batch-run-staleness smoke: FAILURES above" >&2
  exit 1
fi
echo "batch-run-staleness smoke: ok"
