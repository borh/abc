#!/usr/bin/env bash
# Feature unification must not change publication JSON maps to insertion order.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
graph="$(cargo tree --manifest-path "$repo_root/Cargo.toml" --workspace -e features -f '{p} {f}')"
if grep -F "preserve_order" <<<"$graph"; then
  echo "FAIL: serde_json/preserve_order is active in the root workspace" >&2
  exit 1
fi
echo "OK: no preserve_order in the root workspace feature graph"
