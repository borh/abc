#!/usr/bin/env bash
# Guards the feature-unification hazard recorded in
# docs/handoffs/2026-07-10-parser-fork-provenance.md: no crate in the ROOT
# workspace feature graph may activate serde_json/preserve_order, or every
# canonical-JSON producer (ab-aat-to-parser-ir, ab-aozora-aat) silently
# flips to insertion-order output.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export RUSTC_WRAPPER= SCCACHE_DISABLE=1
graph="$(cargo tree --manifest-path "$repo_root/Cargo.toml" --workspace -e features -f '{p} {f}')"
if grep -F "preserve_order" <<<"$graph"; then
  echo "FAIL: serde_json/preserve_order is active in the root workspace" >&2
  exit 1
fi
echo "OK: no preserve_order in the root workspace feature graph"
