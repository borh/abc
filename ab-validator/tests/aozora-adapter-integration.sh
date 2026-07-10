#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
: "${AB_AOZORA_BIN:?AB_AOZORA_BIN must be provided by the Nix check or dev shell}"
test -x "$AB_AOZORA_BIN"

cargo test \
  --manifest-path "$repo_root/adapters/aozora/Cargo.toml" \
  --test integration \
  --offline \
  --locked
