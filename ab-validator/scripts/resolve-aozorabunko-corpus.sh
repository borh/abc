#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [[ -n "${AB_CORPUS:-}" ]]; then
  printf '%s\n' "$AB_CORPUS"
  exit 0
fi

if [[ -n "${AB_AOZORA_CORPUS:-}" ]]; then
  printf '%s\n' "$AB_AOZORA_CORPUS"
  exit 0
fi

nix --option post-build-hook "" build --no-link --print-out-paths "$repo_root#aozorabunko-corpus"
