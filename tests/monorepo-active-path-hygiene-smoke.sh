#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

mapfile -t active_files < <(
  find \
    flake.nix justfile config scripts tests \
    abc/flake.nix abc/bin abc/nix abc/src abc/tools \
    ab-validator/flake.nix ab-validator/justfile ab-validator/adapters \
    ab-validator/benchmarks ab-validator/crates ab-validator/reports \
    ab-validator/scripts \
    -type f \( \
      -name '*.clj' -o \
      -name '*.nix' -o \
      -name '*.py' -o \
      -name '*.rs' -o \
      -name '*.sh' -o \
      -name 'Cargo.toml' -o \
      -name 'justfile' -o \
      -name '*.toml' \
    \) \
    -not -path '*/target/*' \
    -not -path '*/__pycache__/*' \
    -not -name 'monorepo-path-hygiene-smoke.sh' \
    -not -name 'monorepo-active-path-hygiene-smoke.sh' \
    -print | sort
)

if [[ "${#active_files[@]}" -eq 0 ]]; then
  echo "no active files found for path hygiene check" >&2
  exit 1
fi

if rg -n --fixed-strings \
  -e "../abc/" \
  -e "../ab-validator/" \
  -e "references/parsers/" \
  -e "abc/references/TEI/P5" \
  "${active_files[@]}"; then
  echo "active code must not depend on sibling checkout or untracked references/ paths" >&2
  exit 1
fi

echo "monorepo active path hygiene smoke ok"
