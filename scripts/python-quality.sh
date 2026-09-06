#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  mapfile -t python_files < <(git ls-files '*.py')
else
  mapfile -t python_files < <(
    find . \
      -path './ab-validator/benchmarks/baselines' -prune -o \
      -path './ab-validator/docs/superpowers/reports' -prune -o \
      -path './ab-validator/scratch' -prune -o \
      -path './ab-validator/research/out' -prune -o \
      -path './out' -prune -o \
      -path './scratch' -prune -o \
      -name '*.py' -print \
      | sed 's#^\./##' \
      | sort
  )
fi
if [[ "${#python_files[@]}" -eq 0 ]]; then
  echo "no tracked Python files"
  exit 0
fi

ruff format --check "${python_files[@]}"
ruff check "${python_files[@]}"
MYPY_CACHE_DIR="${MYPY_CACHE_DIR:-$repo_root/.mypy_cache}" mypy --config-file pyproject.toml "${python_files[@]}"
