#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
dry_run="$(cd "$repo_root" && just --dry-run validate-migration 2>&1)"

stable_checks="$(grep -c 'nix --option eval-cache false flake check --no-build' <<<"$dry_run" || true)"
stable_builds="$(grep -c 'nix --option eval-cache false build ' <<<"$dry_run" || true)"
stable_evals="$(grep -c 'nix --option eval-cache false eval ' <<<"$dry_run" || true)"

if [[ "$stable_checks" -ne 3 ]]; then
  printf 'expected three uncached flake checks, found %s\n' "$stable_checks" >&2
  exit 1
fi

if [[ "$stable_builds" -ne 2 ]]; then
  printf 'expected two uncached flake builds, found %s\n' "$stable_builds" >&2
  exit 1
fi

if [[ "$stable_evals" -ne 2 ]]; then
  printf 'expected two uncached system evaluations, found %s\n' "$stable_evals" >&2
  exit 1
fi

if grep -Eq '(^|[;&([:space:]])nix (flake check|build |eval )' <<<"$dry_run"; then
  printf 'validate-migration contains a cached flake evaluation boundary\n' >&2
  exit 1
fi

if rg -n "NIX_CONFIG='eval-cache = false' just validate-migration" \
  "$repo_root/abc/docs/superpowers/plans"; then
  printf 'an active plan bypasses the central validate-migration cache policy\n' >&2
  exit 1
fi

printf 'validate-migration eval-cache contract ok\n'
