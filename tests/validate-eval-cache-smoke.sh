#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
dry_run="$(cd "$repo_root" && just --dry-run validate 2>&1)"

if ! grep -q 'nix --option eval-cache false ' <<<"$dry_run"; then
  printf 'validate has no uncached flake evaluation boundary\n' >&2
  exit 1
fi

# Every subcommand that evaluates a flake can be served a stale answer by the
# eval cache, so all of them must carry the uncached option, not just the
# build/check/eval trio. `nix --option ... <sub>` never
# matches, because the subcommand does not directly follow `nix `.
cached_boundary_re='(^|[;&|`$([:space:]])nix (flake (check|show)|build |eval |run )'

if grep -Eq "$cached_boundary_re" <<<"$dry_run"; then
  printf 'validate contains a cached flake evaluation boundary\n' >&2
  grep -nE "$cached_boundary_re" <<<"$dry_run" >&2
  exit 1
fi

# The dry-run shows only the recipe lines, so any `nix` call inside a script the
# gate shells out to is invisible above. Scan those scripts too. One level deep:
# a script that invokes a further script is not followed, so keep gate scripts
# flat or extend this loop.
gate_scripts="$(grep -oE '\b(bash|sh) [A-Za-z0-9_./-]+\.sh' <<<"$dry_run" \
  | awk '{print $2}' | sort -u)"

if [[ -z "$gate_scripts" ]]; then
  printf 'found no gate scripts to scan; the dry-run shape changed\n' >&2
  exit 1
fi

for gate_script in $gate_scripts; do
  script_path="$repo_root/$gate_script"
  if [[ ! -f "$script_path" ]]; then
    printf 'gate script %s does not exist\n' "$gate_script" >&2
    exit 1
  fi
  # Skip this file: it necessarily contains the pattern as data.
  if [[ "$gate_script" == "tests/validate-eval-cache-smoke.sh" ]]; then
    continue
  fi
  if grep -Eq "$cached_boundary_re" "$script_path"; then
    printf 'cached flake evaluation boundary in %s\n' "$gate_script" >&2
    grep -nE "$cached_boundary_re" "$script_path" >&2
    exit 1
  fi
done

printf 'validate eval-cache contract ok\n'
