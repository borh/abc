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

# Six uncached builds: phase5-checkpoint, monorepo-adr-governance,
# evidence-gate (which builds two attrs in one invocation),
# parser-rq-instrument-identity, and the two --rebuild reproducibility builds
# in release-parser-reproducible.
if [[ "$stable_builds" -ne 6 ]]; then
  printf 'expected six uncached flake builds, found %s\n' "$stable_builds" >&2
  exit 1
fi

# Four system evaluations: one each for phase5-checkpoint,
# monorepo-adr-governance, evidence-gate, and parser-rq-instrument-identity.
# release-parser-reproducible names its flake attrs directly, so it needs no
# currentSystem eval.
if [[ "$stable_evals" -ne 4 ]]; then
  printf 'expected four uncached system evaluations, found %s\n' "$stable_evals" >&2
  exit 1
fi

# Every subcommand that evaluates a flake can be served a stale answer by the
# eval cache, so all of them must carry the uncached option — not just the
# build/check/eval trio the counts above pin. `nix --option ... <sub>` never
# matches, because the subcommand does not directly follow `nix `.
cached_boundary_re='(^|[;&|`$([:space:]])nix (flake (check|show)|build |eval |run )'

if grep -Eq "$cached_boundary_re" <<<"$dry_run"; then
  printf 'validate-migration contains a cached flake evaluation boundary\n' >&2
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
  if [[ "$gate_script" == "tests/validate-migration-eval-cache-smoke.sh" ]]; then
    continue
  fi
  if grep -Eq "$cached_boundary_re" "$script_path"; then
    printf 'cached flake evaluation boundary in %s\n' "$gate_script" >&2
    grep -nE "$cached_boundary_re" "$script_path" >&2
    exit 1
  fi
done

if rg -n "NIX_CONFIG='eval-cache = false' just validate-migration" \
  "$repo_root/abc/docs/superpowers/plans"; then
  printf 'an active plan bypasses the central validate-migration cache policy\n' >&2
  exit 1
fi

sudachi_block="$(sed -n '/sudachiCli = rustPlatform\.buildRustPackage {/,/^        };/p' \
  "$repo_root/ab-validator/flake.nix")"

if rg -n 'cargoLock\.lockFile = sudachiRustSource' <<<"$sudachi_block"; then
  printf 'Sudachi reads a fetched Cargo.lock during flake evaluation\n' >&2
  exit 1
fi

if ! rg -q 'cargoHash = "sha256-[A-Za-z0-9+/]+=*";' <<<"$sudachi_block"; then
  printf 'Sudachi does not bind its vendored Cargo dependency closure\n' >&2
  exit 1
fi

printf 'validate-migration eval-cache contract ok\n'
