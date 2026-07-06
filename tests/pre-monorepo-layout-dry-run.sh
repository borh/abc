#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"

out_dir="$(smoke_tmp_dir ab-pre-monorepo-layout)"
trap 'smoke_cleanup "$out_dir"' EXIT

workspace="$out_dir/workspace"
mkdir -p "$workspace"
ln -s "$repo_root" "$workspace/ab-validator"

abc_root="${AB_ABC_ROOT:-}"
if [[ -z "$abc_root" ]]; then
  candidate="$(dirname "$repo_root")/abc"
  if [[ "$(basename "$(dirname "$repo_root")")" == ".worktrees" ]]; then
    candidate="$(dirname "$(dirname "$(dirname "$repo_root")")")/abc"
  fi
  abc_root="$candidate"
fi

if [[ ! -d "$abc_root" ]]; then
  echo "missing ABC repo at $abc_root; set AB_ABC_ROOT=/path/to/abc" >&2
  exit 2
fi
ln -s "$abc_root" "$workspace/abc"

computed_abc="$(
  cd "$workspace/ab-validator"
  env -u AB_ABC_ROOT AB_WORKSPACE_ROOT="$workspace" just --evaluate abc_repo_root
)"
if [[ "$computed_abc" != "$workspace/abc" ]]; then
  echo "expected just abc_repo_root=$workspace/abc, got $computed_abc" >&2
  exit 1
fi

helper_abc="$(
  env -u AB_ABC_ROOT AB_VALIDATOR_ROOT="$workspace/ab-validator" AB_WORKSPACE_ROOT="$workspace" bash -c \
    'source "$AB_VALIDATOR_ROOT/tests/lib/smoke-env.sh"; smoke_abc_root'
)"
if [[ "$helper_abc" != "$workspace/abc" ]]; then
  echo "expected smoke_abc_root=$workspace/abc, got $helper_abc" >&2
  exit 1
fi

python3 "$workspace/ab-validator/scripts/compare_abc_schema_contracts.py" --abc "$workspace/abc"

echo "pre-monorepo layout dry-run ok: $workspace"
