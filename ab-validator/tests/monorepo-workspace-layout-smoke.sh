#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"

out_dir="$(smoke_tmp_dir ab-monorepo-layout)"
trap 'smoke_cleanup "$out_dir"' EXIT

actual_workspace="$(dirname "$repo_root")"

computed_workspace="$(
  cd "$repo_root"
  env -u AB_WORKSPACE_ROOT just --evaluate workspace_root
)"
if [[ "$computed_workspace" != "$actual_workspace" ]]; then
  echo "expected just workspace_root=$actual_workspace, got $computed_workspace" >&2
  exit 1
fi

computed_abc_default="$(
  cd "$repo_root"
  env -u AB_ABC_ROOT -u AB_WORKSPACE_ROOT just --evaluate abc_repo_root
)"
if [[ "$computed_abc_default" != "$actual_workspace/abc" ]]; then
  echo "expected default just abc_repo_root=$actual_workspace/abc, got $computed_abc_default" >&2
  exit 1
fi

helper_abc_default="$(
  env -u AB_ABC_ROOT -u AB_WORKSPACE_ROOT AB_VALIDATOR_ROOT="$repo_root" bash -c \
    'source "$AB_VALIDATOR_ROOT/tests/lib/smoke-env.sh"; smoke_abc_root'
)"
if [[ "$helper_abc_default" != "$actual_workspace/abc" ]]; then
  echo "expected default smoke_abc_root=$actual_workspace/abc, got $helper_abc_default" >&2
  exit 1
fi

workspace="$out_dir/workspace"
mkdir -p "$workspace"
ln -s "$repo_root" "$workspace/ab-validator"

abc_root="$actual_workspace/abc"
if [[ ! -d "$abc_root" ]]; then
  echo "missing ABC component at $abc_root" >&2
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

python "$workspace/ab-validator/scripts/compare_abc_schema_contracts.py" --abc "$workspace/abc"

echo "monorepo workspace layout smoke ok: $workspace"
