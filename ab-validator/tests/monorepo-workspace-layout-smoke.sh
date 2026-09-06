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

computed_research_default="$(
  cd "$repo_root"
  env -u AB_RESEARCH_ROOT -u AB_WORKSPACE_ROOT just --evaluate research_repo_root
)"
if [[ "$computed_research_default" != "$repo_root/research" ]]; then
  echo "expected default just research_repo_root=$repo_root/research, got $computed_research_default" >&2
  exit 1
fi

helper_research_default="$(
  env -u AB_RESEARCH_ROOT -u AB_WORKSPACE_ROOT AB_VALIDATOR_ROOT="$repo_root" bash -c \
    'source "$AB_VALIDATOR_ROOT/tests/lib/smoke-env.sh"; smoke_research_root'
)"
if [[ "$helper_research_default" != "$repo_root/research" ]]; then
  echo "expected default smoke_research_root=$repo_root/research, got $helper_research_default" >&2
  exit 1
fi

workspace="$out_dir/workspace"
mkdir -p "$workspace"
ln -s "$repo_root" "$workspace/ab-validator"

research_root="$repo_root/research"
if [[ ! -d "$research_root" ]]; then
  echo "missing research root at $research_root" >&2
  exit 2
fi

computed_research="$(
  cd "$workspace/ab-validator"
  env -u AB_RESEARCH_ROOT AB_WORKSPACE_ROOT="$workspace" just --evaluate research_repo_root
)"
if [[ "$computed_research" != "$workspace/ab-validator/research" ]]; then
  echo "expected just research_repo_root=$workspace/ab-validator/research, got $computed_research" >&2
  exit 1
fi

helper_research="$(
  env -u AB_RESEARCH_ROOT AB_VALIDATOR_ROOT="$workspace/ab-validator" AB_WORKSPACE_ROOT="$workspace" bash -c \
    'source "$AB_VALIDATOR_ROOT/tests/lib/smoke-env.sh"; smoke_research_root'
)"
if [[ "$helper_research" != "$workspace/ab-validator/research" ]]; then
  echo "expected smoke_research_root=$workspace/ab-validator/research, got $helper_research" >&2
  exit 1
fi



echo "monorepo workspace layout smoke ok: $workspace"
