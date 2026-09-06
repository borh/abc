#!/usr/bin/env bash

export AB_VALIDATOR_ROOT="${AB_VALIDATOR_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
workspace_root="$(cd "$AB_VALIDATOR_ROOT/.." && pwd)"
if [[ -f "$workspace_root/scripts/soranoha-runtime-env.sh" ]]; then
  # shellcheck source=/dev/null
  source "$workspace_root/scripts/soranoha-runtime-env.sh"
fi

smoke_workspace_root() {
  if [[ -n "${AB_WORKSPACE_ROOT:-}" ]]; then
    printf '%s\n' "$AB_WORKSPACE_ROOT"
    return
  fi

  local repo_parent
  repo_parent="$(dirname "$AB_VALIDATOR_ROOT")"
  if [[ "$(basename "$repo_parent")" == ".worktrees" ]]; then
    dirname "$(dirname "$(dirname "$AB_VALIDATOR_ROOT")")"
    return
  fi

  printf '%s\n' "$repo_parent"
}

smoke_research_root() {
  if [[ -n "${AB_RESEARCH_ROOT:-}" ]]; then
    printf '%s\n' "$AB_RESEARCH_ROOT"
    return
  fi

  printf '%s/research\n' "$AB_VALIDATOR_ROOT"
}

smoke_tmp_dir() {
  local name="$1"
  mktemp -d "${TMPDIR:-/tmp}/${name}.XXXXXX"
}

smoke_cleanup() {
  local path="$1"
  if [[ "${AB_KEEP_SMOKE_TMP:-0}" != "1" ]]; then
    rm -rf "$path"
  fi
}
