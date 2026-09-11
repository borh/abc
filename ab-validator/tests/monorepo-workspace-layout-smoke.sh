#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"

actual_workspace="$(dirname "$repo_root")"

computed_workspace="$(
  cd "$repo_root"
  env -u AB_WORKSPACE_ROOT just --evaluate workspace_root
)"
if [[ "$computed_workspace" != "$actual_workspace" ]]; then
  echo "expected just workspace_root=$actual_workspace, got $computed_workspace" >&2
  exit 1
fi

echo "monorepo workspace layout smoke ok: $actual_workspace"
