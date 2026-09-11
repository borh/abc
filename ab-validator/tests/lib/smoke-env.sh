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

# Failure reporting. A smoke script asserts with bare commands under `set -e`
# (`jq -e '...' "$file" >/dev/null`, `rg -q pattern file`), so a false assertion
# aborts with no output at all. Under Nix that surfaces as a check which fails
# with an empty build log and cannot be triaged without editing the script.
smoke_report_failure() {
  local status="$1" source="$2" line="$3" command="$4"
  printf 'smoke failed: %s: line %s: exit %s\n  %s\n' \
    "$source" "$line" "$status" "$command" >&2
}

# Call once after `set -euo pipefail`. `set -E` propagates the trap into
# functions, subshells and command substitutions, which is where several of
# these scripts do their work.
smoke_trace_failures() {
  set -E
  # The trap body expands at the failing site, so BASH_SOURCE and LINENO there
  # name the script and line that failed, not this helper.
  trap 'smoke_report_failure "$?" "${BASH_SOURCE[0]:-$0}" "$LINENO" "$BASH_COMMAND"' ERR
}

# Assert a jq filter over a JSON file. On failure names the filter and prints
# the document, which is the evidence needed to tell a wrong expectation from a
# real regression.
smoke_jq() {
  # Arguments are passed through to `jq -e`; the last one is the JSON file.
  local file="${!#}"
  if jq -e "$@" >/dev/null; then
    return 0
  fi
  # Report against the caller, then exit rather than return: returning would
  # trip the ERR trap a second time and attribute the failure to `return 1`.
  smoke_report_failure 1 "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}" "jq -e $*"
  sed -n '1,200p' "$file" >&2
  exit 1
}

# Assert that a pattern occurs in a file, printing a bounded head of the file
# when it does not.
smoke_grep() {
  local pattern="$1" file="$2"
  if rg -q -- "$pattern" "$file"; then
    return 0
  fi
  smoke_report_failure 1 "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}" "rg -q -- $pattern $file"
  sed -n '1,80p' "$file" >&2
  exit 1
}
