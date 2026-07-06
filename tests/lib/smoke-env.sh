#!/usr/bin/env bash

export AB_VALIDATOR_ROOT="${AB_VALIDATOR_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"

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
