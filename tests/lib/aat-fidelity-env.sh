#!/usr/bin/env bash
set -euo pipefail

export AB_VALIDATOR_ROOT="${AB_VALIDATOR_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export AB_DB_ROOT="${AB_DB_ROOT:-/db/ab-validator}"
export TMPDIR="${TMPDIR:-$AB_DB_ROOT/tmp}"
export TMP="${TMP:-$TMPDIR}"
export TEMP="${TEMP:-$TMPDIR}"
mkdir -p "$TMPDIR" "$AB_DB_ROOT/aat-fidelity"

target_for() {
  local name="$1"
  printf '%s\n' "$AB_DB_ROOT/target-$name"
}

run_cargo() {
  nix develop "$AB_VALIDATOR_ROOT#" --command cargo "$@"
}

adapter_bin_path() {
  local manifest="$1"
  local bin="$2"
  local target="$3"
  run_cargo build --manifest-path "$manifest" --target-dir "$target" --bin "$bin"
  printf '%s/%s\n' "$target/debug" "$bin"
}
