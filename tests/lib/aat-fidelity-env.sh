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

aat_duckdb_bin() {
  if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
    echo "/etc/profiles/per-user/bor/bin/duckdb"
    return
  fi
  echo "${DUCKDB:-duckdb}"
}

aat_setup_duckdb_runtime() {
  local duckdb_bin="$1"
  if [[ -z "$duckdb_bin" ]]; then
    duckdb_bin="$(aat_duckdb_bin)"
  fi
  local libstdcxx_path
  libstdcxx_path="$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')"
  if [[ -n "$libstdcxx_path" ]]; then
    export LD_LIBRARY_PATH="$(dirname "$libstdcxx_path"):${LD_LIBRARY_PATH:-}"
  fi
}

aat_validate_json_report_rows() {
  local report_json="$1"
  local expected_rows="$2"
  python - "$report_json" "$expected_rows" <<'PY'
import json
import pathlib
import sys

path = pathlib.Path(sys.argv[1])
expected = int(sys.argv[2]) if len(sys.argv) > 2 and sys.argv[2] else None
payload = json.loads(path.read_text())
rows = payload.get("rows")
if not isinstance(rows, list):
    raise SystemExit(f"report does not contain rows array: {path}")
if expected is not None and len(rows) != expected:
    raise SystemExit(f"expected {expected} report rows in {path}, got {len(rows)}")
if not rows:
    raise SystemExit(f"report has no rows: {path}")
PY
}

adapter_bin_path() {
  local manifest="$1"
  local bin="$2"
  local target="$3"
  run_cargo build --manifest-path "$manifest" --target-dir "$target" --bin "$bin"
  printf '%s/%s\n' "$target/debug" "$bin"
}
