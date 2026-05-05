#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
notebook="reports/aat-fidelity/fidelity_explorer.py"
tailscale_host="${AB_AAT_FIDELITY_MARIMO_HOST:-$(hostname).hyakutake-barbel.ts.net}"
port="${AB_AAT_FIDELITY_MARIMO_PORT:-27189}"

cd "$repo_root"

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
if command -v "$duckdb_bin" >/dev/null 2>&1; then
  libstdcxx_path="$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')"
  if [[ -n "$libstdcxx_path" ]]; then
    export LD_LIBRARY_PATH="$(dirname "$libstdcxx_path"):${LD_LIBRARY_PATH:-}"
  fi
fi

cmd=(
  uvx --from "marimo==0.23.4" marimo
  edit --sandbox "$notebook"
  --host "$tailscale_host"
  --port "$port"
  --allow-origins "https://$tailscale_host"
  --allow-origins "http://$tailscale_host:$port"
)

if [[ "${AB_AAT_FIDELITY_SKIP_MARIMO:-}" == "1" ]]; then
  printf '%q ' "${cmd[@]}"
  printf '\n'
  exit 0
fi

exec "${cmd[@]}"
