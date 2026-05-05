#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
notebook="reports/morph-warehouse/warehouse_explorer.py"
tailscale_host="${AB_MORPH_MARIMO_HOST:-$(hostname).hyakutake-barbel.ts.net}"
port="${AB_MORPH_MARIMO_PORT:-27188}"

duckdb_bin="${AB_DUCKDB_BIN:-duckdb}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
if command -v "$duckdb_bin" >/dev/null 2>&1; then
  stdcxx_dir="$(
    ldd "$duckdb_bin" 2>/dev/null \
      | sed -n 's|.*=> \\(.*\\)/libstdc++\\.so\\.6 .*|\\1|p' \
      | head -n 1
  )"
  if [[ -n "${stdcxx_dir:-}" ]]; then
    export LD_LIBRARY_PATH="$stdcxx_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  fi
fi

cd "$repo_root"

cmd=(
  uvx --from "marimo[sql]==0.23.4" marimo
  edit --sandbox "$notebook"
  --host "$tailscale_host"
  --port "$port"
  --allow-origins "https://$tailscale_host"
  --allow-origins "http://$tailscale_host:$port"
)

if [[ "${AB_MORPH_SKIP_MARIMO:-}" == "1" ]]; then
  printf '%q ' "${cmd[@]}"
  printf '\n'
  exit 0
fi

exec "${cmd[@]}"
