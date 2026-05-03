#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

test -s "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
test -s "$repo_root/reports/aat-fidelity/fixtures/report.json"
rg -n "oracle_status|upstream_status|schema_status|mo\\.json" "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
uv run --isolated --no-project --with 'marimo==0.23.4' --with 'polars>=1.0' python -m py_compile "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
