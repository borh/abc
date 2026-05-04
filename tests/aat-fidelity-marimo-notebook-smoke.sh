#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

test -s "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
test -s "$repo_root/reports/aat-fidelity/fixtures/report.json"
test -s "$repo_root/reports/aat-fidelity/open-fidelity-explorer.sh"
rg -n "oracle_status|upstream_status|schema_status|mo\\.json" "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
rg -n "duckdb|AB_AAT_FIDELITY_DB|fidelity_rows|fidelity_syntax_rows" "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
rg -n "fidelity_xhtml_observations|xhtml_observations|main_text_equal" "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
rg -n "LD_LIBRARY_PATH|libstdc" "$repo_root/reports/aat-fidelity/open-fidelity-explorer.sh"
uv run --isolated --no-project --with 'marimo==0.23.4' --with 'duckdb>=1.1' --with 'polars>=1.0' python -m py_compile "$repo_root/reports/aat-fidelity/fidelity_explorer.py"
