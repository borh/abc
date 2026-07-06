#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
notebook="$repo_root/reports/morph-warehouse/warehouse_explorer.py"
launcher="$repo_root/reports/morph-warehouse/open-warehouse-explorer.sh"

test -s "$notebook"
test -x "$launcher"
python -m py_compile "$notebook"
env AB_MORPH_SKIP_MARIMO=1 "$launcher" \
  | grep -Fq 'uvx --from marimo\[sql\]==0.23.4 marimo edit --sandbox reports/morph-warehouse/warehouse_explorer.py --host farspark.hyakutake-barbel.ts.net --port 27188'

grep -q '^# /// script' "$notebook"
grep -q 'marimo\[sql\]==0.23.4' "$notebook"
grep -q 'jedi<0.20' "$notebook"
grep -q 'duckdb' "$notebook"
grep -q 'polars' "$notebook"
grep -q 'AB_MORPH_WAREHOUSE_RUN_DIR' "$notebook"
grep -q '/db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12' "$notebook"
grep -q 'read_parquet' "$notebook"
grep -q 'feature_pattern_counts.parquet' "$notebook"
grep -q 'nway_region_analyzers.parquet' "$notebook"
grep -q 'mo.ui.dropdown' "$notebook"
grep -q 'mo.ui.text' "$notebook"
grep -q 'Source/Text lookup' "$notebook"
grep -q 'AAT structure' "$notebook"
grep -q 'controls_for_view' "$notebook"
grep -q 'def _(controls_for_view, view):' "$notebook"
grep -q 'controls_for_view(view.value)' "$notebook"
grep -q 'source_text_filter_clause' "$notebook"
grep -q 'source_ids' "$notebook"
grep -q 'text_ids' "$notebook"
grep -q 'json.load' "$notebook"
grep -q 'mo.json' "$notebook"
! grep -q '```json' "$notebook"
grep -q 'reports/morph-warehouse/open-warehouse-explorer.sh' "$repo_root/reports/morph-warehouse/README.md"
grep -q 'farspark.hyakutake-barbel.ts.net:27188' "$repo_root/reports/morph-warehouse/README.md"
grep -q 'exact `source_id` / `text_id` lists' "$repo_root/reports/morph-warehouse/README.md"
