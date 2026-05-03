#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/cross-adapter-smoke"
summary_md="$out_dir/summary.md"

rm -rf "$out_dir"

AB_AAT_FIDELITY_CASE_ID=gaiji.jis.2-13-47 \
AB_AAT_FIDELITY_OUT_DIR="$out_dir" \
AB_AAT_FIDELITY_SUMMARY="$summary_md" \
  "$repo_root/reports/aat-fidelity/run-cross-adapter-report.sh"

test -s "$out_dir/report.json"
test -s "$out_dir/report.md"
test -s "$summary_md"

rg -n '"adapter": "aozora2"|"adapter": "aozora-rs"|"adapter": "aozora2html"' "$out_dir/report.json"
rg -n '"schema_status"|"upstream_status"|"oracle_status"' "$out_dir/report.json"
rg -n 'Cross-Adapter AAT Oracle Summary|aozora2|aozora-rs|aozora2html' "$summary_md"

echo "aat fidelity cross-adapter script smoke ok: $out_dir"
