#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

repo_root="$AB_VALIDATOR_ROOT"
out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/cross-summary-xhtml-smoke"
db_path="$out_dir/fidelity.duckdb"
report_json="$out_dir/report.json"
summary_md="$out_dir/summary.md"
case_report_dir="$out_dir/case-reports"
case_ids=(
  gaiji.jis.2-13-47
  gaiji.unicode.u546d
  ruby.gaiji.inline_base
)

rm -rf "$out_dir"
mkdir -p "$out_dir" "$case_report_dir"

duckdb_bin="$(aat_duckdb_bin)"
aat_setup_duckdb_runtime "$duckdb_bin"

oracle_target="$(target_for ab-oracle-cross-adapter-smoke)"
aozora2html_bin="$AB_VALIDATOR_ROOT/adapters/aozora2html/aozora2html-adapter"
case_count="${#case_ids[@]}"
for case_id in "${case_ids[@]}"; do
  safe_case_id="${case_id//./_}"
  run_cargo run \
    --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" \
    --target-dir "$oracle_target" \
    -- \
    --oracle "$AB_VALIDATOR_ROOT/data/aat-oracle-cases.toml" \
    --upstream "$AB_VALIDATOR_ROOT/data/aat-upstream-observations.toml" \
    --adapter "aozora2html=$aozora2html_bin" \
    --case-id "$case_id" \
    --report-json "$case_report_dir/$safe_case_id.json"
done

python - "$report_json" "$case_count" "$case_report_dir"/*.json <<'PY'
import json
import pathlib
import sys

out = pathlib.Path(sys.argv[1])
expected = int(sys.argv[2])
parts = [pathlib.Path(path) for path in sys.argv[3:]]
rows = []
for part in parts:
    payload = json.loads(part.read_text())
    rows.extend(payload["rows"])
if len(rows) != expected:
    raise SystemExit(f"expected {expected} report rows, got {len(rows)}")
out.write_text(json.dumps({"rows": rows}, indent=2) + "\n")
PY

aat_validate_json_report_rows "$report_json" "$case_count"

cat > "$out_dir/upstream.xhtml" <<'XHTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml"><body><div class="main_text">本文</div></body></html>
XHTML
cp "$out_dir/upstream.xhtml" "$out_dir/local.xhtml"

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id xhtml-smoke \
  --case-id smoke.xhtml.equal \
  --upstream-xhtml "$out_dir/upstream.xhtml" \
  --local-xhtml "$out_dir/local.xhtml"

uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/cross_adapter_summary.py" \
  --report "$report_json" \
  --oracle "$repo_root/data/aat-oracle-cases.toml" \
  --xhtml-db "$db_path" \
  --xhtml-report-id xhtml-smoke \
  > "$summary_md"

rg -n 'XHTML Source Evidence' "$summary_md"
rg -n 'xhtml-smoke' "$summary_md"
rg -n 'rendered-body proxy eligible | 1' "$summary_md"
rg -n -F "| aozora2html | $case_count | $case_count | $case_count | $case_count | 0 |" "$summary_md"
rg -n 'aozora2html.*source-level oracle failures should be interpreted beside rendered-XHTML evidence' "$summary_md"

echo "aat fidelity cross summary xhtml smoke ok: $summary_md"
