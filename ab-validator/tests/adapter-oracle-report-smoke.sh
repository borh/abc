#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

out_dir="$AB_DB_ROOT/aat-fidelity/smoke"
report_json="$out_dir/report.json"
report_md="$out_dir/report.md"
oracle_target="$(target_for ab-oracle-fidelity-smoke)"

test -s "$report_json"

run_cargo run \
  --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" \
  --target-dir "$oracle_target" \
  -- \
  --report-md-from-json "$report_json" > "$report_md"

rg -n 'schema_status|upstream_status|oracle_status|gaiji.jis.2-13-47' "$report_md"
echo "adapter oracle report smoke ok: $report_md"
