#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

out_dir="${AB_AAT_FIDELITY_OUT_DIR:-$AB_DB_ROOT/aat-fidelity/cross-adapter}"
summary_md="${AB_AAT_FIDELITY_SUMMARY:-$repo_root/reports/aat-fidelity/cross-adapter-summary.md}"
case_id="${AB_AAT_FIDELITY_CASE_ID:-}"

mkdir -p "$out_dir" "$(dirname "$summary_md")"

oracle_target="$(target_for ab-oracle-cross-adapter)"
aozora2_target="$(target_for aozora2-cross-adapter)"
aozora_rs_target="$(target_for aozora-rs-cross-adapter)"

aozora2_bin="$(adapter_bin_path "$AB_VALIDATOR_ROOT/adapters/aozora2/Cargo.toml" aozora2-adapter "$aozora2_target")"
aozora_rs_bin="$(adapter_bin_path "$AB_VALIDATOR_ROOT/adapters/aozora-rs/Cargo.toml" aozora-rs-adapter "$aozora_rs_target")"
aozora2html_bin="$AB_VALIDATOR_ROOT/adapters/aozora2html/aozora2html-adapter"

case_args=()
if [[ -n "$case_id" ]]; then
  case_args=(--case-id "$case_id")
fi

report_json="$out_dir/report.json"
report_md="$out_dir/report.md"

run_cargo run \
  --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" \
  --target-dir "$oracle_target" \
  -- \
  --oracle "$AB_VALIDATOR_ROOT/data/aat-oracle-cases.toml" \
  --upstream "$AB_VALIDATOR_ROOT/data/aat-upstream-observations.toml" \
  --adapter "aozora2=$aozora2_bin" \
  --adapter "aozora-rs=$aozora_rs_bin" \
  --adapter "aozora2html=$aozora2html_bin" \
  "${case_args[@]}" \
  --report-json "$report_json"

run_cargo run \
  --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" \
  --target-dir "$oracle_target" \
  -- \
  --report-md-from-json "$report_json" \
  > "$report_md"

(
  cd "$AB_VALIDATOR_ROOT"
  python3 reports/aat-fidelity/cross_adapter_summary.py \
    --report "$report_json" \
    --oracle data/aat-oracle-cases.toml
) > "$summary_md"

printf 'report_json=%s\n' "$report_json"
printf 'report_md=%s\n' "$report_md"
printf 'summary_md=%s\n' "$summary_md"
