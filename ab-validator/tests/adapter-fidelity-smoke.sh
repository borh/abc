#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

out_dir="$AB_DB_ROOT/aat-fidelity/smoke"
mkdir -p "$out_dir"

aozora2_target="$(target_for aozora2-fidelity-smoke)"
oracle_target="$(target_for ab-oracle-fidelity-smoke)"
aozora2_bin="$(adapter_bin_path "$AB_VALIDATOR_ROOT/adapters/aozora2/Cargo.toml" aozora2-adapter "$aozora2_target")"

if [[ -n "${AB_ORACLE_BIN:-}" ]]; then
  oracle_cmd=("$AB_ORACLE_BIN")
else
  oracle_cmd=(run_cargo run --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" --target-dir "$oracle_target" --)
fi

"${oracle_cmd[@]}" \
  --oracle "$AB_VALIDATOR_ROOT/data/aat-oracle-cases.toml" \
  --upstream "$AB_VALIDATOR_ROOT/data/aat-upstream-observations.toml" \
  --adapter "aozora2=$aozora2_bin" \
  --case-id gaiji.jis.2-13-47 \
  --report-json "$out_dir/report.json"

rg -n '"schema_status"|"upstream_status"|"oracle_status"' "$out_dir/report.json"
echo "adapter fidelity smoke ok: $out_dir/report.json"
