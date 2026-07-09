#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

out_dir="$AB_DB_ROOT/aat-fidelity/aozora-rs-oracle-smoke"
mkdir -p "$out_dir"

oracle_target="$(target_for aozora-rs-oracle-smoke)"
adapter_target="$(target_for aozora-rs-oracle-smoke-adapter)"
adapter_bin="$(adapter_bin_path "$AB_VALIDATOR_ROOT/adapters/aozora-rs/Cargo.toml" aozora-rs-adapter "$adapter_target")"

if [[ -n "${AB_ORACLE_BIN:-}" ]]; then
  oracle_cmd=("$AB_ORACLE_BIN")
else
  oracle_cmd=(run_cargo run --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" --target-dir "$oracle_target" --)
fi

"${oracle_cmd[@]}" \
  --oracle "$AB_VALIDATOR_ROOT/data/aat-oracle-cases.toml" \
  --upstream "$AB_VALIDATOR_ROOT/data/aat-upstream-observations.toml" \
  --adapter "aozora-rs=$adapter_bin" \
  --report-json "$out_dir/report.json"

schema_failures="$(
  jq '[.rows[] | select(.schema_status != "pass")] | length' "$out_dir/report.json"
)"
if [[ "$schema_failures" != "0" ]]; then
  jq -r '.rows[] | select(.schema_status != "pass") | [.case_id, .schema_status] | @tsv' \
    "$out_dir/report.json" >&2
  printf 'aozora-rs schema failures: %s\n' "$schema_failures" >&2
  exit 1
fi

upstream_failures="$(
  jq '[.rows[] | select(.upstream_status != "faithful")] | length' "$out_dir/report.json"
)"
if [[ "$upstream_failures" != "0" ]]; then
  jq -r '.rows[] | select(.upstream_status != "faithful") | [.case_id, .upstream_status] | @tsv' \
    "$out_dir/report.json" >&2
  printf 'aozora-rs upstream observation failures: %s\n' "$upstream_failures" >&2
  exit 1
fi

failures="$(
  jq '[.rows[] | select(.oracle_status != "pass")] | length' "$out_dir/report.json"
)"
if [[ "$failures" != "0" ]]; then
  jq -r '.rows[] | select(.oracle_status != "pass") | [.case_id, (.failures | join(" || "))] | @tsv' \
    "$out_dir/report.json" >&2
  printf 'aozora-rs oracle failures: %s\n' "$failures" >&2
  exit 1
fi

printf 'aozora-rs oracle smoke ok: %s\n' "$out_dir/report.json"
