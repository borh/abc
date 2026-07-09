#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

out_dir="$AB_DB_ROOT/aat-fidelity/audit-smoke"
mkdir -p "$out_dir"

oracle_target="$(target_for ab-oracle-audit-smoke)"

if [[ -n "${AB_ORACLE_BIN:-}" ]]; then
  oracle_cmd=("$AB_ORACLE_BIN")
else
  oracle_cmd=(run_cargo run --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" --target-dir "$oracle_target" --)
fi

"${oracle_cmd[@]}" \
  --oracle "$AB_VALIDATOR_ROOT/data/aat-oracle-cases.toml" \
  --syntax-coverage "$AB_VALIDATOR_ROOT/data/aozora-syntax-coverage.toml" \
  --audit-json "$out_dir/audit.json"

"${oracle_cmd[@]}" \
  --audit-md-from-json "$out_dir/audit.json" > "$out_dir/audit.md"

rg -n '"total_cases"|"review_status_counts"|"syntax_rows_without_reviewed_oracle_coverage"' "$out_dir/audit.json"
rg -n 'Oracle Evidence Audit|Review Status|Evidence Strength|Syntax Rows Without Reviewed Oracle Coverage' "$out_dir/audit.md"
echo "aat oracle audit smoke ok: $out_dir"
