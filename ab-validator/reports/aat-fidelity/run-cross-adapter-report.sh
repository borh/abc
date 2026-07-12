#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

out_dir="${AB_AAT_FIDELITY_OUT_DIR:-$AB_DB_ROOT/aat-fidelity/cross-adapter}"
summary_md="${AB_AAT_FIDELITY_SUMMARY:-$repo_root/reports/aat-fidelity/cross-adapter-summary.md}"
db_path="${AB_AAT_FIDELITY_DB:-$out_dir/fidelity.duckdb}"
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

if [[ -n "${AB_ORACLE_BIN:-}" ]]; then
  oracle_cmd=("$AB_ORACLE_BIN")
else
  oracle_cmd=(run_cargo run --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" --target-dir "$oracle_target" --)
fi

"${oracle_cmd[@]}" \
  --oracle "$AB_VALIDATOR_ROOT/data/aat-oracle-cases.toml" \
  --upstream "$AB_VALIDATOR_ROOT/data/aat-upstream-observations.toml" \
  --adapter "aozora2=$aozora2_bin" \
  --adapter "aozora-rs=$aozora_rs_bin" \
  --adapter "aozora2html=$aozora2html_bin" \
  "${case_args[@]}" \
  --report-json "$report_json"

"${oracle_cmd[@]}" \
  --report-md-from-json "$report_json" \
  > "$report_md"

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
libstdcxx_path=""
if command -v "$duckdb_bin" >/dev/null 2>&1; then
  libstdcxx_path="$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')"
fi
loader_env=()
if [[ -n "$libstdcxx_path" ]]; then
  loader_env=(env "LD_LIBRARY_PATH=$(dirname "$libstdcxx_path"):${LD_LIBRARY_PATH:-}")
fi

"${loader_env[@]}" uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  "$AB_VALIDATOR_ROOT/reports/aat-fidelity/load-report-duckdb.py" \
  --report "$report_json" \
  --oracle "$AB_VALIDATOR_ROOT/data/aat-oracle-cases.toml" \
  --db "$db_path" \
  --report-id cross-adapter

summary_args=(
  --report "$report_json"
  --oracle data/aat-oracle-cases.toml
)
xhtml_report_id="${AB_AAT_FIDELITY_XHTML_REPORT_ID:-upstream-xhtml-full}"
if "$duckdb_bin" -readonly -csv -noheader "$db_path" "
  SELECT count(*)
  FROM information_schema.tables
  WHERE table_name = 'fidelity_xhtml_observations'
" | rg -q '^[1-9][0-9]*$'; then
  xhtml_rows="$("$duckdb_bin" -readonly -csv -noheader "$db_path" \
    "SELECT count(*) FROM fidelity_xhtml_observations WHERE report_id = '$xhtml_report_id'")"
  if [[ "$xhtml_rows" =~ ^[1-9][0-9]*$ ]]; then
    summary_args+=(--xhtml-db "$db_path" --xhtml-report-id "$xhtml_report_id")
  fi
fi

(
  cd "$AB_VALIDATOR_ROOT"
  "${loader_env[@]}" uv run --isolated --no-project \
    --with 'duckdb>=1.1' \
    reports/aat-fidelity/cross_adapter_summary.py \
    "${summary_args[@]}"
) > "$summary_md"

printf 'report_json=%s\n' "$report_json"
printf 'report_md=%s\n' "$report_md"
printf 'db_path=%s\n' "$db_path"
printf 'summary_md=%s\n' "$summary_md"
