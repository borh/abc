#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

manifest=""
metadata=""
out_dir="${AB_AAT_FIDELITY_XHTML_OUT_DIR:-/db/ab-validator/aat-fidelity/upstream-xhtml}"
db_path="${AB_AAT_FIDELITY_DB:-/db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb}"
report_id="${AB_AAT_FIDELITY_REPORT_ID:-cross-adapter}"
aozora_corpus=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --manifest)
      manifest="$2"
      shift 2
      ;;
    --metadata)
      metadata="$2"
      shift 2
      ;;
    --out-dir)
      out_dir="$2"
      shift 2
      ;;
    --db)
      db_path="$2"
      shift 2
      ;;
    --report-id)
      report_id="$2"
      shift 2
      ;;
    *)
      printf 'unknown argument: %s\n' "$1" >&2
      exit 2
      ;;
  esac
done

if [[ -z "$manifest" ]]; then
  cat >&2 <<'EOF'
usage: run-upstream-xhtml-observations.sh \
  --manifest manifest.tsv \
  [--metadata metadata.csv] \
  [--out-dir /db/...] \
  [--db /db/.../fidelity.duckdb] \
  [--report-id cross-adapter]

manifest columns: case_id<TAB>source_txt_or_zip<TAB>upstream_xhtml
Sources must be local files from the Aozora mirror. Zip sources are extracted
to the first .txt member.
EOF
  exit 2
fi

mkdir -p "$out_dir"/{downloads,sources,upstream}

declare -A meta_feature_tags=()
declare -A meta_card_url=()
declare -A meta_source_url=()
declare -A meta_upstream_url=()
declare -A meta_status=()

if [[ -n "$metadata" ]]; then
  while IFS=, read -r meta_case_id feature_tags card_url source_url upstream_url status extra; do
    if [[ -z "${meta_case_id:-}" || "$meta_case_id" == "case_id" ]]; then
      continue
    fi
    if [[ -n "${extra:-}" ]]; then
      printf 'invalid metadata row for case_id=%s\n' "$meta_case_id" >&2
      exit 2
    fi
    meta_feature_tags["$meta_case_id"]="${feature_tags:-}"
    meta_card_url["$meta_case_id"]="${card_url:-}"
    meta_source_url["$meta_case_id"]="${source_url:-}"
    meta_upstream_url["$meta_case_id"]="${upstream_url:-}"
    meta_status["$meta_case_id"]="${status:-}"
  done < "$metadata"
fi

fetch_input() {
  local value="$1"
  local dest="$2"
  value="$(expand_input_path "$value")"
  if [[ "$value" =~ ^https?:// ]]; then
    printf 'remote inputs are not allowed; use local Aozora corpus paths: %s\n' "$value" >&2
    return 2
  fi
  cp "$value" "$dest"
}

reject_remote_input() {
  local value="$1"
  value="$(expand_input_path "$value")"
  if [[ "$value" =~ ^https?:// ]]; then
    printf 'remote inputs are not allowed; use local Aozora corpus paths: %s\n' "$value" >&2
    return 2
  fi
}

resolve_aozora_corpus() {
  if [[ -z "$aozora_corpus" ]]; then
    aozora_corpus="$("$repo_root/scripts/resolve-aozorabunko-corpus.sh")"
  fi
  printf '%s\n' "$aozora_corpus"
}

expand_input_path() {
  local value="$1"
  case "$value" in
    '$AOZORABUNKO_CORPUS/'*)
      printf '%s/%s\n' "$(resolve_aozora_corpus)" "${value#\$AOZORABUNKO_CORPUS/}"
      ;;
    *)
      printf '%s\n' "$value"
      ;;
  esac
}

prepare_source() {
  local input="$1"
  local case_id="$2"
  local fetched="$out_dir/downloads/$case_id.source"
  local source_txt="$out_dir/sources/$case_id.txt"
  fetch_input "$input" "$fetched" || return

  if file "$fetched" | rg -q 'Zip archive'; then
    unzip -p "$fetched" '*.txt' > "$source_txt"
  else
    cp "$fetched" "$source_txt"
  fi
  printf '%s\n' "$source_txt"
}

prepare_upstream() {
  local input="$1"
  local case_id="$2"
  local upstream="$out_dir/upstream/$case_id.xhtml"
  fetch_input "$input" "$upstream" || return
  printf '%s\n' "$upstream"
}

while IFS=$'\t' read -r case_id source_input upstream_input extra; do
  if [[ -z "${case_id:-}" || "$case_id" =~ ^# ]]; then
    continue
  fi
  if [[ "$case_id" == "case_id" ]]; then
    continue
  fi
  if [[ -n "${extra:-}" || -z "${source_input:-}" || -z "${upstream_input:-}" ]]; then
    printf 'invalid manifest row for case_id=%s\n' "$case_id" >&2
    exit 2
  fi
  reject_remote_input "$source_input"
  reject_remote_input "$upstream_input"

  source_txt="$(prepare_source "$source_input" "$case_id")"
  upstream_xhtml="$(prepare_upstream "$upstream_input" "$case_id")"

  "$repo_root/reports/aat-fidelity/compare-aozora-upstream-xhtml.sh" \
    --case-id "$case_id" \
    --source "$source_txt" \
    --upstream-xhtml "$upstream_xhtml" \
    --out-dir "$out_dir" \
    --db "$db_path" \
    --report-id "$report_id" \
    --card-url "${meta_card_url[$case_id]:-}" \
    --source-url "${meta_source_url[$case_id]:-$source_input}" \
    --upstream-url "${meta_upstream_url[$case_id]:-$upstream_input}" \
    --feature-tags "${meta_feature_tags[$case_id]:-}" \
    --manifest-status "${meta_status[$case_id]:-}"
done < "$manifest"

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
"$duckdb_bin" -csv -header "$db_path" \
  "select count(*) as total, sum(raw_equal)::UBIGINT as raw_equal, sum(main_text_equal)::UBIGINT as main_text_equal from fidelity_xhtml_observations where report_id = '$report_id'" \
  > "$out_dir/summary.csv"
"$duckdb_bin" -csv -header "$db_path" \
  "select comparison_status, count(*) as rows from fidelity_xhtml_observations where report_id = '$report_id' group by comparison_status order by comparison_status" \
  > "$out_dir/status-summary.csv"

printf 'db_path=%s\n' "$db_path"
printf 'summary=%s\n' "$out_dir/summary.csv"
printf 'status_summary=%s\n' "$out_dir/status-summary.csv"
