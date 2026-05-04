#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

manifest=""
out_dir="${AB_AAT_FIDELITY_XHTML_OUT_DIR:-/db/ab-validator/aat-fidelity/upstream-xhtml}"
db_path="${AB_AAT_FIDELITY_DB:-/db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb}"
report_id="${AB_AAT_FIDELITY_REPORT_ID:-cross-adapter}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --manifest)
      manifest="$2"
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
  [--out-dir /db/...] \
  [--db /db/.../fidelity.duckdb] \
  [--report-id cross-adapter]

manifest columns: case_id<TAB>source_txt_or_zip<TAB>upstream_xhtml
Sources may be local files or http(s) URLs. Zip sources are extracted to the
first .txt member.
EOF
  exit 2
fi

mkdir -p "$out_dir"/{downloads,sources,upstream}

fetch_input() {
  local value="$1"
  local dest="$2"
  if [[ "$value" =~ ^https?:// ]]; then
    curl -fsSL "$value" -o "$dest"
  else
    cp "$value" "$dest"
  fi
}

prepare_source() {
  local input="$1"
  local case_id="$2"
  local fetched="$out_dir/downloads/$case_id.source"
  local source_txt="$out_dir/sources/$case_id.txt"
  fetch_input "$input" "$fetched"

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
  fetch_input "$input" "$upstream"
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

  source_txt="$(prepare_source "$source_input" "$case_id")"
  upstream_xhtml="$(prepare_upstream "$upstream_input" "$case_id")"

  "$repo_root/reports/aat-fidelity/compare-aozora-upstream-xhtml.sh" \
    --case-id "$case_id" \
    --source "$source_txt" \
    --upstream-xhtml "$upstream_xhtml" \
    --out-dir "$out_dir" \
    --db "$db_path" \
    --report-id "$report_id"
done < "$manifest"

duckdb_bin="${DUCKDB:-duckdb}"
if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
  duckdb_bin=/etc/profiles/per-user/bor/bin/duckdb
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
