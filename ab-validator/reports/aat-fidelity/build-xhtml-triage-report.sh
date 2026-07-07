#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
workspace_root="$(cd "$repo_root/.." && pwd)"
if [[ -f "$workspace_root/scripts/soranoha-runtime-env.sh" ]]; then
  # shellcheck source=/dev/null
  source "$workspace_root/scripts/soranoha-runtime-env.sh"
fi

db_path="${AB_AAT_FIDELITY_DB:-${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/cross-adapter/fidelity.duckdb}"
report_id="${AB_AAT_FIDELITY_REPORT_ID:-upstream-xhtml-full}"
out_dir="${AB_AAT_FIDELITY_XHTML_TRIAGE_OUT_DIR:-${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/upstream-xhtml-full/triage-report}"
limit=50

while [[ $# -gt 0 ]]; do
  case "$1" in
    --db)
      db_path="$2"
      shift 2
      ;;
    --report-id)
      report_id="$2"
      shift 2
      ;;
    --out-dir)
      out_dir="$2"
      shift 2
      ;;
    --limit)
      limit="$2"
      shift 2
      ;;
    *)
      printf 'unknown argument: %s\n' "$1" >&2
      exit 2
      ;;
  esac
done

if [[ ! "$limit" =~ ^[0-9]+$ || "$limit" == "0" ]]; then
  echo "--limit must be a positive integer" >&2
  exit 2
fi
if [[ ! -s "$db_path" ]]; then
  echo "missing DuckDB database: $db_path" >&2
  exit 2
fi

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi

mkdir -p "$out_dir"

sql_quote() {
  printf "%s" "$1" | sed "s/'/''/g"
}

report_id_sql="$(sql_quote "$report_id")"

run_csv() {
  local output="$1"
  local sql="$2"
  "$duckdb_bin" -readonly -csv -header "$db_path" "$sql" > "$out_dir/$output"
}

run_csv "status-summary.csv" "
  SELECT comparison_status, count(*) AS row_count
  FROM fidelity_xhtml_observations
  WHERE report_id = '$report_id_sql'
  GROUP BY comparison_status
  ORDER BY comparison_status
"

run_csv "proxy-summary.csv" "
  SELECT proxy_basis, count(*) AS row_count
  FROM fidelity_xhtml_observations
  WHERE report_id = '$report_id_sql'
  GROUP BY proxy_basis
  ORDER BY proxy_basis
"

run_csv "feature-status-summary.csv" "
  WITH expanded AS (
    SELECT
      comparison_status,
      unnest(
        CASE
          WHEN feature_tags IS NULL OR feature_tags = '' THEN ['unclassified']
          ELSE string_split(feature_tags, ';')
        END
      ) AS feature_tag
    FROM fidelity_xhtml_observations
    WHERE report_id = '$report_id_sql'
  )
  SELECT feature_tag, comparison_status, count(*) AS row_count
  FROM expanded
  GROUP BY feature_tag, comparison_status
  ORDER BY feature_tag, comparison_status
"

run_csv "mismatch-shapes.csv" "
  WITH mismatches AS (
    SELECT
      CASE
        WHEN length(local_main_text) > length(upstream_main_text) THEN 'local_longer'
        WHEN length(local_main_text) < length(upstream_main_text) THEN 'upstream_longer'
        ELSE 'same_length'
      END AS length_direction,
      CASE
        WHEN abs(length(local_main_text) - length(upstream_main_text)) = 0 THEN '0'
        WHEN abs(length(local_main_text) - length(upstream_main_text)) <= 10 THEN '1-10'
        WHEN abs(length(local_main_text) - length(upstream_main_text)) <= 100 THEN '11-100'
        WHEN abs(length(local_main_text) - length(upstream_main_text)) <= 1000 THEN '101-1000'
        ELSE '1001+'
      END AS length_delta_bucket,
      CASE
        WHEN regexp_matches(upstream_diff_context || local_diff_context, '［＃') THEN 'aozora_note_marker'
        WHEN regexp_matches(upstream_diff_context || local_diff_context, '※') THEN 'gaiji_marker'
        WHEN regexp_matches(upstream_diff_context || local_diff_context, '改ページ|改行') THEN 'break_marker'
        WHEN regexp_matches(upstream_diff_context || local_diff_context, '字下げ|地付き|字詰め|ぶら下げ') THEN 'layout_marker'
        WHEN regexp_matches(upstream_diff_context || local_diff_context, 'キャプション|\\.png|\\.jpg|\\.jpeg|\\.gif') THEN 'media_marker'
        ELSE 'text_content'
      END AS first_diff_family
    FROM fidelity_xhtml_observations
    WHERE report_id = '$report_id_sql'
      AND comparison_status = 'main_text_mismatch'
  )
  SELECT length_direction, length_delta_bucket, first_diff_family, count(*) AS row_count
  FROM mismatches
  GROUP BY length_direction, length_delta_bucket, first_diff_family
  ORDER BY row_count DESC, length_direction, length_delta_bucket, first_diff_family
"

run_csv "top-mismatches.csv" "
  SELECT
    case_id,
    length(upstream_main_text) AS upstream_len,
    length(local_main_text) AS local_len,
    length(local_main_text) - length(upstream_main_text) AS length_delta,
    first_diff_index,
    upstream_diff_context,
    local_diff_context,
    feature_tags,
    card_url
  FROM fidelity_xhtml_observations
  WHERE report_id = '$report_id_sql'
    AND comparison_status = 'main_text_mismatch'
  ORDER BY abs(length(local_main_text) - length(upstream_main_text)) DESC, case_id
  LIMIT $limit
"

run_csv "adapter-errors.csv" "
  SELECT
    case_id,
    comparison_status,
    length(upstream_main_text) AS upstream_len,
    local_xhtml_path,
    source_url,
    card_url,
    feature_tags
  FROM fidelity_xhtml_observations
  WHERE report_id = '$report_id_sql'
    AND comparison_status IN ('local_adapter_error', 'upstream_adapter_error', 'local_parse_error', 'upstream_parse_error')
  ORDER BY comparison_status, upstream_len DESC, case_id
  LIMIT $limit
"

run_csv "upstream-missing-main-text.csv" "
  SELECT
    case_id,
    upstream_xhtml_path,
    local_xhtml_path,
    source_url,
    card_url,
    feature_tags
  FROM fidelity_xhtml_observations
  WHERE report_id = '$report_id_sql'
    AND comparison_status = 'upstream_missing_main_text'
  ORDER BY case_id
  LIMIT $limit
"

index="$out_dir/index.md"
{
  printf '# XHTML Fidelity Triage Report\n\n'
  printf -- '- DuckDB: `%s`\n' "$db_path"
  printf -- '- Report id: `%s`\n' "$report_id"
  printf -- '- Row limit: `%s`\n\n' "$limit"
  printf '## Status Summary\n\n```csv\n'
  cat "$out_dir/status-summary.csv"
  printf '```\n\n'
  printf '## Outputs\n\n'
  printf '| Output | Purpose |\n'
  printf '| --- | --- |\n'
  printf '| [`status-summary.csv`](status-summary.csv) | Counts by XHTML comparison status. |\n'
  printf '| [`proxy-summary.csv`](proxy-summary.csv) | Raw/main-text proxy eligibility counts. |\n'
  printf '| [`feature-status-summary.csv`](feature-status-summary.csv) | Status counts after expanding semicolon-separated feature tags. |\n'
  printf '| [`mismatch-shapes.csv`](mismatch-shapes.csv) | Main-text mismatches grouped by length delta and first-diff family. |\n'
  printf '| [`top-mismatches.csv`](top-mismatches.csv) | Largest main-text mismatches with diff context. |\n'
  printf '| [`adapter-errors.csv`](adapter-errors.csv) | Adapter or parser error rows. |\n'
  printf '| [`upstream-missing-main-text.csv`](upstream-missing-main-text.csv) | Rows where upstream XHTML lacks a `main_text` div. |\n'
} > "$index"

printf 'xhtml_triage_report=%s\n' "$index"
