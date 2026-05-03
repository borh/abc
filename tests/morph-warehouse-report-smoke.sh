#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
duckdb_bin="${DUCKDB:-duckdb}"
if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
  duckdb_bin=/etc/profiles/per-user/bor/bin/duckdb
fi

root="${AB_REPORT_TEST_ROOT:-/db/ab-validator/tmp/morph-warehouse-report-test}"
run_dir="$root/run"
out_dir="$root/report"

rm -rf "$root"
mkdir -p "$run_dir"/{sources.parquet,analyses.parquet,morphemes.parquet,nway_regions.parquet,nway_region_analyzers.parquet,feature_pattern_counts.parquet,errors.parquet}

"$duckdb_bin" <<SQL
COPY (
  SELECT 1::UINTEGER AS schema_version,
         'fixture-run' AS run_id,
         '2026-05-03T00:00:00Z' AS created_at_utc,
         'aat-dir' AS input_mode,
         '$root/input' AS input_path,
         2::UBIGINT AS source_count,
         3::UBIGINT AS analyzer_count,
         0::UBIGINT AS error_count
) TO '$run_dir/runs.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (
  SELECT 'fixture-run' AS run_id, 'vibrato:unidic-cwj-202512' AS analyzer_id, 'vibrato' AS analyzer_arg, 'vibrato' AS analyzer_family
  UNION ALL SELECT 'fixture-run', 'sudachi-a', 'sudachi-a', 'sudachi'
  UNION ALL SELECT 'fixture-run', 'sudachi-c', 'sudachi-c', 'sudachi'
) TO '$run_dir/run_analyzers.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (
  SELECT 'fixture-run' AS run_id, 'source-a' AS source_id, 'text-a' AS text_id, '$root/source-a.json' AS aat_path, 12::UBIGINT AS source_bytes, 4::UBIGINT AS source_chars
  UNION ALL SELECT 'fixture-run', 'source-b', 'text-b', '$root/source-b.json', 18::UBIGINT, 6::UBIGINT
) TO '$run_dir/sources.parquet/part-00000.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (
  SELECT 'fixture-run' AS run_id, source_id, text_id, analyzer_id, 2::UBIGINT AS morpheme_count
  FROM (VALUES
    ('source-a', 'text-a', 'vibrato:unidic-cwj-202512'),
    ('source-a', 'text-a', 'sudachi-a'),
    ('source-a', 'text-a', 'sudachi-c'),
    ('source-b', 'text-b', 'vibrato:unidic-cwj-202512'),
    ('source-b', 'text-b', 'sudachi-a'),
    ('source-b', 'text-b', 'sudachi-c')
  ) AS rows(source_id, text_id, analyzer_id)
) TO '$run_dir/analyses.parquet/part-00000.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (
  SELECT 'fixture-run' AS run_id, source_id, text_id, analyzer_id,
         row_number() OVER ()::UBIGINT - 1 AS morpheme_index,
         0::UBIGINT AS byte_start, length(surface)::UBIGINT AS byte_end,
         0::UBIGINT AS char_start, length(surface)::UBIGINT AS char_end,
         surface
  FROM (VALUES
    ('source-a', 'text-a', 'vibrato:unidic-cwj-202512', 'あつた'),
    ('source-a', 'text-a', 'sudachi-a', 'あつ'),
    ('source-a', 'text-a', 'sudachi-c', 'あつ'),
    ('source-b', 'text-b', 'vibrato:unidic-cwj-202512', '「けふ」'),
    ('source-b', 'text-b', 'sudachi-a', '「'),
    ('source-b', 'text-b', 'sudachi-c', '「けふ」')
  ) AS rows(source_id, text_id, analyzer_id, surface)
) TO '$run_dir/morphemes.parquet/part-00000.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (
  SELECT 'fixture-run' AS run_id, 'source-a' AS source_id, 'text-a' AS text_id, 0::UBIGINT AS region_index,
         0::UBIGINT AS byte_start, 9::UBIGINT AS byte_end, 0::UBIGINT AS char_start, 3::UBIGINT AS char_end,
         false AS is_nonempty_whitespace, false AS is_agreement, false AS has_coverage_mismatch,
         true AS has_segmentation_disagreement, true AS has_feature_disagreement
  UNION ALL SELECT 'fixture-run', 'source-b', 'text-b', 0::UBIGINT, 0::UBIGINT, 12::UBIGINT, 0::UBIGINT, 4::UBIGINT,
         false, false, false, true, false
) TO '$run_dir/nway_regions.parquet/part-00000.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (
  SELECT 'fixture-run' AS run_id, source_id, text_id, 0::UBIGINT AS region_index, analyzer_id,
         true AS covers_exactly, 0::UBIGINT AS morpheme_start, 1::UBIGINT AS morpheme_end, surfaces
  FROM (VALUES
    ('source-a', 'text-a', 'vibrato:unidic-cwj-202512', ['あつた']),
    ('source-a', 'text-a', 'sudachi-a', ['あつ', 'た']),
    ('source-a', 'text-a', 'sudachi-c', ['あつ', 'た']),
    ('source-b', 'text-b', 'vibrato:unidic-cwj-202512', ['「', 'けふ', '」']),
    ('source-b', 'text-b', 'sudachi-a', ['「けふ', '」']),
    ('source-b', 'text-b', 'sudachi-c', ['「', 'けふ」'])
  ) AS rows(source_id, text_id, analyzer_id, surfaces)
) TO '$run_dir/nway_region_analyzers.parquet/part-00000.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (
  SELECT 'feature' AS kind, 'core' AS feature_profile, 'pos1' AS feature_key, false AS is_nonempty_whitespace,
         'pos1 whole_region 動詞=>vibrato:unidic-cwj-202512 ; 名詞=>sudachi-a+sudachi-c' AS pattern,
         7::UBIGINT AS examples, 1::UBIGINT AS source_count, 1::UBIGINT AS text_count,
         'source-a' AS sample_source_ids, 'text-a' AS sample_text_ids, '' AS script_categories
) TO '$run_dir/feature_pattern_counts.parquet/part-00000.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (
  SELECT 'fixture-run' AS run_id, '' AS source_id, '' AS text_id, '' AS analyzer_id,
         '' AS stage, '' AS error_code, '' AS message
  WHERE false
) TO '$run_dir/errors.parquet/part-00000.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);
SQL

"$repo_root/reports/morph-warehouse/build-report.sh" "$run_dir" "$out_dir" 5

test -s "$out_dir/index.md"
test -s "$out_dir/outputs/001-run-summary.tsv"
test -s "$out_dir/outputs/010-source-disagreement-density.tsv"
test -s "$out_dir/outputs/030-top-segmentation-patterns.tsv"
test -s "$out_dir/outputs/040-top-pos1-patterns.tsv"
test -s "$out_dir/outputs/070-largest-disagreement-regions.tsv"
test ! -e "$out_dir/tmp"

! grep -R "__RUN_DIR__\|__LIMIT__" "$out_dir/queries"
grep -q "candidate_regions AS" "$repo_root/reports/morph-warehouse/queries/070-largest-disagreement-regions.sql"
grep -q "source-a" "$out_dir/outputs/010-source-disagreement-density.tsv"
grep -q "あつた" "$out_dir/outputs/030-top-segmentation-patterns.tsv"
grep -q "pos1 whole_region" "$out_dir/outputs/040-top-pos1-patterns.tsv"
