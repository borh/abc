-- title: Run summary
WITH
run AS (
  SELECT * FROM read_parquet('__RUN_DIR__/runs.parquet')
),
counts AS (
  SELECT 'sources' AS table_name, count(*)::UBIGINT AS rows FROM read_parquet('__RUN_DIR__/sources.parquet/*.parquet')
  UNION ALL SELECT 'analyses', count(*)::UBIGINT FROM read_parquet('__RUN_DIR__/analyses.parquet/*.parquet')
  UNION ALL SELECT 'morphemes', count(*)::UBIGINT FROM read_parquet('__RUN_DIR__/morphemes.parquet/*.parquet')
  UNION ALL SELECT 'nway_regions', count(*)::UBIGINT FROM read_parquet('__RUN_DIR__/nway_regions.parquet/*.parquet')
  UNION ALL SELECT 'nway_region_analyzers', count(*)::UBIGINT FROM read_parquet('__RUN_DIR__/nway_region_analyzers.parquet/*.parquet')
  UNION ALL SELECT 'feature_pattern_counts', count(*)::UBIGINT FROM read_parquet('__RUN_DIR__/feature_pattern_counts.parquet/*.parquet')
  UNION ALL SELECT 'errors', count(*)::UBIGINT FROM read_parquet('__RUN_DIR__/errors.parquet/*.parquet')
)
SELECT
  run.run_id,
  run.source_count,
  run.analyzer_count,
  run.error_count,
  counts.table_name,
  counts.rows
FROM run
CROSS JOIN counts
ORDER BY counts.rows DESC
