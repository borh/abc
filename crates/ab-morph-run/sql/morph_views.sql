-- This file is a template. Warehouse finalization writes per-run views.sql with absolute paths.

CREATE OR REPLACE VIEW warehouse_runs AS
SELECT * FROM read_parquet('__RUN_DIR__/runs.parquet')
WHERE schema_version = 1;

CREATE OR REPLACE VIEW warehouse_nway_regions AS
SELECT * FROM read_parquet('__RUN_DIR__/nway_regions.parquet');

CREATE OR REPLACE VIEW warehouse_nway_region_analyzers AS
SELECT * FROM read_parquet('__RUN_DIR__/nway_region_analyzers.parquet');

CREATE OR REPLACE VIEW warehouse_nway_feature_diffs AS
SELECT * FROM read_parquet('__RUN_DIR__/nway_feature_diffs.parquet');

CREATE OR REPLACE VIEW top_segmentation_patterns AS
WITH region_patterns AS (
  SELECT
    run_id,
    source_id,
    region_index,
    list(struct_pack(analyzer_id := analyzer_id, surfaces := surfaces) ORDER BY analyzer_id) AS pattern
  FROM warehouse_nway_region_analyzers
  GROUP BY run_id, source_id, region_index
)
SELECT
  run_id,
  pattern,
  count(*) AS regions,
  count(DISTINCT source_id) AS sources
FROM region_patterns
GROUP BY run_id, pattern
ORDER BY regions DESC;

CREATE OR REPLACE VIEW top_feature_differences AS
SELECT
  run_id,
  feature_key,
  scope_type,
  coalesce(scope_surface, cast(scope_position AS VARCHAR), '') AS scope,
  feature_value,
  analyzer_id,
  count(*) AS occurrences,
  count(DISTINCT source_id || ':' || cast(region_index AS VARCHAR)) AS regions
FROM warehouse_nway_feature_diffs
GROUP BY run_id, feature_key, scope_type, scope, feature_value, analyzer_id
ORDER BY regions DESC;
