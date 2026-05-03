-- title: Pairwise segmentation ranking
WITH pairs AS (
  SELECT
    a.analyzer_id AS analyzer_a,
    b.analyzer_id AS analyzer_b,
    r.source_id,
    r.text_id,
    r.region_index,
    a.surfaces AS surfaces_a,
    b.surfaces AS surfaces_b
  FROM read_parquet('__RUN_DIR__/nway_regions.parquet/*.parquet') AS r
  JOIN read_parquet('__RUN_DIR__/nway_region_analyzers.parquet/*.parquet') AS a
    USING (run_id, source_id, text_id, region_index)
  JOIN read_parquet('__RUN_DIR__/nway_region_analyzers.parquet/*.parquet') AS b
    USING (run_id, source_id, text_id, region_index)
  WHERE r.has_segmentation_disagreement
    AND NOT r.is_nonempty_whitespace
    AND a.analyzer_id < b.analyzer_id
    AND a.surfaces <> b.surfaces
)
SELECT
  analyzer_a,
  analyzer_b,
  count(*) AS regions,
  count(DISTINCT source_id) AS source_count,
  count(DISTINCT text_id) AS text_count,
  string_agg(DISTINCT source_id, ',' ORDER BY source_id)[:240] AS sample_source_ids
FROM pairs
GROUP BY analyzer_a, analyzer_b
ORDER BY regions DESC, analyzer_a, analyzer_b
