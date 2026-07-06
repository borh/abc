-- title: Source disagreement density
WITH region_counts AS (
  SELECT
    source_id,
    text_id,
    count(*) AS regions,
    count(*) FILTER (WHERE has_segmentation_disagreement) AS segmentation_regions,
    count(*) FILTER (WHERE has_feature_disagreement) AS feature_regions,
    count(*) FILTER (WHERE has_coverage_mismatch) AS coverage_mismatch_regions
  FROM read_parquet('__RUN_DIR__/nway_regions.parquet/*.parquet')
  WHERE NOT is_nonempty_whitespace
  GROUP BY source_id, text_id
)
SELECT
  source.source_id,
  source.text_id,
  source.source_chars,
  region_counts.regions,
  region_counts.segmentation_regions,
  region_counts.feature_regions,
  region_counts.coverage_mismatch_regions,
  round(region_counts.segmentation_regions * 1000.0 / greatest(source.source_chars, 1), 3) AS segmentation_regions_per_1k_chars,
  round(region_counts.feature_regions * 1000.0 / greatest(source.source_chars, 1), 3) AS feature_regions_per_1k_chars
FROM region_counts
JOIN read_parquet('__RUN_DIR__/sources.parquet/*.parquet') AS source
  USING (source_id, text_id)
ORDER BY segmentation_regions_per_1k_chars DESC, segmentation_regions DESC, source_id
LIMIT __LIMIT__
