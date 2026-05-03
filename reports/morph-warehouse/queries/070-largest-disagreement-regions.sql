-- title: Largest disagreement regions
WITH candidate_regions AS (
  SELECT
    r.source_id,
    r.text_id,
    r.region_index,
    r.char_start,
    r.char_end,
    r.byte_start,
    r.byte_end,
    r.has_segmentation_disagreement,
    r.has_feature_disagreement,
    r.has_coverage_mismatch
  FROM read_parquet('__RUN_DIR__/nway_regions.parquet/*.parquet') AS r
  WHERE NOT r.is_nonempty_whitespace
    AND NOT r.is_agreement
  ORDER BY r.char_end - r.char_start DESC, r.source_id, r.region_index
  LIMIT __LIMIT__
),
region_patterns AS (
  SELECT
    r.source_id,
    r.text_id,
    r.region_index,
    r.char_start,
    r.char_end,
    r.byte_start,
    r.byte_end,
    r.has_segmentation_disagreement,
    r.has_feature_disagreement,
    r.has_coverage_mismatch,
    string_agg(a.analyzer_id || ':[' || array_to_string(a.surfaces, '|') || ']', ' ; ' ORDER BY a.analyzer_id) AS pattern
  FROM candidate_regions AS r
  JOIN read_parquet('__RUN_DIR__/nway_region_analyzers.parquet/*.parquet') AS a
    USING (source_id, text_id, region_index)
  GROUP BY
    r.source_id,
    r.text_id,
    r.region_index,
    r.char_start,
    r.char_end,
    r.byte_start,
    r.byte_end,
    r.has_segmentation_disagreement,
    r.has_feature_disagreement,
    r.has_coverage_mismatch
)
SELECT
  source_id,
  text_id,
  region_index,
  char_start,
  char_end,
  char_end - char_start AS char_len,
  byte_start,
  byte_end,
  has_segmentation_disagreement,
  has_feature_disagreement,
  has_coverage_mismatch,
  pattern
FROM region_patterns
ORDER BY char_len DESC, source_id, region_index
LIMIT __LIMIT__
