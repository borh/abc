-- title: Dialogue punctuation probes
WITH region_patterns AS (
  SELECT
    r.source_id,
    r.text_id,
    r.region_index,
    max(CASE WHEN regexp_matches(array_to_string(a.surfaces, ''), '[「」『』]') THEN 1 ELSE 0 END) AS has_quote,
    string_agg(a.analyzer_id || ':[' || array_to_string(a.surfaces, '|') || ']', ' ; ' ORDER BY a.analyzer_id) AS pattern
  FROM read_parquet('__RUN_DIR__/nway_regions.parquet/*.parquet') AS r
  JOIN read_parquet('__RUN_DIR__/nway_region_analyzers.parquet/*.parquet') AS a
    USING (run_id, source_id, text_id, region_index)
  WHERE r.has_segmentation_disagreement
    AND NOT r.is_nonempty_whitespace
  GROUP BY r.source_id, r.text_id, r.region_index
)
SELECT
  count(*) AS examples,
  count(DISTINCT source_id) AS source_count,
  count(DISTINCT text_id) AS text_count,
  string_agg(DISTINCT source_id, ',' ORDER BY source_id)[:240] AS sample_source_ids,
  pattern
FROM region_patterns
WHERE has_quote = 1
GROUP BY pattern
ORDER BY examples DESC, source_count DESC, pattern
LIMIT __LIMIT__
