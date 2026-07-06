-- title: Historical kana probes
WITH analyzer_surfaces AS (
  SELECT
    r.source_id,
    r.text_id,
    r.region_index,
    a.analyzer_id,
    array_to_string(a.surfaces, '') AS surface_text,
    r.has_segmentation_disagreement,
    r.has_feature_disagreement
  FROM read_parquet('__RUN_DIR__/nway_regions.parquet/*.parquet') AS r
  JOIN read_parquet('__RUN_DIR__/nway_region_analyzers.parquet/*.parquet') AS a
    USING (run_id, source_id, text_id, region_index)
  WHERE NOT r.is_nonempty_whitespace
),
hits AS (
  SELECT 'つて' AS probe, * FROM analyzer_surfaces WHERE contains(surface_text, 'つて')
  UNION ALL SELECT 'あつ', * FROM analyzer_surfaces WHERE contains(surface_text, 'あつ')
  UNION ALL SELECT 'けふ', * FROM analyzer_surfaces WHERE contains(surface_text, 'けふ')
  UNION ALL SELECT 'ゐ', * FROM analyzer_surfaces WHERE contains(surface_text, 'ゐ')
  UNION ALL SELECT 'ゑ', * FROM analyzer_surfaces WHERE contains(surface_text, 'ゑ')
)
SELECT
  probe,
  analyzer_id,
  count(*) AS analyzer_region_hits,
  count(DISTINCT source_id) AS source_count,
  count(DISTINCT text_id) AS text_count,
  count(*) FILTER (WHERE has_segmentation_disagreement) AS segmentation_region_hits,
  count(*) FILTER (WHERE has_feature_disagreement) AS feature_region_hits,
  string_agg(DISTINCT source_id, ',' ORDER BY source_id)[:240] AS sample_source_ids,
  string_agg(DISTINCT surface_text, ' / ' ORDER BY surface_text)[:240] AS sample_surfaces
FROM hits
GROUP BY probe, analyzer_id
ORDER BY analyzer_region_hits DESC, probe, analyzer_id
LIMIT __LIMIT__
