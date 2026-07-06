-- title: Top POS1 patterns
SELECT
  examples,
  source_count,
  text_count,
  sample_source_ids,
  sample_text_ids,
  pattern
FROM read_parquet('__RUN_DIR__/feature_pattern_counts.parquet/*.parquet')
WHERE kind = 'feature'
  AND feature_profile = 'core'
  AND feature_key = 'pos1'
  AND NOT is_nonempty_whitespace
ORDER BY examples DESC, source_count DESC, pattern
LIMIT __LIMIT__
