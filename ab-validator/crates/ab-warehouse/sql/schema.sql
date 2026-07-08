-- Morph warehouse schema version 3.
-- Readers must reject runs.schema_version values greater than the reader's supported maximum (3).
-- source_id is one AAT source record/file within a run.
-- text_id is the logical work id; many source_id values may share one text_id.
-- analyzer_family is closed in v1: vibrato | vaporetto | sudachi.
-- analyses preserves successful zero-morpheme analyzer runs; non-zero morpheme counts are derivable.
-- nway_feature_diffs scope invariant:
--   whole_region   => scope_position IS NULL AND scope_surface IS NULL
--   token_position => scope_position IS NOT NULL AND scope_surface IS NULL
--   surface        => scope_position IS NULL AND scope_surface IS NOT NULL

-- This file documents the Parquet fact schema. It is not used to create the Parquet files.

CREATE TABLE runs (
  schema_version UINTEGER,
  run_id VARCHAR,
  created_at_utc VARCHAR,
  input_mode VARCHAR,
  input_path VARCHAR,
  source_count UBIGINT,
  analyzer_count UBIGINT,
  error_count UBIGINT
);

CREATE TABLE run_analyzers (
  run_id VARCHAR,
  analyzer_id VARCHAR,
  analyzer_arg VARCHAR,
  analyzer_family VARCHAR
);

CREATE TABLE sources (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  aat_path VARCHAR,
  source_bytes UBIGINT,
  source_chars UBIGINT
);

-- projection_spans (schema v2 sidecar): projected-plaintext char offsets → AAT inline nodes.
-- One row per node contributing ≥1 projected char; spans are disjoint and jointly cover the text.
-- is_note is structurally FALSE in v1 emission (note nodes are excluded from projection);
-- the column is forward infrastructure per the interestingness-ranking design.
CREATE TABLE projection_spans (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  projected_char_start UBIGINT,
  projected_char_end UBIGINT,
  aat_pointer VARCHAR,
  inline_kind VARCHAR,
  is_ruby_base BOOLEAN,
  is_gaiji BOOLEAN,
  is_note BOOLEAN
);

CREATE TABLE analyses (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  analyzer_id VARCHAR,
  morpheme_count UBIGINT
);

CREATE TABLE morphemes (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  analyzer_id VARCHAR,
  morpheme_index UBIGINT,
  byte_start UBIGINT,
  byte_end UBIGINT,
  char_start UBIGINT,
  char_end UBIGINT,
  surface VARCHAR
);

CREATE TABLE morpheme_features (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  analyzer_id VARCHAR,
  morpheme_index UBIGINT,
  feature_key VARCHAR,
  feature_value VARCHAR
);

CREATE TABLE nway_regions (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  region_index UBIGINT,
  byte_start UBIGINT,
  byte_end UBIGINT,
  char_start UBIGINT,
  char_end UBIGINT,
  is_nonempty_whitespace BOOLEAN,
  is_agreement BOOLEAN,
  has_coverage_mismatch BOOLEAN,
  has_segmentation_disagreement BOOLEAN,
  has_feature_disagreement BOOLEAN
);

CREATE TABLE nway_region_analyzers (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  region_index UBIGINT,
  analyzer_id VARCHAR,
  covers_exactly BOOLEAN,
  morpheme_start UBIGINT,
  morpheme_end UBIGINT,
  surfaces VARCHAR[]
);

-- nway_region_oracle_evidence (schema v2 sidecar): ruby-oracle adjudication per
-- ruby base. One row per ruby base where ≥1 analyzer reading disagrees with the
-- editor ruby. classification is one of resolved / nonstandard_ruby /
-- no_comparable_reading (see ab-morph-run oracle::ruby::adjudicate).
-- winning_analyzer is set only on a unique match; losing_analyzers
-- lists the disagreeing analyzers; evidence_detail is per-analyzer JSON.
-- adjudicated_reading is the resolved authoritative reading (normalized editor
-- ruby) on resolved bases, NULL otherwise; it never changes tokenization/spans.
CREATE TABLE nway_region_oracle_evidence (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  region_index UBIGINT,
  projected_char_start UBIGINT,
  projected_char_end UBIGINT,
  oracle_source VARCHAR,
  classification VARCHAR,
  winning_analyzer VARCHAR,
  losing_analyzers VARCHAR[],
  evidence_detail VARCHAR,
  adjudicated_reading VARCHAR
);

CREATE TABLE nway_feature_diffs (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  region_index UBIGINT,
  feature_key VARCHAR,
  scope_type VARCHAR,
  scope_position UBIGINT,
  scope_surface VARCHAR,
  feature_value VARCHAR,
  analyzers VARCHAR[]
);

CREATE TABLE feature_pattern_counts (
  kind VARCHAR,
  feature_profile VARCHAR,
  feature_key VARCHAR,
  is_nonempty_whitespace BOOLEAN,
  pattern VARCHAR,
  examples UBIGINT,
  source_count UBIGINT,
  text_count UBIGINT,
  sample_source_ids VARCHAR,
  sample_text_ids VARCHAR,
  script_categories VARCHAR
);

CREATE TABLE errors (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  analyzer_id VARCHAR,
  stage VARCHAR,
  error_code VARCHAR,
  message VARCHAR
);
