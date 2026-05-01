-- Morph warehouse schema version 1.
-- Readers must reject runs.schema_version values other than 1.
-- source_id is one AAT source record/file within a run.
-- text_id is the logical work id; many source_id values may share one text_id.
-- analyzer_family is closed in v1: vibrato | sudachi.
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
  analyzer_id VARCHAR
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
