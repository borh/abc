#![allow(clippy::missing_errors_doc)]

use std::fs::{self, File};
use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result, bail};
use arrow_array::builder::{ArrayBuilder, ListBuilder, StringBuilder, UInt64Builder};
use arrow_array::{ArrayRef, BooleanArray, RecordBatch, StringArray, UInt32Array, UInt64Array};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::ArrowWriter;
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
use parquet::basic::{Compression, ZstdLevel};
use parquet::file::properties::WriterProperties;

use crate::schema::{
    AnalysisRow, ErrorRow, FeaturePatternCountRow, MorphemeRow, NwayFeatureDiffRow,
    NwayRegionAnalyzerRow, NwayRegionOracleEvidenceRow, NwayRegionRow, ProjectionSpanRow,
    RunAnalyzerRow, RunRow, SourceRow, WarehousePaths, WarehouseTable,
};
// `MorphemeFeatureRow` (the `Vec<Row>` intermediate) is retained only for the
// `#[cfg(test)]` reference-transposition path (see
// `append_morpheme_features_reference` and `MorphemeFeaturesColumns`).
#[cfg(test)]
use crate::schema::MorphemeFeatureRow;

const WAREHOUSE_MAX_ROW_GROUP_SIZE: usize = 50_000;

/// Merge-time compaction threshold (§3.12):
/// coalesce a staged table iff it has more than this many parts AND the
/// true median part is smaller than `COMPACTION_MAX_MEDIAN_PART_BYTES`.
/// Values from performance calibration.
pub(crate) const COMPACTION_MIN_PART_COUNT: usize = 64;
pub(crate) const COMPACTION_MAX_MEDIAN_PART_BYTES: u64 = 1_048_576; // 1 MiB

pub struct WarehouseWriter {
    paths: WarehousePaths,
    write_time: std::time::Duration,
    runs: Option<ArrowWriter<File>>,
    run_analyzers: Option<ArrowWriter<File>>,
    sources: Option<ArrowWriter<File>>,
    projection_spans: Option<ArrowWriter<File>>,
    analyses: Option<ArrowWriter<File>>,
    morphemes: Option<ArrowWriter<File>>,
    morpheme_features: Option<ArrowWriter<File>>,
    nway_regions: Option<ArrowWriter<File>>,
    nway_region_analyzers: Option<ArrowWriter<File>>,
    nway_region_oracle_evidence: Option<ArrowWriter<File>>,
    nway_feature_diffs: Option<ArrowWriter<File>>,
    feature_pattern_counts: Option<ArrowWriter<File>>,
    errors: Option<ArrowWriter<File>>,
}

impl WarehouseWriter {
    #[allow(dead_code)]
    pub fn create(paths: WarehousePaths) -> Result<Self> {
        Self::create_for_tables(paths, WarehouseTable::ALL, 3)
    }

    pub fn create_for_tables(
        paths: WarehousePaths,
        tables: &[WarehouseTable],
        zstd_level: i32,
    ) -> Result<Self> {
        if paths.final_dir.exists() {
            bail!("warehouse run {} already exists", paths.run_id);
        }
        cleanup_stale_staging(&paths)?;
        if paths.staging_dir.exists() {
            fs::remove_dir_all(&paths.staging_dir)
                .with_context(|| format!("failed to remove {}", paths.staging_dir.display()))?;
        }
        fs::create_dir_all(&paths.staging_dir)
            .with_context(|| format!("failed to create {}", paths.staging_dir.display()))?;
        crate::sql::write_schema_sql(&paths.warehouse_dir)?;

        Ok(Self {
            write_time: std::time::Duration::ZERO,
            runs: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Runs,
                runs_schema(),
                zstd_level,
            )?,
            run_analyzers: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::RunAnalyzers,
                run_analyzers_schema(),
                zstd_level,
            )?,
            sources: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Sources,
                sources_schema(),
                zstd_level,
            )?,
            projection_spans: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::ProjectionSpans,
                projection_spans_schema(),
                zstd_level,
            )?,
            analyses: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Analyses,
                analyses_schema(),
                zstd_level,
            )?,
            morphemes: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Morphemes,
                morphemes_schema(),
                zstd_level,
            )?,
            morpheme_features: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::MorphemeFeatures,
                morpheme_features_schema(),
                zstd_level,
            )?,
            nway_regions: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::NwayRegions,
                nway_regions_schema(),
                zstd_level,
            )?,
            nway_region_analyzers: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::NwayRegionAnalyzers,
                nway_region_analyzers_schema(),
                zstd_level,
            )?,
            nway_region_oracle_evidence: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::NwayRegionOracleEvidence,
                nway_region_oracle_evidence_schema(),
                zstd_level,
            )?,
            nway_feature_diffs: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::NwayFeatureDiffs,
                nway_feature_diffs_schema(),
                zstd_level,
            )?,
            feature_pattern_counts: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::FeaturePatternCounts,
                feature_pattern_counts_schema(),
                zstd_level,
            )?,
            errors: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Errors,
                errors_schema(),
                zstd_level,
            )?,
            paths,
        })
    }

    #[must_use]
    pub fn writes_table(&self, table: WarehouseTable) -> bool {
        match table {
            WarehouseTable::Runs => self.runs.is_some(),
            WarehouseTable::RunAnalyzers => self.run_analyzers.is_some(),
            WarehouseTable::Sources => self.sources.is_some(),
            WarehouseTable::ProjectionSpans => self.projection_spans.is_some(),
            WarehouseTable::Analyses => self.analyses.is_some(),
            WarehouseTable::Morphemes => self.morphemes.is_some(),
            WarehouseTable::MorphemeFeatures => self.morpheme_features.is_some(),
            WarehouseTable::NwayRegions => self.nway_regions.is_some(),
            WarehouseTable::NwayRegionAnalyzers => self.nway_region_analyzers.is_some(),
            WarehouseTable::NwayRegionOracleEvidence => self.nway_region_oracle_evidence.is_some(),
            WarehouseTable::NwayFeatureDiffs => self.nway_feature_diffs.is_some(),
            WarehouseTable::FeaturePatternCounts => self.feature_pattern_counts.is_some(),
            WarehouseTable::Errors => self.errors.is_some(),
        }
    }

    pub fn append_runs(&mut self, rows: &[RunRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        let schema = runs_schema();
        write_batch(
            self.runs.as_mut().expect("runs writer open"),
            schema.clone(),
            vec![
                u32_array(rows.iter().map(|row| row.schema_version)),
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.created_at_utc.as_str())),
                string_array(rows.iter().map(|row| row.input_mode.as_str())),
                string_array(rows.iter().map(|row| row.input_path.as_str())),
                u64_array(rows.iter().map(|row| row.source_count)),
                u64_array(rows.iter().map(|row| row.analyzer_count)),
                u64_array(rows.iter().map(|row| row.error_count)),
                string_array(rows.iter().map(|row| row.ortho_detect_mode.as_str())),
                nullable_string_array(
                    rows.iter()
                        .map(|row| row.input_normalization_detector_id.as_deref()),
                ),
                string_array(
                    rows.iter()
                        .map(|row| row.input_normalization_policy_hash.as_str()),
                ),
            ],
            &mut self.write_time,
        )
    }

    pub fn append_run_analyzers(&mut self, rows: &[RunAnalyzerRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        write_batch(
            self.run_analyzers
                .as_mut()
                .expect("run_analyzers writer open"),
            run_analyzers_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.analyzer_id.as_str())),
                string_array(rows.iter().map(|row| row.analyzer_arg.as_str())),
                string_array(rows.iter().map(|row| row.analyzer_family.as_str())),
            ],
            &mut self.write_time,
        )
    }

    pub fn append_sources(&mut self, rows: &[SourceRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        write_batch(
            self.sources.as_mut().expect("sources writer open"),
            sources_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
                string_array(rows.iter().map(|row| row.aat_path.as_str())),
                u64_array(rows.iter().map(|row| row.source_bytes)),
                u64_array(rows.iter().map(|row| row.source_chars)),
            ],
            &mut self.write_time,
        )
    }

    pub fn append_projection_spans(&mut self, rows: &[ProjectionSpanRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        let Some(writer) = self.projection_spans.as_mut() else {
            return Ok(());
        };
        write_batch(
            writer,
            projection_spans_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_ref())),
                string_array(rows.iter().map(|row| row.source_id.as_ref())),
                string_array(rows.iter().map(|row| row.text_id.as_ref())),
                u64_array(rows.iter().map(|row| row.projected_char_start)),
                u64_array(rows.iter().map(|row| row.projected_char_end)),
                string_array(rows.iter().map(|row| row.aat_pointer.as_str())),
                string_array(rows.iter().map(|row| row.inline_kind.as_str())),
                bool_array(rows.iter().map(|row| row.is_ruby_base)),
                bool_array(rows.iter().map(|row| row.is_gaiji)),
                bool_array(rows.iter().map(|row| row.is_note)),
            ],
            &mut self.write_time,
        )
    }

    pub fn append_analyses(&mut self, rows: &[AnalysisRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        write_batch(
            self.analyses.as_mut().expect("analyses writer open"),
            analyses_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
                string_array(rows.iter().map(|row| row.analyzer_id.as_str())),
                u64_array(rows.iter().map(|row| row.morpheme_count)),
            ],
            &mut self.write_time,
        )
    }

    pub fn append_morphemes(&mut self, rows: &[MorphemeRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        write_batch(
            self.morphemes.as_mut().expect("morphemes writer open"),
            morphemes_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_ref())),
                string_array(rows.iter().map(|row| row.source_id.as_ref())),
                string_array(rows.iter().map(|row| row.text_id.as_ref())),
                string_array(rows.iter().map(|row| row.analyzer_id.as_ref())),
                u64_array(rows.iter().map(|row| row.morpheme_index)),
                u64_array(rows.iter().map(|row| row.byte_start)),
                u64_array(rows.iter().map(|row| row.byte_end)),
                u64_array(rows.iter().map(|row| row.char_start)),
                u64_array(rows.iter().map(|row| row.char_end)),
                string_array(rows.iter().map(|row| row.surface.as_str())),
            ],
            &mut self.write_time,
        )
    }

    /// Reference `Vec<Row>` -> transposed-arrays implementation, retained only
    /// to characterize byte-identity against the direct-builder path (see
    /// [`append_morpheme_feature_columns`](Self::append_morpheme_feature_columns)).
    /// Production code no longer allocates a `Vec<MorphemeFeatureRow>` for
    /// this table; the producer appends directly into a
    /// [`MorphemeFeaturesColumns`] builder instead.
    #[cfg(test)]
    fn append_morpheme_features_reference(&mut self, rows: &[MorphemeFeatureRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        let Some(writer) = self.morpheme_features.as_mut() else {
            return Ok(());
        };
        write_batch(
            writer,
            morpheme_features_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_ref())),
                string_array(rows.iter().map(|row| row.source_id.as_ref())),
                string_array(rows.iter().map(|row| row.text_id.as_ref())),
                string_array(rows.iter().map(|row| row.analyzer_id.as_ref())),
                u64_array(rows.iter().map(|row| row.morpheme_index)),
                string_array(rows.iter().map(|row| row.feature_key.as_ref())),
                nullable_string_array(rows.iter().map(|row| row.feature_value.as_deref())),
            ],
            &mut self.write_time,
        )
    }

    /// Write a finished [`MorphemeFeaturesColumns`] builder as a single
    /// `RecordBatch`. Producers (see `ab-morph-run`'s
    /// `push_morpheme_features_for_range`) append each morpheme-feature
    /// directly into the builder's Arrow buffers. Consequently, this table never
    /// materializes a `Vec<MorphemeFeatureRow>` on the production path (this is the
    /// highest-row-volume warehouse table, where per-row `Arc::clone` operations
    /// become significant at scale).
    ///
    /// # Errors
    ///
    /// Returns an error if the underlying writer fails to write the batch.
    pub fn append_morpheme_feature_columns(
        &mut self,
        mut columns: MorphemeFeaturesColumns,
    ) -> Result<()> {
        if columns.is_empty() || self.morpheme_features.is_none() {
            return Ok(());
        }
        let batch = columns.finish();
        self.append_record_batch(WarehouseTable::MorphemeFeatures, batch)
    }

    pub fn append_nway_regions(&mut self, rows: &[NwayRegionRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        write_batch(
            self.nway_regions
                .as_mut()
                .expect("nway_regions writer open"),
            nway_regions_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_ref())),
                string_array(rows.iter().map(|row| row.source_id.as_ref())),
                string_array(rows.iter().map(|row| row.text_id.as_ref())),
                u64_array(rows.iter().map(|row| row.region_index)),
                u64_array(rows.iter().map(|row| row.byte_start)),
                u64_array(rows.iter().map(|row| row.byte_end)),
                u64_array(rows.iter().map(|row| row.char_start)),
                u64_array(rows.iter().map(|row| row.char_end)),
                bool_array(rows.iter().map(|row| row.is_nonempty_whitespace)),
                bool_array(rows.iter().map(|row| row.is_agreement)),
                bool_array(rows.iter().map(|row| row.has_coverage_mismatch)),
                bool_array(rows.iter().map(|row| row.has_segmentation_disagreement)),
                bool_array(rows.iter().map(|row| row.has_feature_disagreement)),
            ],
            &mut self.write_time,
        )
    }

    pub fn append_nway_region_analyzers(&mut self, rows: &[NwayRegionAnalyzerRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        write_batch(
            self.nway_region_analyzers
                .as_mut()
                .expect("nway_region_analyzers writer open"),
            nway_region_analyzers_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_ref())),
                string_array(rows.iter().map(|row| row.source_id.as_ref())),
                string_array(rows.iter().map(|row| row.text_id.as_ref())),
                u64_array(rows.iter().map(|row| row.region_index)),
                string_array(rows.iter().map(|row| row.analyzer_id.as_ref())),
                bool_array(rows.iter().map(|row| row.covers_exactly)),
                u64_array(rows.iter().map(|row| row.morpheme_start)),
                u64_array(rows.iter().map(|row| row.morpheme_end)),
                string_list_array(rows.iter().map(|row| row.surfaces.as_slice())),
            ],
            &mut self.write_time,
        )
    }

    pub fn append_nway_region_oracle_evidence(
        &mut self,
        rows: &[NwayRegionOracleEvidenceRow],
    ) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        let Some(writer) = self.nway_region_oracle_evidence.as_mut() else {
            return Ok(());
        };
        write_batch(
            writer,
            nway_region_oracle_evidence_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
                u64_array(rows.iter().map(|row| row.region_index)),
                u64_array(rows.iter().map(|row| row.projected_char_start)),
                u64_array(rows.iter().map(|row| row.projected_char_end)),
                string_array(rows.iter().map(|row| row.oracle_source.as_str())),
                string_array(rows.iter().map(|row| row.classification.as_str())),
                nullable_string_array(rows.iter().map(|row| row.winning_analyzer.as_deref())),
                string_list_array(rows.iter().map(|row| row.losing_analyzers.as_slice())),
                string_array(rows.iter().map(|row| row.evidence_detail.as_str())),
                nullable_string_array(rows.iter().map(|row| row.adjudicated_reading.as_deref())),
            ],
            &mut self.write_time,
        )
    }

    /// Row-based `Vec<Row>` -> transposed-arrays implementation. `nway_feature_diffs`
    /// production writes no longer go through this path (see
    /// [`append_nway_feature_diff_columns`](Self::append_nway_feature_diff_columns);
    /// `ab-morph-run`'s n-way batch driver appends directly into a
    /// [`NwayFeatureDiffsColumns`] builder instead, since this is the
    /// highest-row-volume warehouse table). Unlike `morpheme_features`'s
    /// equivalent reference path, this method stays a regular (non-
    /// `#[cfg(test)]`) `pub fn`: several `ab-morph-run` test fixtures
    /// (`summary/summary_body.rs`, `summary/interesting.rs`) construct
    /// `NwayFeatureDiffRow` literals and call this method across the crate
    /// boundary, where a `#[cfg(test)]` item in this crate would not be
    /// visible because `#[cfg(test)]` gates compilation per-crate rather than
    /// per-workspace. It also still functions as this crate's own byte-identity
    /// reference (see `nway_feature_diffs_direct_builder_matches_reference_bytes`).
    pub fn append_nway_feature_diffs(&mut self, rows: &[NwayFeatureDiffRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        let Some(writer) = self.nway_feature_diffs.as_mut() else {
            return Ok(());
        };
        write_batch(
            writer,
            nway_feature_diffs_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_ref())),
                string_array(rows.iter().map(|row| row.source_id.as_ref())),
                string_array(rows.iter().map(|row| row.text_id.as_ref())),
                u64_array(rows.iter().map(|row| row.region_index)),
                string_array(rows.iter().map(|row| row.feature_key.as_ref())),
                string_array(rows.iter().map(|row| row.scope_type.as_ref())),
                nullable_u64_array(rows.iter().map(|row| row.scope_position)),
                nullable_string_array(rows.iter().map(|row| row.scope_surface.as_deref())),
                nullable_string_array(rows.iter().map(|row| row.feature_value.as_deref())),
                string_list_array(rows.iter().map(|row| row.analyzers.as_slice())),
            ],
            &mut self.write_time,
        )
    }

    /// Write a finished [`NwayFeatureDiffsColumns`] builder as a single
    /// `RecordBatch`. Producers (see `ab-morph-run`'s `push_region_rows`)
    /// append each n-way feature-diff directly into the builder's Arrow
    /// buffers, so this table never materializes a `Vec<NwayFeatureDiffRow>`
    /// on the production path. Because `nway_feature_diffs` is the
    /// highest-row-volume warehouse table (~23.4B rows), eliminating per-row
    /// `Arc::clone` operations was necessary.
    ///
    /// # Errors
    ///
    /// Returns an error if the underlying writer fails to write the batch.
    pub fn append_nway_feature_diff_columns(
        &mut self,
        mut columns: NwayFeatureDiffsColumns,
    ) -> Result<()> {
        if columns.is_empty() || self.nway_feature_diffs.is_none() {
            return Ok(());
        }
        let batch = columns.finish();
        self.append_record_batch(WarehouseTable::NwayFeatureDiffs, batch)
    }

    pub fn append_feature_pattern_counts(&mut self, rows: &[FeaturePatternCountRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        let Some(writer) = self.feature_pattern_counts.as_mut() else {
            return Ok(());
        };
        write_batch(
            writer,
            feature_pattern_counts_schema(),
            vec![
                string_array(rows.iter().map(|row| row.kind.as_str())),
                string_array(rows.iter().map(|row| row.feature_profile.as_str())),
                string_array(rows.iter().map(|row| row.feature_key.as_str())),
                bool_array(rows.iter().map(|row| row.is_nonempty_whitespace)),
                string_array(rows.iter().map(|row| row.pattern.as_str())),
                u64_array(rows.iter().map(|row| row.examples)),
                u64_array(rows.iter().map(|row| row.source_count)),
                u64_array(rows.iter().map(|row| row.text_count)),
                string_array(rows.iter().map(|row| row.sample_source_ids.as_str())),
                string_array(rows.iter().map(|row| row.sample_text_ids.as_str())),
                string_array(rows.iter().map(|row| row.script_categories.as_str())),
            ],
            &mut self.write_time,
        )
    }

    pub fn append_errors(&mut self, rows: &[ErrorRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        write_batch(
            self.errors.as_mut().expect("errors writer open"),
            errors_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                nullable_string_array(rows.iter().map(|row| row.source_id.as_deref())),
                nullable_string_array(rows.iter().map(|row| row.text_id.as_deref())),
                nullable_string_array(rows.iter().map(|row| row.analyzer_id.as_deref())),
                string_array(rows.iter().map(|row| row.stage.as_str())),
                string_array(rows.iter().map(|row| row.error_code.as_str())),
                string_array(rows.iter().map(|row| row.message.as_str())),
            ],
            &mut self.write_time,
        )
    }

    /// Write a raw `RecordBatch` into the parquet writer for `table`. Used by
    /// merge-time compaction (via [`append_parquet_table_file`]) to stream
    /// batches read from staged parts into the destination writer.
    ///
    /// # Errors
    ///
    /// Returns an error if the underlying writer fails to write the batch.
    pub(crate) fn append_record_batch(
        &mut self,
        table: WarehouseTable,
        batch: RecordBatch,
    ) -> Result<()> {
        let start = std::time::Instant::now();
        match table {
            WarehouseTable::Runs => self
                .runs
                .as_mut()
                .expect("runs writer open")
                .write(&batch)?,
            WarehouseTable::RunAnalyzers => self
                .run_analyzers
                .as_mut()
                .expect("run_analyzers writer open")
                .write(&batch)?,
            WarehouseTable::Sources => self
                .sources
                .as_mut()
                .expect("sources writer open")
                .write(&batch)?,
            WarehouseTable::ProjectionSpans => self
                .projection_spans
                .as_mut()
                .expect("projection_spans writer open")
                .write(&batch)?,
            WarehouseTable::Analyses => self
                .analyses
                .as_mut()
                .expect("analyses writer open")
                .write(&batch)?,
            WarehouseTable::Morphemes => self
                .morphemes
                .as_mut()
                .expect("morphemes writer open")
                .write(&batch)?,
            WarehouseTable::MorphemeFeatures => self
                .morpheme_features
                .as_mut()
                .expect("morpheme_features writer open")
                .write(&batch)?,
            WarehouseTable::NwayRegions => self
                .nway_regions
                .as_mut()
                .expect("nway_regions writer open")
                .write(&batch)?,
            WarehouseTable::NwayRegionAnalyzers => self
                .nway_region_analyzers
                .as_mut()
                .expect("nway_region_analyzers writer open")
                .write(&batch)?,
            WarehouseTable::NwayRegionOracleEvidence => self
                .nway_region_oracle_evidence
                .as_mut()
                .expect("nway_region_oracle_evidence writer open")
                .write(&batch)?,
            WarehouseTable::NwayFeatureDiffs => self
                .nway_feature_diffs
                .as_mut()
                .expect("nway_feature_diffs writer open")
                .write(&batch)?,
            WarehouseTable::FeaturePatternCounts => self
                .feature_pattern_counts
                .as_mut()
                .expect("feature_pattern_counts writer open")
                .write(&batch)?,
            WarehouseTable::Errors => self
                .errors
                .as_mut()
                .expect("errors writer open")
                .write(&batch)?,
        }
        self.write_time += start.elapsed();
        Ok(())
    }

    pub fn write_time(&self) -> std::time::Duration {
        self.write_time
    }

    pub fn finalize(mut self) -> Result<std::time::Duration> {
        self.write_time += close_writer(self.runs.take())?;
        self.write_time += close_writer(self.run_analyzers.take())?;
        self.write_time += close_writer(self.sources.take())?;
        self.write_time += close_writer(self.projection_spans.take())?;
        self.write_time += close_writer(self.analyses.take())?;
        self.write_time += close_writer(self.morphemes.take())?;
        self.write_time += close_writer(self.morpheme_features.take())?;
        self.write_time += close_writer(self.nway_regions.take())?;
        self.write_time += close_writer(self.nway_region_analyzers.take())?;
        self.write_time += close_writer(self.nway_region_oracle_evidence.take())?;
        self.write_time += close_writer(self.nway_feature_diffs.take())?;
        self.write_time += close_writer(self.feature_pattern_counts.take())?;
        self.write_time += close_writer(self.errors.take())?;
        crate::sql::write_run_views_sql(&self.paths.staging_dir, &self.paths.final_dir)?;
        finalize_staging_run(&self.paths)?;
        Ok(self.write_time)
    }
}

/// Column-oriented builder for the `morpheme_features` table (7 columns, in
/// `morpheme_features_schema()` order). Producers append each
/// morpheme-feature directly into this builder's Arrow buffers via
/// [`push_row`](Self::push_row) instead of collecting a
/// `Vec<MorphemeFeatureRow>` first. Because `morpheme_features` is the
/// highest-row-volume warehouse table, skipping per-row `Arc::clone` operations
/// (previously ~5 per row: 4 ID columns plus the feature key) and intermediate
/// `Vec` allocations is essential at scale.
pub struct MorphemeFeaturesColumns {
    run_id: StringBuilder,
    source_id: StringBuilder,
    text_id: StringBuilder,
    analyzer_id: StringBuilder,
    morpheme_index: UInt64Builder,
    feature_key: StringBuilder,
    feature_value: StringBuilder,
}

impl MorphemeFeaturesColumns {
    #[must_use]
    pub fn new() -> Self {
        Self {
            run_id: StringBuilder::new(),
            source_id: StringBuilder::new(),
            text_id: StringBuilder::new(),
            analyzer_id: StringBuilder::new(),
            morpheme_index: UInt64Builder::new(),
            feature_key: StringBuilder::new(),
            feature_value: StringBuilder::new(),
        }
    }

    /// Append one `morpheme_features` row directly into the column builders.
    #[allow(clippy::too_many_arguments)]
    pub fn push_row(
        &mut self,
        run_id: &str,
        source_id: &str,
        text_id: &str,
        analyzer_id: &str,
        morpheme_index: u64,
        feature_key: &str,
        feature_value: Option<&str>,
    ) {
        self.run_id.append_value(run_id);
        self.source_id.append_value(source_id);
        self.text_id.append_value(text_id);
        self.analyzer_id.append_value(analyzer_id);
        self.morpheme_index.append_value(morpheme_index);
        self.feature_key.append_value(feature_key);
        self.feature_value.append_option(feature_value);
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.morpheme_index.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Finish every column builder into a single `RecordBatch` matching
    /// `morpheme_features_schema()`'s exact field order and types (`Utf8` /
    /// `UInt64`, no dictionary encoding) so output stays byte-identical to
    /// the retained `Vec<Row>` reference path.
    #[must_use]
    pub fn finish(&mut self) -> RecordBatch {
        RecordBatch::try_new(
            morpheme_features_schema(),
            vec![
                Arc::new(self.run_id.finish()),
                Arc::new(self.source_id.finish()),
                Arc::new(self.text_id.finish()),
                Arc::new(self.analyzer_id.finish()),
                Arc::new(self.morpheme_index.finish()),
                Arc::new(self.feature_key.finish()),
                Arc::new(self.feature_value.finish()),
            ],
        )
        .expect("MorphemeFeaturesColumns builders match the documented schema")
    }
}

impl Default for MorphemeFeaturesColumns {
    fn default() -> Self {
        Self::new()
    }
}

/// Column-oriented builder for the `nway_feature_diffs` table (10 columns, in
/// `nway_feature_diffs_schema()` order). `nway_feature_diffs` is the
/// highest-row-volume warehouse table (~23.4B rows), so (as with
/// [`MorphemeFeaturesColumns`]) producers append each feature-diff directly
/// into this builder's Arrow buffers via [`push_row`](Self::push_row) instead
/// of collecting a `Vec<NwayFeatureDiffRow>` first, skipping the per-row
/// `Arc::clone` bumps that Vec would otherwise require (6 `Arc<str>`/
/// `Option<Arc<str>>` fields per row). `analyzers` is a non-null, non-empty
/// `List<Utf8>` column: one row per distinct value-group, carrying every
/// analyzer that agreed on it (schema v3; see `SCHEMA_VERSION`).
pub struct NwayFeatureDiffsColumns {
    run_id: StringBuilder,
    source_id: StringBuilder,
    text_id: StringBuilder,
    region_index: UInt64Builder,
    feature_key: StringBuilder,
    scope_type: StringBuilder,
    scope_position: UInt64Builder,
    scope_surface: StringBuilder,
    feature_value: StringBuilder,
    analyzers: ListBuilder<StringBuilder>,
}

impl NwayFeatureDiffsColumns {
    #[must_use]
    pub fn new() -> Self {
        Self {
            run_id: StringBuilder::new(),
            source_id: StringBuilder::new(),
            text_id: StringBuilder::new(),
            region_index: UInt64Builder::new(),
            feature_key: StringBuilder::new(),
            scope_type: StringBuilder::new(),
            scope_position: UInt64Builder::new(),
            scope_surface: StringBuilder::new(),
            feature_value: StringBuilder::new(),
            analyzers: ListBuilder::new(StringBuilder::new()),
        }
    }

    /// Append one `nway_feature_diffs` row directly into the column builders.
    #[allow(clippy::too_many_arguments)]
    pub fn push_row(
        &mut self,
        run_id: &str,
        source_id: &str,
        text_id: &str,
        region_index: u64,
        feature_key: &str,
        scope_type: &str,
        scope_position: Option<u64>,
        scope_surface: Option<&str>,
        feature_value: Option<&str>,
        analyzers: &[impl AsRef<str>],
    ) {
        self.run_id.append_value(run_id);
        self.source_id.append_value(source_id);
        self.text_id.append_value(text_id);
        self.region_index.append_value(region_index);
        self.feature_key.append_value(feature_key);
        self.scope_type.append_value(scope_type);
        self.scope_position.append_option(scope_position);
        self.scope_surface.append_option(scope_surface);
        self.feature_value.append_option(feature_value);
        debug_assert!(
            !analyzers.is_empty(),
            "nway_feature_diffs analyzers list must be non-empty; an empty list \
             vanishes under the readers' UNNEST",
        );
        debug_assert!(
            analyzers.windows(2).all(|w| w[0].as_ref() <= w[1].as_ref()),
            "nway_feature_diffs analyzers list must be sorted ascending; the \
             producer guarantees sorted input and readers rely on it",
        );
        for analyzer in analyzers {
            self.analyzers.values().append_value(analyzer.as_ref());
        }
        self.analyzers.append(true);
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.region_index.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Finish every column builder into a single `RecordBatch` matching
    /// `nway_feature_diffs_schema()`'s exact field order and types (`Utf8` /
    /// `UInt64`, no dictionary encoding) so output stays byte-identical to
    /// the row-based reference path (see
    /// `append_nway_feature_diffs`/`nway_feature_diffs_direct_builder_matches_reference_bytes`).
    #[must_use]
    pub fn finish(&mut self) -> RecordBatch {
        RecordBatch::try_new(
            nway_feature_diffs_schema(),
            vec![
                Arc::new(self.run_id.finish()),
                Arc::new(self.source_id.finish()),
                Arc::new(self.text_id.finish()),
                Arc::new(self.region_index.finish()),
                Arc::new(self.feature_key.finish()),
                Arc::new(self.scope_type.finish()),
                Arc::new(self.scope_position.finish()),
                Arc::new(self.scope_surface.finish()),
                Arc::new(self.feature_value.finish()),
                Arc::new(self.analyzers.finish()),
            ],
        )
        .expect("NwayFeatureDiffsColumns builders match the documented schema")
    }
}

impl Default for NwayFeatureDiffsColumns {
    fn default() -> Self {
        Self::new()
    }
}

/// Append every row from a parquet file into `writer`'s table, re-using the
/// writer's `WAREHOUSE_MAX_ROW_GROUP_SIZE` row-group sizing. Used by merge-time
/// compaction to coalesce many tiny staged parts into one well-sized file.
///
/// # Errors
///
/// Returns an error if `path` cannot be opened or its batches fail to write.
pub(crate) fn append_parquet_table_file(
    writer: &mut WarehouseWriter,
    table: WarehouseTable,
    path: &Path,
) -> Result<()> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .with_context(|| format!("failed to read parquet metadata from {}", path.display()))?
        .build()
        .with_context(|| format!("failed to build parquet reader for {}", path.display()))?;
    for batch in reader {
        writer.append_record_batch(
            table,
            batch.with_context(|| format!("failed to read {}", path.display()))?,
        )?;
    }
    Ok(())
}

pub fn parquet_table_row_count(run_dir: &Path, table: WarehouseTable) -> Result<u64> {
    let path = run_dir.join(table.file_name());
    if path.is_dir() {
        return parquet_table_part_paths(&path)?
            .into_iter()
            .try_fold(0u64, |total, path| {
                parquet_file_row_count(&path).map(|count| total + count)
            });
    }
    parquet_file_row_count(&path)
}

/// Compact a table's staged parts into a single parquet file when it has many
/// small parts. Threshold: `part_count > 64 AND median_part_bytes < 1 MiB`
/// (§3.12 decision). Large tables (median ≥ 1 MiB) are returned untouched.
///
/// Implementation: because `WarehouseWriter::create_for_tables` wipes its
/// staging dir, the coalesced output is written to a **same-filesystem
/// sibling temp dir** (`<paths.warehouse_dir>/.compact-<run_id>-<table>`),
/// finalized there (staging→final via `finalize_staging_run`), then the
/// single coalesced part is moved into the real staging dir, replacing the
/// many small parts. Same-FS `fs::rename` is atomic; both paths are under
/// `paths.warehouse_dir`, so no cross-filesystem fallback is needed.
///
/// Returns `true` if the table was compacted, `false` if it was left as-is.
///
/// # Errors
///
/// Returns an error if staging can't be read, the coalesce writer fails, or
/// the part replacement can't be completed.
pub fn compact_staged_table(
    paths: &WarehousePaths,
    table: WarehouseTable,
    zstd_level: i32,
) -> Result<bool> {
    let staged = paths.staging_dir.join(table.file_name());
    if !staged.is_dir() {
        return Ok(false);
    }
    let sizes = parquet_table_part_sizes(&staged)?;
    if sizes.len() <= COMPACTION_MIN_PART_COUNT {
        return Ok(false);
    }
    let median = sizes[sizes.len() / 2];
    if median >= COMPACTION_MAX_MEDIAN_PART_BYTES {
        eprintln!(
            "warehouse compaction: skipping {} ({} parts, median {}B ≥ {}B)",
            table.file_name(),
            sizes.len(),
            median,
            COMPACTION_MAX_MEDIAN_PART_BYTES
        );
        return Ok(false);
    }
    eprintln!(
        "warehouse compaction: compacting {} ({} parts, median {}B) → 1 file",
        table.file_name(),
        sizes.len(),
        median
    );
    let part_paths = parquet_table_part_paths(&staged)?;

    // Coalesce into a same-FS sibling temp dir so rename is atomic.
    let compact_dir =
        paths
            .warehouse_dir
            .join(format!(".compact-{}-{}", paths.run_id, table.file_name()));
    if compact_dir.exists() {
        fs::remove_dir_all(&compact_dir)
            .with_context(|| format!("remove stale {}", compact_dir.display()))?;
    }
    let compact_paths = WarehousePaths::new(&compact_dir, "compact");
    // Compaction rewrites already-staged small parts into one file; it uses the
    // caller's configured ZSTD level so recompacted tables honor
    // `--parquet-zstd-level` the same as the per-worker write path.
    let mut writer =
        WarehouseWriter::create_for_tables(compact_paths.clone(), &[table], zstd_level)?;
    for part in &part_paths {
        append_parquet_table_file(&mut writer, table, part)?;
    }
    let _ = writer.finalize()?;
    // finalize moved compact_paths.staging_dir → compact_paths.final_dir;
    // the single coalesced parquet file is at final_dir/<table.file_name()>
    // (the writer writes one part file for one writer instance). views.sql
    // (also emitted by finalize) is left behind in the temp dir and removed
    // by the cleanup at the end.
    let compacted_dir = compact_paths.final_dir.join(table.file_name());
    //
    // Two-phase replacement: move the original staged dir aside FIRST (keeping
    // originals intact for recovery), create a fresh staged dir, move the
    // coalesced file in, THEN drop the backups. Same-FS renames are atomic, so
    // either the originals or the replacement is always present on disk.
    let backup_dir = paths.warehouse_dir.join(format!(
        ".staged-backup-{}-{}",
        paths.run_id,
        table.file_name()
    ));
    if backup_dir.exists() {
        fs::remove_dir_all(&backup_dir)
            .with_context(|| format!("remove stale {}", backup_dir.display()))?;
    }
    // Move the whole original staged dir aside.
    fs::rename(&staged, &backup_dir).with_context(|| {
        format!(
            "move {} aside to {}",
            staged.display(),
            backup_dir.display()
        )
    })?;
    // Fresh staged dir to receive the coalesced file.
    fs::create_dir_all(&staged).with_context(|| format!("recreate {}", staged.display()))?;
    // The writer produces exactly one coalesced part file; move it in.
    let dest = staged.join(table.file_name());
    fs::rename(&compacted_dir, &dest).with_context(|| {
        format!(
            "move coalesced {} → {}",
            compacted_dir.display(),
            dest.display()
        )
    })?;
    // Replacement committed; originals no longer needed.
    if let Err(e) = fs::remove_dir_all(&backup_dir) {
        eprintln!(
            "warehouse compaction: failed to clean up backup dir {}: {e}",
            backup_dir.display()
        );
    }
    // Clean up the temp compact dir (its staging dir was already moved by finalize).
    if let Err(e) = fs::remove_dir_all(&compact_dir) {
        eprintln!(
            "warehouse compaction: failed to clean up temp dir {}: {e}",
            compact_dir.display()
        );
    }
    Ok(true)
}

pub fn stage_parquet_table_part(
    staging_run_dir: &Path,
    table: WarehouseTable,
    shard_index: usize,
    source_path: &Path,
) -> Result<()> {
    let dataset_dir = staging_run_dir.join(table.file_name());
    fs::create_dir_all(&dataset_dir)
        .with_context(|| format!("failed to create {}", dataset_dir.display()))?;
    if source_path.is_dir() {
        for (part_index, part) in parquet_table_part_paths(source_path)?
            .into_iter()
            .enumerate()
        {
            move_or_copy_parquet_part(
                &part,
                &dataset_dir.join(format!("part-{shard_index:05}-{part_index:05}.parquet")),
            )?;
        }
    } else {
        move_or_copy_parquet_part(
            source_path,
            &dataset_dir.join(format!("part-{shard_index:05}.parquet")),
        )?;
    }
    Ok(())
}

fn parquet_file_row_count(path: &Path) -> Result<u64> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let builder = ParquetRecordBatchReaderBuilder::try_new(file)
        .with_context(|| format!("failed to read parquet metadata from {}", path.display()))?;
    Ok(builder.metadata().file_metadata().num_rows() as u64)
}

fn parquet_table_part_paths(dataset_dir: &Path) -> Result<Vec<std::path::PathBuf>> {
    let mut paths = fs::read_dir(dataset_dir)
        .with_context(|| format!("failed to read {}", dataset_dir.display()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::result::Result<Vec<_>, _>>()
        .with_context(|| format!("failed to read {}", dataset_dir.display()))?;
    paths.retain(|path| {
        path.extension()
            .is_some_and(|extension| extension == "parquet")
    });
    paths.sort();
    Ok(paths)
}

/// Sorted on-disk byte sizes of `.parquet` files in `dir` (ignoring non-parquet
/// files like `views.sql`). Used by merge compaction to compute the true median
/// part size for the threshold decision.
///
/// # Errors
///
/// Returns an error if `dir` cannot be read or a part cannot be stat'd.
pub(crate) fn parquet_table_part_sizes(dir: &Path) -> Result<Vec<u64>> {
    let paths = parquet_table_part_paths(dir)?;
    let mut sizes: Vec<u64> = paths
        .iter()
        .map(|p| fs::metadata(p).map(|m| m.len()))
        .collect::<std::result::Result<Vec<_>, _>>()
        .with_context(|| format!("failed to stat parts in {}", dir.display()))?;
    sizes.sort_unstable();
    Ok(sizes)
}

fn move_or_copy_parquet_part(source: &Path, destination: &Path) -> Result<()> {
    match fs::rename(source, destination) {
        Ok(()) => Ok(()),
        Err(rename_error) => {
            fs::copy(source, destination).with_context(|| {
                format!(
                    "failed to move {} to {} after rename failed with {rename_error}",
                    source.display(),
                    destination.display()
                )
            })?;
            Ok(())
        }
    }
}

#[cfg(test)]
pub fn parquet_file_exists(dir: &Path, table: WarehouseTable) -> bool {
    dir.join(table.file_name()).is_file()
}

fn cleanup_stale_staging(paths: &WarehousePaths) -> Result<()> {
    let Some(staging_root) = paths.staging_dir.parent() else {
        return Ok(());
    };
    if !staging_root.exists() {
        return Ok(());
    }
    let prefix = format!("{}.", paths.run_id);
    for entry in fs::read_dir(staging_root)? {
        let entry = entry?;
        let file_name = entry.file_name();
        let file_name = file_name.to_string_lossy();
        if file_name.starts_with(&prefix) && entry.path() != paths.staging_dir {
            fs::remove_dir_all(entry.path()).with_context(|| {
                format!(
                    "failed to remove stale staging directory {}",
                    entry.path().display()
                )
            })?;
        }
    }
    Ok(())
}

fn finalize_staging_run(paths: &WarehousePaths) -> Result<()> {
    if paths.final_dir.exists() {
        bail!("warehouse run {} already exists", paths.run_id);
    }
    if let Some(parent) = paths.final_dir.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::rename(&paths.staging_dir, &paths.final_dir).with_context(|| {
        format!(
            "failed to publish warehouse run from {} to {}",
            paths.staging_dir.display(),
            paths.final_dir.display()
        )
    })?;
    Ok(())
}

fn open_table_writer(
    paths: &WarehousePaths,
    table: WarehouseTable,
    schema: Arc<Schema>,
    zstd_level: i32,
) -> Result<ArrowWriter<File>> {
    let file = File::create(paths.staging_table_path(table))?;
    Ok(ArrowWriter::try_new(
        file,
        schema,
        Some(writer_properties(zstd_level)),
    )?)
}

fn open_optional_table_writer(
    paths: &WarehousePaths,
    tables: &[WarehouseTable],
    table: WarehouseTable,
    schema: Arc<Schema>,
    zstd_level: i32,
) -> Result<Option<ArrowWriter<File>>> {
    if tables.contains(&table) {
        open_table_writer(paths, table, schema, zstd_level).map(Some)
    } else {
        Ok(None)
    }
}

fn writer_properties(zstd_level: i32) -> WriterProperties {
    WriterProperties::builder()
        .set_max_row_group_size(WAREHOUSE_MAX_ROW_GROUP_SIZE)
        .set_compression(Compression::ZSTD(
            ZstdLevel::try_new(zstd_level).expect("valid zstd level"),
        ))
        .build()
}

fn write_batch<W: std::io::Write + Send>(
    writer: &mut ArrowWriter<W>,
    schema: Arc<Schema>,
    columns: Vec<ArrayRef>,
    write_time: &mut std::time::Duration,
) -> Result<()> {
    let batch = RecordBatch::try_new(schema, columns)?;
    let start = std::time::Instant::now();
    writer.write(&batch)?;
    *write_time += start.elapsed();
    Ok(())
}

fn close_writer(writer: Option<ArrowWriter<File>>) -> Result<std::time::Duration> {
    if let Some(writer) = writer {
        let start = std::time::Instant::now();
        writer.close()?;
        return Ok(start.elapsed());
    }
    Ok(std::time::Duration::ZERO)
}

fn string_array<'a>(values: impl Iterator<Item = &'a str>) -> ArrayRef {
    Arc::new(StringArray::from_iter_values(values))
}

fn nullable_string_array<'a>(values: impl Iterator<Item = Option<&'a str>>) -> ArrayRef {
    Arc::new(StringArray::from_iter(values))
}

fn u32_array(values: impl Iterator<Item = u32>) -> ArrayRef {
    Arc::new(UInt32Array::from_iter_values(values))
}

fn u64_array(values: impl Iterator<Item = u64>) -> ArrayRef {
    Arc::new(UInt64Array::from_iter_values(values))
}

fn nullable_u64_array(values: impl Iterator<Item = Option<u64>>) -> ArrayRef {
    Arc::new(UInt64Array::from_iter(values))
}

fn bool_array(values: impl Iterator<Item = bool>) -> ArrayRef {
    Arc::new(BooleanArray::from_iter(values.map(Some)))
}

fn string_list_array<'a, S: AsRef<str> + 'a>(values: impl Iterator<Item = &'a [S]>) -> ArrayRef {
    let mut builder = ListBuilder::new(StringBuilder::new());
    for list in values {
        for value in list {
            builder.values().append_value(value.as_ref());
        }
        builder.append(true);
    }
    Arc::new(builder.finish())
}

fn schema(fields: Vec<Field>) -> Arc<Schema> {
    Arc::new(Schema::new(fields))
}

fn utf8(name: &'static str, nullable: bool) -> Field {
    Field::new(name, DataType::Utf8, nullable)
}

fn u32_field(name: &'static str) -> Field {
    Field::new(name, DataType::UInt32, false)
}

fn u64_field(name: &'static str, nullable: bool) -> Field {
    Field::new(name, DataType::UInt64, nullable)
}

fn bool_field(name: &'static str) -> Field {
    Field::new(name, DataType::Boolean, false)
}

fn utf8_list(name: &'static str) -> Field {
    Field::new(
        name,
        DataType::List(Arc::new(Field::new("item", DataType::Utf8, true))),
        false,
    )
}

fn runs_schema() -> Arc<Schema> {
    schema(vec![
        u32_field("schema_version"),
        utf8("run_id", false),
        utf8("created_at_utc", false),
        utf8("input_mode", false),
        utf8("input_path", false),
        u64_field("source_count", false),
        u64_field("analyzer_count", false),
        u64_field("error_count", false),
        utf8("ortho_detect_mode", false),
        utf8("input_normalization_detector_id", true),
        utf8("input_normalization_policy_hash", false),
    ])
}

fn run_analyzers_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("analyzer_id", false),
        utf8("analyzer_arg", false),
        utf8("analyzer_family", false),
    ])
}

fn sources_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        utf8("aat_path", false),
        u64_field("source_bytes", false),
        u64_field("source_chars", false),
    ])
}

fn projection_spans_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        u64_field("projected_char_start", false),
        u64_field("projected_char_end", false),
        utf8("aat_pointer", false),
        utf8("inline_kind", false),
        bool_field("is_ruby_base"),
        bool_field("is_gaiji"),
        bool_field("is_note"),
    ])
}

fn analyses_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        utf8("analyzer_id", false),
        u64_field("morpheme_count", false),
    ])
}

fn morphemes_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        utf8("analyzer_id", false),
        u64_field("morpheme_index", false),
        u64_field("byte_start", false),
        u64_field("byte_end", false),
        u64_field("char_start", false),
        u64_field("char_end", false),
        utf8("surface", false),
    ])
}

fn morpheme_features_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        utf8("analyzer_id", false),
        u64_field("morpheme_index", false),
        utf8("feature_key", false),
        utf8("feature_value", true),
    ])
}

fn nway_regions_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        u64_field("region_index", false),
        u64_field("byte_start", false),
        u64_field("byte_end", false),
        u64_field("char_start", false),
        u64_field("char_end", false),
        bool_field("is_nonempty_whitespace"),
        bool_field("is_agreement"),
        bool_field("has_coverage_mismatch"),
        bool_field("has_segmentation_disagreement"),
        bool_field("has_feature_disagreement"),
    ])
}

fn nway_region_analyzers_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        u64_field("region_index", false),
        utf8("analyzer_id", false),
        bool_field("covers_exactly"),
        u64_field("morpheme_start", false),
        u64_field("morpheme_end", false),
        utf8_list("surfaces"),
    ])
}

fn nway_region_oracle_evidence_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        u64_field("region_index", false),
        u64_field("projected_char_start", false),
        u64_field("projected_char_end", false),
        utf8("oracle_source", false),
        utf8("classification", false),
        utf8("winning_analyzer", true),
        utf8_list("losing_analyzers"),
        utf8("evidence_detail", false),
        utf8("adjudicated_reading", true),
    ])
}

fn nway_feature_diffs_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        u64_field("region_index", false),
        utf8("feature_key", false),
        utf8("scope_type", false),
        u64_field("scope_position", true),
        utf8("scope_surface", true),
        utf8("feature_value", true),
        utf8_list("analyzers"),
    ])
}

fn feature_pattern_counts_schema() -> Arc<Schema> {
    schema(vec![
        utf8("kind", false),
        utf8("feature_profile", false),
        utf8("feature_key", false),
        bool_field("is_nonempty_whitespace"),
        utf8("pattern", false),
        u64_field("examples", false),
        u64_field("source_count", false),
        u64_field("text_count", false),
        utf8("sample_source_ids", false),
        utf8("sample_text_ids", false),
        utf8("script_categories", false),
    ])
}

fn errors_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", true),
        utf8("text_id", true),
        utf8("analyzer_id", true),
        utf8("stage", false),
        utf8("error_code", false),
        utf8("message", false),
    ])
}

#[cfg(test)]
mod tests {
    use std::io::Write;
    use std::time::{SystemTime, UNIX_EPOCH};

    use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

    use super::*;

    #[test]
    fn writer_properties_use_bounded_row_groups_for_large_string_tables() {
        assert_eq!(writer_properties(3).max_row_group_size(), 50_000);
    }

    #[test]
    fn writer_properties_honor_zstd_level() {
        let props = writer_properties(1);
        assert_eq!(props.max_row_group_size(), 50_000);
        assert!(matches!(
            props.compression(&"any".into()),
            parquet::basic::Compression::ZSTD(_)
        ));
        // `writer_properties` should accept the full validated CLI range
        // without panicking.
        writer_properties(22);
    }

    #[test]
    fn create_removes_stale_staging_for_same_run_id() {
        let root = temp_dir("stale");
        let stale = root.join(".staging/run-a.12345");
        fs::create_dir_all(&stale).unwrap();
        fs::write(stale.join("sentinel"), b"stale").unwrap();

        let paths = WarehousePaths::new(&root, "run-a");
        let writer = WarehouseWriter::create(paths.clone()).unwrap();

        assert!(!stale.exists());
        assert!(paths.staging_dir.exists());
        drop(writer);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn finalize_moves_complete_run_out_of_staging() {
        let root = temp_dir("finalize");
        let paths = WarehousePaths::new(&root, "run-a");
        let writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.finalize().unwrap();

        assert!(!paths.staging_dir.exists());
        assert!(paths.final_dir.exists());
        assert!(parquet_file_exists(&paths.final_dir, WarehouseTable::Runs));
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn finalize_refuses_to_overwrite_existing_run() {
        let root = temp_dir("overwrite");
        let paths = WarehousePaths::new(&root, "run-a");
        fs::create_dir_all(&paths.final_dir).unwrap();

        let err = match WarehouseWriter::create(paths) {
            Ok(_) => panic!("expected existing run error"),
            Err(error) => error.to_string(),
        };

        assert!(err.contains("already exists"));
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn empty_tables_are_valid_parquet_files() {
        let root = temp_dir("empty-tables");
        let paths = WarehousePaths::new(&root, "run-a");
        let writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.finalize().unwrap();

        for table in WarehouseTable::ALL {
            assert!(
                paths.final_table_path(*table).is_file(),
                "missing {:?}",
                table
            );
        }

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn empty_parquet_schemas_match_documented_columns() {
        let root = temp_dir("empty-schema");
        let paths = WarehousePaths::new(&root, "run-a");
        let writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.finalize().unwrap();

        for table in WarehouseTable::ALL {
            let file = File::open(paths.final_table_path(*table)).unwrap();
            let builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
            let actual: Vec<_> = builder
                .schema()
                .fields()
                .iter()
                .map(|field| field.name().as_str())
                .collect();
            assert_eq!(
                actual,
                table.column_names(),
                "schema mismatch for {table:?}"
            );
        }

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn writes_projection_spans_rows() {
        let root = temp_dir("projection-spans");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_projection_spans(&[ProjectionSpanRow {
                run_id: Arc::from("run-a"),
                source_id: Arc::from("source-a"),
                text_id: Arc::from("work-a"),
                projected_char_start: 0,
                projected_char_end: 3,
                aat_pointer: "/blocks/0/content/1".to_owned(),
                inline_kind: "ruby".to_owned(),
                is_ruby_base: true,
                is_gaiji: false,
                is_note: false,
            }])
            .unwrap();
        writer.finalize().unwrap();

        assert_eq!(
            parquet_table_row_count(&paths.final_dir, WarehouseTable::ProjectionSpans).unwrap(),
            1
        );
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn writes_nway_region_oracle_evidence_rows() {
        let root = temp_dir("oracle-evidence");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_nway_region_oracle_evidence(&[NwayRegionOracleEvidenceRow {
                run_id: "run-a".to_owned(),
                source_id: "source-a".to_owned(),
                text_id: "work-a".to_owned(),
                region_index: 3,
                projected_char_start: 10,
                projected_char_end: 12,
                oracle_source: "ruby".to_owned(),
                classification: "resolved".to_owned(),
                winning_analyzer: Some("sudachi-c".to_owned()),
                losing_analyzers: vec!["vibrato:unidic-novel-202512".to_owned()],
                evidence_detail: r#"{"classification":"resolved"}"#.to_owned(),
                adjudicated_reading: Some("とうきょう".to_owned()),
            }])
            .unwrap();
        writer.finalize().unwrap();
        assert_eq!(
            parquet_table_row_count(&paths.final_dir, WarehouseTable::NwayRegionOracleEvidence)
                .unwrap(),
            1
        );
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn oracle_classification_column_round_trips() {
        let root = temp_dir("oracle-cls");
        let paths = WarehousePaths::new(&root, "run-cls");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_nway_region_oracle_evidence(&[NwayRegionOracleEvidenceRow {
                run_id: "run-cls".into(),
                source_id: "s".into(),
                text_id: "t".into(),
                region_index: 0,
                projected_char_start: 0,
                projected_char_end: 2,
                oracle_source: "ruby".into(),
                classification: "no_comparable_reading".into(),
                winning_analyzer: None,
                losing_analyzers: vec!["vibrato".into(), "sudachi-c".into()],
                evidence_detail: "{}".into(),
                adjudicated_reading: None,
            }])
            .unwrap();
        writer.finalize().unwrap();

        let file = File::open(
            paths
                .final_dir
                .join(WarehouseTable::NwayRegionOracleEvidence.file_name()),
        )
        .unwrap();
        let mut reader = ParquetRecordBatchReaderBuilder::try_new(file)
            .unwrap()
            .build()
            .unwrap();
        let batch = reader.next().unwrap().unwrap();
        let col = batch
            .column_by_name("classification")
            .unwrap()
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap();
        assert_eq!(col.value(0), "no_comparable_reading");
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn writes_non_empty_runs_and_surface_lists() {
        let root = temp_dir("non-empty");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: 1,
                run_id: "run-a".to_owned(),
                created_at_utc: "2026-05-01T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 1,
                error_count: 0,
                ortho_detect_mode: "off".to_owned(),
                input_normalization_detector_id: None,
                input_normalization_policy_hash: "sha256:identity".to_owned(),
            }])
            .unwrap();
        writer
            .append_nway_region_analyzers(&[NwayRegionAnalyzerRow {
                run_id: "run-a".into(),
                source_id: "source-a".into(),
                text_id: "work-a".into(),
                region_index: 0,
                analyzer_id: "sudachi-c".into(),
                covers_exactly: true,
                morpheme_start: 0,
                morpheme_end: 2,
                surfaces: vec!["今".to_owned(), "日".to_owned()],
            }])
            .unwrap();
        writer.finalize().unwrap();

        assert!(paths.final_table_path(WarehouseTable::Runs).is_file());
        assert!(
            paths
                .final_table_path(WarehouseTable::NwayRegionAnalyzers)
                .is_file()
        );
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn write_time_accumulates_across_appends() {
        use std::time::Duration;
        let root = temp_dir("write-time");
        let paths = WarehousePaths::new(&root, "wt-run");
        let mut writer = WarehouseWriter::create_for_tables(paths, WarehouseTable::ALL, 3).unwrap();
        assert_eq!(writer.write_time(), Duration::ZERO);

        // Append enough error rows to force at least one write() call.
        let rows: Vec<ErrorRow> = (0..1000)
            .map(|i| ErrorRow {
                run_id: "wt-run".to_owned(),
                source_id: Some(format!("source-{i}")),
                text_id: Some(format!("work-{i}")),
                analyzer_id: Some("sudachi-c".to_owned()),
                stage: "analysis".to_owned(),
                error_code: "E_TEST".to_owned(),
                message: format!("synthetic error {i}"),
            })
            .collect();
        writer.append_errors(&rows).unwrap();
        assert!(
            writer.write_time() > Duration::ZERO,
            "append must record write time"
        );

        let before_close = writer.write_time();
        let total = writer.finalize().unwrap();
        assert!(total >= before_close, "finalize folds in close() time");

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn morpheme_features_direct_builder_matches_reference_bytes() {
        // Covers: repeated ids across rows, a `None` feature_value, and
        // multiple analyzers/morphemes, exercising the byte layout the
        // 7-column schema must preserve.
        let run_id: Arc<str> = Arc::from("run-a");
        let source_id: Arc<str> = Arc::from("source-a");
        let text_id: Arc<str> = Arc::from("work-a");
        let analyzer_a: Arc<str> = Arc::from("vibrato:unidic");
        let analyzer_b: Arc<str> = Arc::from("sudachi-c");
        let rows = vec![
            MorphemeFeatureRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                analyzer_id: Arc::clone(&analyzer_a),
                morpheme_index: 0,
                feature_key: Arc::from("pos1"),
                feature_value: Some(Arc::from("名詞")),
            },
            MorphemeFeatureRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                analyzer_id: Arc::clone(&analyzer_a),
                morpheme_index: 0,
                feature_key: Arc::from("lemma"),
                feature_value: None,
            },
            MorphemeFeatureRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                analyzer_id: Arc::clone(&analyzer_a),
                morpheme_index: 1,
                feature_key: Arc::from("pos1"),
                feature_value: Some(Arc::from("助詞")),
            },
            MorphemeFeatureRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                analyzer_id: Arc::clone(&analyzer_b),
                morpheme_index: 0,
                feature_key: Arc::from("pos1"),
                feature_value: Some(Arc::from("名詞")),
            },
        ];

        let root = temp_dir("mf-byte-identity");

        let reference_paths = WarehousePaths::new(root.join("reference"), "run-a");
        let mut reference_writer = WarehouseWriter::create_for_tables(
            reference_paths.clone(),
            &[WarehouseTable::MorphemeFeatures],
            3,
        )
        .unwrap();
        reference_writer
            .append_morpheme_features_reference(&rows)
            .unwrap();
        reference_writer.finalize().unwrap();

        let direct_paths = WarehousePaths::new(root.join("direct"), "run-a");
        let mut direct_writer = WarehouseWriter::create_for_tables(
            direct_paths.clone(),
            &[WarehouseTable::MorphemeFeatures],
            3,
        )
        .unwrap();
        let mut columns = MorphemeFeaturesColumns::new();
        for row in &rows {
            columns.push_row(
                row.run_id.as_ref(),
                row.source_id.as_ref(),
                row.text_id.as_ref(),
                row.analyzer_id.as_ref(),
                row.morpheme_index,
                row.feature_key.as_ref(),
                row.feature_value.as_deref(),
            );
        }
        direct_writer
            .append_morpheme_feature_columns(columns)
            .unwrap();
        direct_writer.finalize().unwrap();

        let reference_bytes =
            fs::read(reference_paths.final_table_path(WarehouseTable::MorphemeFeatures)).unwrap();
        let direct_bytes =
            fs::read(direct_paths.final_table_path(WarehouseTable::MorphemeFeatures)).unwrap();
        assert_eq!(
            reference_bytes, direct_bytes,
            "direct-builder parquet bytes must match the reference transposition exactly"
        );

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn nway_feature_diffs_direct_builder_matches_reference_bytes() {
        // Covers: `None` scope_position (whole_region scope), `None`
        // scope_surface (whole_region/token_position scopes), `None`
        // feature_value, and multiple analyzers per value-group, exercising
        // the null handling and byte layout the 10-column schema must
        // preserve.
        let run_id: Arc<str> = Arc::from("run-a");
        let source_id: Arc<str> = Arc::from("source-a");
        let text_id: Arc<str> = Arc::from("work-a");
        let rows = vec![
            // whole_region scope: scope_position and scope_surface both None.
            NwayFeatureDiffRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                region_index: 0,
                feature_key: Arc::from("pos1"),
                scope_type: Arc::from("whole_region"),
                scope_position: None,
                scope_surface: None,
                feature_value: Some(Arc::from("名詞")),
                analyzers: vec![Arc::from("vibrato")],
            },
            // Same group, second analyzer, and a `None` feature_value.
            NwayFeatureDiffRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                region_index: 0,
                feature_key: Arc::from("pos1"),
                scope_type: Arc::from("whole_region"),
                scope_position: None,
                scope_surface: None,
                feature_value: None,
                analyzers: vec![Arc::from("sudachi-a")],
            },
            // token_position scope: scope_position set, scope_surface still None.
            NwayFeatureDiffRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                region_index: 1,
                feature_key: Arc::from("pos2"),
                scope_type: Arc::from("token_position"),
                scope_position: Some(0),
                scope_surface: None,
                feature_value: Some(Arc::from("A")),
                analyzers: vec![Arc::from("vibrato")],
            },
            NwayFeatureDiffRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                region_index: 1,
                feature_key: Arc::from("pos2"),
                scope_type: Arc::from("token_position"),
                scope_position: Some(0),
                scope_surface: None,
                feature_value: Some(Arc::from("B")),
                analyzers: vec![Arc::from("sudachi-c")],
            },
            // surface scope: scope_surface set, scope_position None, plus a
            // third analyzer sharing this value group.
            NwayFeatureDiffRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                region_index: 1,
                feature_key: Arc::from("pos3"),
                scope_type: Arc::from("surface"),
                scope_position: None,
                scope_surface: Some(Arc::from("東京")),
                feature_value: Some(Arc::from("E")),
                analyzers: vec![Arc::from("vibrato")],
            },
            NwayFeatureDiffRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                region_index: 1,
                feature_key: Arc::from("pos3"),
                scope_type: Arc::from("surface"),
                scope_position: None,
                scope_surface: Some(Arc::from("東京")),
                feature_value: Some(Arc::from("E")),
                analyzers: vec![Arc::from("sudachi-a")],
            },
            NwayFeatureDiffRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                region_index: 1,
                feature_key: Arc::from("pos3"),
                scope_type: Arc::from("surface"),
                scope_position: None,
                scope_surface: Some(Arc::from("東京")),
                feature_value: None,
                analyzers: vec![Arc::from("sudachi-c")],
            },
            // A value group shared by two analyzers in one row (sorted
            // ascending), exercising the new list column's multi-element case.
            NwayFeatureDiffRow {
                run_id: Arc::clone(&run_id),
                source_id: Arc::clone(&source_id),
                text_id: Arc::clone(&text_id),
                region_index: 0,
                feature_key: Arc::from("pos"),
                scope_type: Arc::from("whole_region"),
                scope_position: None,
                scope_surface: None,
                feature_value: Some(Arc::from("名詞")),
                analyzers: vec![Arc::from("sudachi-c"), Arc::from("vibrato")],
            },
        ];

        let root = temp_dir("nfd-byte-identity");

        let reference_paths = WarehousePaths::new(root.join("reference"), "run-a");
        let mut reference_writer = WarehouseWriter::create_for_tables(
            reference_paths.clone(),
            &[WarehouseTable::NwayFeatureDiffs],
            3,
        )
        .unwrap();
        reference_writer.append_nway_feature_diffs(&rows).unwrap();
        reference_writer.finalize().unwrap();

        let direct_paths = WarehousePaths::new(root.join("direct"), "run-a");
        let mut direct_writer = WarehouseWriter::create_for_tables(
            direct_paths.clone(),
            &[WarehouseTable::NwayFeatureDiffs],
            3,
        )
        .unwrap();
        let mut columns = NwayFeatureDiffsColumns::new();
        for row in &rows {
            columns.push_row(
                row.run_id.as_ref(),
                row.source_id.as_ref(),
                row.text_id.as_ref(),
                row.region_index,
                row.feature_key.as_ref(),
                row.scope_type.as_ref(),
                row.scope_position,
                row.scope_surface.as_deref(),
                row.feature_value.as_deref(),
                row.analyzers.as_slice(),
            );
        }
        direct_writer
            .append_nway_feature_diff_columns(columns)
            .unwrap();
        direct_writer.finalize().unwrap();

        let reference_bytes =
            fs::read(reference_paths.final_table_path(WarehouseTable::NwayFeatureDiffs)).unwrap();
        let direct_bytes =
            fs::read(direct_paths.final_table_path(WarehouseTable::NwayFeatureDiffs)).unwrap();
        assert_eq!(
            reference_bytes, direct_bytes,
            "direct-builder parquet bytes must match the reference transposition exactly"
        );

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn nway_feature_diffs_analyzers_is_non_null_sorted_list() {
        use arrow_array::{Array, ListArray};
        let mut columns = NwayFeatureDiffsColumns::new();
        columns.push_row(
            "r",
            "s",
            "t",
            0,
            "pos",
            "whole_region",
            None,
            None,
            Some("名詞"),
            &["sudachi-c", "vibrato"],
        );
        let batch = columns.finish();
        // Field: List<Utf8>, list itself non-null.
        let field = batch.schema().field(9).clone();
        assert_eq!(field.name(), "analyzers");
        assert!(
            !field.is_nullable(),
            "analyzers list column must be non-null"
        );
        let list = batch
            .column(9)
            .as_any()
            .downcast_ref::<ListArray>()
            .unwrap();
        assert!(!list.is_null(0), "no null list entries");
        let values = list.value(0);
        let strs = values
            .as_any()
            .downcast_ref::<arrow_array::StringArray>()
            .unwrap();
        let got: Vec<&str> = (0..strs.len()).map(|i| strs.value(i)).collect();
        assert_eq!(
            got,
            vec!["sudachi-c", "vibrato"],
            "elements preserved in ascending order, non-empty"
        );
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic(expected = "non-empty")]
    fn push_row_rejects_empty_analyzers() {
        let mut columns = NwayFeatureDiffsColumns::new();
        let empty: &[&str] = &[];
        columns.push_row(
            "r",
            "s",
            "t",
            0,
            "pos",
            "whole_region",
            None,
            None,
            Some("名詞"),
            empty,
        );
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic(expected = "sorted ascending")]
    fn push_row_rejects_unsorted_analyzers() {
        let mut columns = NwayFeatureDiffsColumns::new();
        columns.push_row(
            "r",
            "s",
            "t",
            0,
            "pos",
            "whole_region",
            None,
            None,
            Some("名詞"),
            &["vibrato", "sudachi-c"],
        );
    }

    #[test]
    fn append_parquet_table_file_coalesces_tiny_row_groups() {
        let root = temp_dir("coalesce-row-groups");
        fs::create_dir_all(&root).unwrap();
        let shard_sources = root.join("shard-sources.parquet");
        let rows = (0..100)
            .map(|index| SourceRow {
                run_id: "run-a".to_owned(),
                source_id: format!("source-{index}"),
                text_id: format!("work-{index}"),
                aat_path: format!("aat/{index}.json"),
                source_bytes: 10,
                source_chars: 10,
            })
            .collect::<Vec<_>>();
        let mut shard = ArrowWriter::try_new(
            File::create(&shard_sources).unwrap(),
            sources_schema(),
            Some(
                WriterProperties::builder()
                    .set_max_row_group_size(1)
                    .build(),
            ),
        )
        .unwrap();
        let mut discard_write_time = std::time::Duration::ZERO;
        write_batch(
            &mut shard,
            sources_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
                string_array(rows.iter().map(|row| row.aat_path.as_str())),
                u64_array(rows.iter().map(|row| row.source_bytes)),
                u64_array(rows.iter().map(|row| row.source_chars)),
            ],
            &mut discard_write_time,
        )
        .unwrap();
        shard.close().unwrap();
        assert!(
            row_group_count(&shard_sources) > 10,
            "fixture should create many source row groups"
        );

        let merged_paths = WarehousePaths::new(root.join("merged"), "run-a");
        let mut merged = WarehouseWriter::create(merged_paths.clone()).unwrap();
        append_parquet_table_file(&mut merged, WarehouseTable::Sources, &shard_sources).unwrap();
        merged.finalize().unwrap();

        assert_eq!(
            parquet_table_row_count(&merged_paths.final_dir, WarehouseTable::Sources).unwrap(),
            100
        );
        assert!(
            row_group_count(&merged_paths.final_table_path(WarehouseTable::Sources)) <= 2,
            "merged file should not preserve one row group per tiny shard batch"
        );

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn parquet_table_row_count_sums_partitioned_table_dir() {
        let root = temp_dir("partition-count");
        let part_dir = root.join("run").join(WarehouseTable::Sources.file_name());
        fs::create_dir_all(&part_dir).unwrap();
        write_sources_part(&part_dir.join("part-00000.parquet"), "a", 2);
        write_sources_part(&part_dir.join("part-00001.parquet"), "b", 3);

        assert_eq!(
            parquet_table_row_count(&root.join("run"), WarehouseTable::Sources).unwrap(),
            5
        );

        let _ = fs::remove_dir_all(root);
    }

    fn write_sources_part(path: &Path, prefix: &str, count: usize) {
        let rows = (0..count)
            .map(|index| SourceRow {
                run_id: "run-a".to_owned(),
                source_id: format!("{prefix}-source-{index}"),
                text_id: format!("{prefix}-work-{index}"),
                aat_path: format!("aat/{prefix}-{index}.json"),
                source_bytes: 10,
                source_chars: 10,
            })
            .collect::<Vec<_>>();
        let mut writer = ArrowWriter::try_new(
            File::create(path).unwrap(),
            sources_schema(),
            Some(writer_properties(3)),
        )
        .unwrap();
        let mut discard_write_time = std::time::Duration::ZERO;
        write_batch(
            &mut writer,
            sources_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
                string_array(rows.iter().map(|row| row.aat_path.as_str())),
                u64_array(rows.iter().map(|row| row.source_bytes)),
                u64_array(rows.iter().map(|row| row.source_chars)),
            ],
            &mut discard_write_time,
        )
        .unwrap();
        writer.close().unwrap();
    }

    #[test]
    fn compact_staged_table_coalesces_many_small_parts_into_one_file() {
        // Build a staging dir with 65 tiny part files for Sources (just over the
        // 64-part threshold, each well under 1 MiB median).
        let root = temp_dir("compact-many-small");
        let paths = WarehousePaths::new(&root, "r");
        let sources_staged = paths.staging_dir.join(WarehouseTable::Sources.file_name());
        fs::create_dir_all(&sources_staged).unwrap();
        for i in 0..65u32 {
            let path = sources_staged.join(format!("part-{i:05}.parquet"));
            write_sources_part(&path, &format!("s{i}"), 1); // tiny readable parquet
        }
        let compacted = compact_staged_table(&paths, WarehouseTable::Sources, 3).unwrap();
        assert!(compacted, "should compact (65 parts, median <1 MiB)");
        let remaining: Vec<_> = fs::read_dir(&sources_staged)
            .unwrap()
            .filter_map(Result::ok)
            .filter(|e| e.path().extension().is_some_and(|x| x == "parquet"))
            .collect();
        assert_eq!(remaining.len(), 1, "coalesced to a single file");
    }

    #[test]
    fn compact_staged_table_skips_when_median_too_large() {
        // 65 parts but each >1 MiB → median ≥ 1 MiB → skip (no compaction).
        let root = temp_dir("compact-skip-large");
        let paths = WarehousePaths::new(&root, "r");
        let sources_staged = paths.staging_dir.join(WarehouseTable::Sources.file_name());
        fs::create_dir_all(&sources_staged).unwrap();
        for i in 0..65u32 {
            let path = sources_staged.join(format!("part-{i:05}.parquet"));
            write_sources_part(&path, &format!("s{i}"), 1);
            // Pad the file to >1 MiB so median crosses threshold.
            let mut f = std::fs::OpenOptions::new()
                .append(true)
                .open(&path)
                .unwrap();
            f.write_all(&vec![0u8; 1_100_000]).unwrap();
        }
        let compacted = compact_staged_table(&paths, WarehouseTable::Sources, 3).unwrap();
        assert!(!compacted, "should NOT compact (median ≥1 MiB)");
        let remaining: Vec<_> = fs::read_dir(&sources_staged)
            .unwrap()
            .filter_map(Result::ok)
            .filter(|e| e.path().extension().is_some_and(|x| x == "parquet"))
            .collect();
        assert_eq!(remaining.len(), 65, "parts unchanged");
    }

    #[test]
    fn parquet_table_part_sizes_returns_sorted_sizes_ignoring_non_parquet() {
        let root = temp_dir("part-sizes");
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join("part-00001.parquet"), b"longer-junk-bytes").unwrap();
        fs::write(root.join("part-00000.parquet"), b"short").unwrap();
        fs::write(root.join("views.sql"), b"select 1").unwrap();
        let sizes = parquet_table_part_sizes(&root).unwrap();
        assert_eq!(
            sizes,
            vec![b"short".len() as u64, b"longer-junk-bytes".len() as u64]
        );
    }

    fn row_group_count(path: &Path) -> usize {
        let file = File::open(path).unwrap();
        let builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
        builder.metadata().num_row_groups()
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-warehouse-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
