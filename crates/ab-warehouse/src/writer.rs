#![allow(clippy::missing_errors_doc)]

use std::fs::{self, File};
use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result, bail};
use arrow_array::builder::{ListBuilder, StringBuilder};
use arrow_array::{ArrayRef, BooleanArray, RecordBatch, StringArray, UInt32Array, UInt64Array};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::ArrowWriter;
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
use parquet::basic::{Compression, ZstdLevel};
use parquet::file::properties::WriterProperties;

use crate::schema::{
    AnalysisRow, ErrorRow, FeaturePatternCountRow, MorphemeFeatureRow, MorphemeRow,
    NwayFeatureDiffRow, NwayRegionAnalyzerRow, NwayRegionRow, RunAnalyzerRow, RunRow, SourceRow,
    WarehousePaths, WarehouseTable,
};

const WAREHOUSE_MAX_ROW_GROUP_SIZE: usize = 50_000;

pub struct WarehouseWriter {
    paths: WarehousePaths,
    runs: Option<ArrowWriter<File>>,
    run_analyzers: Option<ArrowWriter<File>>,
    sources: Option<ArrowWriter<File>>,
    analyses: Option<ArrowWriter<File>>,
    morphemes: Option<ArrowWriter<File>>,
    morpheme_features: Option<ArrowWriter<File>>,
    nway_regions: Option<ArrowWriter<File>>,
    nway_region_analyzers: Option<ArrowWriter<File>>,
    nway_feature_diffs: Option<ArrowWriter<File>>,
    feature_pattern_counts: Option<ArrowWriter<File>>,
    errors: Option<ArrowWriter<File>>,
}

impl WarehouseWriter {
    #[allow(dead_code)]
    pub fn create(paths: WarehousePaths) -> Result<Self> {
        Self::create_for_tables(paths, WarehouseTable::ALL)
    }

    pub fn create_for_tables(paths: WarehousePaths, tables: &[WarehouseTable]) -> Result<Self> {
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
            runs: open_optional_table_writer(&paths, tables, WarehouseTable::Runs, runs_schema())?,
            run_analyzers: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::RunAnalyzers,
                run_analyzers_schema(),
            )?,
            sources: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Sources,
                sources_schema(),
            )?,
            analyses: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Analyses,
                analyses_schema(),
            )?,
            morphemes: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Morphemes,
                morphemes_schema(),
            )?,
            morpheme_features: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::MorphemeFeatures,
                morpheme_features_schema(),
            )?,
            nway_regions: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::NwayRegions,
                nway_regions_schema(),
            )?,
            nway_region_analyzers: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::NwayRegionAnalyzers,
                nway_region_analyzers_schema(),
            )?,
            nway_feature_diffs: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::NwayFeatureDiffs,
                nway_feature_diffs_schema(),
            )?,
            feature_pattern_counts: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::FeaturePatternCounts,
                feature_pattern_counts_schema(),
            )?,
            errors: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::Errors,
                errors_schema(),
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
            WarehouseTable::Analyses => self.analyses.is_some(),
            WarehouseTable::Morphemes => self.morphemes.is_some(),
            WarehouseTable::MorphemeFeatures => self.morpheme_features.is_some(),
            WarehouseTable::NwayRegions => self.nway_regions.is_some(),
            WarehouseTable::NwayRegionAnalyzers => self.nway_region_analyzers.is_some(),
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
            ],
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
        )
    }

    pub fn append_morpheme_features(&mut self, rows: &[MorphemeFeatureRow]) -> Result<()> {
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
                string_array(rows.iter().map(|row| row.feature_key.as_str())),
                nullable_string_array(rows.iter().map(|row| row.feature_value.as_deref())),
            ],
        )
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
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
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
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
                u64_array(rows.iter().map(|row| row.region_index)),
                string_array(rows.iter().map(|row| row.analyzer_id.as_str())),
                bool_array(rows.iter().map(|row| row.covers_exactly)),
                u64_array(rows.iter().map(|row| row.morpheme_start)),
                u64_array(rows.iter().map(|row| row.morpheme_end)),
                string_list_array(rows.iter().map(|row| row.surfaces.as_slice())),
            ],
        )
    }

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
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
                u64_array(rows.iter().map(|row| row.region_index)),
                string_array(rows.iter().map(|row| row.feature_key.as_str())),
                string_array(rows.iter().map(|row| row.scope_type.as_str())),
                nullable_u64_array(rows.iter().map(|row| row.scope_position)),
                nullable_string_array(rows.iter().map(|row| row.scope_surface.as_deref())),
                nullable_string_array(rows.iter().map(|row| row.feature_value.as_deref())),
                string_array(rows.iter().map(|row| row.analyzer_id.as_str())),
            ],
        )
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
        )
    }

    /// Write a raw `RecordBatch` into the parquet writer for `table`. Used by
    /// merge-time compaction (via [`append_parquet_table_file`]) to stream
    /// batches read from staged parts into the destination writer.
    ///
    /// # Errors
    ///
    /// Returns an error if the underlying writer fails to write the batch.
    pub fn append_record_batch(&mut self, table: WarehouseTable, batch: RecordBatch) -> Result<()> {
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
        Ok(())
    }

    pub fn finalize(mut self) -> Result<()> {
        close_writer(self.runs.take())?;
        close_writer(self.run_analyzers.take())?;
        close_writer(self.sources.take())?;
        close_writer(self.analyses.take())?;
        close_writer(self.morphemes.take())?;
        close_writer(self.morpheme_features.take())?;
        close_writer(self.nway_regions.take())?;
        close_writer(self.nway_region_analyzers.take())?;
        close_writer(self.nway_feature_diffs.take())?;
        close_writer(self.feature_pattern_counts.take())?;
        close_writer(self.errors.take())?;
        crate::sql::write_run_views_sql(&self.paths.staging_dir, &self.paths.final_dir)?;
        finalize_staging_run(&self.paths)
    }
}

/// Append every row from a parquet file into `writer`'s table, re-using the
/// writer's `WAREHOUSE_MAX_ROW_GROUP_SIZE` row-group sizing. Used by merge-time
/// compaction to coalesce many tiny staged parts into one well-sized file.
///
/// # Errors
///
/// Returns an error if `path` cannot be opened or its batches fail to write.
pub fn append_parquet_table_file(
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
pub fn parquet_table_part_sizes(dir: &Path) -> Result<Vec<u64>> {
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
) -> Result<ArrowWriter<File>> {
    let file = File::create(paths.staging_table_path(table))?;
    Ok(ArrowWriter::try_new(
        file,
        schema,
        Some(writer_properties()),
    )?)
}

fn open_optional_table_writer(
    paths: &WarehousePaths,
    tables: &[WarehouseTable],
    table: WarehouseTable,
    schema: Arc<Schema>,
) -> Result<Option<ArrowWriter<File>>> {
    if tables.contains(&table) {
        open_table_writer(paths, table, schema).map(Some)
    } else {
        Ok(None)
    }
}

fn writer_properties() -> WriterProperties {
    WriterProperties::builder()
        .set_max_row_group_size(WAREHOUSE_MAX_ROW_GROUP_SIZE)
        .set_compression(Compression::ZSTD(
            ZstdLevel::try_new(3).expect("valid zstd level"),
        ))
        .build()
}

fn write_batch<W: std::io::Write + Send>(
    writer: &mut ArrowWriter<W>,
    schema: Arc<Schema>,
    columns: Vec<ArrayRef>,
) -> Result<()> {
    let batch = RecordBatch::try_new(schema, columns)?;
    writer.write(&batch)?;
    Ok(())
}

fn close_writer(writer: Option<ArrowWriter<File>>) -> Result<()> {
    if let Some(writer) = writer {
        writer.close()?;
    }
    Ok(())
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

fn string_list_array<'a>(values: impl Iterator<Item = &'a [String]>) -> ArrayRef {
    let mut builder = ListBuilder::new(StringBuilder::new());
    for list in values {
        for value in list {
            builder.values().append_value(value);
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
        utf8("analyzer_id", false),
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
    use std::time::{SystemTime, UNIX_EPOCH};

    use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

    use super::*;

    #[test]
    fn writer_properties_use_bounded_row_groups_for_large_string_tables() {
        assert_eq!(writer_properties().max_row_group_size(), 50_000);
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
            }])
            .unwrap();
        writer
            .append_nway_region_analyzers(&[NwayRegionAnalyzerRow {
                run_id: "run-a".to_owned(),
                source_id: "source-a".to_owned(),
                text_id: "work-a".to_owned(),
                region_index: 0,
                analyzer_id: "sudachi-c".to_owned(),
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
            Some(writer_properties()),
        )
        .unwrap();
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
        )
        .unwrap();
        writer.close().unwrap();
    }

    #[test]
    fn parquet_table_part_sizes_returns_sorted_sizes_ignoring_non_parquet() {
        let root = temp_dir("part-sizes");
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join("part-00001.parquet"), b"longer-junk-bytes").unwrap();
        fs::write(root.join("part-00000.parquet"), b"short").unwrap();
        fs::write(root.join("views.sql"), b"select 1").unwrap();
        let sizes = parquet_table_part_sizes(&root).unwrap();
        assert_eq!(sizes, vec![b"short".len() as u64, b"longer-junk-bytes".len() as u64]);
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
