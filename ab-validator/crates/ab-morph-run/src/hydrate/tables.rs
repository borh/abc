//! Streaming, key-filtered readers over warehouse parquet. Full-corpus
//! morphemes.parquet is tens of GB; every reader here visits one
//! `RecordBatch` at a time and retains only rows matching the wanted keys
//! (`crate::summary::read_warehouse_table` collects a whole table into
//! memory and must not be used for `morphemes`/`morpheme_features`).

use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::path::Path;

use anyhow::{Context, Result};
use arrow_array::{
    Array, BooleanArray, Int32Array, ListArray, RecordBatch, StringArray, UInt64Array,
};
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

use crate::warehouse::schema::WarehouseTable;

/// Streams every `RecordBatch` of `table` under `run_dir` through `f`,
/// without collecting the table into memory. Handles both a single
/// `<table>.parquet` file and a directory of part files (visited in sorted
/// order), mirroring `summary::read_warehouse_table`'s dir-of-parts logic.
pub fn for_each_table_batch(
    run_dir: &Path,
    table: WarehouseTable,
    mut f: impl FnMut(&RecordBatch) -> Result<()>,
) -> Result<()> {
    let path = run_dir.join(table.file_name());
    if path.is_dir() {
        let mut parts: Vec<_> = fs::read_dir(&path)
            .with_context(|| format!("failed to read {}", path.display()))?
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<std::result::Result<Vec<_>, _>>()
            .with_context(|| format!("failed to read {}", path.display()))?;
        parts.retain(|part| part.extension().is_some_and(|ext| ext == "parquet"));
        parts.sort();
        for part in parts {
            for_each_file_batch(&part, &mut f)?;
        }
        return Ok(());
    }
    for_each_file_batch(&path, &mut f)
}

fn for_each_file_batch(path: &Path, f: &mut impl FnMut(&RecordBatch) -> Result<()>) -> Result<()> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .with_context(|| format!("failed to read parquet metadata from {}", path.display()))?
        .build()
        .with_context(|| format!("failed to build parquet reader for {}", path.display()))?;
    for batch in reader {
        let batch = batch.with_context(|| format!("failed to read {}", path.display()))?;
        f(&batch)?;
    }
    Ok(())
}

fn str_col<'a>(batch: &'a RecordBatch, name: &str) -> Result<&'a StringArray> {
    let index = batch
        .schema()
        .index_of(name)
        .with_context(|| format!("missing column {name}"))?;
    batch
        .column(index)
        .as_any()
        .downcast_ref::<StringArray>()
        .with_context(|| format!("column {name} is not a StringArray"))
}

fn u64_col<'a>(batch: &'a RecordBatch, name: &str) -> Result<&'a UInt64Array> {
    let index = batch
        .schema()
        .index_of(name)
        .with_context(|| format!("missing column {name}"))?;
    batch
        .column(index)
        .as_any()
        .downcast_ref::<UInt64Array>()
        .with_context(|| format!("column {name} is not a UInt64Array"))
}

fn bool_col<'a>(batch: &'a RecordBatch, name: &str) -> Result<&'a BooleanArray> {
    let index = batch
        .schema()
        .index_of(name)
        .with_context(|| format!("missing column {name}"))?;
    batch
        .column(index)
        .as_any()
        .downcast_ref::<BooleanArray>()
        .with_context(|| format!("column {name} is not a BooleanArray"))
}

fn list_str_col<'a>(batch: &'a RecordBatch, name: &str) -> Result<&'a ListArray> {
    let index = batch
        .schema()
        .index_of(name)
        .with_context(|| format!("missing column {name}"))?;
    batch
        .column(index)
        .as_any()
        .downcast_ref::<ListArray>()
        .with_context(|| format!("column {name} is not a ListArray"))
}

fn list_str_value(array: &ListArray, row: usize) -> Result<Vec<String>> {
    if array.is_null(row) {
        return Ok(Vec::new());
    }
    let values = array.value(row);
    let values = values
        .as_any()
        .downcast_ref::<StringArray>()
        .context("list value is not a StringArray")?;
    Ok((0..values.len())
        .map(|index| values.value(index).to_owned())
        .collect())
}

/// One row of `sources.parquet` for a wanted `source_id`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceInfo {
    pub text_id: String,
    pub aat_path: String,
    pub source_chars: u64,
}

/// Reads `sources.parquet`, keeping only rows whose `source_id` is in
/// `source_ids`. Key: `source_id`.
pub fn read_sources_for(
    run_dir: &Path,
    source_ids: &BTreeSet<String>,
) -> Result<BTreeMap<String, SourceInfo>> {
    let mut sources = BTreeMap::new();
    for_each_table_batch(run_dir, WarehouseTable::Sources, |batch| {
        let ids = str_col(batch, "source_id")?;
        let text_ids = str_col(batch, "text_id")?;
        let aat_paths = str_col(batch, "aat_path")?;
        let source_chars = u64_col(batch, "source_chars")?;
        for row in 0..batch.num_rows() {
            let source_id = ids.value(row);
            if !source_ids.contains(source_id) {
                continue;
            }
            sources.insert(
                source_id.to_owned(),
                SourceInfo {
                    text_id: text_ids.value(row).to_owned(),
                    aat_path: aat_paths.value(row).to_owned(),
                    source_chars: source_chars.value(row),
                },
            );
        }
        Ok(())
    })?;
    Ok(sources)
}

/// One row of `nway_region_analyzers.parquet` for a wanted `(source_id,
/// region_index)`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegionAnalyzerRow {
    pub analyzer_id: String,
    pub covers_exactly: bool,
    pub morpheme_start: u64,
    pub morpheme_end: u64,
    pub surfaces: Vec<String>,
}

/// Reads `nway_region_analyzers.parquet`, keeping only rows whose
/// `(source_id, region_index)` is in `wanted`. Key: `(source_id,
/// region_index)`; rows within each key are sorted by `analyzer_id`.
pub fn read_region_analyzers_for(
    run_dir: &Path,
    wanted: &BTreeSet<(String, u64)>,
) -> Result<BTreeMap<(String, u64), Vec<RegionAnalyzerRow>>> {
    // Build a borrowed lookup index: source_id (as &str) → set of wanted region_indices.
    // This avoids allocating a String for every row before the reject check.
    let wanted_by_source: BTreeMap<&str, BTreeSet<u64>> =
        wanted
            .iter()
            .fold(BTreeMap::new(), |mut acc, (source_id, region_idx)| {
                acc.entry(source_id.as_str())
                    .or_default()
                    .insert(*region_idx);
                acc
            });

    let mut regions: BTreeMap<(String, u64), Vec<RegionAnalyzerRow>> = BTreeMap::new();
    for_each_table_batch(run_dir, WarehouseTable::NwayRegionAnalyzers, |batch| {
        let source_ids = str_col(batch, "source_id")?;
        let region_indices = u64_col(batch, "region_index")?;
        let analyzer_ids = str_col(batch, "analyzer_id")?;
        let covers_exactly = bool_col(batch, "covers_exactly")?;
        let morpheme_starts = u64_col(batch, "morpheme_start")?;
        let morpheme_ends = u64_col(batch, "morpheme_end")?;
        let surfaces = list_str_col(batch, "surfaces")?;
        for row in 0..batch.num_rows() {
            let source_id = source_ids.value(row);
            let region_idx = region_indices.value(row);
            // Borrowed check first: does this source_id exist, and does it have this region_idx?
            if !wanted_by_source
                .get(source_id)
                .is_some_and(|regions| regions.contains(&region_idx))
            {
                continue;
            }
            // Only allocate the owned key after we know the row is wanted.
            let key = (source_id.to_owned(), region_idx);
            regions.entry(key).or_default().push(RegionAnalyzerRow {
                analyzer_id: analyzer_ids.value(row).to_owned(),
                covers_exactly: covers_exactly.value(row),
                morpheme_start: morpheme_starts.value(row),
                morpheme_end: morpheme_ends.value(row),
                surfaces: list_str_value(surfaces, row)?,
            });
        }
        Ok(())
    })?;
    for rows in regions.values_mut() {
        rows.sort_by(|a, b| a.analyzer_id.cmp(&b.analyzer_id));
    }
    Ok(regions)
}

/// One morpheme, with its `morpheme_features` folded in by
/// `read_tokens_for`.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct Token {
    pub surface: String,
    pub char_start: u64,
    pub char_end: u64,
    pub features: BTreeMap<String, String>,
}

/// Reads `morphemes.parquet` then `morpheme_features.parquet`, keeping
/// only rows inside a wanted `[morpheme_start, morpheme_end)` interval.
/// `ranges` maps `(source_id, analyzer_id)` to its wanted intervals. Key:
/// `(source_id, analyzer_id, morpheme_index)`.
pub fn read_tokens_for(
    run_dir: &Path,
    ranges: &BTreeMap<(String, String), Vec<(u64, u64)>>,
) -> Result<BTreeMap<(String, String, u64), Token>> {
    // Build a borrowed lookup index: source_id (as &str) → analyzer_id (as &str) → intervals.
    // This avoids allocating owned Strings for every row before the reject check.
    #[allow(clippy::type_complexity)]
    let ranges_by_source: BTreeMap<&str, BTreeMap<&str, &Vec<(u64, u64)>>> = ranges.iter().fold(
        BTreeMap::new(),
        |mut acc, ((source_id, analyzer_id), intervals)| {
            acc.entry(source_id.as_str())
                .or_default()
                .insert(analyzer_id.as_str(), intervals);
            acc
        },
    );

    let wanted = |source_id: &str, analyzer_id: &str, morpheme_index: u64| -> bool {
        ranges_by_source
            .get(source_id)
            .and_then(|by_analyzer| by_analyzer.get(analyzer_id))
            .is_some_and(|intervals| {
                intervals
                    .iter()
                    .any(|&(start, end)| morpheme_index >= start && morpheme_index < end)
            })
    };

    let mut tokens = BTreeMap::new();
    for_each_table_batch(run_dir, WarehouseTable::Morphemes, |batch| {
        let source_ids = str_col(batch, "source_id")?;
        let analyzer_ids = str_col(batch, "analyzer_id")?;
        let indices = u64_col(batch, "morpheme_index")?;
        let char_starts = u64_col(batch, "char_start")?;
        let char_ends = u64_col(batch, "char_end")?;
        let surfaces = str_col(batch, "surface")?;
        for row in 0..batch.num_rows() {
            let source_id = source_ids.value(row);
            let analyzer_id = analyzer_ids.value(row);
            let morpheme_index = indices.value(row);
            if !wanted(source_id, analyzer_id, morpheme_index) {
                continue;
            }
            tokens.insert(
                (source_id.to_owned(), analyzer_id.to_owned(), morpheme_index),
                Token {
                    surface: surfaces.value(row).to_owned(),
                    char_start: char_starts.value(row),
                    char_end: char_ends.value(row),
                    features: BTreeMap::new(),
                },
            );
        }
        Ok(())
    })?;
    for_each_table_batch(run_dir, WarehouseTable::MorphemeFeatures, |batch| {
        let source_ids = str_col(batch, "source_id")?;
        let analyzer_ids = str_col(batch, "analyzer_id")?;
        let indices = u64_col(batch, "morpheme_index")?;
        let keys = str_col(batch, "feature_key")?;
        let values = str_col(batch, "feature_value")?;
        for row in 0..batch.num_rows() {
            if !values.is_null(row) {
                let source_id = source_ids.value(row);
                let analyzer_id = analyzer_ids.value(row);
                // Interval-precise pre-check: is this morpheme_index within a wanted interval?
                // Only allocate the owned key if the interval check passes.
                if wanted(source_id, analyzer_id, indices.value(row)) {
                    let key = (
                        source_id.to_owned(),
                        analyzer_id.to_owned(),
                        indices.value(row),
                    );
                    if let Some(token) = tokens.get_mut(&key) {
                        token
                            .features
                            .insert(keys.value(row).to_owned(), values.value(row).to_owned());
                    }
                }
            }
        }
        Ok(())
    })?;
    Ok(tokens)
}

/// One row of the optional `aozora_works.parquet` sidecar for a wanted
/// `source_id`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkRow {
    pub work_id: String,
    pub title: String,
    pub author_person_id: Option<String>,
    pub publication_year: Option<i32>,
    pub orthographic_style: Option<String>,
}

/// Reads the optional `aozora_works.parquet` sidecar, keeping only rows
/// whose `source_id` is in `source_ids`. Returns `Ok(None)` when the
/// sidecar file is absent (the works-sidecar-missing case). Key:
/// `source_id`. Optional columns (`author_person_id`, `publication_year`,
/// `orthographic_style`) are read by name; if absent from the schema
/// entirely (an older importer), they are treated as all-null rather than
/// erroring.
pub fn read_works_for(
    run_dir: &Path,
    source_ids: &BTreeSet<String>,
) -> Result<Option<BTreeMap<String, WorkRow>>> {
    let path = run_dir.join("aozora_works.parquet");
    if !path.exists() {
        return Ok(None);
    }
    let mut works = BTreeMap::new();
    for_each_file_batch(&path, &mut |batch| {
        let schema = batch.schema();
        let source_id_index = schema
            .index_of("source_id")
            .context("aozora_works.parquet is missing a source_id column")?;
        let source_ids_arr = batch
            .column(source_id_index)
            .as_any()
            .downcast_ref::<StringArray>()
            .context("aozora_works.source_id is not a StringArray")?;
        let work_ids = str_col(batch, "work_id")?;
        let titles = str_col(batch, "title")?;
        let author_person_ids = schema
            .index_of("author_person_id")
            .ok()
            .map(|index| -> Result<&StringArray> {
                batch
                    .column(index)
                    .as_any()
                    .downcast_ref::<StringArray>()
                    .context("aozora_works.author_person_id is not a StringArray")
            })
            .transpose()?;
        let publication_years = schema
            .index_of("publication_year")
            .ok()
            .map(|index| -> Result<&Int32Array> {
                batch
                    .column(index)
                    .as_any()
                    .downcast_ref::<Int32Array>()
                    .context("aozora_works.publication_year is not an Int32Array")
            })
            .transpose()?;
        let orthographic_styles = schema
            .index_of("orthographic_style")
            .ok()
            .map(|index| -> Result<&StringArray> {
                batch
                    .column(index)
                    .as_any()
                    .downcast_ref::<StringArray>()
                    .context("aozora_works.orthographic_style is not a StringArray")
            })
            .transpose()?;
        for row in 0..batch.num_rows() {
            let source_id = source_ids_arr.value(row);
            if !source_ids.contains(source_id) {
                continue;
            }
            let author_person_id = author_person_ids
                .and_then(|array| (!array.is_null(row)).then(|| array.value(row).to_owned()));
            let publication_year =
                publication_years.and_then(|array| (!array.is_null(row)).then(|| array.value(row)));
            let orthographic_style = orthographic_styles
                .and_then(|array| (!array.is_null(row)).then(|| array.value(row).to_owned()));
            works.insert(
                source_id.to_owned(),
                WorkRow {
                    work_id: work_ids.value(row).to_owned(),
                    title: titles.value(row).to_owned(),
                    author_person_id,
                    publication_year,
                    orthographic_style,
                },
            );
        }
        Ok(())
    })?;
    Ok(Some(works))
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::warehouse::schema::{
        MorphemeRow, NwayRegionAnalyzerRow, RunRow, SCHEMA_VERSION, SourceRow, WarehousePaths,
    };
    use crate::warehouse::writer::{MorphemeFeaturesColumns, WarehouseWriter};
    use std::sync::Arc;

    const RUN: &str = "run-h";

    fn arc(s: &str) -> Arc<str> {
        Arc::from(s)
    }

    /// Also used by `hydrate::tests::write_e2e_fixture`
    /// end-to-end orchestration fixture); `pub(crate)` for that cross-module
    /// `#[cfg(test)]` reuse.
    pub(crate) fn write_fixture(root: &std::path::Path) -> std::path::PathBuf {
        let paths = WarehousePaths::new(root, RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: SCHEMA_VERSION,
                run_id: RUN.to_owned(),
                created_at_utc: "2026-07-10T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 2,
                error_count: 0,
                ortho_detect_mode: "off".to_owned(),
                input_normalization_detector_id: None,
                input_normalization_policy_hash: "sha256:identity".to_owned(),
            }])
            .unwrap();
        writer
            .append_sources(&[SourceRow {
                run_id: RUN.to_owned(),
                source_id: "src-a".to_owned(),
                text_id: "txt-a".to_owned(),
                aat_path: root.join("src-a.json").display().to_string(),
                source_bytes: 36,
                source_chars: 12,
            }])
            .unwrap();
        writer
            .append_nway_region_analyzers(&[
                NwayRegionAnalyzerRow {
                    run_id: arc(RUN),
                    source_id: arc("src-a"),
                    text_id: arc("txt-a"),
                    region_index: 2,
                    analyzer_id: arc("vibrato:unidic-cwj-202512"),
                    covers_exactly: true,
                    morpheme_start: 1,
                    morpheme_end: 2,
                    surfaces: vec!["猫である".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: arc(RUN),
                    source_id: arc("src-a"),
                    text_id: arc("txt-a"),
                    region_index: 2,
                    analyzer_id: arc("sudachi-a"),
                    covers_exactly: true,
                    morpheme_start: 1,
                    morpheme_end: 3,
                    surfaces: vec!["猫".to_owned(), "である".to_owned()],
                },
            ])
            .unwrap();
        writer
            .append_morphemes(&[
                MorphemeRow {
                    run_id: arc(RUN),
                    source_id: arc("src-a"),
                    text_id: arc("txt-a"),
                    analyzer_id: arc("vibrato:unidic-cwj-202512"),
                    morpheme_index: 1,
                    byte_start: 0,
                    byte_end: 0,
                    char_start: 6,
                    char_end: 10,
                    surface: "猫である".to_owned(),
                },
                MorphemeRow {
                    run_id: arc(RUN),
                    source_id: arc("src-a"),
                    text_id: arc("txt-a"),
                    analyzer_id: arc("sudachi-a"),
                    morpheme_index: 1,
                    byte_start: 0,
                    byte_end: 0,
                    char_start: 6,
                    char_end: 7,
                    surface: "猫".to_owned(),
                },
                MorphemeRow {
                    run_id: arc(RUN),
                    source_id: arc("src-a"),
                    text_id: arc("txt-a"),
                    analyzer_id: arc("sudachi-a"),
                    morpheme_index: 2,
                    byte_start: 0,
                    byte_end: 0,
                    char_start: 7,
                    char_end: 10,
                    surface: "である".to_owned(),
                },
                // A morpheme outside every wanted range; must not be returned.
                MorphemeRow {
                    run_id: arc(RUN),
                    source_id: arc("src-a"),
                    text_id: arc("txt-a"),
                    analyzer_id: arc("sudachi-a"),
                    morpheme_index: 9,
                    byte_start: 0,
                    byte_end: 0,
                    char_start: 0,
                    char_end: 1,
                    surface: "外".to_owned(),
                },
            ])
            .unwrap();
        let mut features = MorphemeFeaturesColumns::new();
        features.push_row(RUN, "src-a", "txt-a", "sudachi-a", 1, "pos1", Some("名詞"));
        features.push_row(
            RUN,
            "src-a",
            "txt-a",
            "vibrato:unidic-cwj-202512",
            1,
            "pos1",
            Some("動詞"),
        );
        writer.append_morpheme_feature_columns(features).unwrap();
        writer.finalize().unwrap();
        paths.final_dir
    }

    #[test]
    fn readers_filter_to_wanted_keys() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(dir.path());

        let sources = read_sources_for(&run_dir, &["src-a".to_owned()].into()).unwrap();
        assert_eq!(sources["src-a"].text_id, "txt-a");
        assert_eq!(sources["src-a"].source_chars, 12);

        let wanted: BTreeSet<(String, u64)> = [("src-a".to_owned(), 2)].into();
        let regions = read_region_analyzers_for(&run_dir, &wanted).unwrap();
        let rows = &regions[&("src-a".to_owned(), 2)];
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].analyzer_id, "sudachi-a"); // sorted by analyzer_id

        let mut ranges = BTreeMap::new();
        for row in rows {
            ranges
                .entry(("src-a".to_owned(), row.analyzer_id.clone()))
                .or_insert_with(Vec::new)
                .push((row.morpheme_start, row.morpheme_end));
        }
        let tokens = read_tokens_for(&run_dir, &ranges).unwrap();
        assert_eq!(tokens.len(), 3); // index 9 filtered out
        let t = &tokens[&("src-a".to_owned(), "sudachi-a".to_owned(), 1)];
        assert_eq!(t.surface, "猫");
        assert_eq!(t.features["pos1"], "名詞");

        // No aozora_works.parquet in the fixture ⇒ Ok(None).
        assert!(
            read_works_for(&run_dir, &["src-a".to_owned()].into())
                .unwrap()
                .is_none()
        );
    }
}
