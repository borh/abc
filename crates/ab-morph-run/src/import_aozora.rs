//! Producer for the `aozora_works.parquet` sidecar: an imported projection
//! of ABC's `metadata-record.schema.json` export.
//! Design: docs/superpowers/specs/2026-07-06-aozora-works-import-design.md

use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result, bail};
use arrow_array::{Array, Int32Array, RecordBatch, StringArray};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::ArrowWriter;
use parquet::basic::{Compression, ZstdLevel};
use parquet::file::properties::WriterProperties;
use serde::Deserialize;

use crate::summary::read_warehouse_table;
use crate::warehouse::schema::WarehouseTable;

/// ABC metadata-record schema hash this importer was written against.
/// Every export record must declare exactly this hash; an ABC schema
/// change updates this constant and the field mapping together in one
/// reviewed change (spec Decision 3).
pub const ABC_METADATA_RECORD_SCHEMA_HASH: &str =
    "sha256:692dfa23215ea6c58e21f5d293b364dff06371c75fb32ebc9694f52e8e5c9b1f";

/// Parses a warehouse `source_id` of the form `<person>_<card>-<hash12>`
/// (e.g. `000136_731-9559c30ae312`) into the 6-digit zero-padded ABC
/// `work_id` (`000731`). The person prefix is not part of work identity:
/// the same card can appear under two person pages (author + translator).
/// Returns `None` for non-conforming ids (e.g. `000025_kantou-…`), which
/// the importer skips — the rarity reader falls back to per-source keys.
fn parse_source_id(source_id: &str) -> Option<String> {
    let (person, rest) = source_id.split_once('_')?;
    let (card, hash) = rest.rsplit_once('-')?;
    if person.len() != 6 || !person.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    if card.is_empty() || card.len() > 6 || !card.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    if hash.len() != 12 || !hash.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    Some(format!("{card:0>6}"))
}

/// Extracts the first substring matching `[0-9]{4}` as a year
/// (spec §Column projection). Handles ABC date strings like
/// `1988（昭和63）年10月25日`; 元号-only dates yield `None`.
fn extract_year(value: &str) -> Option<i32> {
    let bytes = value.as_bytes();
    for start in 0..bytes.len().saturating_sub(3) {
        if bytes[start..start + 4].iter().all(u8::is_ascii_digit) {
            // Pure-ASCII window, so the byte slice is valid UTF-8.
            return std::str::from_utf8(&bytes[start..start + 4])
                .ok()?
                .parse()
                .ok();
        }
    }
    None
}

/// The consumed subset of ABC's `metadata-record.schema.json`. Unknown
/// fields are ignored by serde; validation covers only consumed fields
/// (spec Decision 3: the pinned hash is the drift gate).
#[derive(Debug, Deserialize)]
struct MetadataRecord {
    metadata_record_schema_hash: String,
    work: WorkRecord,
    #[serde(default)]
    contributors: Vec<ContributorRecord>,
}

#[derive(Debug, Deserialize)]
struct WorkRecord {
    work_id: String,
    title: String,
    #[serde(default)]
    first_published: Option<String>,
    orthographic_style: String,
    #[serde(default)]
    source_editions: Vec<SourceEditionRecord>,
}

#[derive(Debug, Deserialize)]
struct SourceEditionRecord {
    #[serde(default)]
    first_edition_year: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ContributorRecord {
    person_id: String,
    relation_to_work: String,
}

/// ABC `orthographic_style` enum, verbatim (spec Global Constraints).
const ORTHOGRAPHIC_STYLES: &[&str] = &[
    "新字新仮名",
    "新字旧仮名",
    "旧字新仮名",
    "旧字旧仮名",
    "その他",
];

fn validate_record(record: &MetadataRecord, expected_work_id: &str, path: &Path) -> Result<()> {
    if record.metadata_record_schema_hash != ABC_METADATA_RECORD_SCHEMA_HASH {
        bail!(
            "{}: ABC metadata-record schema hash mismatch: importer expects \
             {ABC_METADATA_RECORD_SCHEMA_HASH} but the record declares {}; update the \
             importer's pinned hash and field mapping together",
            path.display(),
            record.metadata_record_schema_hash,
        );
    }
    let work_id = &record.work.work_id;
    if work_id.len() != 6 || !work_id.bytes().all(|b| b.is_ascii_digit()) {
        bail!(
            "{}: work.work_id {work_id:?} does not match ^[0-9]{{6}}$",
            path.display(),
        );
    }
    if work_id != expected_work_id {
        bail!(
            "{}: work.work_id {work_id:?} does not match {expected_work_id:?} derived \
             from the run's source_id",
            path.display(),
        );
    }
    if !ORTHOGRAPHIC_STYLES.contains(&record.work.orthographic_style.as_str()) {
        bail!(
            "{}: orthographic_style {:?} is not an ABC enum value",
            path.display(),
            record.work.orthographic_style,
        );
    }
    Ok(())
}

/// First contributor with `relation_to_work = 著者` in record order (ABC
/// pre-sorts contributors by person_id). Never a free-text name.
fn author_person_id(record: &MetadataRecord) -> Option<&str> {
    record
        .contributors
        .iter()
        .find(|contributor| contributor.relation_to_work == "著者")
        .map(|contributor| contributor.person_id.as_str())
}

/// Prefer `work.first_published`, else the first source edition whose
/// `first_edition_year` yields a year (spec §Column projection).
fn publication_year(record: &MetadataRecord) -> Option<i32> {
    record
        .work
        .first_published
        .as_deref()
        .and_then(extract_year)
        .or_else(|| {
            record
                .work
                .source_editions
                .iter()
                .filter_map(|edition| edition.first_edition_year.as_deref())
                .find_map(extract_year)
        })
}

/// Outcome counts for one `import-aozora-metadata` invocation.
#[derive(Debug)]
pub struct ImportSummary {
    pub works_imported: usize,
    pub sources_mapped: usize,
    pub skipped_source_ids: Vec<String>,
}

/// Materializes `<run_dir>/aozora_works.parquet` from an ABC export
/// (`<from>/works/<work_id>.json`). Spec: 2026-07-06 design doc; error
/// behavior follows its table verbatim.
pub fn run_import_aozora_metadata(
    run_dir: &Path,
    from: &Path,
    force: bool,
) -> Result<ImportSummary> {
    let output = run_dir.join("aozora_works.parquet");
    if output.exists() && !force {
        bail!(
            "refusing to overwrite {} (pass --force to allow)",
            output.display()
        );
    }
    let works_dir = from.join("works");
    if !works_dir.is_dir() {
        bail!(
            "{} is not an ABC export root: missing works/ directory",
            from.display()
        );
    }

    let mut source_ids = BTreeSet::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::Sources)? {
        let index = batch
            .schema()
            .index_of("source_id")
            .context("sources.parquet is missing a source_id column")?;
        let values = batch
            .column(index)
            .as_any()
            .downcast_ref::<StringArray>()
            .context("sources.source_id is not a StringArray")?;
        for row in 0..batch.num_rows() {
            source_ids.insert(values.value(row).to_owned());
        }
    }

    let mut sources_by_work = BTreeMap::<String, Vec<String>>::new();
    let mut skipped_source_ids = Vec::new();
    for source_id in source_ids {
        match parse_source_id(&source_id) {
            Some(work_id) => sources_by_work.entry(work_id).or_default().push(source_id),
            None => skipped_source_ids.push(source_id),
        }
    }

    let retrieved_at = chrono::Utc::now().to_rfc3339();
    let mut rows = SidecarColumns::default();
    let mut works_imported = 0usize;
    for (work_id, work_sources) in sources_by_work {
        let path = works_dir.join(format!("{work_id}.json"));
        if !path.exists() {
            // Absence is a coverage gap (tolerated, warned); corruption
            // below is a contract violation (hard error).
            eprintln!(
                "import-aozora-metadata: no export record {}; skipping {} source(s)",
                path.display(),
                work_sources.len()
            );
            skipped_source_ids.extend(work_sources);
            continue;
        }
        let bytes =
            fs::read(&path).with_context(|| format!("failed to read {}", path.display()))?;
        let record: MetadataRecord = serde_json::from_slice(&bytes)
            .with_context(|| format!("failed to parse {}", path.display()))?;
        validate_record(&record, &work_id, &path)?;
        works_imported += 1;
        for source_id in work_sources {
            rows.push(&work_id, &source_id, &record, &retrieved_at);
        }
    }
    skipped_source_ids.sort();

    if rows.len() == 0 {
        bail!(
            "zero sources mapped to ABC works; refusing to write an empty projection \
             (is {} the right export root for this run?)",
            from.display()
        );
    }

    let temp = run_dir.join("aozora_works.parquet.tmp");
    write_sidecar(&temp, &rows).with_context(|| format!("failed to write {}", temp.display()))?;
    fs::rename(&temp, &output).with_context(|| {
        format!(
            "failed to rename {} to {}",
            temp.display(),
            output.display()
        )
    })?;

    Ok(ImportSummary {
        works_imported,
        sources_mapped: rows.len(),
        skipped_source_ids,
    })
}

/// Column-major accumulator for the 9 sidecar columns. Rows arrive
/// pre-sorted by (work_id, source_id) via the BTreeMap iteration order.
#[derive(Default)]
struct SidecarColumns {
    work_ids: Vec<String>,
    source_ids: Vec<String>,
    titles: Vec<String>,
    author_person_ids: Vec<Option<String>>,
    publication_years: Vec<Option<i32>>,
    orthographic_styles: Vec<String>,
    schema_hashes: Vec<String>,
    retrieved_ats: Vec<String>,
}

impl SidecarColumns {
    fn len(&self) -> usize {
        self.work_ids.len()
    }

    fn push(
        &mut self,
        work_id: &str,
        source_id: &str,
        record: &MetadataRecord,
        retrieved_at: &str,
    ) {
        self.work_ids.push(work_id.to_owned());
        self.source_ids.push(source_id.to_owned());
        self.titles.push(record.work.title.clone());
        self.author_person_ids
            .push(author_person_id(record).map(str::to_owned));
        self.publication_years.push(publication_year(record));
        self.orthographic_styles
            .push(record.work.orthographic_style.clone());
        self.schema_hashes
            .push(record.metadata_record_schema_hash.clone());
        self.retrieved_ats.push(retrieved_at.to_owned());
    }
}

fn write_sidecar(path: &Path, rows: &SidecarColumns) -> Result<()> {
    let schema = Arc::new(Schema::new(vec![
        Field::new("work_id", DataType::Utf8, false),
        Field::new("source_id", DataType::Utf8, false),
        Field::new("title", DataType::Utf8, false),
        Field::new("author_person_id", DataType::Utf8, true),
        Field::new("publication_year", DataType::Int32, true),
        Field::new("orthographic_style", DataType::Utf8, false),
        Field::new("genre", DataType::Utf8, true),
        Field::new("metadata_record_schema_hash", DataType::Utf8, false),
        Field::new("metadata_record_retrieved_at", DataType::Utf8, false),
    ]));
    let genre: Vec<Option<&str>> = vec![None; rows.len()];
    let batch = RecordBatch::try_new(
        schema.clone(),
        vec![
            Arc::new(StringArray::from_iter_values(rows.work_ids.iter())),
            Arc::new(StringArray::from_iter_values(rows.source_ids.iter())),
            Arc::new(StringArray::from_iter_values(rows.titles.iter())),
            Arc::new(StringArray::from(
                rows.author_person_ids
                    .iter()
                    .map(|value| value.as_deref())
                    .collect::<Vec<_>>(),
            )),
            Arc::new(Int32Array::from(rows.publication_years.clone())),
            Arc::new(StringArray::from_iter_values(
                rows.orthographic_styles.iter(),
            )),
            Arc::new(StringArray::from(genre)),
            Arc::new(StringArray::from_iter_values(rows.schema_hashes.iter())),
            Arc::new(StringArray::from_iter_values(rows.retrieved_ats.iter())),
        ],
    )?;
    let file = File::create(path)?;
    let properties = WriterProperties::builder()
        .set_compression(Compression::ZSTD(
            ZstdLevel::try_new(3).expect("valid zstd level"),
        ))
        .build();
    let mut writer = ArrowWriter::try_new(file, schema, Some(properties))?;
    writer.write(&batch)?;
    writer.close()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn parses_canonical_source_id() {
        assert_eq!(
            parse_source_id("000136_731-9559c30ae312").as_deref(),
            Some("000731")
        );
    }

    #[test]
    fn parses_long_card_id_without_padding_loss() {
        assert_eq!(
            parse_source_id("001154_44352-da75dade75fc").as_deref(),
            Some("044352")
        );
    }

    #[test]
    fn rejects_non_numeric_card_slug() {
        assert_eq!(parse_source_id("000025_kantou-20379f2add12"), None);
    }

    #[test]
    fn rejects_malformed_source_ids() {
        assert_eq!(parse_source_id("no-underscore-9559c30ae312"), None);
        assert_eq!(parse_source_id("000136_731"), None); // no hash suffix
        assert_eq!(parse_source_id("000136_731-9559c3"), None); // short hash
        assert_eq!(parse_source_id("00013_731-9559c30ae312"), None); // 5-digit person
        assert_eq!(parse_source_id("000136_1234567-9559c30ae312"), None); // 7-digit card
        assert_eq!(parse_source_id("000136_-9559c30ae312"), None); // empty card
        assert_eq!(parse_source_id(""), None);
    }

    #[test]
    fn extracts_first_four_digit_window_as_year() {
        assert_eq!(extract_year("1988（昭和63）年10月25日"), Some(1988));
        assert_eq!(extract_year("1907"), Some(1907));
        assert_eq!(extract_year("19881025"), Some(1988)); // regex [0-9]{4} semantics
        assert_eq!(extract_year("昭和63年10月25日"), None); // no 4-digit window
        assert_eq!(extract_year("西暦672年"), None); // 3-digit run
        assert_eq!(extract_year(""), None);
    }

    fn record_value(work_id: &str) -> serde_json::Value {
        serde_json::json!({
            "metadata_record_schema_id": "https://w3id.org/abc/schemas/metadata-record.schema.json",
            "metadata_record_schema_hash": ABC_METADATA_RECORD_SCHEMA_HASH,
            "work": {
                "work_id": work_id,
                "title": "聖三稜玻璃",
                "first_published": null,
                "orthographic_style": "旧字旧仮名",
                "source_editions": [
                    {"title": "t", "publisher": "p", "first_edition_year": null},
                    {"title": "t", "publisher": "p", "first_edition_year": "1988（昭和63）年10月25日"}
                ]
            },
            "contributors": [
                {"person_id": "000100", "person_record_hash": "sha256:0", "relation_to_work": "翻訳者"},
                {"person_id": "000136", "person_record_hash": "sha256:0", "relation_to_work": "著者"}
            ]
        })
    }

    fn record(work_id: &str) -> MetadataRecord {
        serde_json::from_value(record_value(work_id)).unwrap()
    }

    #[test]
    fn valid_record_passes_validation() {
        validate_record(&record("000731"), "000731", Path::new("000731.json")).unwrap();
    }

    #[test]
    fn schema_hash_mismatch_names_both_hashes() {
        let mut value = record_value("000731");
        value["metadata_record_schema_hash"] = serde_json::json!("sha256:deadbeef");
        let parsed: MetadataRecord = serde_json::from_value(value).unwrap();
        let err = validate_record(&parsed, "000731", Path::new("000731.json"))
            .unwrap_err()
            .to_string();
        assert!(err.contains(ABC_METADATA_RECORD_SCHEMA_HASH), "{err}");
        assert!(err.contains("sha256:deadbeef"), "{err}");
    }

    #[test]
    fn work_id_mismatch_with_derived_id_is_rejected() {
        let err = validate_record(&record("000731"), "000732", Path::new("000731.json"))
            .unwrap_err()
            .to_string();
        assert!(err.contains("000732"), "{err}");
    }

    #[test]
    fn work_id_regex_violation_is_rejected() {
        let err = validate_record(&record("73a"), "73a", Path::new("x.json"))
            .unwrap_err()
            .to_string();
        assert!(err.contains("[0-9]{6}"), "{err}");
    }

    #[test]
    fn unknown_orthographic_style_is_rejected() {
        let mut value = record_value("000731");
        value["work"]["orthographic_style"] = serde_json::json!("新字");
        let parsed: MetadataRecord = serde_json::from_value(value).unwrap();
        let err = validate_record(&parsed, "000731", Path::new("x.json"))
            .unwrap_err()
            .to_string();
        assert!(err.contains("orthographic_style"), "{err}");
    }

    #[test]
    fn author_is_first_contributor_with_author_relation() {
        assert_eq!(author_person_id(&record("000731")), Some("000136"));
        let mut value = record_value("000731");
        value["contributors"] = serde_json::json!([
            {"person_id": "000100", "person_record_hash": "sha256:0", "relation_to_work": "翻訳者"}
        ]);
        let parsed: MetadataRecord = serde_json::from_value(value).unwrap();
        assert_eq!(author_person_id(&parsed), None);
    }

    #[test]
    fn publication_year_prefers_first_published_then_editions() {
        // record(): first_published null -> falls back to the first non-null
        // source_edition year.
        assert_eq!(publication_year(&record("000731")), Some(1988));
        let mut value = record_value("000731");
        value["work"]["first_published"] = serde_json::json!("1907-05-01");
        let parsed: MetadataRecord = serde_json::from_value(value).unwrap();
        assert_eq!(publication_year(&parsed), Some(1907));
        let mut value = record_value("000731");
        value["work"]["source_editions"] = serde_json::json!([]);
        let parsed: MetadataRecord = serde_json::from_value(value).unwrap();
        assert_eq!(publication_year(&parsed), None);
    }

    use std::fs::{self, File};
    use std::sync::Arc;

    use arrow_array::{RecordBatch, StringArray};
    use arrow_schema::{DataType, Field, Schema};
    use parquet::arrow::ArrowWriter;

    fn write_sources_parquet(run_dir: &Path, source_ids: &[&str]) {
        let schema = Arc::new(Schema::new(vec![Field::new(
            "source_id",
            DataType::Utf8,
            false,
        )]));
        let batch = RecordBatch::try_new(
            schema.clone(),
            vec![Arc::new(StringArray::from(source_ids.to_vec()))],
        )
        .unwrap();
        let file = File::create(run_dir.join("sources.parquet")).unwrap();
        let mut writer = ArrowWriter::try_new(file, schema, None).unwrap();
        writer.write(&batch).unwrap();
        writer.close().unwrap();
    }

    fn write_export_record(export_dir: &Path, work_id: &str, value: &serde_json::Value) {
        let works = export_dir.join("works");
        fs::create_dir_all(&works).unwrap();
        fs::write(
            works.join(format!("{work_id}.json")),
            serde_json::to_vec(value).unwrap(),
        )
        .unwrap();
    }

    fn read_sidecar(run_dir: &Path) -> Vec<RecordBatch> {
        let file = File::open(run_dir.join("aozora_works.parquet")).unwrap();
        parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file)
            .unwrap()
            .build()
            .unwrap()
            .collect::<std::result::Result<Vec<_>, _>>()
            .unwrap()
    }

    fn string_at(batch: &RecordBatch, column: &str, row: usize) -> Option<String> {
        let index = batch.schema().index_of(column).unwrap();
        let values = batch
            .column(index)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap();
        values.is_valid(row).then(|| values.value(row).to_owned())
    }

    #[test]
    fn imports_run_scoped_projection() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = dir.path().join("run");
        let export = dir.path().join("export");
        fs::create_dir_all(&run_dir).unwrap();
        // Two files of one card (multi-file merge) + one distinct work.
        write_sources_parquet(
            &run_dir,
            &[
                "000001_10-aaaaaaaaaaaa",
                "000001_10-bbbbbbbbbbbb",
                "000002_20-cccccccccccc",
            ],
        );
        write_export_record(&export, "000010", &record_value("000010"));
        write_export_record(&export, "000020", &record_value("000020"));

        let summary = run_import_aozora_metadata(&run_dir, &export, false).unwrap();

        assert_eq!(summary.works_imported, 2);
        assert_eq!(summary.sources_mapped, 3);
        assert!(summary.skipped_source_ids.is_empty());
        let batches = read_sidecar(&run_dir);
        let rows: usize = batches.iter().map(RecordBatch::num_rows).sum();
        assert_eq!(rows, 3);
        let batch = &batches[0];
        assert_eq!(
            batch
                .schema()
                .fields()
                .iter()
                .map(|f| f.name().as_str())
                .collect::<Vec<_>>(),
            vec![
                "work_id",
                "source_id",
                "title",
                "author_person_id",
                "publication_year",
                "orthographic_style",
                "genre",
                "metadata_record_schema_hash",
                "metadata_record_retrieved_at",
            ]
        );
        // Rows are sorted by (work_id, source_id).
        assert_eq!(string_at(batch, "work_id", 0).as_deref(), Some("000010"));
        assert_eq!(
            string_at(batch, "source_id", 0).as_deref(),
            Some("000001_10-aaaaaaaaaaaa")
        );
        assert_eq!(string_at(batch, "title", 0).as_deref(), Some("聖三稜玻璃"));
        assert_eq!(
            string_at(batch, "author_person_id", 0).as_deref(),
            Some("000136")
        );
        assert_eq!(
            string_at(batch, "orthographic_style", 0).as_deref(),
            Some("旧字旧仮名")
        );
        assert_eq!(string_at(batch, "genre", 0), None);
        assert_eq!(
            string_at(batch, "metadata_record_schema_hash", 0).as_deref(),
            Some(ABC_METADATA_RECORD_SCHEMA_HASH)
        );
        let years = batch
            .column(batch.schema().index_of("publication_year").unwrap())
            .as_any()
            .downcast_ref::<arrow_array::Int32Array>()
            .unwrap();
        assert_eq!(years.value(0), 1988);
    }

    #[test]
    fn skips_unparsable_and_missing_works_with_warning() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = dir.path().join("run");
        let export = dir.path().join("export");
        fs::create_dir_all(&run_dir).unwrap();
        write_sources_parquet(
            &run_dir,
            &[
                "000001_10-aaaaaaaaaaaa",
                "000025_kantou-20379f2add12", // unparsable slug
                "000989_352-eeeeeeeeeeee",    // no export record
            ],
        );
        write_export_record(&export, "000010", &record_value("000010"));

        let summary = run_import_aozora_metadata(&run_dir, &export, false).unwrap();

        assert_eq!(summary.works_imported, 1);
        assert_eq!(summary.sources_mapped, 1);
        assert_eq!(
            summary.skipped_source_ids,
            vec![
                "000025_kantou-20379f2add12".to_owned(),
                "000989_352-eeeeeeeeeeee".to_owned(),
            ]
        );
    }

    #[test]
    fn zero_mapped_sources_is_a_hard_error_and_writes_nothing() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = dir.path().join("run");
        let export = dir.path().join("export");
        fs::create_dir_all(&run_dir).unwrap();
        fs::create_dir_all(export.join("works")).unwrap();
        write_sources_parquet(&run_dir, &["000025_kantou-20379f2add12"]);

        let err = run_import_aozora_metadata(&run_dir, &export, false)
            .unwrap_err()
            .to_string();

        assert!(err.contains("zero"), "{err}");
        assert!(!run_dir.join("aozora_works.parquet").exists());
    }

    #[test]
    fn missing_works_directory_is_a_hard_error() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = dir.path().join("run");
        fs::create_dir_all(&run_dir).unwrap();
        write_sources_parquet(&run_dir, &["000001_10-aaaaaaaaaaaa"]);

        let err = run_import_aozora_metadata(&run_dir, dir.path(), false)
            .unwrap_err()
            .to_string();

        assert!(err.contains("works"), "{err}");
    }

    #[test]
    fn malformed_record_json_is_a_hard_error_naming_the_file() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = dir.path().join("run");
        let export = dir.path().join("export");
        fs::create_dir_all(&run_dir).unwrap();
        write_sources_parquet(&run_dir, &["000001_10-aaaaaaaaaaaa"]);
        fs::create_dir_all(export.join("works")).unwrap();
        fs::write(export.join("works/000010.json"), b"{not json").unwrap();

        let err = run_import_aozora_metadata(&run_dir, &export, false)
            .unwrap_err()
            .to_string();

        assert!(err.contains("000010.json"), "{err}");
    }

    #[test]
    fn refuses_overwrite_without_force_and_allows_with_force() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = dir.path().join("run");
        let export = dir.path().join("export");
        fs::create_dir_all(&run_dir).unwrap();
        write_sources_parquet(&run_dir, &["000001_10-aaaaaaaaaaaa"]);
        write_export_record(&export, "000010", &record_value("000010"));

        run_import_aozora_metadata(&run_dir, &export, false).unwrap();
        let err = run_import_aozora_metadata(&run_dir, &export, false)
            .unwrap_err()
            .to_string();
        assert!(err.contains("--force"), "{err}");
        run_import_aozora_metadata(&run_dir, &export, true).unwrap();
    }

    #[test]
    fn missing_sources_parquet_is_a_hard_error() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = dir.path().join("run");
        let export = dir.path().join("export");
        fs::create_dir_all(&run_dir).unwrap();
        fs::create_dir_all(export.join("works")).unwrap();

        assert!(run_import_aozora_metadata(&run_dir, &export, false).is_err());
    }
}
