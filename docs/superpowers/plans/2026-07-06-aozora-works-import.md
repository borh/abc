# `import-aozora-metadata` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `ab-morph-run import-aozora-metadata --run-dir <run> --from <abc export dir>` writes `<run_dir>/aozora_works.parquet` (an imported projection of ABC's metadata-record export), flipping the ranker's rarity basis from `"source"` to `"work"`; plus two reader hardenings.

**Architecture:** New module `crates/ab-morph-run/src/import_aozora.rs` with a pure core (source-id parsing, record validation, field projection) and an IO shell (read the run's `sources.parquet`, load `works/<work_id>.json` records, write the sidecar atomically). CLI wiring in `main.rs`. Reader hardenings in `summary/interesting.rs`. Spec: `docs/superpowers/specs/2026-07-06-aozora-works-import-design.md` (read it first; its Decisions and Error-behavior tables are the contract).

**Tech Stack:** Rust; existing workspace deps only (`serde_json`, `arrow-array`, `arrow-schema`, `parquet`, `chrono`, `anyhow`, `tempfile` for tests). No new dependencies.

## Global Constraints

- ALL test invocations: `cargo test -p ab-morph-run --features test-analyzer` (a bare `cargo test -p ab-morph-run` false-fails 2 bin tests).
- Pinned ABC schema hash (verbatim): `sha256:692dfa23215ea6c58e21f5d293b364dff06371c75fb32ebc9694f52e8e5c9b1f`.
- `orthographic_style` enum (verbatim, exactly these five): `新字新仮名`, `新字旧仮名`, `旧字新仮名`, `旧字旧仮名`, `その他`.
- Sidecar column order and types (spec §Column projection): `work_id` Utf8 non-null, `source_id` Utf8 non-null, `title` Utf8 non-null, `author_person_id` Utf8 nullable, `publication_year` Int32 nullable, `orthographic_style` Utf8 non-null, `genre` Utf8 nullable (always null), `metadata_record_schema_hash` Utf8 non-null, `metadata_record_retrieved_at` Utf8 non-null.
- No `SCHEMA_VERSION` bump, no `ab-warehouse/src/sql.rs` change (spec Decision 2).
- Parquet compression: ZSTD (warehouse convention, `ab-warehouse/src/writer.rs:785`).
- Commit after every task.

---

### Task 1: Pure core — `parse_source_id` and `extract_year`

**Files:**
- Create: `crates/ab-morph-run/src/import_aozora.rs`
- Modify: `crates/ab-morph-run/src/lib.rs` (add `mod import_aozora;` next to the other `mod` declarations)
- Test: same file, `#[cfg(test)] mod tests`

**Interfaces:**
- Consumes: nothing.
- Produces: `pub const ABC_METADATA_RECORD_SCHEMA_HASH: &str`; `fn parse_source_id(source_id: &str) -> Option<String>` (returns the 6-digit zero-padded ABC work_id); `fn extract_year(value: &str) -> Option<i32>` (first `[0-9]{4}` window). Task 2 and 3 live in this same file and call these.

- [ ] **Step 1: Write the failing tests**

Create `crates/ab-morph-run/src/import_aozora.rs`:

```rust
//! Producer for the `aozora_works.parquet` sidecar: an imported projection
//! of ABC's `metadata-record.schema.json` export.
//! Design: docs/superpowers/specs/2026-07-06-aozora-works-import-design.md

/// ABC metadata-record schema hash this importer was written against.
/// Every export record must declare exactly this hash; an ABC schema
/// change updates this constant and the field mapping together in one
/// reviewed change (spec Decision 3).
pub const ABC_METADATA_RECORD_SCHEMA_HASH: &str =
    "sha256:692dfa23215ea6c58e21f5d293b364dff06371c75fb32ebc9694f52e8e5c9b1f";

#[cfg(test)]
mod tests {
    use super::*;

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
}
```

Add to `crates/ab-morph-run/src/lib.rs`, next to the existing `mod` declarations (e.g. after `mod compact;`):

```rust
mod import_aozora;
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer import_aozora -- --nocapture`
Expected: COMPILE ERROR — `parse_source_id` / `extract_year` not found.

- [ ] **Step 3: Write the implementations**

Add above the tests module:

```rust
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
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer import_aozora`
Expected: 5 tests PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/import_aozora.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat(import-aozora): source_id parsing and year extraction"
```

---

### Task 2: Record model, validation, and projection helpers

**Files:**
- Modify: `crates/ab-morph-run/src/import_aozora.rs`

**Interfaces:**
- Consumes: `ABC_METADATA_RECORD_SCHEMA_HASH` (Task 1).
- Produces (all in this file, used by Task 3): `struct MetadataRecord` (serde `Deserialize`; fields `metadata_record_schema_hash: String`, `work: WorkRecord`, `contributors: Vec<ContributorRecord>`), `fn validate_record(record: &MetadataRecord, expected_work_id: &str, path: &Path) -> Result<()>`, `fn author_person_id(record: &MetadataRecord) -> Option<&str>`, `fn publication_year(record: &MetadataRecord) -> Option<i32>`.

- [ ] **Step 1: Write the failing tests**

Append inside `mod tests`:

```rust
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
```

Add `use std::path::Path;` to the tests module imports if not already present.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer import_aozora`
Expected: COMPILE ERROR — `MetadataRecord` / `validate_record` not found.

- [ ] **Step 3: Write the implementation**

Add near the top of the file:

```rust
use std::path::Path;

use anyhow::{Result, bail};
use serde::Deserialize;

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
const ORTHOGRAPHIC_STYLES: &[&str] =
    &["新字新仮名", "新字旧仮名", "旧字新仮名", "旧字旧仮名", "その他"];

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
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer import_aozora`
Expected: 12 tests PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/import_aozora.rs
git commit -m "feat(import-aozora): record model, validation, projection helpers"
```

---

### Task 3: IO shell — `run_import_aozora_metadata`

**Files:**
- Modify: `crates/ab-morph-run/src/import_aozora.rs`
- Modify: `crates/ab-morph-run/src/summary/summary_body.rs:3549` (`read_warehouse_table` visibility `pub(super)` → `pub(crate)`)
- Modify: `crates/ab-morph-run/src/summary/mod.rs` (add `pub(crate) use summary_body::read_warehouse_table;`)
- Modify: `crates/ab-morph-run/src/lib.rs` (re-export: `pub use import_aozora::{ImportSummary, run_import_aozora_metadata};`)

**Interfaces:**
- Consumes: Tasks 1–2 helpers; `crate::summary::read_warehouse_table(run_dir: &Path, table: WarehouseTable) -> Result<Vec<RecordBatch>>` (handles both single-file and sharded-directory `sources.parquet`); `crate::warehouse::schema::WarehouseTable::Sources`.
- Produces: `pub struct ImportSummary { pub works_imported: usize, pub sources_mapped: usize, pub skipped_source_ids: Vec<String> }`; `pub fn run_import_aozora_metadata(run_dir: &Path, from: &Path, force: bool) -> Result<ImportSummary>`. Task 4 (CLI) and Task 6 (e2e) call this via the `lib.rs` re-export.

- [ ] **Step 1: Make `read_warehouse_table` crate-visible**

In `crates/ab-morph-run/src/summary/summary_body.rs`, change:

```rust
pub(super) fn read_warehouse_table(run_dir: &Path, table: WarehouseTable) -> Result<Vec<RecordBatch>> {
```

to:

```rust
pub(crate) fn read_warehouse_table(run_dir: &Path, table: WarehouseTable) -> Result<Vec<RecordBatch>> {
```

In `crates/ab-morph-run/src/summary/mod.rs`, after the existing `pub(crate) use types::WAREHOUSE_CORE_FEATURE_KEYS;` line, add:

```rust
pub(crate) use summary_body::read_warehouse_table;
```

Run: `cargo check -p ab-morph-run --features test-analyzer`
Expected: clean (possibly an unused-import warning until Step 3 — proceed).

- [ ] **Step 2: Write the failing integration tests**

Append inside `mod tests` in `import_aozora.rs`:

```rust
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
            batch.schema().fields().iter().map(|f| f.name().as_str()).collect::<Vec<_>>(),
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
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer import_aozora`
Expected: COMPILE ERROR — `run_import_aozora_metadata` not found.

- [ ] **Step 4: Write the implementation**

Add to `import_aozora.rs` (extending the existing `use` block as needed):

```rust
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::sync::Arc;

use anyhow::Context;
use arrow_array::{Int32Array, RecordBatch, StringArray};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::ArrowWriter;
use parquet::basic::{Compression, ZstdLevel};
use parquet::file::properties::WriterProperties;

use crate::summary::read_warehouse_table;
use crate::warehouse::schema::WarehouseTable;

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
        let bytes = fs::read(&path).with_context(|| format!("failed to read {}", path.display()))?;
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
    write_sidecar(&temp, &rows)
        .with_context(|| format!("failed to write {}", temp.display()))?;
    fs::rename(&temp, &output)
        .with_context(|| format!("failed to rename {} to {}", temp.display(), output.display()))?;

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

    fn push(&mut self, work_id: &str, source_id: &str, record: &MetadataRecord, retrieved_at: &str) {
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
            Arc::new(StringArray::from_iter_values(rows.orthographic_styles.iter())),
            Arc::new(StringArray::from(genre)),
            Arc::new(StringArray::from_iter_values(rows.schema_hashes.iter())),
            Arc::new(StringArray::from_iter_values(rows.retrieved_ats.iter())),
        ],
    )?;
    let file = File::create(path)?;
    let properties = WriterProperties::builder()
        .set_compression(Compression::ZSTD(ZstdLevel::default()))
        .build();
    let mut writer = ArrowWriter::try_new(file, schema, Some(properties))?;
    writer.write(&batch)?;
    writer.close()?;
    Ok(())
}
```

In `crates/ab-morph-run/src/lib.rs`, next to the other `pub use` lines:

```rust
pub use import_aozora::{ImportSummary, run_import_aozora_metadata};
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer import_aozora`
Expected: all import_aozora tests PASS (12 unit + 7 integration).

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/import_aozora.rs crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/summary/summary_body.rs crates/ab-morph-run/src/summary/mod.rs
git commit -m "feat(import-aozora): run-scoped aozora_works.parquet producer"
```

---

### Task 4: CLI wiring

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs` (Command enum ~line 18–289; match ~line 322–707; tests module)

**Interfaces:**
- Consumes: `ab_morph_run::run_import_aozora_metadata(&run_dir, &from, force) -> Result<ImportSummary>` (Task 3).
- Produces: `ab-morph-run import-aozora-metadata --run-dir <dir> --from <dir> [--force]`.

- [ ] **Step 1: Write the failing parse test**

Add to the `tests` module in `main.rs` (model: `parses_rerun_full_command`):

```rust
    #[test]
    fn parses_import_aozora_metadata_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "import-aozora-metadata",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-07-05",
            "--from",
            "../abc/out/corpus",
            "--force",
        ]);

        let Command::ImportAozoraMetadata {
            run_dir,
            from,
            force,
        } = args.command
        else {
            panic!("expected import-aozora-metadata command");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-07-05")
        );
        assert_eq!(from, PathBuf::from("../abc/out/corpus"));
        assert!(force);
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p ab-morph-run --features test-analyzer parses_import_aozora_metadata_command`
Expected: COMPILE ERROR — no variant `ImportAozoraMetadata`.

- [ ] **Step 3: Add the Command variant and match arm**

In the `Command` enum, after the `RerunFull { ... }` variant:

```rust
    ImportAozoraMetadata {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long)]
        from: PathBuf,
        #[arg(long)]
        force: bool,
    },
```

In the `main()` match, after the `Command::RerunFull { ... } => ...` arm:

```rust
        Command::ImportAozoraMetadata {
            run_dir,
            from,
            force,
        } => {
            let summary = ab_morph_run::run_import_aozora_metadata(&run_dir, &from, force)?;
            let skipped = if summary.skipped_source_ids.is_empty() {
                String::new()
            } else {
                format!(": {}", summary.skipped_source_ids.join(", "))
            };
            eprintln!(
                "imported {} works covering {} sources into {}; skipped {} source(s){}",
                summary.works_imported,
                summary.sources_mapped,
                run_dir.join("aozora_works.parquet").display(),
                summary.skipped_source_ids.len(),
                skipped,
            );
            Ok(())
        }
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer parses_import_aozora_metadata_command`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "feat(cli): import-aozora-metadata subcommand"
```

---

### Task 5: Reader hardenings

**Files:**
- Modify: `crates/ab-morph-run/src/summary/interesting.rs:695-746` (`read_optional_work_map`, `rarity_config`)
- Test: same file's `tests` module

**Interfaces:**
- Consumes: existing `RarityConfig { work_by_source, basis, total }`, `write_fixture`, `write_aozora_works` test helpers.
- Produces: (1) a present-but-empty `aozora_works.parquet` behaves as absent; (2) `rarity_config` total = distinct mapped works + count of unmapped sources. No signature changes — the DuckDB engine (`interesting_sql.rs`) receives the same `RarityConfig` and already `coalesce`s unmapped sources to per-source keys, so this single function fixes both engines' denominators.

- [ ] **Step 1: Write the failing tests**

Add to the `tests` module in `interesting.rs` (near `work_map_switches_rarity_basis_and_dedups`):

```rust
    #[test]
    fn empty_work_map_is_treated_as_absent() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(dir.path());
        write_aozora_works(&run_dir, &[]);

        let summary = summarize(&run_dir, options_all());

        assert_eq!(summary.score_version.rarity_basis, "source");
    }

    #[test]
    fn rarity_denominator_counts_unmapped_sources() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(dir.path());
        // src-a mapped to w1; src-b left unmapped (falls back to a
        // per-source rarity key, so it must count in the denominator).
        write_aozora_works(&run_dir, &[("w1", "src-a")]);
        let source_ids: BTreeSet<String> =
            ["src-a", "src-b"].iter().map(|s| (*s).to_owned()).collect();

        let rarity = rarity_config(&run_dir, &source_ids).unwrap();

        assert_eq!(rarity.basis, "work");
        assert_eq!(rarity.total, 2);
    }
```

If the existing tests use different helper names than `summarize`/`options_all`, mirror whatever `work_map_switches_rarity_basis_and_dedups` calls — the assertion targets (`score_version.rarity_basis`, `rarity_config`) are the contract.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer empty_work_map_is_treated_as_absent rarity_denominator_counts_unmapped_sources`
Expected: `empty_work_map_is_treated_as_absent` FAILS (basis is `"work"`); `rarity_denominator_counts_unmapped_sources` FAILS (total is 1).

- [ ] **Step 3: Implement both hardenings**

In `read_optional_work_map` (interesting.rs:695), before the final `Ok(Some(map))`:

```rust
    if map.is_empty() {
        // A present-but-empty projection would flip rarity_basis to
        // "work" with total 0 and degenerate the IDF; the importer
        // refuses to create one, and the reader refuses to honor one.
        return Ok(None);
    }
```

Replace the `Some(map)` arm in `rarity_config` (interesting.rs:731-738):

```rust
        Some(map) => {
            let mut works = BTreeSet::new();
            let mut unmapped = 0usize;
            for source_id in source_ids {
                match map.get(source_id) {
                    Some(work_id) => {
                        works.insert(work_id.as_str());
                    }
                    // Unmapped sources contribute per-source rarity keys
                    // (see InMemoryAccumulators::record and the SQL
                    // coalesce), so they belong in the denominator.
                    None => unmapped += 1,
                }
            }
            ("work", works.len() + unmapped)
        }
```

- [ ] **Step 4: Run the full summarizer test suite**

Run: `cargo test -p ab-morph-run --features test-analyzer summary::interesting`
Expected: all PASS, including the two new tests and the untouched `work_map_switches_rarity_basis_and_dedups`.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/summary/interesting.rs
git commit -m "fix(interesting): empty work map treated as absent; unmapped sources count in rarity denominator"
```

---

### Task 6: End-to-end test (import → summarize flips basis)

**Files:**
- Modify: `crates/ab-morph-run/src/import_aozora.rs` (tests module)

**Interfaces:**
- Consumes: `run_import_aozora_metadata` (Task 3); `crate::summarize_warehouse_interesting`, `crate::WarehouseInterestingOptions`, `crate::InterestingEngine`, `crate::InterestingTextFilter`, `crate::WarehouseFeatureProfile`; `crate::warehouse::schema::{NwayRegionAnalyzerRow, NwayRegionRow, RunAnalyzerRow, RunRow, SCHEMA_VERSION, SourceRow, WarehousePaths}`; `crate::warehouse::writer::WarehouseWriter`.
- Produces: proof that a produced sidecar drives `rarity_basis = "work"` through the real reader.

- [ ] **Step 1: Write the failing test**

Append inside `mod tests` in `import_aozora.rs`. This builds a minimal real warehouse run (mirroring `interesting.rs::write_fixture` but with Aozora-style source ids — two files of one card plus one distinct card, sharing a segmentation disagreement):

```rust
    use crate::warehouse::schema::{
        NwayRegionAnalyzerRow, NwayRegionRow, RunAnalyzerRow, RunRow, SCHEMA_VERSION, SourceRow,
        WarehousePaths,
    };
    use crate::warehouse::writer::WarehouseWriter;

    fn aozora_run_fixture(root: &Path) -> std::path::PathBuf {
        const RUN: &str = "run-aozora";
        const SOURCES: [&str; 3] = [
            "000001_10-aaaaaaaaaaaa",
            "000001_10-bbbbbbbbbbbb",
            "000002_20-cccccccccccc",
        ];
        let paths = WarehousePaths::new(root, RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: SCHEMA_VERSION,
                run_id: RUN.to_owned(),
                created_at_utc: "2026-07-06T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: SOURCES.len() as u64,
                analyzer_count: 2,
                error_count: 0,
            }])
            .unwrap();
        writer
            .append_run_analyzers(&[
                RunAnalyzerRow {
                    run_id: RUN.to_owned(),
                    analyzer_id: "vibrato".to_owned(),
                    analyzer_arg: "vibrato".to_owned(),
                    analyzer_family: "vibrato".to_owned(),
                },
                RunAnalyzerRow {
                    run_id: RUN.to_owned(),
                    analyzer_id: "sudachi-a".to_owned(),
                    analyzer_arg: "sudachi-a".to_owned(),
                    analyzer_family: "sudachi".to_owned(),
                },
            ])
            .unwrap();
        writer
            .append_sources(
                &SOURCES
                    .iter()
                    .map(|source_id| SourceRow {
                        run_id: RUN.to_owned(),
                        source_id: (*source_id).to_owned(),
                        text_id: (*source_id).to_owned(),
                        aat_path: format!("{source_id}.json"),
                        source_bytes: 100,
                        source_chars: 50,
                    })
                    .collect::<Vec<_>>(),
            )
            .unwrap();
        writer
            .append_nway_regions(
                &SOURCES
                    .iter()
                    .map(|source_id| NwayRegionRow {
                        run_id: RUN.to_owned(),
                        source_id: (*source_id).to_owned(),
                        text_id: (*source_id).to_owned(),
                        region_index: 0,
                        byte_start: 0,
                        byte_end: 6,
                        char_start: 0,
                        char_end: 2,
                        is_nonempty_whitespace: false,
                        is_agreement: false,
                        has_coverage_mismatch: false,
                        has_segmentation_disagreement: true,
                        has_feature_disagreement: false,
                    })
                    .collect::<Vec<_>>(),
            )
            .unwrap();
        let mut region_analyzers = Vec::new();
        for source_id in SOURCES {
            region_analyzers.push(NwayRegionAnalyzerRow {
                run_id: RUN.to_owned(),
                source_id: source_id.to_owned(),
                text_id: source_id.to_owned(),
                region_index: 0,
                analyzer_id: "vibrato".to_owned(),
                covers_exactly: true,
                morpheme_start: 0,
                morpheme_end: 1,
                surfaces: vec!["今日".to_owned()],
            });
            region_analyzers.push(NwayRegionAnalyzerRow {
                run_id: RUN.to_owned(),
                source_id: source_id.to_owned(),
                text_id: source_id.to_owned(),
                region_index: 0,
                analyzer_id: "sudachi-a".to_owned(),
                covers_exactly: true,
                morpheme_start: 0,
                morpheme_end: 2,
                surfaces: vec!["今".to_owned(), "日".to_owned()],
            });
        }
        writer.append_nway_region_analyzers(&region_analyzers).unwrap();
        writer.finalize().unwrap();
        paths.final_dir
    }

    #[test]
    fn imported_sidecar_flips_rarity_basis_to_work() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = aozora_run_fixture(dir.path());
        let export = dir.path().join("export");
        write_export_record(&export, "000010", &record_value("000010"));
        write_export_record(&export, "000020", &record_value("000020"));

        run_import_aozora_metadata(&run_dir, &export, false).unwrap();
        let summary = crate::summarize_warehouse_interesting(
            &run_dir,
            crate::WarehouseInterestingOptions {
                limit: 10,
                filter: crate::InterestingTextFilter::All,
                anomalies: 0,
                explain: None,
                max_region_examples: 5,
                engine: crate::InterestingEngine::InMemory,
                feature_profile: crate::WarehouseFeatureProfile::Core,
            },
        )
        .unwrap();

        assert_eq!(summary.score_version.rarity_basis, "work");
    }
```

If `WarehouseInterestingOptions` field names differ, mirror the construction used in `interesting.rs` tests — the assertion target is `score_version.rarity_basis == "work"` after a real import.

- [ ] **Step 2: Run test to verify current state**

Run: `cargo test -p ab-morph-run --features test-analyzer imported_sidecar_flips_rarity_basis_to_work`
Expected: PASS immediately if Tasks 3+5 are correct (this is an integration proof, not a red-green cycle — if it FAILS, the producer/reader contract is broken; debug before proceeding).

- [ ] **Step 3: Run the whole package suite**

Run: `cargo test -p ab-morph-run --features test-analyzer`
Expected: all PASS.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-morph-run/src/import_aozora.rs
git commit -m "test(import-aozora): end-to-end import flips rarity_basis to work"
```

---

### Task 7: Governing-spec amendments

**Files:**
- Modify: `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`

**Interfaces:** documentation only; exact edits below (spec §Governing-spec amendments).

- [ ] **Step 1: Resolve the import-step TBD (§Cross-Repo Dependency, boundary rules bullet)**

Replace the sentence fragment:

```
The `aozora_works` projection is regenerated by an import step (TBD: `ab-morph-run import-aozora-metadata --from <abc export dir>`); it is **never hand-edited** in the warehouse.
```

with:

```
The `aozora_works` projection is regenerated by `ab-morph-run import-aozora-metadata --run-dir <run> --from <abc export dir>`, where the export dir is ABC's `out/corpus` layout (`works/<work_id>.json`); it is **never hand-edited** in the warehouse. Producer design: `docs/superpowers/specs/2026-07-06-aozora-works-import-design.md`.
```

- [ ] **Step 2: Correct the rarity dedup claim (§Per-Signal Definitions, bold paragraph after the signal table)**

Replace:

```
**Rarity counting is work-based, not file-based.** An earlier draft conflated the column name `source_count` (inherited from feature-pattern materialization, which counts source files) with the semantic value. Pattern frequency is inflated ~2× when one work has two editions (旧字/新字, or different encodings), so the IDF denominator is **distinct `work_id`**, joined through `aozora_works`. When `aozora_works` is absent, the ranker falls back to `sources` count and records `rarity_basis = "source"` so consumers don't cross-compare scores across different rarity bases.
```

with:

```
**Rarity counting is work-based, not file-based.** An earlier draft conflated the column name `source_count` (inherited from feature-pattern materialization, which counts source files) with the semantic value. The IDF denominator is **distinct `work_id`** (the Aozora card id, imported through `aozora_works`), so a pattern appearing in multiple files of one card counts once — measured 2026-07-06: 287 multi-file-card sources, 1.6% of the corpus, a 2× inflation *per affected pattern*. Paired 旧字/新字 editions are separate cards = separate work_ids and **deliberately count as distinct works** (owner decision 2026-07-06: they are distinct analysis targets that may pair better with different tokenizers, and they feed the paired-edition metamorphic tests; any future merge is a TEI-edition/token-variant concern, and an edition-cluster identity would arrive as a new `rarity_basis` value, never silently under `"work"`). When `aozora_works` is absent, the ranker falls back to `sources` count and records `rarity_basis = "source"` so consumers don't cross-compare scores across different rarity bases.
```

- [ ] **Step 3: Same correction in §Aozora-Specific Corpus Handling / Deduplication**

Replace:

```
Aozora works may appear as multiple files (different encodings, anthologized reprints, multi-volume works). Pattern frequency counting uses distinct `work_id` from `aozora_works` (which is itself an ABC projection of `work.work_id`), not raw file count. Without this, pattern frequencies are inflated ~2× and the rarity signal is corrupted. This is enforced at the rarity-signal level (§Per-Signal Definitions) and recorded as `rarity_basis = "work"` in the score block.
```

with:

```
Aozora works may appear as multiple files under one card (different encodings, anthologized reprints). Pattern frequency counting uses distinct `work_id` from `aozora_works` (an ABC projection of `work.work_id`, i.e. the Aozora card id), not raw file count — without this, affected patterns are counted 2× (287 multi-file-card sources measured 2026-07-06). Paired 旧字/新字 editions are separate cards and deliberately remain distinct works (see §Per-Signal Definitions); serials sharing a title (e.g. 銭形平次捕物控, 438 cards) are correctly distinct works. Enforced at the rarity-signal level and recorded as `rarity_basis = "work"` in the score block.
```

- [ ] **Step 4: Amend Decision 3 (§Decisions row 3)**

Replace the Decision column text:

```
`SCHEMA_VERSION` bumped at first sidecar table; reader rule relaxed to "reject > reader max"
```

with:

```
`SCHEMA_VERSION` bumped at the first *analysis-pass-produced* sidecar table (Phase 3 `projection_spans`); reader rule then relaxed to "reject > reader max". Post-hoc imported, presence-probed sidecars (`aozora_works`) are version-neutral
```

- [ ] **Step 5: Add the zero-mapped-import row (§Error Behavior table)**

After the row `| aozora_works import out of sync with ABC schema … |`, add:

```
| `import-aozora-metadata` maps zero sources to ABC works (wrong export root, empty `works/`) | Hard error at import; no file written. A present-but-empty `aozora_works.parquet` is likewise treated as absent by readers |
```

- [ ] **Step 6: Commit**

```bash
git add docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md
git commit -m "docs(spec): resolve aozora_works import TBD; correct dedup semantics; amend Decision 3"
```

---

### Task 8: Real-data validation on the canonical run

**Files:**
- Create: `scratch/full-novel-interesting-workbasis.json` (ranking output, committed like the existing `scratch/full-novel-interesting.json`)

**Interfaces:** consumes the shipped binary; produces the canonical run's sidecar and the work-basis ranking artifact.

- [ ] **Step 1: Build release binary**

Run: `cargo build --release -p ab-morph-run`
Expected: clean build.

- [ ] **Step 2: Import into the canonical run**

Run:

```bash
target/release/ab-morph-run import-aozora-metadata \
  --run-dir /db/ab-validator/morph-warehouse/runs/full-2026-07-05_164518-jobs0 \
  --from /home/bor/Projects/abc/out/corpus
```

Expected stderr (exact counts per spec §Reader hardenings): `imported 17596 works covering 17883 sources into …/aozora_works.parquet; skipped 2 source(s): 000025_kantou-20379f2add12, 000989_352-69e66488e1fe`.
If counts differ, STOP and investigate before proceeding.

- [ ] **Step 3: Re-rank with work basis (~10 min, DuckDB engine auto)**

Run:

```bash
target/release/ab-morph-run summarize-warehouse-interesting \
  --run-dir /db/ab-validator/morph-warehouse/runs/full-2026-07-05_164518-jobs0 \
  --limit 50 --format json \
  --output scratch/full-novel-interesting-workbasis.json
```

Expected: exits 0.

- [ ] **Step 4: Verify the score block and compare rankings**

Run:

```bash
python3 - <<'EOF'
import json
new = json.load(open('scratch/full-novel-interesting-workbasis.json'))
old = json.load(open('scratch/full-novel-interesting.json'))
assert new['score_version']['rarity_basis'] == 'work', new['score_version']
new_ids = [p['pattern_id'] for p in new['patterns']]
old_ids = [p['pattern_id'] for p in old['patterns']]
overlap = len(set(new_ids) & set(old_ids))
print(f"rarity_basis=work OK; top-50 overlap with source-basis: {overlap}/50")
EOF
```

Expected: assertion passes; overlap printed (expected high — only 1.6% of sources merged). If the JSON key names differ (e.g. `score_version` nesting), adapt the probe to the actual output shape; the required check is `rarity_basis == "work"`.

- [ ] **Step 5: Commit the artifact**

```bash
git add scratch/full-novel-interesting-workbasis.json
git commit -m "data: work-basis top-50 ranking for canonical full run"
```

---

## Self-review notes

- Spec coverage: CLI contract (Task 4), data flow + error table (Task 3), column projection (Tasks 2–3), reader hardenings (Task 5), e2e (Task 6), governing-spec amendments (Task 7), real-data validation with expected counts (Task 8). Decision 2 (no version bump) = no task, verified by absence of `sql.rs`/`schema.rs` edits.
- Helper-name caveats are explicit where the implementer must mirror existing test scaffolding (`summarize`/`options_all` in Task 5, options struct in Task 6) — the assertion targets are fixed, the scaffolding names are not part of the contract.
- Type consistency: `parse_source_id -> Option<String>`, `extract_year -> Option<i32>`, `run_import_aozora_metadata(&Path, &Path, bool) -> Result<ImportSummary>` used identically in Tasks 3, 4, 6.
