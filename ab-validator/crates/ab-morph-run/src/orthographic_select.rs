//! Lane A (historical-kana): select which works a run processes by their
//! `orthographic_style`, so old-kana works can be analyzed under a historical
//! UniDic (e.g. `vibrato:unidic-kindai-bungo-202512`).
//!
//! This is a **run-eligibility filter, outside normalization** (Issue 2
//! decision I2-D17b): it only narrows the *set of works* a run analyzes; it
//! never changes how any work's text is normalized, so a work's derived input
//! stays a pure function of `(source, normalization policy)`. The metadata that
//! drives the filter (`orthographic_style`) is read from the post-run
//! `aozora_works.parquet` sidecar (produced by `import-aozora-metadata`), keyed
//! by `source_id`; it is never consulted per-document during analysis.

use std::collections::BTreeSet;
use std::fs::File;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use arrow_array::{Array, StringArray};

use crate::compact::source_id_from_aat_path;

/// The old-**kana** `orthographic_style` values — those written in historical
/// kana orthography (歴史的仮名遣い). Deliberately **excludes** `旧字新仮名`
/// (old kanji but already-modern kana — a separate axis) and `その他`, matching
/// the U3 contract (`2026-07-08-ortho-historical-scope-and-determinism-tier-design.md`).
pub(crate) const OLD_KANA_STYLES: [&str; 2] = ["新字旧仮名", "旧字旧仮名"];

/// The set of old-kana styles as owned strings, for use as an eligibility filter.
pub(crate) fn old_kana_styles() -> BTreeSet<String> {
    OLD_KANA_STYLES.iter().map(|s| (*s).to_owned()).collect()
}

/// Read an `aozora_works.parquet` sidecar and return the set of `source_id`s
/// whose `orthographic_style` is in `allowed`.
///
/// # Errors
///
/// Returns an error if the file cannot be opened/parsed or is missing the
/// `source_id` / `orthographic_style` columns.
pub(crate) fn eligible_source_ids(
    works_parquet: &Path,
    allowed: &BTreeSet<String>,
) -> Result<BTreeSet<String>> {
    let file = File::open(works_parquet)
        .with_context(|| format!("failed to open {}", works_parquet.display()))?;
    let reader = parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file)
        .with_context(|| format!("failed to read {}", works_parquet.display()))?
        .build()
        .with_context(|| {
            format!(
                "failed to build parquet reader for {}",
                works_parquet.display()
            )
        })?;

    let mut ids = BTreeSet::new();
    for batch in reader {
        let batch = batch.context("failed to read a record batch from aozora_works.parquet")?;
        let sid_idx = batch
            .schema()
            .index_of("source_id")
            .context("aozora_works.parquet is missing a source_id column")?;
        let style_idx = batch
            .schema()
            .index_of("orthographic_style")
            .context("aozora_works.parquet is missing an orthographic_style column")?;
        let sids = batch
            .column(sid_idx)
            .as_any()
            .downcast_ref::<StringArray>()
            .context("aozora_works.source_id is not a StringArray")?;
        let styles = batch
            .column(style_idx)
            .as_any()
            .downcast_ref::<StringArray>()
            .context("aozora_works.orthographic_style is not a StringArray")?;
        for row in 0..batch.num_rows() {
            if styles.is_valid(row) && allowed.contains(styles.value(row)) {
                ids.insert(sids.value(row).to_owned());
            }
        }
    }
    Ok(ids)
}

/// Retain only the AAT inputs whose `source_id` (the file stem, matching
/// [`source_id_from_aat_path`]) is in `allowed`; the rest are the works the
/// eligibility filter excludes.
pub(crate) fn filter_inputs_by_source_ids(
    inputs: Vec<PathBuf>,
    allowed: &BTreeSet<String>,
) -> Vec<PathBuf> {
    inputs
        .into_iter()
        .filter(|path| allowed.contains(&source_id_from_aat_path(path)))
        .collect()
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use arrow_array::RecordBatch;
    use arrow_schema::{DataType, Field, Schema};
    use parquet::arrow::ArrowWriter;

    use super::*;

    fn write_works_parquet(path: &Path, rows: &[(&str, &str)]) {
        let schema = Arc::new(Schema::new(vec![
            Field::new("source_id", DataType::Utf8, false),
            Field::new("orthographic_style", DataType::Utf8, true),
        ]));
        let source_ids = StringArray::from(rows.iter().map(|(s, _)| *s).collect::<Vec<_>>());
        let styles = StringArray::from(rows.iter().map(|(_, o)| *o).collect::<Vec<_>>());
        let batch =
            RecordBatch::try_new(schema.clone(), vec![Arc::new(source_ids), Arc::new(styles)])
                .unwrap();
        let file = File::create(path).unwrap();
        let mut writer = ArrowWriter::try_new(file, schema, None).unwrap();
        writer.write(&batch).unwrap();
        writer.close().unwrap();
    }

    #[test]
    fn selects_only_old_kana_source_ids() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("aozora_works.parquet");
        write_works_parquet(
            &path,
            &[
                ("natsume_100", "新字新仮名"), // modern → excluded
                ("mori_200", "新字旧仮名"),    // old kana → included
                ("koda_300", "旧字旧仮名"),    // old kana → included
                ("izumi_400", "旧字新仮名"),   // old kanji, modern kana → excluded
                ("other_500", "その他"),       // unknown → excluded
            ],
        );

        let ids = eligible_source_ids(&path, &old_kana_styles()).unwrap();
        assert_eq!(
            ids,
            ["koda_300".to_owned(), "mori_200".to_owned()]
                .into_iter()
                .collect()
        );
    }

    #[test]
    fn filter_inputs_keeps_only_matching_source_ids() {
        let allowed: BTreeSet<String> = ["mori_200".to_owned(), "koda_300".to_owned()]
            .into_iter()
            .collect();
        let inputs = vec![
            PathBuf::from("/aat/natsume_100.json"),
            PathBuf::from("/aat/mori_200.json"),
            PathBuf::from("/aat/koda_300.json"),
        ];
        let kept = filter_inputs_by_source_ids(inputs, &allowed);
        assert_eq!(
            kept,
            vec![
                PathBuf::from("/aat/mori_200.json"),
                PathBuf::from("/aat/koda_300.json"),
            ]
        );
    }

    #[test]
    fn old_kana_styles_excludes_modern_and_kyuji_shinkana() {
        let styles = old_kana_styles();
        assert!(styles.contains("新字旧仮名"));
        assert!(styles.contains("旧字旧仮名"));
        assert!(!styles.contains("旧字新仮名"));
        assert!(!styles.contains("新字新仮名"));
        assert!(!styles.contains("その他"));
    }
}
