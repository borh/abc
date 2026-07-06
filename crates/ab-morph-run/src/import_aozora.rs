//! Producer for the `aozora_works.parquet` sidecar: an imported projection
//! of ABC's `metadata-record.schema.json` export.
//! Design: docs/superpowers/specs/2026-07-06-aozora-works-import-design.md

use std::path::Path;

use anyhow::{Result, bail};
use serde::Deserialize;

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
}
