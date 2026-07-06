//! Producer for the `aozora_works.parquet` sidecar: an imported projection
//! of ABC's `metadata-record.schema.json` export.
//! Design: docs/superpowers/specs/2026-07-06-aozora-works-import-design.md

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
