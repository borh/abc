//! Content-hash pattern identity for the interestingness ranker.
//!
//! `pattern_id` is a contract: two implementations must produce identical
//! bytes for the same pattern or review verdicts orphan. The canonical part
//! sequence below is pinned by golden tests; any change to normalization,
//! part order, scope tokens, or the hash algorithm requires bumping
//! [`PATTERN_ID_VERSION`], which orphans existing verdicts.

use ab_diff_utils::hash_string_sequence;
use unicode_normalization::UnicodeNormalization;

use super::summary_body::NwayPatternKey;
use crate::nway::NwayFeatureScopeRow;

pub(crate) const PATTERN_ID_VERSION: u32 = 1;

/// Unicode NFC, collapse runs of ASCII whitespace to one U+0020, trim ends.
pub(crate) fn nfc_collapse_ws(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    let mut pending_space = false;
    for ch in value.nfc() {
        if ch.is_ascii_whitespace() {
            pending_space = true;
            continue;
        }
        if pending_space && !out.is_empty() {
            out.push(' ');
        }
        pending_space = false;
        out.push(ch);
    }
    out
}

/// Canonical token for a feature scope. The mapping is a closed table:
/// adding a `NwayFeatureScopeRow` variant fails this match at compile time,
/// forcing an explicit decision (and a `PATTERN_ID_VERSION` bump).
fn scope_token(scope: &NwayFeatureScopeRow) -> String {
    match scope {
        NwayFeatureScopeRow::WholeRegion => "whole_region".to_owned(),
        NwayFeatureScopeRow::TokenPosition { position } => format!("pos:{position}"),
        NwayFeatureScopeRow::Surface { surface } => {
            format!("surf:{}", nfc_collapse_ws(surface))
        }
    }
}

/// The pinned canonical part sequence. Injective because
/// [`hash_string_sequence`] length-prefixes every part and the tag/count
/// structure admits exactly one parse:
/// `["pattern-v1", kind, "fkey", fkey, "scope", scope,
///   ("group", n, surface*n, "analyzers", ids)*,
///   ("value"|"null", value, "analyzers", ids)*]`.
fn canonical_parts(key: &NwayPatternKey) -> Vec<String> {
    let mut parts = vec!["pattern-v1".to_owned(), key.kind.clone()];
    parts.push("fkey".to_owned());
    parts.push(key.feature_key.clone().unwrap_or_default());
    parts.push("scope".to_owned());
    parts.push(
        key.feature_scope
            .as_ref()
            .map(scope_token)
            .unwrap_or_default(),
    );

    let mut groups = key
        .segmentation_groups
        .iter()
        .map(|group| {
            let surfaces = group
                .surfaces
                .iter()
                .map(|surface| nfc_collapse_ws(surface))
                .collect::<Vec<_>>();
            let mut analyzers = group.analyzers.clone();
            analyzers.sort();
            (surfaces, analyzers.join(","))
        })
        .collect::<Vec<_>>();
    groups.sort();
    for (surfaces, analyzers) in groups {
        parts.push("group".to_owned());
        parts.push(surfaces.len().to_string());
        parts.extend(surfaces);
        parts.push("analyzers".to_owned());
        parts.push(analyzers);
    }

    let mut values = key
        .feature_values
        .iter()
        .map(|value_group| {
            let (tag, value) = match &value_group.value {
                None => ("null", String::new()),
                Some(value) => ("value", nfc_collapse_ws(value)),
            };
            let mut analyzers = value_group.analyzers.clone();
            analyzers.sort();
            (tag, value, analyzers.join(","))
        })
        .collect::<Vec<_>>();
    values.sort();
    for (tag, value, analyzers) in values {
        parts.push(tag.to_owned());
        parts.push(value);
        parts.push("analyzers".to_owned());
        parts.push(analyzers);
    }
    parts
}

/// Returns `"sha256:<hex>"` over the canonical part sequence.
pub(crate) fn pattern_id(key: &NwayPatternKey) -> String {
    hash_string_sequence(&canonical_parts(key))
}

/// Raw digest variant for digest-keyed maps: full-corpus accumulation
/// holds tens of millions of patterns, and 32 raw bytes beat a 71-byte
/// hex string (and beat cloning the whole key, which is what an
/// `index_of: BTreeMap<NwayPatternKey, _>` costs).
pub(crate) fn pattern_digest(key: &NwayPatternKey) -> [u8; 32] {
    ab_diff_utils::hash_string_sequence_raw(&canonical_parts(key))
}

/// `"sha256:<hex>"` from a raw digest; matches [`pattern_id`] output.
pub(crate) fn pattern_id_from_digest(digest: &[u8; 32]) -> String {
    use std::fmt::Write;
    let mut out = String::with_capacity(7 + 64);
    out.push_str("sha256:");
    for byte in digest {
        let _ = write!(out, "{byte:02x}");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nway::{NwayFeatureValueGroupRow, NwaySegmentationGroupRow};
    use proptest::prelude::*;

    fn feature_key(
        feature_key: &str,
        scope: NwayFeatureScopeRow,
        values: Vec<(Option<&str>, Vec<&str>)>,
    ) -> NwayPatternKey {
        NwayPatternKey {
            kind: "feature".to_owned(),
            segmentation_groups: Vec::new(),
            feature_key: Some(feature_key.to_owned()),
            feature_scope: Some(scope),
            feature_values: values
                .into_iter()
                .map(|(value, analyzers)| NwayFeatureValueGroupRow {
                    value: value.map(str::to_owned),
                    analyzers: analyzers.into_iter().map(str::to_owned).collect(),
                })
                .collect(),
        }
    }

    fn segmentation_key(kind: &str, groups: Vec<(Vec<&str>, Vec<&str>)>) -> NwayPatternKey {
        NwayPatternKey {
            kind: kind.to_owned(),
            segmentation_groups: groups
                .into_iter()
                .map(|(surfaces, analyzers)| NwaySegmentationGroupRow {
                    surfaces: surfaces.into_iter().map(str::to_owned).collect(),
                    analyzers: analyzers.into_iter().map(str::to_owned).collect(),
                })
                .collect(),
            feature_key: None,
            feature_scope: None,
            feature_values: Vec::new(),
        }
    }

    #[test]
    fn nfc_collapse_ws_normalizes_and_collapses() {
        // U+304B U+3099 (decomposed が) -> U+304C (precomposed).
        assert_eq!(nfc_collapse_ws("\u{304b}\u{3099}"), "\u{304c}");
        assert_eq!(nfc_collapse_ws("  a \t\n b  "), "a b");
        assert_eq!(nfc_collapse_ws(""), "");
        assert_eq!(nfc_collapse_ws(" \t "), "");
    }

    #[test]
    fn pattern_id_is_stable_golden() {
        let key = feature_key(
            "pos1",
            NwayFeatureScopeRow::WholeRegion,
            vec![
                (Some("名詞"), vec!["vibrato"]),
                (Some("動詞"), vec!["sudachi-a", "sudachi-c"]),
            ],
        );
        let expected_parts = [
            "pattern-v1",
            "feature",
            "fkey",
            "pos1",
            "scope",
            "whole_region",
            "value",
            "動詞",
            "analyzers",
            "sudachi-a,sudachi-c",
            "value",
            "名詞",
            "analyzers",
            "vibrato",
        ]
        .iter()
        .map(|part| (*part).to_owned())
        .collect::<Vec<_>>();
        assert_eq!(pattern_id(&key), hash_string_sequence(&expected_parts));
        // Byte-pinned golden: changing normalization, part order, scope
        // tokens, or hashing breaks this and requires a PATTERN_ID_VERSION
        // bump (see module docs).
        assert_eq!(
            pattern_id(&key),
            "sha256:3ba48f4844619d68d4e390f5c027c0f0628dc4b9a629ff6097b3d3514eb153f8"
        );
    }

    #[test]
    fn digest_path_matches_string_path() {
        let key = feature_key(
            "pos1",
            NwayFeatureScopeRow::WholeRegion,
            vec![(Some("x"), vec!["a"]), (Some("y"), vec!["b"])],
        );
        assert_eq!(pattern_id(&key), pattern_id_from_digest(&pattern_digest(&key)));
    }

    #[test]
    fn analyzer_order_does_not_change_pattern_id() {
        let left = feature_key(
            "pos1",
            NwayFeatureScopeRow::WholeRegion,
            vec![(Some("x"), vec!["b", "a"]), (Some("y"), vec!["c"])],
        );
        let right = feature_key(
            "pos1",
            NwayFeatureScopeRow::WholeRegion,
            vec![(Some("y"), vec!["c"]), (Some("x"), vec!["a", "b"])],
        );
        assert_eq!(pattern_id(&left), pattern_id(&right));
    }

    #[test]
    fn value_to_analyzer_assignment_is_direction_sensitive() {
        let left = feature_key(
            "pos1",
            NwayFeatureScopeRow::WholeRegion,
            vec![(Some("x"), vec!["a"]), (Some("y"), vec!["b"])],
        );
        let right = feature_key(
            "pos1",
            NwayFeatureScopeRow::WholeRegion,
            vec![(Some("y"), vec!["a"]), (Some("x"), vec!["b"])],
        );
        assert_ne!(pattern_id(&left), pattern_id(&right));
    }

    #[test]
    fn null_value_differs_from_empty_string_value() {
        let null = feature_key(
            "pos1",
            NwayFeatureScopeRow::WholeRegion,
            vec![(None, vec!["a"]), (Some("x"), vec!["b"])],
        );
        let empty = feature_key(
            "pos1",
            NwayFeatureScopeRow::WholeRegion,
            vec![(Some(""), vec!["a"]), (Some("x"), vec!["b"])],
        );
        assert_ne!(pattern_id(&null), pattern_id(&empty));
    }

    #[test]
    fn nfc_equivalent_values_share_pattern_id() {
        let decomposed = feature_key(
            "kana",
            NwayFeatureScopeRow::Surface {
                surface: "\u{304b}\u{3099}".to_owned(),
            },
            vec![(Some("\u{304b}\u{3099}"), vec!["a"]), (Some("x"), vec!["b"])],
        );
        let precomposed = feature_key(
            "kana",
            NwayFeatureScopeRow::Surface {
                surface: "\u{304c}".to_owned(),
            },
            vec![(Some("\u{304c}"), vec!["a"]), (Some("x"), vec!["b"])],
        );
        assert_eq!(pattern_id(&decomposed), pattern_id(&precomposed));
    }

    #[test]
    fn kind_separates_identical_group_structures() {
        let seg = segmentation_key(
            "segmentation",
            vec![(vec!["今日"], vec!["a"]), (vec!["今", "日"], vec!["b"])],
        );
        let cov = segmentation_key(
            "coverage",
            vec![(vec!["今日"], vec!["a"]), (vec!["今", "日"], vec!["b"])],
        );
        assert_ne!(pattern_id(&seg), pattern_id(&cov));
    }

    #[test]
    fn surface_split_boundaries_are_unambiguous() {
        let joined = segmentation_key(
            "segmentation",
            vec![(vec!["今日"], vec!["a"]), (vec!["x"], vec!["b"])],
        );
        let split = segmentation_key(
            "segmentation",
            vec![(vec!["今", "日"], vec!["a"]), (vec!["x"], vec!["b"])],
        );
        assert_ne!(pattern_id(&joined), pattern_id(&split));
    }

    fn arb_analyzers() -> impl Strategy<Value = Vec<String>> {
        proptest::collection::btree_set("[a-z][a-z0-9-]{0,8}", 1..4)
            .prop_map(|set| set.into_iter().collect())
    }

    fn arb_feature_values() -> impl Strategy<Value = Vec<NwayFeatureValueGroupRow>> {
        proptest::collection::vec(
            (
                proptest::option::of("\\PC{0,6}"),
                arb_analyzers(),
            )
                .prop_map(|(value, analyzers)| NwayFeatureValueGroupRow { value, analyzers }),
            2..4,
        )
    }

    proptest! {
        #[test]
        fn group_and_analyzer_permutations_are_id_invariant(
            values in arb_feature_values(),
            seed in any::<u64>(),
        ) {
            let key = feature_key("pos1", NwayFeatureScopeRow::WholeRegion, Vec::new());
            let key = NwayPatternKey { feature_values: values.clone(), ..key };

            let mut shuffled = values;
            // Deterministic permutation from the seed: rotate groups and
            // reverse analyzer lists on odd seeds.
            let rotation = (seed as usize) % shuffled.len().max(1);
            shuffled.rotate_left(rotation);
            if seed % 2 == 1 {
                for group in &mut shuffled {
                    group.analyzers.reverse();
                }
            }
            let base = feature_key("pos1", NwayFeatureScopeRow::WholeRegion, Vec::new());
            let shuffled_key = NwayPatternKey { feature_values: shuffled, ..base };

            prop_assert_eq!(pattern_id(&key), pattern_id(&shuffled_key));
        }

        #[test]
        fn equal_ids_imply_equal_canonical_parts(
            left in arb_feature_values(),
            right in arb_feature_values(),
        ) {
            let base = feature_key("pos1", NwayFeatureScopeRow::WholeRegion, Vec::new());
            let left_key = NwayPatternKey { feature_values: left, ..base.clone() };
            let right_key = NwayPatternKey { feature_values: right, ..base };
            let ids_equal = pattern_id(&left_key) == pattern_id(&right_key);
            let parts_equal = canonical_parts(&left_key) == canonical_parts(&right_key);
            prop_assert_eq!(ids_equal, parts_equal);
        }
    }
}
