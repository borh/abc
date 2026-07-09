//! Analyzer analysis grouping and ID shortening.
//!
//! Assembles `RegionAnalyzerRow`s into `AnalyzerAnalysis` groups, where each group
//! collects analyzers that agree on `(covers_exactly, surfaces, tokens)`. Also provides ID
//! shortening that strips leading `vibrato:unidic-` and trailing `-<digits>` suffixes.

use std::collections::{BTreeMap, BTreeSet};

use crate::hydrate::tables::{RegionAnalyzerRow, Token};

/// A group of analyzer results that agree on coverage and tokens.
#[derive(Debug, Clone, PartialEq, serde::Serialize)]
pub struct AnalyzerAnalysis {
    pub analyzer_ids: Vec<String>,
    pub covers_exactly: bool,
    pub surfaces: Vec<String>,
    pub tokens: Vec<Token>,
}

/// Groups analyses by `(covers_exactly, surfaces, tokens)`, collecting analyzers that agree.
/// Groups are ordered by first (sorted) analyzer ID. `analyzer_ids` within each group
/// stay sorted (rows arrive pre-sorted by analyzer_id per tables.rs contract).
pub fn group_analyses(
    rows: &[RegionAnalyzerRow],
    tokens: &BTreeMap<(String, String, u64), Token>,
    source_id: &str,
) -> Vec<AnalyzerAnalysis> {
    // rows arrive sorted by analyzer_id (tables.rs contract).
    let mut groups: Vec<AnalyzerAnalysis> = Vec::new();
    for row in rows {
        let row_tokens: Vec<Token> = (row.morpheme_start..row.morpheme_end)
            .filter_map(|index| {
                tokens
                    .get(&(source_id.to_owned(), row.analyzer_id.clone(), index))
                    .cloned()
            })
            .collect();
        match groups.iter_mut().find(|group| {
            group.covers_exactly == row.covers_exactly
                && group.surfaces == row.surfaces
                && group.tokens == row_tokens
        }) {
            Some(group) => group.analyzer_ids.push(row.analyzer_id.clone()),
            None => groups.push(AnalyzerAnalysis {
                analyzer_ids: vec![row.analyzer_id.clone()],
                covers_exactly: row.covers_exactly,
                surfaces: row.surfaces.clone(),
                tokens: row_tokens,
            }),
        }
    }
    groups
}

/// Maps full analyzer IDs to shortened forms. Strips leading `vibrato:unidic-` and
/// a trailing `-<digits>` run. If two full IDs collide on the same short form,
/// both keep their full IDs.
pub fn short_analyzer_ids(full_ids: &BTreeSet<String>) -> BTreeMap<String, String> {
    let candidate = |id: &str| -> String {
        let stripped = id.strip_prefix("vibrato:unidic-").unwrap_or(id);
        match stripped.rfind('-') {
            Some(idx)
                if stripped[idx + 1..].chars().all(|c| c.is_ascii_digit())
                    && !stripped[idx + 1..].is_empty() =>
            {
                stripped[..idx].to_owned()
            }
            _ => stripped.to_owned(),
        }
    };
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();
    for id in full_ids {
        *counts.entry(candidate(id)).or_default() += 1;
    }
    full_ids
        .iter()
        .map(|id| {
            let short = candidate(id);
            let value = if counts[&short] > 1 {
                id.clone()
            } else {
                short
            };
            (id.clone(), value)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn groups_identical_analyses_and_orders_deterministically() {
        use crate::hydrate::tables::{RegionAnalyzerRow, Token};
        use std::collections::BTreeMap;

        let token = |surface: &str, cs: u64, ce: u64, pos1: &str| Token {
            surface: surface.to_owned(),
            char_start: cs,
            char_end: ce,
            features: [("pos1".to_owned(), pos1.to_owned())].into(),
        };
        // vibrato and sudachi-c agree (one token); sudachi-a splits in two.
        let rows = vec![
            RegionAnalyzerRow {
                analyzer_id: "sudachi-a".to_owned(),
                covers_exactly: true,
                morpheme_start: 1,
                morpheme_end: 3,
                surfaces: vec!["猫".to_owned(), "である".to_owned()],
            },
            RegionAnalyzerRow {
                analyzer_id: "sudachi-c".to_owned(),
                covers_exactly: true,
                morpheme_start: 1,
                morpheme_end: 2,
                surfaces: vec!["猫である".to_owned()],
            },
            RegionAnalyzerRow {
                analyzer_id: "vibrato:unidic-cwj-202512".to_owned(),
                covers_exactly: true,
                morpheme_start: 1,
                morpheme_end: 2,
                surfaces: vec!["猫である".to_owned()],
            },
        ];
        let mut tokens: BTreeMap<(String, String, u64), Token> = BTreeMap::new();
        tokens.insert(
            ("src-a".into(), "sudachi-a".into(), 1),
            token("猫", 6, 7, "名詞"),
        );
        tokens.insert(
            ("src-a".into(), "sudachi-a".into(), 2),
            token("である", 7, 10, "助動詞"),
        );
        tokens.insert(
            ("src-a".into(), "sudachi-c".into(), 1),
            token("猫である", 6, 10, "名詞"),
        );
        tokens.insert(
            ("src-a".into(), "vibrato:unidic-cwj-202512".into(), 1),
            token("猫である", 6, 10, "名詞"),
        );

        let groups = group_analyses(&rows, &tokens, "src-a");
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].analyzer_ids, vec!["sudachi-a"]);
        assert_eq!(groups[0].tokens.len(), 2);
        assert_eq!(
            groups[1].analyzer_ids,
            vec!["sudachi-c", "vibrato:unidic-cwj-202512"]
        );
        assert_eq!(groups[1].tokens[0].surface, "猫である");
    }

    #[test]
    fn differing_surfaces_prevent_grouping_and_render_from_surfaces() {
        use crate::hydrate::tables::RegionAnalyzerRow;
        use std::collections::BTreeMap;

        // No tokens recorded at all: without surfaces in the key these two
        // would collapse into one "identical" group despite segmenting
        // differently.
        let rows = vec![
            RegionAnalyzerRow {
                analyzer_id: "sudachi-a".to_owned(),
                covers_exactly: true,
                morpheme_start: 0,
                morpheme_end: 2,
                surfaces: vec!["今".to_owned(), "日".to_owned()],
            },
            RegionAnalyzerRow {
                analyzer_id: "vibrato".to_owned(),
                covers_exactly: true,
                morpheme_start: 0,
                morpheme_end: 1,
                surfaces: vec!["今日".to_owned()],
            },
        ];
        let tokens = BTreeMap::new();
        let groups = group_analyses(&rows, &tokens, "src-a");
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].surfaces, vec!["今", "日"]);
        assert_eq!(groups[1].surfaces, vec!["今日"]);
    }

    #[test]
    fn short_ids_strip_vibrato_prefix_and_date_suffix() {
        let ids: std::collections::BTreeSet<String> = [
            "vibrato:unidic-csj-202512",
            "vibrato:unidic-kinsei-bungo-202512",
            "sudachi-a",
        ]
        .map(str::to_owned)
        .into();
        let map = short_analyzer_ids(&ids);
        assert_eq!(map["vibrato:unidic-csj-202512"], "csj");
        assert_eq!(map["vibrato:unidic-kinsei-bungo-202512"], "kinsei-bungo");
        assert_eq!(map["sudachi-a"], "sudachi-a");
    }

    #[test]
    fn short_ids_keep_full_form_on_collision() {
        let ids: std::collections::BTreeSet<String> =
            ["vibrato:unidic-csj-202512", "vibrato:unidic-csj-202601"]
                .map(str::to_owned)
                .into();
        let map = short_analyzer_ids(&ids);
        assert_eq!(
            map["vibrato:unidic-csj-202512"],
            "vibrato:unidic-csj-202512"
        );
        assert_eq!(
            map["vibrato:unidic-csj-202601"],
            "vibrato:unidic-csj-202601"
        );
    }
}
