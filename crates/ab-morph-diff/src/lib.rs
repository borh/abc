mod align;
mod error;
mod features;
mod model;
mod nway;
mod stats;
mod streaming;
mod validate;

pub use error::MorphDiffError;
pub use model::{
    AlignedMorpheme, Analysis, AnalyzerId, ChangedValue, CompactComparison,
    CompactComparisonExample, CompactExampleKind, CompactFeatureChange, Comparison,
    ComparisonStats, CoverageMismatch, CoverageMismatchKind, FeatureDiff, FeatureKey, FeatureMap,
    Morpheme, NwayAnalyzerRegion, NwayComparison, NwayFeatureGroup, NwayFeatureScope,
    NwayFeatureValueGroup, NwayRegion, NwaySegmentationGroup, NwayStats, Region, SegmentationDiff,
    SegmentationKind, TextId,
};
pub use validate::{validate_analysis, validate_analysis_against_source};

pub fn compare_pair(
    from: &Analysis,
    to: &Analysis,
    feature_context_keys: &[FeatureKey],
) -> Result<Comparison, MorphDiffError> {
    if from.text_id != to.text_id {
        return Err(MorphDiffError::TextIdMismatch {
            from: from.text_id.clone(),
            to: to.text_id.clone(),
        });
    }
    if from.source_text != to.source_text {
        return Err(MorphDiffError::SourceTextMismatch {
            text_id: from.text_id.clone(),
        });
    }
    validate_analysis(from)?;
    validate_analysis(to)?;

    let regions = align::align_regions(from, to)?;
    let feature_diffs = features::compare_feature_diffs(from, to, &regions, feature_context_keys);
    let stats = stats::derive_stats(from, to, &regions, &feature_diffs);
    Ok(Comparison {
        from_analyzer: from.analyzer.clone(),
        to_analyzer: to.analyzer.clone(),
        text_id: from.text_id.clone(),
        regions,
        feature_diffs,
        stats,
    })
}

pub fn compare_pair_with_source_text(
    from: &Analysis,
    to: &Analysis,
    source_text: &str,
    feature_context_keys: &[FeatureKey],
) -> Result<Comparison, MorphDiffError> {
    if from.text_id != to.text_id {
        return Err(MorphDiffError::TextIdMismatch {
            from: from.text_id.clone(),
            to: to.text_id.clone(),
        });
    }
    validate::validate_analysis_against_source(from, source_text)?;
    validate::validate_analysis_against_source(to, source_text)?;

    let source_len = source_text.chars().count();
    let regions = align::align_regions_with_source_len(from, to, source_len)?;
    let feature_diffs = features::compare_feature_diffs(from, to, &regions, feature_context_keys);
    let stats = stats::derive_stats_with_source_text(
        from,
        to,
        &regions,
        &feature_diffs,
        source_text,
        source_len,
    );
    Ok(Comparison {
        from_analyzer: from.analyzer.clone(),
        to_analyzer: to.analyzer.clone(),
        text_id: from.text_id.clone(),
        regions,
        feature_diffs,
        stats,
    })
}

pub fn compare_pair_compact_with_source_text(
    from: &Analysis,
    to: &Analysis,
    source_text: &str,
    feature_context_keys: &[FeatureKey],
    max_examples: usize,
) -> Result<CompactComparison, MorphDiffError> {
    streaming::compare_pair_compact_with_source_text(
        from,
        to,
        source_text,
        feature_context_keys,
        max_examples,
    )
}

pub fn compare_nway_with_source_text(
    analyses: &[Analysis],
    source_text: &str,
    compare_keys: &[FeatureKey],
) -> Result<NwayComparison, MorphDiffError> {
    nway::compare_nway_with_source_text(analyses, source_text, compare_keys)
}

pub fn visit_nway_regions_with_source_text(
    analyses: &[Analysis],
    source_text: &str,
    compare_keys: &[FeatureKey],
    visit: impl FnMut(&NwayRegion),
) -> Result<NwayStats, MorphDiffError> {
    nway::visit_nway_regions_with_source_text(analyses, source_text, compare_keys, visit)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        Analysis, ChangedValue, FeatureMap, MorphDiffError, Morpheme, Region, SegmentationKind,
        compare_pair, compare_pair_with_source_text,
    };

    fn features(values: &[(&str, Option<&str>)]) -> FeatureMap {
        values
            .iter()
            .map(|(key, value)| (key.to_string(), value.map(str::to_owned)))
            .collect()
    }

    fn m(source: &str, surface: &str, start: usize, end: usize, features: FeatureMap) -> Morpheme {
        let byte_start = source
            .char_indices()
            .nth(start)
            .map(|(idx, _)| idx)
            .unwrap_or(source.len());
        let byte_end = source
            .char_indices()
            .nth(end)
            .map(|(idx, _)| idx)
            .unwrap_or(source.len());
        Morpheme {
            surface: surface.to_owned(),
            byte_span: byte_start..byte_end,
            char_span: start..end,
            features,
        }
    }

    fn analysis(analyzer: &str, text_id: &str, source: &str, morphemes: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: text_id.to_owned(),
            source_text: source.to_owned(),
            morphemes,
        }
    }

    #[test]
    fn compare_pair_rejects_text_id_mismatch_before_validation() {
        let from = analysis("a", "one", "今日", vec![]);
        let to = analysis("b", "two", "今日", vec![]);
        assert!(matches!(
            compare_pair(&from, &to, &[]),
            Err(MorphDiffError::TextIdMismatch { .. })
        ));
    }

    #[test]
    fn compare_pair_rejects_source_text_mismatch_before_validation() {
        let from = analysis("a", "t", "今日", vec![]);
        let to = analysis("b", "t", "明日", vec![]);
        assert!(matches!(
            compare_pair(&from, &to, &[]),
            Err(MorphDiffError::SourceTextMismatch { .. })
        ));
    }

    #[test]
    fn compare_pair_with_source_text_allows_trimmed_analysis_sources() {
        let source = "今日";
        let mut from = analysis(
            "a",
            "t",
            source,
            vec![m(source, "今日", 0, 2, features(&[]))],
        );
        let mut to = analysis(
            "b",
            "t",
            source,
            vec![
                m(source, "今", 0, 1, features(&[])),
                m(source, "日", 1, 2, features(&[])),
            ],
        );
        from.source_text.clear();
        to.source_text.clear();

        let comparison = compare_pair_with_source_text(&from, &to, source, &[]).unwrap();

        assert_eq!(comparison.stats.segmentation_regions, 1);
    }

    #[test]
    fn compare_pair_produces_feature_diff_adjacent_to_segmentation_region() {
        let source = "今日は";
        let from = analysis(
            "a",
            "t",
            source,
            vec![
                m(source, "今日", 0, 2, features(&[])),
                m(source, "は", 2, 3, features(&[("pos", Some("助詞"))])),
            ],
        );
        let to = analysis(
            "b",
            "t",
            source,
            vec![
                m(source, "今", 0, 1, features(&[])),
                m(source, "日", 1, 2, features(&[])),
                m(source, "は", 2, 3, features(&[("pos", Some("名詞"))])),
            ],
        );
        let comparison = compare_pair(&from, &to, &[]).unwrap();
        assert!(
            matches!(&comparison.regions[0], Region::Segmentation(diff) if diff.kind == SegmentationKind::Split)
        );
        assert_eq!(comparison.feature_diffs.len(), 1);
        assert_eq!(comparison.feature_diffs[0].region_index, 1);
        assert_eq!(
            comparison.feature_diffs[0].changed["pos"],
            ChangedValue {
                from: Some("助詞".to_owned()),
                to: Some("名詞".to_owned())
            }
        );
    }

    #[test]
    fn compare_pair_handles_empty_input() {
        let from = analysis("a", "t", "", vec![]);
        let to = analysis("b", "t", "", vec![]);
        let comparison = compare_pair(&from, &to, &[]).unwrap();
        assert!(comparison.regions.is_empty());
        assert_eq!(comparison.stats.from_morphemes, 0);
        assert_eq!(comparison.stats.to_morphemes, 0);
    }

    #[test]
    fn compare_pair_handles_punctuation_only_input() {
        let source = "。";
        let from = analysis(
            "a",
            "t",
            source,
            vec![m(source, "。", 0, 1, BTreeMap::new())],
        );
        let to = analysis(
            "b",
            "t",
            source,
            vec![m(source, "。", 0, 1, BTreeMap::new())],
        );
        let comparison = compare_pair(&from, &to, &[]).unwrap();
        assert_eq!(comparison.stats.one_to_one_regions, 1);
        assert!(comparison.feature_diffs.is_empty());
    }

    #[test]
    fn compare_pair_identical_segmentation_and_features_has_no_feature_diffs() {
        let source = "今日は";
        let from = analysis(
            "a",
            "t",
            source,
            vec![
                m(source, "今日", 0, 2, features(&[("pos", Some("名詞"))])),
                m(source, "は", 2, 3, features(&[("pos", Some("助詞"))])),
            ],
        );
        let to = analysis(
            "b",
            "t",
            source,
            vec![
                m(source, "今日", 0, 2, features(&[("pos", Some("名詞"))])),
                m(source, "は", 2, 3, features(&[("pos", Some("助詞"))])),
            ],
        );
        let comparison = compare_pair(&from, &to, &[]).unwrap();
        assert_eq!(comparison.stats.one_to_one_regions, 2);
        assert!(comparison.feature_diffs.is_empty());
    }

    #[test]
    fn nway_model_uses_analyzer_ids_in_segmentation_groups() {
        let group = crate::NwaySegmentationGroup {
            surfaces: vec!["今日".to_owned()],
            analyzers: vec!["vibrato".to_owned(), "sudachi-a".to_owned()],
        };

        assert_eq!(group.analyzers, vec!["vibrato", "sudachi-a"]);
    }

    #[test]
    fn compare_nway_groups_three_analyzer_segmentation_partition() {
        let source = "今日";
        let analyses = vec![
            analysis(
                "vibrato",
                "t",
                source,
                vec![m(source, "今日", 0, 2, features(&[]))],
            ),
            analysis(
                "sudachi-a",
                "t",
                source,
                vec![m(source, "今日", 0, 2, features(&[]))],
            ),
            analysis(
                "sudachi-c",
                "t",
                source,
                vec![
                    m(source, "今", 0, 1, features(&[])),
                    m(source, "日", 1, 2, features(&[])),
                ],
            ),
        ];

        let comparison = crate::compare_nway_with_source_text(&analyses, source, &[]).unwrap();

        assert_eq!(
            comparison.analyzers,
            vec!["vibrato", "sudachi-a", "sudachi-c"]
        );
        assert_eq!(comparison.regions.len(), 1);
        assert_eq!(comparison.regions[0].segmentation_groups.len(), 2);
        assert!(
            comparison.regions[0]
                .segmentation_groups
                .iter()
                .any(|group| {
                    group.surfaces == vec!["今日"]
                        && group.analyzers == vec!["sudachi-a", "vibrato"]
                })
        );
        assert!(
            comparison.regions[0]
                .segmentation_groups
                .iter()
                .any(|group| {
                    group.surfaces == vec!["今", "日"] && group.analyzers == vec!["sudachi-c"]
                })
        );
        assert_eq!(comparison.stats.regions_with_segmentation_disagreement, 1);
    }

    #[test]
    fn compare_nway_reports_whole_region_feature_groups() {
        let source = "今日";
        let analyses = vec![
            analysis(
                "a",
                "t",
                source,
                vec![m(source, "今日", 0, 2, features(&[("pos1", Some("名詞"))]))],
            ),
            analysis(
                "b",
                "t",
                source,
                vec![m(source, "今日", 0, 2, features(&[("pos1", Some("名詞"))]))],
            ),
            analysis(
                "c",
                "t",
                source,
                vec![m(source, "今日", 0, 2, features(&[("pos1", Some("空白"))]))],
            ),
        ];

        let comparison =
            crate::compare_nway_with_source_text(&analyses, source, &["pos1".to_owned()]).unwrap();

        assert_eq!(comparison.regions.len(), 1);
        assert_eq!(comparison.regions[0].feature_groups[0].key, "pos1");
        assert_eq!(comparison.regions[0].feature_groups[0].values.len(), 2);
        assert_eq!(comparison.stats.regions_with_feature_disagreement, 1);
    }
}
