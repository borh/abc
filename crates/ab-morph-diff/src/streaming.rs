use std::collections::BTreeSet;
use std::ops::Range;

use crate::{
    Analysis, CompactComparison, CompactComparisonExample, CompactExampleKind,
    CompactFeatureChange, ComparisonStats, CoverageMismatchKind, FeatureKey, FeatureMap,
    MorphDiffError, Region, SegmentationKind, validate,
};

pub(crate) fn compare_pair_compact_with_source_text(
    from: &Analysis,
    to: &Analysis,
    source_text: &str,
    _feature_context_keys: &[FeatureKey],
    max_examples: usize,
) -> Result<CompactComparison, MorphDiffError> {
    if from.text_id != to.text_id {
        return Err(MorphDiffError::TextIdMismatch {
            from: from.text_id.clone(),
            to: to.text_id.clone(),
        });
    }
    validate::validate_analysis_against_source(from, source_text)?;
    validate::validate_analysis_against_source(to, source_text)?;

    let source_len = source_text.chars().count();
    let mut accumulator = CompactAccumulator::new(from, to, source_text, max_examples);
    crate::align::visit_regions_with_source_len(from, to, source_len, |region_index, region| {
        accumulator.visit_region(region_index, region);
    })?;
    Ok(accumulator.finish(source_len))
}

struct CompactAccumulator<'a> {
    from: &'a Analysis,
    to: &'a Analysis,
    source_text: &'a str,
    max_examples: usize,
    one_to_one_regions: usize,
    one_to_one_with_feature_differences: usize,
    segmentation_regions: usize,
    whitespace_segmentation_regions: usize,
    lexical_segmentation_regions: usize,
    coverage_mismatch_regions: usize,
    split_regions: usize,
    merge_regions: usize,
    resegment_regions: usize,
    whitespace_feature_diff_regions: usize,
    lexical_feature_diff_regions: usize,
    from_morphemes_in_segmentation: usize,
    to_morphemes_in_segmentation: usize,
    ignored_spans: Vec<Range<usize>>,
    region_examples: Vec<CompactComparisonExample>,
    feature_examples: Vec<CompactComparisonExample>,
}

impl<'a> CompactAccumulator<'a> {
    fn new(
        from: &'a Analysis,
        to: &'a Analysis,
        source_text: &'a str,
        max_examples: usize,
    ) -> Self {
        Self {
            from,
            to,
            source_text,
            max_examples,
            one_to_one_regions: 0,
            one_to_one_with_feature_differences: 0,
            segmentation_regions: 0,
            whitespace_segmentation_regions: 0,
            lexical_segmentation_regions: 0,
            coverage_mismatch_regions: 0,
            split_regions: 0,
            merge_regions: 0,
            resegment_regions: 0,
            whitespace_feature_diff_regions: 0,
            lexical_feature_diff_regions: 0,
            from_morphemes_in_segmentation: 0,
            to_morphemes_in_segmentation: 0,
            ignored_spans: Vec::new(),
            region_examples: Vec::new(),
            feature_examples: Vec::new(),
        }
    }

    fn visit_region(&mut self, region_index: usize, region: Region) {
        match region {
            Region::OneToOne(aligned) => {
                self.one_to_one_regions += 1;
                let from_morpheme = &self.from.morphemes[aligned.from_index];
                let to_morpheme = &self.to.morphemes[aligned.to_index];
                let feature_changes =
                    changed_features(&from_morpheme.features, &to_morpheme.features);
                if !feature_changes.is_empty() {
                    self.one_to_one_with_feature_differences += 1;
                    if crate::stats::char_span_is_whitespace_only(
                        self.source_text,
                        &aligned.text_span,
                    ) {
                        self.whitespace_feature_diff_regions += 1;
                    } else {
                        self.lexical_feature_diff_regions += 1;
                    }
                    self.push_feature_example(CompactComparisonExample {
                        region_index,
                        kind: CompactExampleKind::FeatureDiff,
                        text_span: aligned.text_span,
                        from_indices: aligned.from_index..aligned.from_index + 1,
                        to_indices: aligned.to_index..aligned.to_index + 1,
                        from_surfaces: vec![from_morpheme.surface.clone()],
                        to_surfaces: vec![to_morpheme.surface.clone()],
                        feature_changes: Some(feature_changes),
                    });
                }
            }
            Region::Segmentation(diff) => {
                self.segmentation_regions += 1;
                if crate::stats::char_span_is_whitespace_only(self.source_text, &diff.text_span) {
                    self.whitespace_segmentation_regions += 1;
                } else {
                    self.lexical_segmentation_regions += 1;
                }
                self.from_morphemes_in_segmentation += diff.from_indices.len();
                self.to_morphemes_in_segmentation += diff.to_indices.len();
                let kind = match diff.kind {
                    SegmentationKind::Split => {
                        self.split_regions += 1;
                        CompactExampleKind::Split
                    }
                    SegmentationKind::Merge => {
                        self.merge_regions += 1;
                        CompactExampleKind::Merge
                    }
                    SegmentationKind::Resegment => {
                        self.resegment_regions += 1;
                        CompactExampleKind::Resegment
                    }
                };
                self.push_region_example(CompactComparisonExample {
                    region_index,
                    kind,
                    text_span: diff.text_span,
                    from_indices: diff.from_indices,
                    to_indices: diff.to_indices,
                    from_surfaces: diff.from_surfaces,
                    to_surfaces: diff.to_surfaces,
                    feature_changes: None,
                });
            }
            Region::CoverageMismatch(mismatch) => {
                self.coverage_mismatch_regions += 1;
                self.ignored_spans.push(mismatch.text_span.clone());
                let kind = match mismatch.reason {
                    CoverageMismatchKind::MissingFrom => CompactExampleKind::CoverageMissingFrom,
                    CoverageMismatchKind::MissingTo => CompactExampleKind::CoverageMissingTo,
                    CoverageMismatchKind::UnequalCoverage => CompactExampleKind::CoverageUnequal,
                    CoverageMismatchKind::InvalidInput => CompactExampleKind::CoverageInvalidInput,
                };
                self.push_region_example(CompactComparisonExample {
                    region_index,
                    kind,
                    text_span: mismatch.text_span,
                    from_surfaces: surfaces(self.from, mismatch.from_indices.clone()),
                    to_surfaces: surfaces(self.to, mismatch.to_indices.clone()),
                    from_indices: mismatch.from_indices,
                    to_indices: mismatch.to_indices,
                    feature_changes: None,
                });
            }
        }
    }

    fn push_region_example(&mut self, example: CompactComparisonExample) {
        if self.region_examples.len() < self.max_examples {
            self.region_examples.push(example);
            let remaining = self.max_examples.saturating_sub(self.region_examples.len());
            self.feature_examples.truncate(remaining);
        }
    }

    fn push_feature_example(&mut self, example: CompactComparisonExample) {
        let remaining = self.max_examples.saturating_sub(self.region_examples.len());
        if self.feature_examples.len() < remaining {
            self.feature_examples.push(example);
        }
    }

    fn finish(self, source_len: usize) -> CompactComparison {
        let boundary_metrics = crate::stats::derive_boundary_metrics(
            self.from,
            self.to,
            &self.ignored_spans,
            source_len,
        );
        let mut examples = self.region_examples;
        examples.extend(self.feature_examples);

        CompactComparison {
            from_analyzer: self.from.analyzer.clone(),
            to_analyzer: self.to.analyzer.clone(),
            text_id: self.from.text_id.clone(),
            stats: ComparisonStats {
                from_morphemes: self.from.morphemes.len(),
                to_morphemes: self.to.morphemes.len(),
                one_to_one_regions: self.one_to_one_regions,
                one_to_one_with_feature_differences: self.one_to_one_with_feature_differences,
                segmentation_regions: self.segmentation_regions,
                whitespace_segmentation_regions: self.whitespace_segmentation_regions,
                lexical_segmentation_regions: self.lexical_segmentation_regions,
                coverage_mismatch_regions: self.coverage_mismatch_regions,
                split_regions: self.split_regions,
                merge_regions: self.merge_regions,
                resegment_regions: self.resegment_regions,
                whitespace_feature_diff_regions: self.whitespace_feature_diff_regions,
                lexical_feature_diff_regions: self.lexical_feature_diff_regions,
                from_morphemes_in_segmentation: self.from_morphemes_in_segmentation,
                to_morphemes_in_segmentation: self.to_morphemes_in_segmentation,
                boundary_precision: boundary_metrics.precision,
                boundary_recall: boundary_metrics.recall,
                boundary_f1: boundary_metrics.f1,
            },
            examples,
        }
    }
}

fn changed_features(from: &FeatureMap, to: &FeatureMap) -> Vec<CompactFeatureChange> {
    from.keys()
        .chain(to.keys())
        .cloned()
        .collect::<BTreeSet<_>>()
        .into_iter()
        .filter_map(|key| {
            let from_value = from.get(&key).cloned().unwrap_or(None);
            let to_value = to.get(&key).cloned().unwrap_or(None);
            (from_value != to_value).then_some(CompactFeatureChange {
                key,
                from: from_value,
                to: to_value,
            })
        })
        .collect()
}

fn surfaces(analysis: &Analysis, indices: Range<usize>) -> Vec<String> {
    analysis.morphemes[indices]
        .iter()
        .map(|morpheme| morpheme.surface.clone())
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::{
        Analysis, CompactExampleKind, FeatureMap, Morpheme, compare_pair,
        compare_pair_compact_with_source_text,
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

    fn analysis(
        analyzer: &str,
        source: &str,
        morphemes: impl IntoIterator<Item = Morpheme>,
    ) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "t".to_owned(),
            source_text: source.to_owned(),
            morphemes: morphemes.into_iter().collect(),
        }
    }

    #[test]
    fn compact_streaming_stats_match_full_comparison() {
        let source = "今日明日";
        let from = analysis(
            "from",
            source,
            [
                m(source, "今日", 0, 2, features(&[])),
                m(source, "明日", 2, 4, features(&[("pos", Some("名詞"))])),
            ],
        );
        let to = analysis(
            "to",
            source,
            [
                m(source, "今", 0, 1, features(&[])),
                m(source, "日", 1, 2, features(&[])),
                m(source, "明日", 2, 4, features(&[("pos", Some("副詞"))])),
            ],
        );

        let full = compare_pair(&from, &to, &[]).unwrap();
        let compact = compare_pair_compact_with_source_text(&from, &to, source, &[], 10).unwrap();

        assert_eq!(compact.stats, full.stats);
        assert_eq!(compact.examples.len(), 2);
        assert_eq!(compact.examples[0].kind, CompactExampleKind::Split);
        assert_eq!(compact.examples[1].kind, CompactExampleKind::FeatureDiff);
    }

    #[test]
    fn compact_streaming_respects_shared_example_budget() {
        let source = "今日明日";
        let from = analysis(
            "from",
            source,
            [
                m(source, "今日", 0, 2, features(&[])),
                m(source, "明日", 2, 4, features(&[("pos", Some("名詞"))])),
            ],
        );
        let to = analysis(
            "to",
            source,
            [
                m(source, "今", 0, 1, features(&[])),
                m(source, "日", 1, 2, features(&[])),
                m(source, "明日", 2, 4, features(&[("pos", Some("副詞"))])),
            ],
        );

        let compact = compare_pair_compact_with_source_text(&from, &to, source, &[], 1).unwrap();

        assert_eq!(compact.examples.len(), 1);
        assert_eq!(compact.examples[0].kind, CompactExampleKind::Split);
    }
}
