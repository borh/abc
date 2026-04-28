use std::collections::BTreeSet;

use crate::{Analysis, ComparisonStats, FeatureDiff, Region, SegmentationKind};

pub(crate) fn derive_stats(
    from: &Analysis,
    to: &Analysis,
    regions: &[Region],
    feature_diffs: &[FeatureDiff],
) -> ComparisonStats {
    let mut one_to_one_regions = 0usize;
    let mut segmentation_regions = 0usize;
    let mut coverage_mismatch_regions = 0usize;
    let mut split_regions = 0usize;
    let mut merge_regions = 0usize;
    let mut resegment_regions = 0usize;
    let mut from_morphemes_in_segmentation = 0usize;
    let mut to_morphemes_in_segmentation = 0usize;

    for region in regions {
        match region {
            Region::OneToOne(_) => one_to_one_regions += 1,
            Region::Segmentation(diff) => {
                segmentation_regions += 1;
                from_morphemes_in_segmentation += diff.from_indices.len();
                to_morphemes_in_segmentation += diff.to_indices.len();
                match diff.kind {
                    SegmentationKind::Split => split_regions += 1,
                    SegmentationKind::Merge => merge_regions += 1,
                    SegmentationKind::Resegment => resegment_regions += 1,
                }
            }
            Region::CoverageMismatch(_) => coverage_mismatch_regions += 1,
        }
    }

    let one_to_one_with_feature_differences = feature_diffs
        .iter()
        .map(|diff| diff.region_index)
        .collect::<BTreeSet<_>>()
        .len();
    let ignored_spans = regions
        .iter()
        .filter_map(|region| match region {
            Region::CoverageMismatch(mismatch) => Some(mismatch.text_span.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();
    let from_boundaries = comparable_boundaries(from, &ignored_spans);
    let to_boundaries = comparable_boundaries(to, &ignored_spans);
    let shared = from_boundaries.intersection(&to_boundaries).count();
    let boundary_precision = ratio(shared, to_boundaries.len());
    let boundary_recall = ratio(shared, from_boundaries.len());
    let boundary_f1 = match (boundary_precision, boundary_recall) {
        (Some(precision), Some(recall)) if precision + recall > 0.0 => {
            Some(2.0 * precision * recall / (precision + recall))
        }
        _ => None,
    };

    ComparisonStats {
        from_morphemes: from.morphemes.len(),
        to_morphemes: to.morphemes.len(),
        one_to_one_regions,
        one_to_one_with_feature_differences,
        segmentation_regions,
        coverage_mismatch_regions,
        split_regions,
        merge_regions,
        resegment_regions,
        from_morphemes_in_segmentation,
        to_morphemes_in_segmentation,
        boundary_precision,
        boundary_recall,
        boundary_f1,
    }
}

fn comparable_boundaries(
    analysis: &Analysis,
    ignored_spans: &[std::ops::Range<usize>],
) -> BTreeSet<usize> {
    let source_len = analysis.source_text.chars().count();
    let mut boundaries = BTreeSet::new();
    for morpheme in &analysis.morphemes {
        boundaries.insert(morpheme.char_span.start);
        boundaries.insert(morpheme.char_span.end);
    }
    boundaries.remove(&0);
    boundaries.remove(&source_len);
    boundaries
        .into_iter()
        .filter(|boundary| {
            !ignored_spans
                .iter()
                .any(|span| span.start < *boundary && *boundary < span.end)
        })
        .collect()
}

fn ratio(numerator: usize, denominator: usize) -> Option<f64> {
    (denominator != 0).then_some(numerator as f64 / denominator as f64)
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};

    use crate::{
        AlignedMorpheme, Analysis, CoverageMismatch, CoverageMismatchKind, FeatureDiff, Morpheme,
        Region, SegmentationDiff, SegmentationKind,
    };

    use super::derive_stats;

    fn m(source: &str, surface: &str, start: usize, end: usize) -> Morpheme {
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
            features: BTreeMap::new(),
        }
    }

    fn analysis(parts: &[(&str, usize, usize)]) -> Analysis {
        let source = "abcdef";
        Analysis {
            analyzer: "a".to_owned(),
            text_id: "t".to_owned(),
            source_text: source.to_owned(),
            morphemes: parts
                .iter()
                .map(|(surface, start, end)| m(source, surface, *start, *end))
                .collect(),
        }
    }

    #[test]
    fn counts_region_kinds_and_segmentation_subtypes() {
        let from = analysis(&[("a", 0, 1), ("b", 1, 2), ("c", 2, 3)]);
        let to = analysis(&[("a", 0, 1), ("b", 1, 2), ("c", 2, 3)]);
        let regions = vec![
            Region::Segmentation(SegmentationDiff {
                text_span: 0..2,
                from_indices: 0..1,
                to_indices: 0..2,
                from_surfaces: vec![],
                to_surfaces: vec![],
                kind: SegmentationKind::Split,
            }),
            Region::Segmentation(SegmentationDiff {
                text_span: 2..4,
                from_indices: 1..3,
                to_indices: 2..3,
                from_surfaces: vec![],
                to_surfaces: vec![],
                kind: SegmentationKind::Merge,
            }),
            Region::Segmentation(SegmentationDiff {
                text_span: 4..6,
                from_indices: 0..2,
                to_indices: 1..3,
                from_surfaces: vec![],
                to_surfaces: vec![],
                kind: SegmentationKind::Resegment,
            }),
        ];
        let stats = derive_stats(&from, &to, &regions, &[]);
        assert_eq!(stats.segmentation_regions, 3);
        assert_eq!(stats.split_regions, 1);
        assert_eq!(stats.merge_regions, 1);
        assert_eq!(stats.resegment_regions, 1);
    }

    #[test]
    fn counts_unique_one_to_one_regions_with_feature_diffs() {
        let from = analysis(&[("a", 0, 1)]);
        let to = analysis(&[("a", 0, 1)]);
        let regions = vec![Region::OneToOne(AlignedMorpheme {
            text_span: 0..1,
            from_index: 0,
            to_index: 0,
        })];
        let feature_diffs = vec![
            FeatureDiff {
                region_index: 0,
                text_span: 0..1,
                surface: "a".to_owned(),
                from_index: 0,
                to_index: 0,
                changed: BTreeMap::new(),
                same_context: BTreeMap::new(),
            },
            FeatureDiff {
                region_index: 0,
                text_span: 0..1,
                surface: "a".to_owned(),
                from_index: 0,
                to_index: 0,
                changed: BTreeMap::new(),
                same_context: BTreeMap::new(),
            },
        ];
        assert_eq!(
            derive_stats(&from, &to, &regions, &feature_diffs).one_to_one_with_feature_differences,
            1
        );
    }

    #[test]
    fn boundary_metrics_exclude_outer_boundaries() {
        let from = analysis(&[("ab", 0, 2), ("cd", 2, 4)]);
        let to = analysis(&[("ab", 0, 2), ("cd", 2, 4)]);
        let stats = derive_stats(&from, &to, &[], &[]);
        assert_eq!(stats.boundary_precision, Some(1.0));
        assert_eq!(stats.boundary_recall, Some(1.0));
        assert_eq!(stats.boundary_f1, Some(1.0));
    }

    #[test]
    fn boundary_metrics_ignore_coverage_mismatch_spans() {
        let from = analysis(&[("a", 0, 1), ("b", 1, 2), ("cd", 2, 4)]);
        let to = analysis(&[("ab", 0, 2), ("cd", 2, 4)]);
        let regions = vec![Region::CoverageMismatch(CoverageMismatch {
            text_span: 0..2,
            from_indices: 0..2,
            to_indices: 0..1,
            reason: CoverageMismatchKind::UnequalCoverage,
        })];
        let stats = derive_stats(&from, &to, &regions, &[]);
        assert_eq!(stats.boundary_precision, Some(1.0));
        assert_eq!(stats.boundary_recall, Some(1.0));
    }

    #[test]
    fn boundary_metrics_are_none_when_no_comparable_internal_boundaries() {
        let from = analysis(&[("abcdef", 0, 6)]);
        let to = analysis(&[("abcdef", 0, 6)]);
        let stats = derive_stats(&from, &to, &[], &[]);
        assert_eq!(stats.boundary_precision, None);
        assert_eq!(stats.boundary_recall, None);
        assert_eq!(stats.boundary_f1, None);
    }

    #[test]
    fn stats_invariants_hold_for_region_counts() {
        let from = analysis(&[("ab", 0, 2), ("cd", 2, 4)]);
        let to = analysis(&[("ab", 0, 2), ("cd", 2, 4)]);
        let regions = vec![
            Region::OneToOne(AlignedMorpheme {
                text_span: 0..2,
                from_index: 0,
                to_index: 0,
            }),
            Region::CoverageMismatch(CoverageMismatch {
                text_span: 2..3,
                from_indices: 1..2,
                to_indices: 1..1,
                reason: CoverageMismatchKind::MissingTo,
            }),
        ];
        let stats = derive_stats(&from, &to, &regions, &[]);
        assert_eq!(
            regions.len(),
            stats.one_to_one_regions + stats.segmentation_regions + stats.coverage_mismatch_regions
        );
        assert_eq!(
            stats.segmentation_regions,
            stats.split_regions + stats.merge_regions + stats.resegment_regions
        );
        assert_eq!(BTreeSet::<usize>::new().len(), 0);
    }
}
