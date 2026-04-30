use crate::{
    AlignedMorpheme, Analysis, CoverageMismatch, CoverageMismatchKind, MorphDiffError, Region,
    SegmentationDiff, SegmentationKind,
};

pub(crate) fn align_regions(from: &Analysis, to: &Analysis) -> Result<Vec<Region>, MorphDiffError> {
    align_regions_with_source_len(from, to, from.source_text.chars().count())
}

pub(crate) fn align_regions_with_source_len(
    from: &Analysis,
    to: &Analysis,
    source_len: usize,
) -> Result<Vec<Region>, MorphDiffError> {
    let mut regions = Vec::new();
    visit_regions_with_source_len(from, to, source_len, |_index, region| {
        regions.push(region);
    })?;
    Ok(regions)
}

pub(crate) fn visit_regions_with_source_len(
    from: &Analysis,
    to: &Analysis,
    source_len: usize,
    mut visit: impl FnMut(usize, Region),
) -> Result<(), MorphDiffError> {
    let analyses = [from, to];
    let regions = crate::nway::shared_regions_with_source_len(&analyses, source_len, &[])?;
    for (region_index, region) in regions.into_iter().enumerate() {
        visit(region_index, project_pair_region(region));
    }

    Ok(())
}

fn project_pair_region(region: crate::NwayRegion) -> Region {
    let left = &region.per_analyzer[0];
    let right = &region.per_analyzer[1];
    if left.covers_exactly
        && right.covers_exactly
        && left.indices.len() == 1
        && right.indices.len() == 1
    {
        return Region::OneToOne(AlignedMorpheme {
            text_span: region.text_span,
            from_index: left.indices.start,
            to_index: right.indices.start,
        });
    }
    if left.indices.is_empty() && !right.indices.is_empty() {
        return Region::CoverageMismatch(CoverageMismatch {
            text_span: region.text_span,
            from_indices: left.indices.clone(),
            to_indices: right.indices.clone(),
            reason: CoverageMismatchKind::MissingFrom,
        });
    }
    if right.indices.is_empty() && !left.indices.is_empty() {
        return Region::CoverageMismatch(CoverageMismatch {
            text_span: region.text_span,
            from_indices: left.indices.clone(),
            to_indices: right.indices.clone(),
            reason: CoverageMismatchKind::MissingTo,
        });
    }
    if left.covers_exactly && right.covers_exactly {
        return Region::Segmentation(SegmentationDiff {
            text_span: region.text_span,
            from_indices: left.indices.clone(),
            to_indices: right.indices.clone(),
            from_surfaces: left.surfaces.clone(),
            to_surfaces: right.surfaces.clone(),
            kind: segmentation_kind(left.indices.len(), right.indices.len()),
        });
    }
    Region::CoverageMismatch(CoverageMismatch {
        text_span: region.text_span,
        from_indices: left.indices.clone(),
        to_indices: right.indices.clone(),
        reason: CoverageMismatchKind::UnequalCoverage,
    })
}

fn segmentation_kind(from_count: usize, to_count: usize) -> SegmentationKind {
    match (from_count, to_count) {
        (1, n) if n > 1 => SegmentationKind::Split,
        (n, 1) if n > 1 => SegmentationKind::Merge,
        _ => SegmentationKind::Resegment,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        Analysis, CoverageMismatch, CoverageMismatchKind, Morpheme, Region, SegmentationDiff,
        SegmentationKind,
    };

    use super::{align_regions, visit_regions_with_source_len};

    fn m(source: &str, surface: &str, char_start: usize, char_end: usize) -> Morpheme {
        let byte_start = source
            .char_indices()
            .nth(char_start)
            .map(|(idx, _)| idx)
            .unwrap_or(source.len());
        let byte_end = source
            .char_indices()
            .nth(char_end)
            .map(|(idx, _)| idx)
            .unwrap_or(source.len());
        Morpheme {
            surface: surface.to_owned(),
            byte_span: byte_start..byte_end,
            char_span: char_start..char_end,
            features: BTreeMap::new(),
        }
    }

    fn a(analyzer: &str, source: &str, morphemes: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "t".to_owned(),
            source_text: source.to_owned(),
            morphemes,
        }
    }

    #[test]
    fn visit_regions_matches_align_regions_order_and_shape() {
        let source = "今日明日";
        let from = a(
            "from",
            source,
            vec![m(source, "今日", 0, 2), m(source, "明日", 2, 4)],
        );
        let to = a(
            "to",
            source,
            vec![
                m(source, "今", 0, 1),
                m(source, "日", 1, 2),
                m(source, "明日", 2, 4),
            ],
        );

        let collected = align_regions(&from, &to).unwrap();
        let mut visited = Vec::new();
        visit_regions_with_source_len(&from, &to, source.chars().count(), |index, region| {
            assert_eq!(index, visited.len());
            visited.push(region);
        })
        .unwrap();

        assert_eq!(visited, collected);
    }

    #[test]
    fn split_region_has_exact_shape() {
        let source = "今日";
        let from = a("a", source, vec![m(source, "今日", 0, 2)]);
        let to = a(
            "b",
            source,
            vec![m(source, "今", 0, 1), m(source, "日", 1, 2)],
        );
        assert_eq!(
            align_regions(&from, &to).unwrap(),
            vec![Region::Segmentation(SegmentationDiff {
                text_span: 0..2,
                from_indices: 0..1,
                to_indices: 0..2,
                from_surfaces: vec!["今日".to_owned()],
                to_surfaces: vec!["今".to_owned(), "日".to_owned()],
                kind: SegmentationKind::Split,
            })]
        );
    }

    #[test]
    fn merge_region_is_classified() {
        let source = "では";
        let from = a(
            "a",
            source,
            vec![m(source, "で", 0, 1), m(source, "は", 1, 2)],
        );
        let to = a("b", source, vec![m(source, "では", 0, 2)]);
        assert!(matches!(
            &align_regions(&from, &to).unwrap()[0],
            Region::Segmentation(diff) if diff.kind == SegmentationKind::Merge
        ));
    }

    #[test]
    fn resegment_region_is_classified() {
        let source = "abcdef";
        let from = a(
            "a",
            source,
            vec![m(source, "abc", 0, 3), m(source, "def", 3, 6)],
        );
        let to = a(
            "b",
            source,
            vec![m(source, "ab", 0, 2), m(source, "cdef", 2, 6)],
        );
        assert!(matches!(
            &align_regions(&from, &to).unwrap()[0],
            Region::Segmentation(diff) if diff.kind == SegmentationKind::Resegment
        ));
    }

    #[test]
    fn repeated_surfaces_align_by_span() {
        let source = "はは";
        let from = a(
            "a",
            source,
            vec![m(source, "は", 0, 1), m(source, "は", 1, 2)],
        );
        let to = a(
            "b",
            source,
            vec![m(source, "は", 0, 1), m(source, "は", 1, 2)],
        );
        let regions = align_regions(&from, &to).unwrap();
        assert_eq!(regions.len(), 2);
        assert!(matches!(&regions[0], Region::OneToOne(aligned) if aligned.text_span == (0..1)));
        assert!(matches!(&regions[1], Region::OneToOne(aligned) if aligned.text_span == (1..2)));
    }

    #[test]
    fn one_side_zero_morphemes_for_non_empty_text_is_missing_from() {
        let source = "本文";
        let from = a("a", source, vec![]);
        let to = a("b", source, vec![m(source, "本文", 0, 2)]);
        assert_eq!(
            align_regions(&from, &to).unwrap(),
            vec![Region::CoverageMismatch(CoverageMismatch {
                text_span: 0..2,
                from_indices: 0..0,
                to_indices: 0..1,
                reason: CoverageMismatchKind::MissingFrom,
            })]
        );
    }

    #[test]
    fn mid_text_gap_on_one_side_emits_missing_from() {
        let source = "abc";
        let from = a(
            "a",
            source,
            vec![m(source, "a", 0, 1), m(source, "c", 2, 3)],
        );
        let to = a(
            "b",
            source,
            vec![
                m(source, "a", 0, 1),
                m(source, "b", 1, 2),
                m(source, "c", 2, 3),
            ],
        );
        let regions = align_regions(&from, &to).unwrap();
        assert_eq!(regions.len(), 3);
        assert_eq!(
            regions[1],
            Region::CoverageMismatch(CoverageMismatch {
                text_span: 1..2,
                from_indices: 1..1,
                to_indices: 1..2,
                reason: CoverageMismatchKind::MissingFrom,
            })
        );
    }

    #[test]
    fn unequal_coverage_reason_is_emitted_for_non_contiguous_consumed_spans() {
        let source = "abc";
        let from = a(
            "a",
            source,
            vec![m(source, "a", 0, 1), m(source, "c", 2, 3)],
        );
        let to = a("b", source, vec![m(source, "abc", 0, 3)]);
        assert_eq!(
            align_regions(&from, &to).unwrap(),
            vec![Region::CoverageMismatch(CoverageMismatch {
                text_span: 0..3,
                from_indices: 0..2,
                to_indices: 0..1,
                reason: CoverageMismatchKind::UnequalCoverage,
            })]
        );
    }

    #[test]
    fn multiple_segmentation_regions_are_separate() {
        let source = "今日Xでは";
        let from = a(
            "a",
            source,
            vec![
                m(source, "今日", 0, 2),
                m(source, "X", 2, 3),
                m(source, "で", 3, 4),
                m(source, "は", 4, 5),
            ],
        );
        let to = a(
            "b",
            source,
            vec![
                m(source, "今", 0, 1),
                m(source, "日", 1, 2),
                m(source, "X", 2, 3),
                m(source, "では", 3, 5),
            ],
        );
        let regions = align_regions(&from, &to).unwrap();
        assert_eq!(regions.len(), 3);
        assert!(
            matches!(&regions[0], Region::Segmentation(diff) if diff.kind == SegmentationKind::Split)
        );
        assert!(matches!(&regions[1], Region::OneToOne(_)));
        assert!(
            matches!(&regions[2], Region::Segmentation(diff) if diff.kind == SegmentationKind::Merge)
        );
    }
}
