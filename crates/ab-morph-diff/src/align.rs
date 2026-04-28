use std::ops::Range;

use crate::{
    AlignedMorpheme, Analysis, CoverageMismatch, CoverageMismatchKind, MorphDiffError, Region,
    SegmentationDiff, SegmentationKind,
};

pub(crate) fn align_regions(from: &Analysis, to: &Analysis) -> Result<Vec<Region>, MorphDiffError> {
    let mut regions = Vec::new();
    let mut i = 0usize;
    let mut j = 0usize;
    let source_len = from.source_text.chars().count();

    while i < from.morphemes.len() || j < to.morphemes.len() {
        if let (Some(left), Some(right)) = (from.morphemes.get(i), to.morphemes.get(j))
            && left.char_span == right.char_span
        {
            regions.push(Region::OneToOne(AlignedMorpheme {
                text_span: left.char_span.clone(),
                from_index: i,
                to_index: j,
            }));
            i += 1;
            j += 1;
            continue;
        }

        let region_start = next_start(from, to, i, j, source_len);
        let mut region_end = next_end(from, to, i, j, source_len);
        let from_start = i;
        let to_start = j;

        loop {
            let old_i = i;
            let old_j = j;
            while i < from.morphemes.len() && from.morphemes[i].char_span.start < region_end {
                region_end = region_end.max(from.morphemes[i].char_span.end);
                i += 1;
            }
            while j < to.morphemes.len() && to.morphemes[j].char_span.start < region_end {
                region_end = region_end.max(to.morphemes[j].char_span.end);
                j += 1;
            }
            if old_i == i && old_j == j {
                break;
            }
        }

        if from_start == i && to_start < j {
            regions.push(Region::CoverageMismatch(CoverageMismatch {
                text_span: region_start..region_end,
                from_indices: from_start..from_start,
                to_indices: to_start..j,
                reason: CoverageMismatchKind::MissingFrom,
            }));
        } else if to_start == j && from_start < i {
            regions.push(Region::CoverageMismatch(CoverageMismatch {
                text_span: region_start..region_end,
                from_indices: from_start..i,
                to_indices: to_start..to_start,
                reason: CoverageMismatchKind::MissingTo,
            }));
        } else if covers_exactly(from, from_start..i, region_start..region_end)
            && covers_exactly(to, to_start..j, region_start..region_end)
        {
            regions.push(Region::Segmentation(SegmentationDiff {
                text_span: region_start..region_end,
                from_indices: from_start..i,
                to_indices: to_start..j,
                from_surfaces: from.morphemes[from_start..i]
                    .iter()
                    .map(|m| m.surface.clone())
                    .collect(),
                to_surfaces: to.morphemes[to_start..j]
                    .iter()
                    .map(|m| m.surface.clone())
                    .collect(),
                kind: segmentation_kind(i - from_start, j - to_start),
            }));
        } else {
            regions.push(Region::CoverageMismatch(CoverageMismatch {
                text_span: region_start..region_end,
                from_indices: from_start..i,
                to_indices: to_start..j,
                reason: CoverageMismatchKind::UnequalCoverage,
            }));
        }
    }

    Ok(regions)
}

fn next_start(from: &Analysis, to: &Analysis, i: usize, j: usize, source_len: usize) -> usize {
    match (from.morphemes.get(i), to.morphemes.get(j)) {
        (Some(a), Some(b)) => a.char_span.start.min(b.char_span.start),
        (Some(a), None) => a.char_span.start,
        (None, Some(b)) => b.char_span.start,
        (None, None) => source_len,
    }
}

fn next_end(from: &Analysis, to: &Analysis, i: usize, j: usize, source_len: usize) -> usize {
    match (from.morphemes.get(i), to.morphemes.get(j)) {
        (Some(a), Some(b)) => a
            .char_span
            .end
            .min(b.char_span.end)
            .max(next_start(from, to, i, j, source_len) + 1),
        (Some(a), None) => a.char_span.end,
        (None, Some(b)) => b.char_span.end,
        (None, None) => source_len,
    }
}

fn covers_exactly(analysis: &Analysis, indices: Range<usize>, span: Range<usize>) -> bool {
    if indices.is_empty() {
        return false;
    }
    let first = &analysis.morphemes[indices.start];
    let last = &analysis.morphemes[indices.end - 1];
    if first.char_span.start != span.start || last.char_span.end != span.end {
        return false;
    }
    analysis.morphemes[indices]
        .windows(2)
        .all(|pair| pair[0].char_span.end == pair[1].char_span.start)
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

    use super::align_regions;

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
