use ab_morph_diff::{Analysis, FeatureMap, Morpheme, Region, compare_pair};
use proptest::prelude::*;

fn analysis(analyzer: &str, text: &str, cuts: &[usize]) -> Analysis {
    let mut morphemes = Vec::new();
    for pair in cuts.windows(2) {
        let start_char = pair[0];
        let end_char = pair[1];
        let start_byte = text
            .char_indices()
            .nth(start_char)
            .map(|(idx, _)| idx)
            .unwrap_or(text.len());
        let end_byte = text
            .char_indices()
            .nth(end_char)
            .map(|(idx, _)| idx)
            .unwrap_or(text.len());
        morphemes.push(Morpheme {
            surface: text[start_byte..end_byte].to_owned(),
            byte_span: start_byte..end_byte,
            char_span: start_char..end_char,
            features: FeatureMap::new(),
        });
    }
    Analysis {
        analyzer: analyzer.to_owned(),
        text_id: "t".to_owned(),
        source_text: text.to_owned(),
        morphemes,
        warnings: Vec::new(),
        ortho_annotations: None,
        ortho_offset_map: None,
    }
}

fn cuts_from_mask(len: usize, mask: u16) -> Vec<usize> {
    let mut cuts = vec![0];
    for i in 1..len {
        if (mask & (1 << (i - 1))) != 0 {
            cuts.push(i);
        }
    }
    cuts.push(len);
    cuts
}

fn assert_region_invariants(comparison: &ab_morph_diff::Comparison) -> Result<(), TestCaseError> {
    let mut previous_end = 0usize;
    for region in &comparison.regions {
        let span = match region {
            Region::OneToOne(region) => region.text_span.clone(),
            Region::Segmentation(region) => region.text_span.clone(),
            Region::CoverageMismatch(region) => region.text_span.clone(),
        };
        prop_assert!(span.start >= previous_end);
        prop_assert!(span.start <= span.end);
        previous_end = span.end;
    }
    prop_assert_eq!(
        comparison.regions.len(),
        comparison.stats.one_to_one_regions
            + comparison.stats.segmentation_regions
            + comparison.stats.coverage_mismatch_regions
    );
    prop_assert_eq!(
        comparison.stats.segmentation_regions,
        comparison.stats.split_regions
            + comparison.stats.merge_regions
            + comparison.stats.resegment_regions
    );
    for diff in &comparison.feature_diffs {
        prop_assert!(matches!(
            comparison.regions.get(diff.region_index),
            Some(Region::OneToOne(_))
        ));
    }
    Ok(())
}

proptest! {
    #[test]
    fn ascii_regions_are_sorted_and_stats_are_consistent(a_mask in 0u16..256, b_mask in 0u16..256) {
        let text = "abcdefgh";
        let a = analysis("a", text, &cuts_from_mask(8, a_mask));
        let b = analysis("b", text, &cuts_from_mask(8, b_mask));
        let comparison = compare_pair(&a, &b, &[]).unwrap();
        assert_region_invariants(&comparison)?;
    }

    #[test]
    fn multibyte_regions_are_sorted_and_stats_are_consistent(a_mask in 0u16..256, b_mask in 0u16..256) {
        let text = "今日は晴れだ";
        let a = analysis("a", text, &cuts_from_mask(6, a_mask));
        let b = analysis("b", text, &cuts_from_mask(6, b_mask));
        let comparison = compare_pair(&a, &b, &[]).unwrap();
        assert_region_invariants(&comparison)?;
    }
}
