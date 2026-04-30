use std::collections::{BTreeMap, BTreeSet};

use crate::{Analysis, ChangedValue, FeatureDiff, FeatureKey, Region};

pub(crate) fn compare_feature_diffs(
    from: &Analysis,
    to: &Analysis,
    regions: &[Region],
    context_keys: &[FeatureKey],
) -> Vec<FeatureDiff> {
    let context_keys = context_keys.iter().cloned().collect::<BTreeSet<_>>();
    let mut diffs = Vec::new();

    for (region_index, region) in regions.iter().enumerate() {
        let Region::OneToOne(aligned) = region else {
            continue;
        };
        let from_m = &from.morphemes[aligned.from_index];
        let to_m = &to.morphemes[aligned.to_index];
        let keys = from_m
            .features
            .keys()
            .chain(to_m.features.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        let mut changed = BTreeMap::new();
        let mut same_context = BTreeMap::new();
        for key in keys {
            let from_value = from_m.features.get(&key).cloned().unwrap_or(None);
            let to_value = to_m.features.get(&key).cloned().unwrap_or(None);
            if from_value != to_value {
                changed.insert(
                    key,
                    ChangedValue {
                        from: from_value,
                        to: to_value,
                    },
                );
            } else if context_keys.contains(&key) {
                same_context.insert(key, from_value);
            }
        }
        if !changed.is_empty() {
            diffs.push(FeatureDiff {
                region_index,
                text_span: aligned.text_span.clone(),
                surface: from_m.surface.clone(),
                from_index: aligned.from_index,
                to_index: aligned.to_index,
                changed,
                same_context,
            });
        }
    }

    diffs
}

#[cfg(test)]
mod tests {
    use crate::{
        AlignedMorpheme, Analysis, ChangedValue, FeatureMap, Morpheme, Region, SegmentationDiff,
        SegmentationKind,
    };

    use super::compare_feature_diffs;

    fn features(values: &[(&str, Option<&str>)]) -> FeatureMap {
        values
            .iter()
            .map(|(k, v)| ((*k).into(), v.map(Into::into)))
            .collect()
    }

    fn analysis(analyzer: &str, features: FeatureMap) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "t".to_owned(),
            source_text: "今日".to_owned(),
            morphemes: vec![Morpheme {
                surface: "今日".to_owned(),
                byte_span: 0..6,
                char_span: 0..2,
                features,
            }],
        }
    }

    #[test]
    fn identical_features_emit_no_diff() {
        let from = analysis("a", features(&[("pos", Some("名詞"))]));
        let to = analysis("b", features(&[("pos", Some("名詞"))]));
        let regions = vec![Region::OneToOne(AlignedMorpheme {
            text_span: 0..2,
            from_index: 0,
            to_index: 0,
        })];
        assert!(compare_feature_diffs(&from, &to, &regions, &[]).is_empty());
    }

    #[test]
    fn changed_pos_emits_feature_diff_with_region_index() {
        let from = analysis("a", features(&[("pos", Some("名詞"))]));
        let to = analysis("b", features(&[("pos", Some("動詞"))]));
        let regions = vec![Region::OneToOne(AlignedMorpheme {
            text_span: 0..2,
            from_index: 0,
            to_index: 0,
        })];
        let diffs = compare_feature_diffs(&from, &to, &regions, &[]);
        assert_eq!(diffs[0].region_index, 0);
        assert_eq!(
            diffs[0].changed["pos"],
            ChangedValue {
                from: Some("名詞".into()),
                to: Some("動詞".into())
            }
        );
    }

    #[test]
    fn missing_feature_is_explicit_none() {
        let from = analysis("a", features(&[("lemma", Some("今日"))]));
        let to = analysis("b", features(&[]));
        let regions = vec![Region::OneToOne(AlignedMorpheme {
            text_span: 0..2,
            from_index: 0,
            to_index: 0,
        })];
        let diffs = compare_feature_diffs(&from, &to, &regions, &[]);
        assert_eq!(
            diffs[0].changed["lemma"],
            ChangedValue {
                from: Some("今日".into()),
                to: None
            }
        );
    }

    #[test]
    fn context_keys_are_deduplicated_and_sorted() {
        let from = analysis(
            "a",
            features(&[("lemma", Some("今日")), ("pos", Some("名詞"))]),
        );
        let to = analysis(
            "b",
            features(&[("lemma", Some("今日")), ("pos", Some("動詞"))]),
        );
        let regions = vec![Region::OneToOne(AlignedMorpheme {
            text_span: 0..2,
            from_index: 0,
            to_index: 0,
        })];
        let diffs = compare_feature_diffs(&from, &to, &regions, &["lemma".into(), "lemma".into()]);
        assert_eq!(
            diffs[0].same_context.keys().collect::<Vec<_>>(),
            vec!["lemma"]
        );
    }

    #[test]
    fn segmentation_regions_do_not_emit_feature_diffs() {
        let from = analysis("a", features(&[("pos", Some("名詞"))]));
        let to = analysis("b", features(&[("pos", Some("動詞"))]));
        let regions = vec![Region::Segmentation(SegmentationDiff {
            text_span: 0..2,
            from_indices: 0..1,
            to_indices: 0..1,
            from_surfaces: vec!["今日".to_owned()],
            to_surfaces: vec!["今日".to_owned()],
            kind: SegmentationKind::Resegment,
        })];
        assert!(compare_feature_diffs(&from, &to, &regions, &[]).is_empty());
    }
}
