use std::collections::{BTreeMap, BTreeSet};

use crate::{Analysis, ChangedValue, FeatureDiff, FeatureKey, FeatureMap, FeatureValue, Region};

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
        let mut changed = BTreeMap::new();
        let mut same_context = BTreeMap::new();
        visit_feature_pairs(
            &from_m.features,
            &to_m.features,
            |key, from_value, to_value| {
                if from_value != to_value {
                    changed.insert(
                        key.clone(),
                        ChangedValue {
                            from: from_value.cloned(),
                            to: to_value.cloned(),
                        },
                    );
                } else if context_keys.contains(key) {
                    same_context.insert(key.clone(), from_value.cloned());
                }
            },
        );
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

pub(crate) fn visit_feature_pairs(
    from: &FeatureMap,
    to: &FeatureMap,
    mut visit: impl FnMut(&FeatureKey, Option<&FeatureValue>, Option<&FeatureValue>),
) {
    let mut from_iter = from.iter().peekable();
    let mut to_iter = to.iter().peekable();

    loop {
        match (from_iter.peek(), to_iter.peek()) {
            (Some((from_key, _)), Some((to_key, _))) => {
                match from_key.as_ref().cmp(to_key.as_ref()) {
                    std::cmp::Ordering::Less => {
                        let (key, value) = from_iter.next().expect("peeked from value");
                        visit(key, value.as_ref(), None);
                    }
                    std::cmp::Ordering::Equal => {
                        let (key, from_value) = from_iter.next().expect("peeked from value");
                        let (_, to_value) = to_iter.next().expect("peeked to value");
                        visit(key, from_value.as_ref(), to_value.as_ref());
                    }
                    std::cmp::Ordering::Greater => {
                        let (key, value) = to_iter.next().expect("peeked to value");
                        visit(key, None, value.as_ref());
                    }
                }
            }
            (Some(_), None) => {
                let (key, value) = from_iter.next().expect("peeked from value");
                visit(key, value.as_ref(), None);
            }
            (None, Some(_)) => {
                let (key, value) = to_iter.next().expect("peeked to value");
                visit(key, None, value.as_ref());
            }
            (None, None) => break,
        }
    }
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
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
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
            diffs[0]
                .changed
                .get("pos")
                .expect("feature diff should include pos"),
            &ChangedValue {
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
            diffs[0]
                .changed
                .get("lemma")
                .expect("feature diff should include lemma"),
            &ChangedValue {
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
            diffs[0]
                .same_context
                .keys()
                .map(|key| key.as_ref())
                .collect::<Vec<_>>(),
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
