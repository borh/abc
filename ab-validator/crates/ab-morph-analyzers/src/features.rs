use std::sync::{Arc, LazyLock};

use ab_morph_diff::{FeatureKey, FeatureMap, FeatureValue};

const VIBRATO_UNIDIC_KEYS: &[&str] = &[
    "pos1",
    "pos2",
    "pos3",
    "pos4",
    "ctype",
    "cform",
    "lform",
    "lemma",
    "orth",
    "pron",
    "orth_base",
    "pron_base",
    "goshu",
    "itype",
    "iform",
    "ftype",
    "fform",
    "icon_type",
    "fcon_type",
    "type",
    "kana",
    "kana_base",
    "form",
    "form_base",
    "atype",
    "acon_type",
    "amod_type",
    "lex_type",
];
const VAPORETTO_UNIDIC_KEYS: &[&str] = &[
    "pos1",
    "pos2",
    "pos3",
    "pos4",
    "ctype",
    "cform",
    "lemma",
    "orth",
    "pron",
    "orth_base",
    "pron_base",
    "goshu",
    "itype",
    "iform",
    "ftype",
    "fform",
    "icon_type",
    "fcon_type",
    "type",
    "kana",
    "kana_base",
    "form",
    "form_base",
    "atype",
    "acon_type",
    "amod_type",
    "lex_type",
];

// Interned key names shared across tokens: cloning an Arc<str> is a refcount
// bump, while `&'static str -> Arc<str>` allocates per token.
static VIBRATO_UNIDIC_KEY_ARCS: LazyLock<Vec<FeatureKey>> = LazyLock::new(|| {
    VIBRATO_UNIDIC_KEYS
        .iter()
        .map(|key| (*key).into())
        .collect()
});
static VAPORETTO_UNIDIC_KEY_ARCS: LazyLock<Vec<FeatureKey>> = LazyLock::new(|| {
    VAPORETTO_UNIDIC_KEYS
        .iter()
        .map(|key| (*key).into())
        .collect()
});

pub(crate) fn feature_value(value: impl AsRef<str>) -> Option<FeatureValue> {
    let value = value.as_ref();
    if value.is_empty() || value == "*" {
        None
    } else {
        Some(value.into())
    }
}

pub(crate) fn parse_vibrato_feature_string(feature: &str) -> FeatureMap {
    FeatureMap::from_entries(feature.split(',').enumerate().map(|(index, value)| {
        let key = VIBRATO_UNIDIC_KEY_ARCS
            .get(index)
            .map(Arc::clone)
            .unwrap_or_else(|| format!("field_{index}").into());
        (key, feature_value(value))
    }))
}

pub(crate) fn parse_vaporetto_feature_string<'a>(
    feature_tags: impl IntoIterator<Item = Option<std::borrow::Cow<'a, str>>>,
) -> FeatureMap {
    FeatureMap::from_entries(feature_tags.into_iter().enumerate().map(|(index, tag)| {
        let key = VAPORETTO_UNIDIC_KEY_ARCS
            .get(index)
            .map(Arc::clone)
            .unwrap_or_else(|| format!("field_{index}").into());
        (key, tag.and_then(|tag| feature_value(tag.as_ref())))
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalizes_missing_values() {
        assert_eq!(feature_value("*"), None);
        assert_eq!(feature_value(""), None);
        assert_eq!(feature_value("名詞"), Some("名詞".into()));
    }

    #[test]
    fn parses_known_and_missing_vibrato_fields() {
        let features = parse_vibrato_feature_string("名詞,普通名詞,*,*,extra");
        assert_eq!(features.get("pos1"), Some(&Some("名詞".into())));
        assert_eq!(features.get("pos2"), Some(&Some("普通名詞".into())));
        assert_eq!(features.get("pos3"), Some(&None));
        assert_eq!(features.get("pos4"), Some(&None));
        assert_eq!(features.get("ctype"), Some(&Some("extra".into())));
    }

    #[test]
    fn stores_extra_vibrato_fields_with_numbered_names() {
        let mut fields = vec!["*"; 28];
        fields.push("tail");
        let features = parse_vibrato_feature_string(&fields.join(","));
        assert_eq!(features.get("field_28"), Some(&Some("tail".into())));
    }

    #[test]
    fn parses_known_and_missing_vaporetto_fields() {
        let features = parse_vaporetto_feature_string([
            Some("名詞".into()),
            Some("普通名詞".into()),
            None,
            Some("*".into()),
        ]);
        assert_eq!(features.get("pos1"), Some(&Some("名詞".into())));
        assert_eq!(features.get("pos2"), Some(&Some("普通名詞".into())));
        assert_eq!(features.get("pos3"), Some(&None));
        assert_eq!(features.get("pos4"), Some(&None));
    }

    #[test]
    fn stores_extra_vaporetto_fields_with_numbered_names() {
        let tags = (0..30)
            .map(|index| {
                if index == 28 {
                    Some(format!("tag-{index}").into())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        let features = parse_vaporetto_feature_string(tags);
        assert_eq!(features.get("field_28"), Some(&Some("tag-28".into())));
    }
}
