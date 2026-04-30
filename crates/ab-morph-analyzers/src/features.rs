use ab_morph_diff::{FeatureMap, FeatureValue};

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

pub(crate) fn feature_value(value: impl AsRef<str>) -> Option<FeatureValue> {
    let value = value.as_ref();
    if value.is_empty() || value == "*" {
        None
    } else {
        Some(value.into())
    }
}

pub(crate) fn parse_vibrato_feature_string(feature: &str) -> FeatureMap {
    let mut features = FeatureMap::with_capacity(VIBRATO_UNIDIC_KEYS.len());

    for (index, value) in feature.split(',').enumerate() {
        let key = VIBRATO_UNIDIC_KEYS
            .get(index)
            .map(|key| (*key).into())
            .unwrap_or_else(|| format!("field_{index}").into());
        features.insert(key, feature_value(value));
    }

    features
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
        assert_eq!(features["pos1"], Some("名詞".into()));
        assert_eq!(features["pos2"], Some("普通名詞".into()));
        assert_eq!(features["pos3"], None);
        assert_eq!(features["pos4"], None);
        assert_eq!(features["ctype"], Some("extra".into()));
    }

    #[test]
    fn stores_extra_vibrato_fields_with_numbered_names() {
        let mut fields = vec!["*"; 28];
        fields.push("tail");
        let features = parse_vibrato_feature_string(&fields.join(","));
        assert_eq!(features["field_28"], Some("tail".into()));
    }
}
