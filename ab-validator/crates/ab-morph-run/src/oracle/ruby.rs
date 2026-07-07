//! Ruby-base extraction and per-analyzer reading selection for the ruby oracle.

use ab_morph_diff::Morpheme;
use ab_plaintext::ProjectionSpan;
use serde_json::Value;

pub(crate) struct RubyBase {
    pub char_start: u64,
    pub char_end: u64,
    pub base: String,
    pub reading: String,
}

/// Extract ruby bases (with editor reading) from the projected ruby spans.
/// The reading is resolved from the retained AAT via the span's JSON pointer.
/// Bases without a non-empty reading are skipped (no oracle judgment possible).
pub(crate) fn ruby_bases(aat: &Value, spans: &[ProjectionSpan]) -> Vec<RubyBase> {
    spans
        .iter()
        .filter(|span| span.is_ruby_base)
        .filter_map(|span| {
            let node = aat.pointer(&span.aat_pointer)?;
            let reading = node.get("reading").and_then(Value::as_str)?;
            if reading.is_empty() {
                return None;
            }
            let base = node
                .get("base")
                .and_then(Value::as_str)
                .unwrap_or("")
                .to_owned();
            Some(RubyBase {
                char_start: span.projected_char_start,
                char_end: span.projected_char_end,
                base,
                reading: reading.to_owned(),
            })
        })
        .collect()
}

/// Select the reading feature for a morpheme by analyzer family (spec R7).
pub(crate) fn morpheme_reading(analyzer_id: &str, morpheme: &Morpheme) -> Option<String> {
    let feat = |key: &str| {
        morpheme
            .features
            .get(key)
            .and_then(|value| value.as_ref())
            .map(|value| value.to_string())
            // A present-but-empty or "*" feature is absent, not a reading — else a
            // kana="*" would normalize to "" and become a false non-match instead
            // of falling back to pron. (Analyzers already map "*"→None at parse
            // time, but the guard makes the contract robust to any caller.)
            .filter(|value| !value.is_empty() && value != "*")
    };
    if analyzer_id.starts_with("vibrato") || analyzer_id.starts_with("vaporetto") {
        feat("kana").or_else(|| feat("pron"))
    } else if analyzer_id.starts_with("sudachi") {
        feat("reading_form")
    } else {
        None // test analyzers emit no reading
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ab_morph_diff::{FeatureMap, Morpheme};
    use serde_json::json;

    fn morph(surface: &str, chars: std::ops::Range<usize>, feats: &[(&str, &str)]) -> Morpheme {
        let mut features = FeatureMap::default();
        for (k, v) in feats {
            let _ = features.insert((*k).into(), Some((*v).into()));
        }
        Morpheme {
            surface: surface.to_owned(),
            byte_span: 0..0,
            char_span: chars,
            features,
        }
    }

    #[test]
    fn extracts_ruby_bases_with_reading_from_pointer() {
        let aat = json!({
            "blocks": [ { "content": [ { "kind": "ruby", "base": "東京", "reading": "とうきょう" } ] } ]
        });
        let spans = vec![ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 2,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "ruby".to_owned(),
            is_ruby_base: true,
            is_gaiji: false,
            is_note: false,
        }];
        let bases = ruby_bases(&aat, &spans);
        assert_eq!(bases.len(), 1);
        assert_eq!(bases[0].reading, "とうきょう");
        assert_eq!(bases[0].char_start, 0);
        assert_eq!(bases[0].char_end, 2);
    }

    #[test]
    fn skips_ruby_without_reading() {
        let aat = json!({ "blocks": [ { "content": [ { "kind": "ruby", "base": "x" } ] } ] });
        let spans = vec![ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 1,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "ruby".to_owned(),
            is_ruby_base: true,
            is_gaiji: false,
            is_note: false,
        }];
        assert!(ruby_bases(&aat, &spans).is_empty());
    }

    #[test]
    fn reading_by_family() {
        let vib = morph(
            "東京",
            0..2,
            &[("kana", "トウキョウ"), ("pron", "トーキョー")],
        );
        assert_eq!(
            morpheme_reading("vibrato:unidic-novel-202512", &vib).as_deref(),
            Some("トウキョウ")
        );
        let vib_pron_only = morph("x", 0..1, &[("pron", "トーキョー")]);
        assert_eq!(
            morpheme_reading("vibrato", &vib_pron_only).as_deref(),
            Some("トーキョー")
        );
        // kana present but "*" → falls back to pron, not a false empty reading.
        let vib_star = morph("x", 0..1, &[("kana", "*"), ("pron", "トーキョー")]);
        assert_eq!(
            morpheme_reading("vibrato", &vib_star).as_deref(),
            Some("トーキョー")
        );
        let sud = morph("東京", 0..2, &[("reading_form", "トウキョウ")]);
        assert_eq!(
            morpheme_reading("sudachi-c", &sud).as_deref(),
            Some("トウキョウ")
        );
        let t = morph("x", 0..1, &[]);
        assert_eq!(morpheme_reading("test:single", &t), None);
    }
}
