use std::ops::Range;

use ab_morph_diff::{Analysis, AnalyzerId, CharByteMap, FeatureMap, Morpheme, TextId};

use crate::AnalyzerError;

use ab_ortho_detect::OffsetMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawToken {
    pub emitted_surface: String,
    pub byte_span: Option<Range<usize>>,
    pub features: FeatureMap,
}

pub(crate) fn build_analysis_from_tokens(
    analyzer: AnalyzerId,
    text_id: TextId,
    source_text: String,
    tokens: impl IntoIterator<Item = RawToken>,
) -> Result<Analysis, AnalyzerError> {
    let morphemes = build_morphemes_from_tokens(&analyzer, &text_id, &source_text, tokens)?;

    Ok(Analysis {
        analyzer,
        text_id,
        source_text,
        morphemes,
        warnings: Vec::new(),
        ortho_annotations: None,
        ortho_offset_map: None,
    })
}

pub(crate) fn build_morphemes_from_tokens(
    analyzer: &str,
    text_id: &str,
    source_text: &str,
    tokens: impl IntoIterator<Item = RawToken>,
) -> Result<Vec<Morpheme>, AnalyzerError> {
    let char_map = CharByteMap::new(source_text);
    let mut cursor = 0usize;
    let mut morphemes = Vec::new();

    for token in tokens {
        if matches!(&token.byte_span, Some(span) if span.start == span.end) {
            continue;
        }

        let byte_span = match token.byte_span {
            Some(span) => validate_reported_span(analyzer, text_id, source_text, &span)?,
            None => find_sequential_span(
                analyzer,
                text_id,
                source_text,
                cursor,
                &token.emitted_surface,
            )?,
        };

        let surface = source_text[byte_span.clone()].to_owned();
        let char_start = char_map.char_count_at_byte(byte_span.start);
        let char_end = char_map.char_count_at_byte(byte_span.end);
        cursor = byte_span.end;

        morphemes.push(Morpheme {
            surface,
            byte_span,
            char_span: char_start..char_end,
            features: token.features,
        });
    }

    Ok(morphemes)
}

fn validate_reported_span(
    analyzer: &str,
    text_id: &str,
    source_text: &str,
    span: &Range<usize>,
) -> Result<Range<usize>, AnalyzerError> {
    if span.start > span.end
        || span.end > source_text.len()
        || !source_text.is_char_boundary(span.start)
        || !source_text.is_char_boundary(span.end)
        || span.start == span.end
    {
        return Err(AnalyzerError::InvalidTokenSpan {
            analyzer: analyzer.to_owned(),
            text_id: text_id.to_owned(),
            span: span.clone(),
            message: "span is empty, out of bounds, reversed, or not on UTF-8 boundaries"
                .to_owned(),
        });
    }

    Ok(span.clone())
}

fn find_sequential_span(
    analyzer: &str,
    text_id: &str,
    source_text: &str,
    cursor: usize,
    surface: &str,
) -> Result<Range<usize>, AnalyzerError> {
    let mut cursor = cursor;

    if !surface.chars().all(char::is_whitespace) {
        while cursor < source_text.len() {
            let ch = source_text[cursor..]
                .chars()
                .next()
                .expect("cursor is in bounds");
            if !ch.is_whitespace() {
                break;
            }
            cursor += ch.len_utf8();
        }
    }

    if source_text[cursor..].starts_with(surface) {
        Ok(cursor..cursor + surface.len())
    } else {
        Err(AnalyzerError::SurfaceMismatch {
            analyzer: analyzer.to_owned(),
            text_id: text_id.to_owned(),
            cursor,
            expected_surface: surface.to_owned(),
        })
    }
}

/// Post-process an Analysis to remap morpheme byte_spans and char_spans
/// from normalized-text coordinates to original-text coordinates.
pub fn remap_spans(analysis: &mut Analysis, offset_map: &OffsetMap) {
    if offset_map.is_empty() {
        return;
    }
    for morpheme in &mut analysis.morphemes {
        // Split spans that cross annotation boundaries before remapping.
        // For Phase 1, we assume single-annotation spans (the common case);
        // cross-boundary spans (ヴ→う゛ edge) are rare and handled by
        // the OffsetMap panic guard.
        morpheme.byte_span = offset_map.to_original(morpheme.byte_span.clone());
        // char_span recalculation requires the original source text.
        // For Phase 1, keep the normalized-text char_span as an approximation.
        // Follow-up: rebuild char_map from original text bytes.
    }
}

#[cfg(test)]
mod tests {
    use ab_morph_diff::FeatureMap;

    use super::*;

    fn raw(surface: &str) -> RawToken {
        RawToken {
            emitted_surface: surface.to_owned(),
            byte_span: None,
            features: FeatureMap::new(),
        }
    }

    #[test]
    fn builds_multibyte_byte_and_char_spans() {
        let analysis = build_analysis_from_tokens(
            "test".to_owned(),
            "t1".to_owned(),
            "今日はA".to_owned(),
            vec![raw("今日"), raw("は"), raw("A")],
        )
        .unwrap();

        assert_eq!(analysis.morphemes[0].byte_span, 0..6);
        assert_eq!(analysis.morphemes[0].char_span, 0..2);
        assert_eq!(analysis.morphemes[1].byte_span, 6..9);
        assert_eq!(analysis.morphemes[1].char_span, 2..3);
        assert_eq!(analysis.morphemes[2].byte_span, 9..10);
        assert_eq!(analysis.morphemes[2].char_span, 3..4);
    }

    #[test]
    fn skips_whitespace_without_covering_it() {
        let analysis = build_analysis_from_tokens(
            "test".to_owned(),
            "t2".to_owned(),
            "吾輩\nは 猫".to_owned(),
            vec![raw("吾輩"), raw("は"), raw("猫")],
        )
        .unwrap();

        assert_eq!(analysis.morphemes[0].byte_span, 0..6);
        assert_eq!(analysis.morphemes[1].byte_span, 7..10);
        assert_eq!(analysis.morphemes[2].byte_span, 11..14);
    }

    #[test]
    fn matches_emitted_whitespace_token_without_skipping_it() {
        let analysis = build_analysis_from_tokens(
            "test".to_owned(),
            "t3".to_owned(),
            "吾輩\nは".to_owned(),
            vec![raw("吾輩"), raw("\n"), raw("は")],
        )
        .unwrap();

        assert_eq!(analysis.morphemes[0].byte_span, 0..6);
        assert_eq!(analysis.morphemes[1].surface, "\n");
        assert_eq!(analysis.morphemes[1].byte_span, 6..7);
        assert_eq!(analysis.morphemes[2].byte_span, 7..10);
    }

    #[test]
    fn uses_original_source_slice_for_reported_spans() {
        let token = RawToken {
            emitted_surface: "ABC".to_owned(),
            byte_span: Some(0..9),
            features: FeatureMap::new(),
        };
        let analysis = build_analysis_from_tokens(
            "test".to_owned(),
            "t3".to_owned(),
            "ＡＢＣ".to_owned(),
            vec![token],
        )
        .unwrap();

        assert_eq!(analysis.morphemes[0].surface, "ＡＢＣ");
        assert_eq!(analysis.morphemes[0].byte_span, 0..9);
        assert_eq!(analysis.morphemes[0].char_span, 0..3);
    }

    #[test]
    fn skips_zero_length_reported_spans() {
        let tokens = vec![
            RawToken {
                emitted_surface: "吾輩".to_owned(),
                byte_span: Some(0..6),
                features: FeatureMap::new(),
            },
            RawToken {
                emitted_surface: String::new(),
                byte_span: Some(6..6),
                features: FeatureMap::new(),
            },
            RawToken {
                emitted_surface: "は".to_owned(),
                byte_span: Some(6..9),
                features: FeatureMap::new(),
            },
        ];

        let analysis = build_analysis_from_tokens(
            "test".to_owned(),
            "t4".to_owned(),
            "吾輩は".to_owned(),
            tokens,
        )
        .unwrap();

        assert_eq!(analysis.morphemes.len(), 2);
        assert_eq!(analysis.morphemes[0].surface, "吾輩");
        assert_eq!(analysis.morphemes[1].surface, "は");
    }

    #[test]
    fn returns_surface_mismatch_without_skipping_non_whitespace() {
        let err = build_analysis_from_tokens(
            "test".to_owned(),
            "t4".to_owned(),
            "今日は".to_owned(),
            vec![raw("は")],
        )
        .unwrap_err();

        assert!(matches!(
            err,
            AnalyzerError::SurfaceMismatch { cursor: 0, .. }
        ));
    }

    #[test]
    fn allows_trailing_uncovered_text() {
        let analysis = build_analysis_from_tokens(
            "test".to_owned(),
            "t5".to_owned(),
            "吾輩は猫".to_owned(),
            vec![raw("吾輩")],
        )
        .unwrap();

        assert_eq!(analysis.morphemes.len(), 1);
        assert_eq!(analysis.morphemes[0].byte_span, 0..6);
    }
}
