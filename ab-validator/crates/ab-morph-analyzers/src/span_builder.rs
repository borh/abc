use std::ops::Range;
use std::sync::Arc;

use ab_morph_diff::{Analysis, AnalyzerId, CharByteMap, FeatureMap, Morpheme, TextId};

use crate::AnalyzerError;

use ab_ortho_detect::{OffsetMap, OrthoMapError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawToken {
    /// Surface text as emitted by the analyzer. Only needed when `byte_span`
    /// is `None` (sequential span matching); span-reporting analyzers pass
    /// `None` so no per-token String is allocated.
    pub emitted_surface: Option<String>,
    pub byte_span: Option<Range<usize>>,
    pub features: FeatureMap,
}

pub(crate) fn build_analysis_from_tokens(
    analyzer: AnalyzerId,
    text_id: TextId,
    source_text: &str,
    tokens: impl IntoIterator<Item = RawToken>,
) -> Result<Analysis, AnalyzerError> {
    let morphemes = build_morphemes_from_tokens(&analyzer, &text_id, source_text, tokens)?;

    Ok(Analysis {
        analyzer,
        text_id,
        source_text: Arc::from(source_text),
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
                token.emitted_surface.as_deref().unwrap_or(""),
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

/// Post-process an Analysis to remap morpheme `byte_span`, `char_span`, and
/// `surface` from normalized-text coordinates to original-text coordinates.
///
/// This honors the spec invariant #2: after remapping, `byte_span`,
/// `char_span`, and `surface` of every morpheme refer to the ORIGINAL source
/// text (the same text the analyzer's caller will validate against), not the
/// normalized view that was actually tokenized.
///
/// # Errors
///
/// Returns [`OrthoMapError::CrossesBoundary`] if a morpheme span crosses an
/// annotation boundary where byte-length changed. The pipeline routes this to
/// `errors_writer`; the morpheme is left in normalized coords for that case
/// (diagnostic, not a crash). Returns [`OrthoMapError::UncoveredOffset`] if a
/// morpheme byte offset does not map to any entry — this should not happen for
/// well-formed inputs and is treated as a hard diagnostic.
pub fn remap_spans(
    analysis: &mut Analysis,
    offset_map: &OffsetMap,
    original_source_text: &str,
) -> Result<(), OrthoMapError> {
    if offset_map.is_empty() {
        return Ok(());
    }
    let char_map = CharByteMap::new(original_source_text);
    let mut first_err: Option<OrthoMapError> = None;
    for morpheme in &mut analysis.morphemes {
        match offset_map.to_original(morpheme.byte_span.clone()) {
            Ok(remapped) => {
                morpheme.byte_span = remapped.clone();
                // Rebuild surface from the ORIGINAL text at the remapped range so
                // the morpheme reports the original-doc substring rather than the
                // normalized-text substring (e.g. "ヴ" instead of "う゛").
                if original_source_text.is_char_boundary(remapped.start)
                    && original_source_text.is_char_boundary(remapped.end)
                    && remapped.end <= original_source_text.len()
                {
                    morpheme.surface = original_source_text[remapped.clone()].to_owned();
                }
                // Rebuild char_span from the original text's char map so it is
                // expressed in original-doc char coordinates.
                if let Some(cs) = char_byte_to_char_span(&char_map, remapped) {
                    morpheme.char_span = cs;
                }
            }
            Err(e) => {
                // Leave this morpheme in normalized coords. Record the first
                // error so the caller knows the Analysis is partial.
                if first_err.is_none() {
                    first_err = Some(e);
                }
            }
        }
    }
    match first_err {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

fn char_byte_to_char_span(char_map: &CharByteMap, byte_span: Range<usize>) -> Option<Range<usize>> {
    if byte_span.start > byte_span.end {
        return None;
    }
    Some(char_map.char_count_at_byte(byte_span.start)..char_map.char_count_at_byte(byte_span.end))
}

#[cfg(test)]
mod tests {
    use ab_morph_diff::FeatureMap;

    use super::*;

    fn raw(surface: &str) -> RawToken {
        RawToken {
            emitted_surface: Some(surface.to_owned()),
            byte_span: None,
            features: FeatureMap::new(),
        }
    }

    #[test]
    fn builds_multibyte_byte_and_char_spans() {
        let analysis = build_analysis_from_tokens(
            "test".to_owned(),
            "t1".to_owned(),
            "今日はA",
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
            "吾輩\nは 猫",
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
            "吾輩\nは",
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
            emitted_surface: Some("ABC".to_owned()),
            byte_span: Some(0..9),
            features: FeatureMap::new(),
        };
        let analysis =
            build_analysis_from_tokens("test".to_owned(), "t3".to_owned(), "ＡＢＣ", vec![token])
                .unwrap();

        assert_eq!(analysis.morphemes[0].surface, "ＡＢＣ");
        assert_eq!(analysis.morphemes[0].byte_span, 0..9);
        assert_eq!(analysis.morphemes[0].char_span, 0..3);
    }

    #[test]
    fn skips_zero_length_reported_spans() {
        let tokens = vec![
            RawToken {
                emitted_surface: None,
                byte_span: Some(0..6),
                features: FeatureMap::new(),
            },
            RawToken {
                emitted_surface: None,
                byte_span: Some(6..6),
                features: FeatureMap::new(),
            },
            RawToken {
                emitted_surface: None,
                byte_span: Some(6..9),
                features: FeatureMap::new(),
            },
        ];

        let analysis =
            build_analysis_from_tokens("test".to_owned(), "t4".to_owned(), "吾輩は", tokens)
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
            "今日は",
            vec![raw("は")],
        )
        .unwrap_err();

        assert!(matches!(
            err,
            AnalyzerError::SurfaceMismatch { cursor: 0, .. }
        ));
    }

    #[test]
    fn reported_spans_cover_whitespace_leading_surfaces() {
        // The full-corpus qkana failure class: a token whose surface begins
        // with whitespace but contains non-whitespace cannot be re-matched
        // sequentially (the walk skips the source whitespace first), but a
        // reported span is authoritative. Vibrato reports spans for exactly
        // this reason.
        let source = "あ　　～\n";
        let tokens = vec![
            RawToken {
                emitted_surface: None,
                byte_span: Some(0..3),
                features: FeatureMap::new(),
            },
            RawToken {
                emitted_surface: None,
                byte_span: Some(3..12),
                features: FeatureMap::new(),
            },
        ];

        let analysis =
            build_analysis_from_tokens("test".to_owned(), "t6".to_owned(), source, tokens).unwrap();

        assert_eq!(analysis.morphemes[1].surface, "　　～");
        assert_eq!(analysis.morphemes[1].byte_span, 3..12);
        assert_eq!(analysis.morphemes[1].char_span, 1..4);
    }

    #[test]
    fn sequential_matching_rejects_whitespace_leading_surfaces() {
        // Companion to the test above: the same token stream expressed as
        // emitted surfaces (no reported spans) fails, because the
        // whitespace-skip heuristic consumes the source whitespace the
        // surface itself claims.
        let err = build_analysis_from_tokens(
            "test".to_owned(),
            "t6".to_owned(),
            "あ　　～\n",
            vec![raw("あ"), raw("　　～")],
        )
        .unwrap_err();

        assert!(matches!(err, AnalyzerError::SurfaceMismatch { .. }));
    }

    #[test]
    fn allows_trailing_uncovered_text() {
        let analysis = build_analysis_from_tokens(
            "test".to_owned(),
            "t5".to_owned(),
            "吾輩は猫",
            vec![raw("吾輩")],
        )
        .unwrap();

        assert_eq!(analysis.morphemes.len(), 1);
        assert_eq!(analysis.morphemes[0].byte_span, 0..6);
    }
}
