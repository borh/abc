use std::ops::Range;
use std::sync::Arc;

use ab_morph_diff::{
    Analysis, AnalyzerId, AnalyzerWarning, CharByteMap, FeatureMap, Morpheme, TextId,
};

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

/// Per-analysis summary of how [`remap_spans`] carried morphemes back to
/// original-text coordinates.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct RemapReport {
    /// Morphemes whose normalized span crossed an offset-map boundary and were
    /// widened to the covering original span (see
    /// [`OffsetMap::to_original_covering`]).
    pub snapped: usize,
    /// Morphemes folded into their predecessor because widening made the two
    /// original spans overlap (both tokens lay inside one rewritten span).
    pub merged: usize,
}

/// Remap each morpheme's `byte_span`, `char_span`, and
/// `surface` from normalized-text coordinates to original-text coordinates.
///
/// This honors the spec invariant #2: after remapping, `byte_span`,
/// `char_span`, and `surface` of every morpheme refer to the ORIGINAL source
/// text (the same text the analyzer's caller will validate against), not the
/// normalized view that was actually tokenized.
///
/// A morpheme whose normalized span crosses an annotation boundary where the
/// byte length changed (the analyzer segmented a rewritten span differently
/// from the rewrite) cannot be mapped exactly. It is widened to the covering
/// original span; if that overlaps the preceding morpheme's span the two are
/// folded into one morpheme (surface and spans from the original text, features
/// of the first). Both cases are counted in the returned [`RemapReport`] and
/// recorded as an `ortho_remap` warning on the analysis, so the projected
/// tokenization is never silently coarser than the analyzer's.
///
/// # Errors
///
/// Returns [`OrthoMapError::UncoveredOffset`] if a morpheme byte offset does
/// not map to any entry — this should not happen for well-formed inputs and is
/// treated as a hard diagnostic. In that case the analysis is left untouched
/// (still in normalized coordinates).
pub fn remap_spans(
    analysis: &mut Analysis,
    offset_map: &OffsetMap,
    original_source_text: &str,
) -> Result<RemapReport, OrthoMapError> {
    if offset_map.is_empty() {
        return Ok(RemapReport::default());
    }
    // Pass 1 (fallible, no mutation): the original range of every morpheme.
    let mut report = RemapReport::default();
    let mut first_snap_offset = None;
    let mut remapped = Vec::with_capacity(analysis.morphemes.len());
    for morpheme in &analysis.morphemes {
        let range = match offset_map.to_original(morpheme.byte_span.clone()) {
            Ok(range) => range,
            Err(OrthoMapError::CrossesBoundary { .. }) => {
                let range = offset_map.to_original_covering(morpheme.byte_span.clone())?;
                report.snapped += 1;
                first_snap_offset.get_or_insert(range.start);
                range
            }
            Err(error) => return Err(error),
        };
        remapped.push(range);
    }
    // Pass 2: apply, folding overlaps into the predecessor.
    let char_map = CharByteMap::new(original_source_text);
    let mut out: Vec<Morpheme> = Vec::with_capacity(analysis.morphemes.len());
    for (mut morpheme, range) in analysis.morphemes.drain(..).zip(remapped) {
        if let Some(previous) = out
            .last_mut()
            .filter(|previous| range.start < previous.byte_span.end)
        {
            previous.byte_span.end = previous.byte_span.end.max(range.end);
            set_from_original(previous, original_source_text, &char_map);
            report.merged += 1;
            continue;
        }
        morpheme.byte_span = range;
        set_from_original(&mut morpheme, original_source_text, &char_map);
        out.push(morpheme);
    }
    analysis.morphemes = out;
    if report.snapped > 0 {
        analysis.warnings.push(AnalyzerWarning {
            analyzer_id: analysis.analyzer.clone(),
            text_id: analysis.text_id.clone(),
            stage: "ortho_remap".to_owned(),
            message: format!(
                "{} morpheme spans crossed a normalization boundary and were widened to the covering original span; {} folded into their predecessor",
                report.snapped, report.merged
            ),
            count: report.snapped,
            first_byte_offset: first_snap_offset.unwrap_or(0),
            hard_limit_bytes: 0,
        });
    }
    Ok(report)
}

/// Rebuild `surface` and `char_span` from the ORIGINAL text at the morpheme's
/// (already remapped) `byte_span`, so the morpheme reports the original-doc
/// substring rather than the normalized-text substring (e.g. "ヴ" instead of
/// "う゛"), in original-doc char coordinates.
fn set_from_original(morpheme: &mut Morpheme, original_source_text: &str, char_map: &CharByteMap) {
    let range = morpheme.byte_span.clone();
    if range.end <= original_source_text.len()
        && original_source_text.is_char_boundary(range.start)
        && original_source_text.is_char_boundary(range.end)
    {
        morpheme.surface = original_source_text[range.clone()].to_owned();
    }
    if let Some(char_span) = char_byte_to_char_span(char_map, range) {
        morpheme.char_span = char_span;
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
