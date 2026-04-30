use std::ops::Range;

use crate::{Analysis, MorphDiffError};

pub fn validate_analysis(analysis: &Analysis) -> Result<(), MorphDiffError> {
    validate_analysis_against_source(analysis, &analysis.source_text)
}

pub fn validate_analysis_against_source(
    analysis: &Analysis,
    source_text: &str,
) -> Result<(), MorphDiffError> {
    for (index, morpheme) in analysis.morphemes.iter().enumerate() {
        if morpheme.byte_span.start >= morpheme.byte_span.end
            || morpheme.byte_span.end > source_text.len()
            || !source_text.is_char_boundary(morpheme.byte_span.start)
            || !source_text.is_char_boundary(morpheme.byte_span.end)
        {
            return Err(MorphDiffError::InvalidByteSpan {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            });
        }

        if index > 0 {
            let previous = &analysis.morphemes[index - 1];
            if morpheme.byte_span.start < previous.byte_span.start {
                return Err(MorphDiffError::OutOfOrderSpan {
                    analyzer: analysis.analyzer.clone(),
                    text_id: analysis.text_id.clone(),
                    index,
                });
            }
            if morpheme.byte_span.start < previous.byte_span.end {
                return Err(MorphDiffError::OverlappingSpan {
                    analyzer: analysis.analyzer.clone(),
                    text_id: analysis.text_id.clone(),
                    previous: index - 1,
                    current: index,
                });
            }
        }

        let expected_char_span = byte_span_to_char_span(source_text, morpheme.byte_span.clone())
            .ok_or_else(|| MorphDiffError::InvalidByteSpan {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            })?;
        if expected_char_span != morpheme.char_span {
            return Err(MorphDiffError::CharSpanMismatch {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            });
        }
        if &source_text[morpheme.byte_span.clone()] != morpheme.surface.as_str() {
            return Err(MorphDiffError::SurfaceMismatch {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            });
        }
    }
    Ok(())
}

fn byte_span_to_char_span(source: &str, byte_span: Range<usize>) -> Option<Range<usize>> {
    if byte_span.start > byte_span.end
        || byte_span.end > source.len()
        || !source.is_char_boundary(byte_span.start)
        || !source.is_char_boundary(byte_span.end)
    {
        return None;
    }
    let start = source[..byte_span.start].chars().count();
    let end = source[..byte_span.end].chars().count();
    Some(start..end)
}

#[cfg(test)]
mod tests {
    use crate::{Analysis, FeatureMap, MorphDiffError, Morpheme};

    use super::validate_analysis;

    fn m(source: &str, surface: &str, byte_start: usize, byte_end: usize) -> Morpheme {
        let char_start = source[..byte_start].chars().count();
        let char_end = source[..byte_end].chars().count();
        Morpheme {
            surface: surface.to_owned(),
            byte_span: byte_start..byte_end,
            char_span: char_start..char_end,
            features: FeatureMap::new(),
        }
    }

    fn analysis(morphemes: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: "a".to_owned(),
            text_id: "t".to_owned(),
            source_text: "今日はabc".to_owned(),
            morphemes,
        }
    }

    #[test]
    fn accepts_touching_spans() {
        let a = analysis(vec![
            m("今日はabc", "今日", 0, 6),
            m("今日はabc", "は", 6, 9),
            m("今日はabc", "abc", 9, 12),
        ]);
        assert!(validate_analysis(&a).is_ok());
    }

    #[test]
    fn rejects_out_of_order_spans() {
        let a = analysis(vec![
            m("今日はabc", "は", 6, 9),
            m("今日はabc", "今日", 0, 6),
        ]);
        assert!(matches!(
            validate_analysis(&a),
            Err(MorphDiffError::OutOfOrderSpan { .. })
        ));
    }

    #[test]
    fn rejects_overlapping_spans() {
        let a = analysis(vec![
            m("今日はabc", "今日", 0, 6),
            Morpheme {
                surface: "日は".to_owned(),
                byte_span: 3..9,
                char_span: 1..3,
                features: FeatureMap::new(),
            },
        ]);
        assert!(matches!(
            validate_analysis(&a),
            Err(MorphDiffError::OverlappingSpan { .. })
        ));
    }

    #[test]
    fn rejects_empty_spans() {
        let a = analysis(vec![Morpheme {
            surface: String::new(),
            byte_span: 0..0,
            char_span: 0..0,
            features: FeatureMap::new(),
        }]);
        assert!(matches!(
            validate_analysis(&a),
            Err(MorphDiffError::InvalidByteSpan { .. })
        ));
    }

    #[test]
    fn rejects_non_char_boundary_byte_span() {
        let a = analysis(vec![Morpheme {
            surface: "今".to_owned(),
            byte_span: 0..1,
            char_span: 0..1,
            features: FeatureMap::new(),
        }]);
        assert!(matches!(
            validate_analysis(&a),
            Err(MorphDiffError::InvalidByteSpan { .. })
        ));
    }

    #[test]
    fn rejects_char_span_mismatch() {
        let a = analysis(vec![Morpheme {
            surface: "今日".to_owned(),
            byte_span: 0..6,
            char_span: 0..1,
            features: FeatureMap::new(),
        }]);
        assert!(matches!(
            validate_analysis(&a),
            Err(MorphDiffError::CharSpanMismatch { .. })
        ));
    }

    #[test]
    fn rejects_surface_source_mismatch() {
        let a = analysis(vec![Morpheme {
            surface: "明日".to_owned(),
            byte_span: 0..6,
            char_span: 0..2,
            features: FeatureMap::new(),
        }]);
        assert!(matches!(
            validate_analysis(&a),
            Err(MorphDiffError::SurfaceMismatch { .. })
        ));
    }
}
