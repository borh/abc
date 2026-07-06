//! Regression test for Task 7.5: coordinate coherence of remapped `Analysis`
//! when normalization changes byte length (`ヴ → う゛`).
//!
//! Spec invariant #2: after `remap_spans`, every morpheme's `byte_span`,
//! `char_span`, AND `surface` must refer to the ORIGINAL source text — not
//! the normalized view that was actually tokenized.

use std::sync::Arc;

use ab_morph_analyzers::span_builder::remap_spans;
use ab_morph_diff::{Analysis, FeatureMap, Morpheme};
use ab_ortho_detect::{OrthoAnnotation, OrthoNormalization, ortho_normalize};

/// normalized `今日う゛` = 12 bytes (今=3, 日=3, う=3, ゛=3)
/// original   `今日ヴ`   =  9 bytes (今=3, 日=3, ヴ=3)
/// First 6 bytes identity, then ヴ(3 bytes orig) ↔ う゛(6 bytes norm).
fn vu_case() -> (String, ab_ortho_detect::OffsetMap) {
    let annotations = vec![OrthoAnnotation {
        source_byte_range: 6..9,
        normalized_text: "う゛".to_owned(),
        kind: OrthoNormalization::ScriptKatakanaToHiragana,
        confidence: None,
    }];
    ortho_normalize("今日ヴ", &annotations)
}

fn make_analysis(norm_source: String, morphemes: Vec<Morpheme>) -> Analysis {
    Analysis {
        analyzer: "test".to_owned(),
        text_id: "t".to_owned(),
        source_text: Arc::from(norm_source),
        morphemes,
        warnings: Vec::new(),
        ortho_annotations: None,
        ortho_offset_map: None,
    }
}

#[test]
fn remap_rebuilds_byte_span_surface_and_char_span_for_vu() {
    let (normalized_text, offset_map) = vu_case();
    assert_eq!(normalized_text, "今日う゛");

    // A morpheme spanning the normalized `う゛` range (bytes 6..12).
    // In normalized text: 今=char0, 日=char1, う=char2, ゛=char3 → char_span 2..4.
    let morpheme = Morpheme {
        surface: "う゛".to_owned(),
        byte_span: 6..12,
        char_span: 2..4,
        features: FeatureMap::new(),
    };
    let mut analysis = make_analysis(normalized_text, vec![morpheme]);

    remap_spans(&mut analysis, &offset_map, "今日ヴ").expect("vu case should remap cleanly");

    let m = &analysis.morphemes[0];
    // byte_span: original `今日ヴ` bytes 6..9 (the `ヴ`).
    assert_eq!(m.byte_span, 6..9);
    // surface: original-doc substring at the remapped range.
    assert_eq!(m.surface, "ヴ");
    // char_span: original `今日ヴ` chars 今=0, 日=1, ヴ=2 → ヴ occupies 2..3.
    assert_eq!(m.char_span, 2..3);
}

#[test]
fn remap_preserves_identity_region_morpheme() {
    let (normalized_text, offset_map) = vu_case();
    assert_eq!(normalized_text, "今日う゛");

    // A morpheme spanning the unchanged prefix `今日` (norm bytes 0..6).
    let morpheme = Morpheme {
        surface: "今日".to_owned(),
        byte_span: 0..6,
        char_span: 0..2,
        features: FeatureMap::new(),
    };
    let mut analysis = make_analysis(normalized_text, vec![morpheme]);

    remap_spans(&mut analysis, &offset_map, "今日ヴ").expect("identity region remaps cleanly");

    let m = &analysis.morphemes[0];
    assert_eq!(m.byte_span, 0..6);
    assert_eq!(m.surface, "今日");
    assert_eq!(m.char_span, 0..2);
}

#[test]
fn remap_errors_when_morpheme_spans_annotation_boundary() {
    let (normalized_text, offset_map) = vu_case();
    assert_eq!(normalized_text, "今日う゛");

    // A morpheme spanning norm bytes 3..9: starts in `日` (entry 0: 0..6) and
    // ends in `う゛` (entry 1: 6..12). This crosses the boundary where
    // byte-length changed and cannot be remapped as a single span.
    let morpheme = Morpheme {
        surface: String::new(),
        byte_span: 3..9,
        char_span: 1..3,
        features: FeatureMap::new(),
    };
    let mut analysis = make_analysis(normalized_text, vec![morpheme]);

    let err = remap_spans(&mut analysis, &offset_map, "今日ヴ").unwrap_err();
    assert!(
        matches!(err, ab_ortho_detect::OrthoMapError::CrossesBoundary { .. }),
        "expected CrossesBoundary, got {err:?}"
    );
}

#[test]
fn remap_is_identity_for_empty_offset_map() {
    let mut analysis = make_analysis(
        "今日ヴ".to_owned(),
        vec![Morpheme {
            surface: "ヴ".to_owned(),
            byte_span: 6..9,
            char_span: 2..3,
            features: FeatureMap::new(),
        }],
    );

    // Empty OffsetMap = no annotations were applied (the typical case).
    remap_spans(
        &mut analysis,
        &ab_ortho_detect::OffsetMap::empty(),
        "今日ヴ",
    )
    .unwrap();

    // Nothing changed.
    let m = &analysis.morphemes[0];
    assert_eq!(m.byte_span, 6..9);
    assert_eq!(m.surface, "ヴ");
    assert_eq!(m.char_span, 2..3);
}
