//! ortho-normalization reproducibility golden.
//!
//! Pins the full **detector-driven** input-derivation chain that the warehouse
//! run path executes (pipeline.rs: `sentence_split` → `OrthoDetector::detect`
//! → `ortho_normalize` → `remap_spans`), end-to-end, from a fixed source:
//!
//!   1. the real `HeuristicV1` detector fires on a katakana sentence,
//!   2. `ortho_normalize` produces the derived (normalized) analyzer input,
//!   3. `remap_spans` carries morpheme spans back to ORIGINAL-doc coordinates,
//!      rebuilding each surface from the original katakana, and
//!   4. the recorded `NormalizationPolicy::policy_hash()` for that detector is
//!      the pinned heuristic-v1 constant — the same value the warehouse run
//!      records (`resolve_run_normalization`, ab-morph-run) and the tokenizer
//!      profile declares (abc). One transformation, one identity.
//!
//! Reproducibility (spec P5): re-running the chain from the same source under
//! the same policy regenerates a byte-identical derived input — asserted by
//! running the chain twice and comparing.
//!
//! Complements `remap_vu.rs`: that test pins the length-CHANGING remap (ヴ→う゛)
//! with hand-built annotations; this one pins the length-PRESERVING kata→hira
//! path driven by the actual detector, where byte spans are unchanged but every
//! surface must still be rebuilt from the original text.

use std::sync::Arc;

use ab_morph_analyzers::span_builder::remap_spans;
use ab_morph_diff::{Analysis, FeatureMap, Morpheme};
use ab_ortho_detect::heuristic::{HeuristicConfig, HeuristicV1};
use ab_ortho_detect::{
    NormalizationPolicy, OrthoDetector, OrthoDetectorId, OrthoNormalization, OrthoToken,
    OrthoTokenizer, ortho_normalize,
};

/// The pinned heuristic-v1 normalization-policy hash. Single-sourced with
/// `ab-ortho-detect`'s `policy::tests` and `ab-morph-run`'s
/// `resolve_run_normalization_heuristic_matches_policy_hash`; a change here is a
/// cross-project breaking change to the recorded input-normalization identity.
const HEURISTIC_V1_POLICY_HASH: &str =
    "sha256:1670ff1d5ff27575dc63ffd448cb140b3d497247bb7b36e1b4e2f1623aa0fa2c";

/// The golden source: a katakana-dominant sentence the character-level cascade
/// accepts with an empty first-pass token stream (no dictionary needed). Same
/// sentence used by `ab-ortho-detect`'s `accepts_katakana_prose_without_dictionary`.
const GOLDEN_SOURCE: &str = "吾輩ハ猫デアル果テ";

/// A first-pass tokenizer that yields no tokens, so `HeuristicV1` runs its
/// character-level cascade without loading a real analyzer dictionary. Mirrors
/// the stub in `ab-ortho-detect`'s heuristic tests.
struct StubTokenizer;
impl OrthoTokenizer for StubTokenizer {
    fn tokenize(&self, _text: &str) -> Vec<OrthoToken> {
        Vec::new()
    }
}

/// Run the detector-driven derivation exactly as the pipeline does:
/// `sentence_split` → `detect` → `ortho_normalize`. Returns the detector id,
/// the derived (normalized) text, and the offset map back to source coords.
fn derive_input(source: &str) -> (OrthoDetectorId, String, ab_ortho_detect::OffsetMap) {
    let detector = HeuristicV1::new(Arc::new(StubTokenizer), HeuristicConfig::default());
    let sentences = ab_plaintext::sentence_split(source);
    let annotations = detector.detect(&sentences);
    assert!(
        !annotations.is_empty(),
        "golden source must trigger normalization; got no annotations"
    );
    let (normalized_text, offset_map) = ortho_normalize(source, &annotations);
    (detector.detector_id(), normalized_text, offset_map)
}

/// Build a fixed morpheme segmentation over the DERIVED (normalized) text.
/// Spans are in normalized coordinates; `remap_spans` carries them to source
/// coordinates. Segmentation is hand-fixed (not analyzer-produced) so the
/// golden is deterministic and dictionary-free — the unit under test is the
/// remap, not the tokenizer.
fn normalized_morphemes() -> Vec<Morpheme> {
    let seg = [
        ("吾輩", 0usize, 6usize, 0usize, 2usize),
        ("は", 6, 9, 2, 3),
        ("猫", 9, 12, 3, 4),
        ("である", 12, 21, 4, 7),
        ("果て", 21, 27, 7, 9),
    ];
    seg.iter()
        .map(|&(surface, bs, be, cs, ce)| Morpheme {
            surface: surface.to_owned(),
            byte_span: bs..be,
            char_span: cs..ce,
            features: FeatureMap::new(),
        })
        .collect()
}

fn make_analysis(norm_text: &str, morphemes: Vec<Morpheme>) -> Analysis {
    Analysis {
        analyzer: "test".to_owned(),
        text_id: "golden".to_owned(),
        source_text: Arc::from(norm_text),
        morphemes,
        warnings: Vec::new(),
        ortho_annotations: None,
        ortho_offset_map: None,
    }
}

/// Serialize the derived bundle in the shape of the checked-in golden fixture.
fn derived_bundle(source: &str) -> serde_json::Value {
    let (detector_id, normalized_text, offset_map) = derive_input(source);
    let mut analysis = make_analysis(&normalized_text, normalized_morphemes());
    remap_spans(&mut analysis, &offset_map, source).expect("kata→hira remap must be clean");

    let policy = NormalizationPolicy::ortho_normalize_v1(
        detector_id.clone(),
        vec![OrthoNormalization::ScriptKatakanaToHiragana],
    );
    let remapped: Vec<serde_json::Value> = analysis
        .morphemes
        .iter()
        .map(|m| {
            serde_json::json!({
                "surface": m.surface,
                "byte_span": [m.byte_span.start, m.byte_span.end],
                "char_span": [m.char_span.start, m.char_span.end],
            })
        })
        .collect();
    let detector_label = match detector_id {
        OrthoDetectorId::HeuristicV1 => "HeuristicV1".to_owned(),
        OrthoDetectorId::MlLogisticRegression { model_hash } => {
            format!("MlLogisticRegression:{model_hash}")
        }
        OrthoDetectorId::HistoricalRewriteV1 {
            dictionary_hash,
            rules_hash,
        } => format!("HistoricalRewriteV1:{dictionary_hash}:{rules_hash}"),
    };
    serde_json::json!({
        "source_text": source,
        "detector_id": detector_label,
        "policy_hash": policy.policy_hash(),
        "normalized_text": normalized_text,
        "remapped_morphemes": remapped,
    })
}

#[test]
fn detector_driven_chain_matches_golden_fixture() {
    let expected: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/fixtures/ortho-reproducibility-golden.json"),
        )
        .expect("read golden fixture"),
    )
    .expect("parse golden fixture");

    assert_eq!(derived_bundle(GOLDEN_SOURCE), expected);
}

#[test]
fn recorded_policy_hash_identifies_the_transformation() {
    // The detector that produced the derived input is HeuristicV1, and the
    // policy built from its id hashes to the pinned constant — the same hash the
    // warehouse run records and the tokenizer profile declares.
    let (detector_id, _norm, _map) = derive_input(GOLDEN_SOURCE);
    assert_eq!(detector_id, OrthoDetectorId::HeuristicV1);
    let policy = NormalizationPolicy::ortho_normalize_v1(
        detector_id,
        vec![OrthoNormalization::ScriptKatakanaToHiragana],
    );
    assert_eq!(policy.policy_hash(), HEURISTIC_V1_POLICY_HASH);
    assert!(!policy.is_identity());
}

#[test]
fn rerunning_regenerates_identical_derived_input() {
    // Spec P5: a run + its recorded policy hash regenerate the identical derived
    // input from source. The policy is deterministic, so two independent runs of
    // the same detector over the same source must produce byte-identical output.
    let (id_a, norm_a, map_a) = derive_input(GOLDEN_SOURCE);
    let (id_b, norm_b, map_b) = derive_input(GOLDEN_SOURCE);

    assert_eq!(id_a, id_b);
    assert_eq!(norm_a, norm_b, "derived text must be reproducible");
    assert_eq!(map_a, map_b, "offset map must be reproducible");

    // And the remapped result is reproducible too.
    let mut analysis_a = make_analysis(&norm_a, normalized_morphemes());
    remap_spans(&mut analysis_a, &map_a, GOLDEN_SOURCE).unwrap();
    let mut analysis_b = make_analysis(&norm_b, normalized_morphemes());
    remap_spans(&mut analysis_b, &map_b, GOLDEN_SOURCE).unwrap();
    assert_eq!(analysis_a.morphemes, analysis_b.morphemes);
}

#[test]
fn remap_rebuilds_original_surfaces_under_length_preserving_normalization() {
    // kata→hira is byte-length-preserving, so byte spans are UNCHANGED by the
    // remap; the observable effect is that each surface is rebuilt from the
    // ORIGINAL katakana (は→ハ, である→デアル, 果て→果テ) while kanji stay put.
    let (_id, normalized_text, offset_map) = derive_input(GOLDEN_SOURCE);
    assert_eq!(normalized_text, "吾輩は猫である果て");

    let mut analysis = make_analysis(&normalized_text, normalized_morphemes());
    remap_spans(&mut analysis, &offset_map, GOLDEN_SOURCE).unwrap();

    let surfaces: Vec<&str> = analysis
        .morphemes
        .iter()
        .map(|m| m.surface.as_str())
        .collect();
    assert_eq!(surfaces, ["吾輩", "ハ", "猫", "デアル", "果テ"]);
    // Byte spans unchanged (length-preserving) but now denote original-doc bytes.
    assert_eq!(analysis.morphemes[1].byte_span, 6..9);
    assert_eq!(analysis.morphemes[3].byte_span, 12..21);
    // The reconstructed original substrings match the source verbatim.
    for m in &analysis.morphemes {
        assert_eq!(&GOLDEN_SOURCE[m.byte_span.clone()], m.surface);
    }
}
