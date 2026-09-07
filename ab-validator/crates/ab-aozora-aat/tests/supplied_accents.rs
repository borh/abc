//! Supplied diacritics retain their source notation beside the realized text.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

fn accents<'a>(node: &'a Value, found: &mut Vec<&'a Value>) {
    match node {
        Value::Object(fields) => {
            if node["kind"] == "supplied-diacritic" {
                found.push(node);
            }
            for value in fields.values() {
                accents(value, found);
            }
        }
        Value::Array(values) => {
            for value in values {
                accents(value, found);
            }
        }
        _ => {}
    }
}

#[test]
fn unresolved_selectors_do_not_create_realization_claims() {
    for body in [
        "［＃「e」はアクサン（´）付き］",
        "Sam［＃２つめのmは上ドット付き］",
        "〔ba_ndhava_h［＃hは下ドット付き］〕",
    ] {
        let source = format!("題\n作者\n\n{body}\n");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let mut found = Vec::new();
        accents(&aat["blocks"], &mut found);
        assert!(found.is_empty(), "{aat}");
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}

#[test]
fn native_composition_survives_projection_once_with_original_spans() {
    for (body, expected) in [
        ("〔ru_pam［＃mは上ドット付き］〕", "rūpaṁ"),
        ("Bzan［＃nは上ドット付き］", "Bzaṅ"),
        ("Sas［＃sは下ドット付き］", "Saṣ"),
        (
            "〔rattha-sva_mi_〕［＃tはともに下ドット付き］",
            "raṭṭha-svāmī",
        ),
        (
            "Konkana［＃前のnは上ドット付き、後のnは下ドット付き］",
            "Koṅkaṇa",
        ),
        ("Venus［＃「e」はアクサン（´）付き］", "é"),
    ] {
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let mut found = Vec::new();
        accents(&aat["blocks"], &mut found);
        assert_eq!(found.len(), 1, "{body}: {aat}");
        let accent = found[0];
        assert_eq!(accent["text"], expected, "{body}");
        let start = usize::try_from(accent["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(accent["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(accent["source"], &source[start..end]);
        assert!(
            accent["interpretation_marker_spans"]
                .as_array()
                .is_some_and(|spans| spans.len() == 1)
        );
        assert!(
            !aat.to_string().contains("uninterpreted-formatting"),
            "{aat}"
        );
    }
}

#[test]
fn supplied_dot_after_ruby_realizes_only_its_source_owned_base() {
    let body = "Samgha《サングハ》［＃mは上ドット付き］";
    let source = format!("題\n作者\n\n{body}\n");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut found = Vec::new();
    accents(&aat["blocks"], &mut found);
    assert_eq!(found.len(), 1, "{aat}");
    assert_eq!(found[0]["text"], "Saṁgha");
    assert_eq!(found[0]["source"], "Samgha");
    assert!(aat.to_string().contains("サングハ"));
}

#[test]
fn ruby_dot_target_does_not_cross_boundaries_or_resolve_ambiguous_letters() {
    for body in [
        "Samgha《サングハ》 ［＃mは上ドット付き］",
        "Samgha《サングハ》\n［＃mは上ドット付き］",
        "mama《ママ》［＃mは上ドット付き］",
        "Sanga《m》［＃mは上ドット付き］",
    ] {
        let source = format!("題\n作者\n\n{body}\n");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let mut found = Vec::new();
        accents(&aat["blocks"], &mut found);
        assert!(found.is_empty(), "{body}: {aat}");
        assert!(aat.to_string().contains("interpretation_problem"));
    }
}

#[test]
fn ruby_dot_spans_bind_the_original_decoded_target_and_annotation() {
    let source = "題\r\n作者\r\n\r\n〔cafe'〕 Samgha《サングハ》［＃mは上ドット付き］ 後\r\n";
    let (encoded, _, errors) = encoding_rs::SHIFT_JIS.encode(source);
    assert!(!errors);
    for bytes in [source.as_bytes(), encoded.as_ref()] {
        let aat: Value = serde_json::from_slice(&aat_json_from_bytes(bytes).unwrap()).unwrap();
        let mut found = Vec::new();
        accents(&aat["blocks"], &mut found);
        assert_eq!(found.len(), 1);
        let target = &found[0]["span"];
        let marker = &found[0]["interpretation_marker_spans"][0];
        for (span, expected) in [(target, "Samgha"), (marker, "［＃mは上ドット付き］")] {
            let start = usize::try_from(span["byte_start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(span["byte_end"].as_u64().unwrap()).unwrap();
            assert_eq!(&source[start..end], expected);
        }
    }
}

#[test]
fn supplied_dot_uses_the_realized_accent_scope_even_with_literal_punctuation() {
    let source = "題\n作者\n\n〔samgha_disesa.v〕［＃mは上ドット付き］\n";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut found = Vec::new();
    accents(&aat["blocks"], &mut found);
    assert_eq!(found.len(), 1, "{aat}");
    assert_eq!(found[0]["text"], "saṁghādisesa.v");
}

#[test]
fn accent_scope_ownership_does_not_widen_unscoped_or_ambiguous_targets() {
    for body in [
        "samghādisesa.v［＃mは上ドット付き］",
        "〔samgha_disesa.v〕 ［＃mは上ドット付き］",
        "〔samgha_disesa.v〕\n［＃mは上ドット付き］",
        "〔ma_.m〕［＃mは上ドット付き］",
        "〔ma_［＃未対応］.m〕［＃mは上ドット付き］",
    ] {
        let source = format!("題\n作者\n\n{body}\n");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let mut found = Vec::new();
        accents(&aat["blocks"], &mut found);
        assert!(found.is_empty(), "{body}: {aat}");
        assert!(aat.to_string().contains("unknown-notation"), "{aat}");
    }
}

#[test]
fn realized_accent_scope_survives_quote_rebasing() {
    for body in [
        "≪〔samgha_disesa.v〕［＃mは上ドット付き］≫",
        "〔sa_.m〕［＃mは上ドット付き］",
    ] {
        let source = format!("題\r\n作者\r\n\r\n前〔cafe'〕\r\n{body}\r\n");
        let (encoded, _, errors) = encoding_rs::SHIFT_JIS.encode(&source);
        assert!(!errors);
        for bytes in [source.as_bytes(), encoded.as_ref()] {
            let aat: Value = serde_json::from_slice(&aat_json_from_bytes(bytes).unwrap()).unwrap();
            let mut found = Vec::new();
            accents(&aat["blocks"], &mut found);
            assert!(!found.is_empty(), "{body}: {aat}");
            for accent in found {
                let start =
                    usize::try_from(accent["span"]["byte_start"].as_u64().unwrap()).unwrap();
                let end = usize::try_from(accent["span"]["byte_end"].as_u64().unwrap()).unwrap();
                assert_eq!(accent["source"], &source[start..end]);
                assert!(accent["text"].as_str().unwrap().contains('ṁ'));
            }
        }
    }
}
