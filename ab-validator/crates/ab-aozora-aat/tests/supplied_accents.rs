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
