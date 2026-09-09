//! A mark the source places at a junction publishes a position, not a range.
use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json")).unwrap();
    let schemas = SchemaSet::for_aat_version(&repo, None, 2).unwrap();
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap(),
        mapping,
        schemas,
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

fn marks(ir: &Value) -> Vec<&Value> {
    ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|node| node["type"] == "annotated-text" && node["note_kind"] == "supplied-mark")
        .collect()
}

#[test]
fn a_junction_mark_states_a_side_and_an_anchor_separately() {
    let ir = convert("自然［＃「自」と「然」の間に白三角傍点］");
    let found = marks(&ir);
    let [mark] = found.as_slice() else {
        panic!("{ir}");
    };
    // The source states both, and states them independently: every such mark
    // sits right, and `の間に` says where along the line rather than which
    // side of it. Neither may be read off the other.
    assert_eq!(mark["position"], "right", "{ir}");
    assert_eq!(mark["anchor"], "between", "{ir}");
    // The pair carries the mark; neither character is adopted as its target.
    assert_eq!(mark["text"], "自然", "{ir}");
    assert_eq!(mark["annotation_children"][0]["text"], "白三角傍点", "{ir}");
    assert!(
        ir["interpretation_problems"].as_array().unwrap().is_empty(),
        "{ir}"
    );
}

#[test]
fn the_claim_covers_the_marker_and_names_the_mark_it_supplies() {
    let ir = convert("自然［＃「自」と「然」の間に白三角傍点］");
    let facts = ir["interpretation_facts"].as_array().unwrap();
    assert_eq!(facts.len(), 1, "{ir}");
    assert_eq!(facts[0]["kind"], "supplied-mark", "{ir}");
    assert_eq!(facts[0]["outcome"], "established", "{ir}");
}

#[test]
fn each_of_a_run_of_junction_marks_claims_only_the_pair_it_names() {
    // The corpus line that motivates this reads three in a row.
    let ir = convert(
        "自然［＃「自」と「然」の間に白三角傍点］はう［＃「は」と「う」の間に白三角傍点］たふ［＃「た」と「ふ」の間に白三角傍点］",
    );
    assert_eq!(
        marks(&ir)
            .iter()
            .map(|mark| mark["text"].as_str().unwrap())
            .collect::<Vec<_>>(),
        ["自然", "はう", "たふ"],
        "{ir}"
    );
    assert!(
        ir["interpretation_problems"].as_array().unwrap().is_empty(),
        "{ir}"
    );
}

#[test]
fn runs_that_do_not_meet_leave_the_marker_unread() {
    for body in [
        // Something stands between them, so there is no one place to put it.
        "自X然［＃「自」と「然」の間に白三角傍点］",
        // The pair is not the run the marker follows.
        "自然他［＃「自」と「然」の間に白三角傍点］",
        // Named in an order the text does not have.
        "然自［＃「自」と「然」の間に白三角傍点］",
        // Three runs have two junctions and the marker names neither.
        "自然は［＃「自」と「然」と「は」の間に白三角傍点］",
        // Not a mark this vocabulary knows.
        "自然［＃「自」と「然」の間に何か］",
    ] {
        let ir = convert(body);
        assert!(marks(&ir).is_empty(), "{body}: {ir}");
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}: {ir}"
        );
    }
}
