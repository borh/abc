//! An emphasis selector that names what to leave out marks everything else.
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

/// The body nodes in source order, as `(type, text)`.
///
/// The order is the assertion: an exclusion selector is only read correctly if
/// the marked ranges and the ranges it left out alternate exactly as the
/// source has them. The colophon the fixture appends is a separate paragraph
/// and is left out here.
fn sequence(ir: &Value) -> Vec<(String, String)> {
    let body = ir["paragraphs"]
        .as_array()
        .unwrap()
        .iter()
        .find(|paragraph| paragraph["role"] == "body")
        .unwrap();
    let start = body["node_range"]["start"].as_u64().unwrap() as usize;
    let end = body["node_range"]["end"].as_u64().unwrap() as usize;
    ir["nodes"].as_array().unwrap()[start..end]
        .iter()
        .map(|node| {
            (
                node["type"].as_str().unwrap_or_default().to_owned(),
                node["text"].as_str().unwrap_or_default().to_owned(),
            )
        })
        .filter(|(_, text)| !text.trim().is_empty())
        .collect()
}

fn problems(ir: &Value) -> Vec<&str> {
    ir["interpretation_problems"]
        .as_array()
        .unwrap()
        .iter()
        .map(|problem| problem["code"].as_str().unwrap())
        .collect()
}

#[test]
fn an_exclusion_selector_marks_every_range_the_cut_out_leaves() {
    // The source names the separator once and means all three of them.
    let ir =
        convert("自律・自由・人格・性格［＃「自律・自由・人格・性格」の「・」を除く部分に傍点］");
    assert_eq!(
        sequence(&ir),
        [
            ("emphasis", "自律"),
            ("text", "・"),
            ("emphasis", "自由"),
            ("text", "・"),
            ("emphasis", "人格"),
            ("text", "・"),
            ("emphasis", "性格"),
        ]
        .map(|(kind, text)| (kind.to_owned(), text.to_owned())),
        "{ir}"
    );
    assert!(problems(&ir).is_empty(), "{ir}");
}

#[test]
fn the_marked_ranges_carry_the_mark_the_source_named() {
    let ir = convert("行・実践［＃「行・実践」の「・」を除く部分に傍点］");
    let marked: Vec<_> = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|node| node["type"] == "emphasis")
        .collect();
    assert_eq!(marked.len(), 2, "{ir}");
    for node in marked {
        assert_eq!(node["style"], "bouten", "{ir}");
        assert_eq!(node["decoration"]["kind"], "傍点", "{ir}");
        assert_eq!(node["decoration"]["position"], "right", "{ir}");
    }
}

#[test]
fn the_selector_reads_whatever_run_the_source_cuts_out() {
    // Nothing about the cut-out is punctuation-specific; the source says which
    // characters it means and that is the whole of it.
    let ir = convert("自分＝モラル＝文学［＃「自分＝モラル＝文学」の「＝」を除く部分に傍点］");
    assert_eq!(
        sequence(&ir),
        [
            ("emphasis", "自分"),
            ("text", "＝"),
            ("emphasis", "モラル"),
            ("text", "＝"),
            ("emphasis", "文学"),
        ]
        .map(|(kind, text)| (kind.to_owned(), text.to_owned())),
        "{ir}"
    );
    assert!(problems(&ir).is_empty(), "{ir}");
}

#[test]
fn the_marker_is_claimed_once_across_every_range_it_selects() {
    let ir = convert("行・実践［＃「行・実践」の「・」を除く部分に傍点］");
    let facts = ir["interpretation_facts"].as_array().unwrap();
    assert_eq!(facts.len(), 1, "{ir}");
    assert_eq!(facts[0]["kind"], "emphasis", "{ir}");
    assert_eq!(facts[0]["outcome"], "established", "{ir}");
    // The claim covers the marker, not the ranges the marker points at.
    let source = format!(
        "題\n作者\n\n{}",
        "行・実践［＃「行・実践」の「・」を除く部分に傍点］"
    );
    let span = &facts[0]["source_span"];
    assert_eq!(span["start"], source.find('［').unwrap(), "{ir}");
    assert_eq!(span["end"], source.len(), "{ir}");
}

#[test]
fn a_cut_out_the_context_does_not_contain_leaves_the_marker_unread() {
    // Marking the whole context would be a guess about what the source meant
    // by a run that is not there.
    let ir = convert("行・実践［＃「行・実践」の「＝」を除く部分に傍点］");
    assert_eq!(problems(&ir), ["unknown-notation"], "{ir}");
    assert_eq!(
        sequence(&ir),
        [("text".to_owned(), "行・実践".to_owned())],
        "{ir}"
    );
}

#[test]
fn the_context_must_be_the_run_the_marker_follows() {
    let ir = convert("行・実践、他［＃「行・実践」の「・」を除く部分に傍点］");
    assert_eq!(problems(&ir), ["unknown-notation"], "{ir}");
}

#[test]
fn a_cut_out_at_an_edge_contributes_no_range() {
    let ir = convert("・行・［＃「・行・」の「・」を除く部分に傍点］");
    assert_eq!(
        sequence(&ir),
        [("text", "・"), ("emphasis", "行"), ("text", "・")]
            .map(|(kind, text)| (kind.to_owned(), text.to_owned())),
        "{ir}"
    );
    assert!(problems(&ir).is_empty(), "{ir}");
}

#[test]
fn cutting_out_the_whole_context_leaves_nothing_to_mark() {
    let ir = convert("行・実践［＃「行・実践」の「行・実践」を除く部分に傍点］");
    assert_eq!(problems(&ir), ["unknown-notation"], "{ir}");
}

#[test]
fn naming_a_range_still_marks_that_range_rather_than_its_complement() {
    // The connector is the same in both readings, so the suffix is the only
    // thing that tells them apart.
    let ir = convert("行・実践［＃「行・実践」の「実践」に傍点］");
    assert_eq!(
        sequence(&ir),
        [("text", "行・"), ("emphasis", "実践")]
            .map(|(kind, text)| (kind.to_owned(), text.to_owned())),
        "{ir}"
    );
    assert!(problems(&ir).is_empty(), "{ir}");
}
