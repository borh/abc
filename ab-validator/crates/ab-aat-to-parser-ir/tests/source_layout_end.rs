//! Source break markers retain exact spans through final-line layout projection.
use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn objects(value: &Value) -> Vec<&Value> {
    let mut found = Vec::new();
    match value {
        Value::Object(map) => {
            found.push(value);
            for child in map.values() {
                found.extend(objects(child));
            }
        }
        Value::Array(items) => {
            for child in items {
                found.extend(objects(child));
            }
        }
        _ => {}
    }
    found
}

#[test]
fn real_page_break_does_not_create_a_spanless_terminal_duplicate() {
    let source = "題\n作者\n\n［＃改ページ］\n本文。［＃地から２字上げ］（日付）\n\n底本：本\n";
    let aat: Value =
        serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let all = objects(&aat);
    let breaks: Vec<_> = all
        .iter()
        .filter(|node| node["kind"] == "layout_break")
        .collect();
    assert_eq!(breaks.len(), 1);
    let start = usize::try_from(breaks[0]["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(breaks[0]["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], "［＃改ページ］");
    assert!(
        !all.iter()
            .any(|node| node["kind"] == "raw" && node["source"] == "［＃改ページ］")
    );
    let layout = all
        .iter()
        .find(|node| node["kind"] == "layout_block")
        .unwrap();
    assert_eq!(layout["span"]["byte_end"], source.find("\n\n底本").unwrap());
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let result = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap();
    assert_eq!(result.parser_ir["layout_blocks"][0]["align"], "right");
}

#[test]
fn page_placement_ends_at_the_real_page_boundary_not_a_column_break() {
    let source = "題\n作者\n\n［＃ここからページの左右中央］\n甲\n［＃改段］\n乙\n［＃改丁］\n後\n\n底本：本\n";
    let aat: Value =
        serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let all = objects(&aat);
    let layout = all
        .iter()
        .find(|node| node["kind"] == "layout_block")
        .unwrap();
    let end = &layout["source_end"]["span"];
    let start = usize::try_from(end["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(end["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], "［＃改丁］");
    let children = objects(&layout["children"]);
    assert!(
        children
            .iter()
            .any(|node| node["kind"] == "layout_break" && node["break_kind"] == "column")
    );
    assert!(
        !children
            .iter()
            .any(|node| node["kind"] == "layout_break" && node["break_kind"] == "kaicho")
    );
    assert_eq!(
        all.iter()
            .filter(|node| node["kind"] == "layout_break")
            .count(),
        2
    );
}
