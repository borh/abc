//! Complete repetition notation retains source spelling beside its Unicode realization.
use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn nodes<'a>(value: &'a Value, kind_key: &str, kind: &str, found: &mut Vec<&'a Value>) {
    match value {
        Value::Object(map) => {
            if value[kind_key] == kind {
                found.push(value);
            }
            for child in map.values() {
                nodes(child, kind_key, kind, found);
            }
        }
        Value::Array(items) => {
            for child in items {
                nodes(child, kind_key, kind, found);
            }
        }
        _ => {}
    }
}

#[test]
fn native_marks_survive_conversion_with_exact_source_spans() {
    for body in [
        "フゴ／＼、とき／″＼",
        "「とき／″＼」",
        "時々《とき／″＼》",
        "｜とき／″＼《時々》",
        "｜漢［＃レ］とき／″＼《かん》",
        "とき／″＼［＃「とき／″＼」は太字］",
    ] {
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let aat: Value =
            serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        let mut marks = Vec::new();
        nodes(&aat["blocks"], "kind", "iteration-mark", &mut marks);
        assert!(!marks.is_empty(), "{body}: {aat}");
        for mark in &marks {
            let start = mark["span"]["byte_start"].as_u64().unwrap() as usize;
            let end = mark["span"]["byte_end"].as_u64().unwrap() as usize;
            assert_eq!(
                &source[start..end],
                mark["source"].as_str().unwrap(),
                "{body}"
            );
        }
        let count = marks.len();
        let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let result = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat,
            mapping: MappingDocument::from_path(
                &repo.join("data/aat-to-parser-ir-mapping-v2.json"),
            )
            .unwrap(),
            schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
            options: ConversionOptions::default(),
        })
        .unwrap();
        let mut converted = Vec::new();
        nodes(&result.parser_ir, "type", "iteration-mark", &mut converted);
        assert_eq!(converted.len(), count, "{body}");
        for mark in converted {
            let start = mark["source_span"]["start"].as_u64().unwrap() as usize;
            let end = mark["source_span"]["end"].as_u64().unwrap() as usize;
            assert_eq!(&source[start..end], mark["source"].as_str().unwrap());
        }
    }
}
