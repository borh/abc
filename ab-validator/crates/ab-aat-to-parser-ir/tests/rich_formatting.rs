use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;
use std::path::Path;

fn convert(body: &str) -> Value {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

#[test]
fn retrospective_targets_preserve_located_kunten_instead_of_source_notation() {
    for (target, suffix, visible, marks) in [
        ("野［＃（ノ）］宮ごもり", "に傍線", "野宮ごもり", 1),
        (
            "神祝々之。此［＃（ヲ）］云［＃（フ）］［＃二］加武保佐枳保佐枳々［＃（ト）］［＃一］",
            "は縦中横",
            "神祝々之。此云加武保佐枳保佐枳々",
            5,
        ),
    ] {
        let body = format!("{target}［＃「{target}」{suffix}］");
        let ir = convert(&body);
        assert_eq!(ir["nodes"][0]["text"], visible);
        let facts: Vec<_> = ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|fact| fact["kind"] == "kunten")
            .collect();
        assert_eq!(facts.len(), marks);
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        for fact in facts {
            let span = &fact["source_span"];
            let raw = &source
                [span["start"].as_u64().unwrap() as usize..span["end"].as_u64().unwrap() as usize];
            assert!(raw.starts_with("［＃"));
            assert!(!raw.contains('「'));
        }
        assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
    }
}
