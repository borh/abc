use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, MappingDocument, PreparedConverter, SchemaSet};
use serde_json::Value;

fn converter() -> PreparedConverter {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let research =
        std::env::var_os("AB_RESEARCH_ROOT").map_or_else(|| root.join("research"), Into::into);
    let mapping =
        MappingDocument::from_path(&root.join("data/aat-to-parser-ir-mapping-v2.json")).unwrap();
    let schemas = SchemaSet::load_for_aat_version(&root, &research, 2).unwrap();
    PreparedConverter::new(mapping, schemas).unwrap()
}

#[test]
fn literal_brackets_preserve_body_and_nested_notation() {
    let converter = converter();
    for (source, expected, unknown) in [
        ("前［普通］後", "前［普通］後", None),
        (
            "前［中［＃未知の注記］後］末",
            "前［中後］末",
            Some("［＃未知の注記］"),
        ),
        (
            "前［外［内［＃未知の注記］］］後",
            "前［外［内］］後",
            Some("［＃未知の注記］"),
        ),
        (
            "前「［中［＃未知の注記］後］」末",
            "前「［中後］」末",
            Some("［＃未知の注記］"),
        ),
        ("前［漢字《かんじ》］後", "前［漢字］後", None),
        (
            "Ich schenke Herrn Tanaka ein Pferd.［※［＃下側の右ダブル引用符、U+201E、165-22］思想］〔Die Lehrer gru:s&en die Lehrerinnen.〕",
            "Ich schenke Herrn Tanaka ein Pferd.［„思想］Die Lehrer grüßen die Lehrerinnen.",
            None,
        ),
    ] {
        let aat: Value =
            serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        assert_eq!(
            ab_plaintext::visible_text_projection(&aat),
            expected,
            "{source}"
        );
        let output = converter
            .convert(aat, ConversionOptions::default())
            .unwrap();
        let problems = output.parser_ir["interpretation_problems"]
            .as_array()
            .unwrap();
        if let Some(raw) = unknown {
            assert_eq!(problems.len(), 1, "{source}");
            assert_eq!(problems[0]["code"], "unknown-notation");
            assert_eq!(problems[0]["raw"], raw);
            let start =
                usize::try_from(problems[0]["source_span"]["start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(problems[0]["source_span"]["end"].as_u64().unwrap()).unwrap();
            assert_eq!(&source[start..end], raw);
        } else {
            assert!(problems.is_empty(), "{source}: {problems:?}");
        }
        assert!(
            output.parser_ir["nodes"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["type"] == "text")
        );
    }
}

#[test]
fn residual_source_gaps_have_explicit_interpretation_uncertainty() {
    let converter = converter();
    for (source, complete) in [("前〔cafe'〕［＃tail\r\n", false), ("前※後", true)] {
        let aat: Value =
            serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        assert_eq!(aat["meta"]["parse_complete"], complete);
        let output = converter
            .convert(aat, ConversionOptions::default())
            .unwrap();
        let problems = output.parser_ir["interpretation_problems"]
            .as_array()
            .unwrap();
        assert_eq!(problems.len(), 1, "{source}");
        assert_eq!(problems[0]["code"], "unparsed-source-gap");
        assert_eq!(problems[0]["kind"], "uninterpreted-notation");
        assert_eq!(problems[0]["raw"], source);
        assert_eq!(problems[0]["influence"]["kind"], "document");
    }
}
