//! Target-associated diacritics survive conversion as source realizations.

use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

#[test]
fn supplied_diacritics_preserve_composition_and_source() {
    let source = "題\n作者\n\nVenus［＃「e」はアクサン（´）付き］と〔ru_pam［＃mは上ドット付き］〕\n\n底本：本\n";
    let aat: Value =
        serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let result = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap();
    let nodes = result.parser_ir["nodes"].as_array().unwrap();
    let accents: Vec<_> = nodes
        .iter()
        .filter(|node| node["type"] == "supplied-diacritic")
        .collect();
    assert_eq!(accents.len(), 2, "{}", result.parser_ir);
    assert_eq!(accents[0]["text"], "é");
    assert_eq!(accents[1]["text"], "rūpaṁ");
    for accent in accents {
        let start = usize::try_from(accent["source_span"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(accent["source_span"]["end"].as_u64().unwrap()).unwrap();
        assert_eq!(accent["source"], &source[start..end]);
        let annotation_start =
            usize::try_from(accent["annotation_span"]["start"].as_u64().unwrap()).unwrap();
        let annotation_end =
            usize::try_from(accent["annotation_span"]["end"].as_u64().unwrap()).unwrap();
        assert!(source[annotation_start..annotation_end].starts_with("［＃"));
        assert!(source[annotation_start..annotation_end].ends_with('］'));
    }
    assert!(
        result.parser_ir["interpretation_problems"]
            .as_array()
            .unwrap()
            .is_empty()
    );
}
