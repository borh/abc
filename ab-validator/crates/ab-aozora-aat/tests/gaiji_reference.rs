//! Whole-glyph identifiers and component descriptions across the source-to-AAT boundary.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn explicit_gaiji_identifiers_establish_content_without_rewriting_references() {
    for (marker, reference, expected) in [
        (
            "※［＃「えんにょう＋囘」、第４水準2-12-11］",
            "第４水準2-12-11",
            "𢌞",
        ),
        (
            "※［＃「つつみがまえ＋夕」、読みは「そう」、第3水準1-14-76、30-8］",
            "読みは「そう」、第3水準1-14-76、30-8",
            "匇",
        ),
        (
            "※［＃「滷－さんずい」、第3水準1-83-35］",
            "第3水準1-83-35",
            "鹵",
        ),
    ] {
        let source = format!("題\n作者\n\n前{marker}後。\n\n底本：本\n");
        let document: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let gaiji = &document["blocks"][0]["content"][1];
        assert_eq!(gaiji["kind"], "gaiji");
        assert_eq!(gaiji["resolved"], expected);
        assert_eq!(gaiji["jis_code"], reference);
        let start = usize::try_from(gaiji["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(gaiji["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], marker);
        let fact = &document["meta"]["interpretation_facts"][0];
        assert_eq!(fact["kind"], "gaiji");
        assert_eq!(fact["outcome"], "established");
        assert_eq!(fact["source_span"]["start"], start);
        assert_eq!(fact["source_span"]["end"], end);
    }
}

#[test]
fn a_component_identifier_does_not_establish_the_containing_glyph() {
    let marker = "※［＃非0213外字：「厂＋菫」、ただし「菫」は第3水準1-92-16のつくりの形、読みは「わづ」、289-上-12］";
    let source = format!("題\n作者\n\n前{marker}後。\n\n底本：本\n");
    let document: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let gaiji = &document["blocks"][0]["content"][1];
    assert_eq!(gaiji["kind"], "gaiji");
    assert!(gaiji["resolved"].is_null());
    assert_eq!(gaiji["jis_code"], "289-上-12");
    assert!(document["meta"]["interpretation_facts"].is_null());
}

#[test]
fn ruby_association_requires_a_resolved_base_and_supplied_reading() {
    let marker = "※［＃「需＋頁」、第3水準1-94-6］";
    for (body, expected) in [
        (format!("{marker}《じゅ》"), 1),
        (format!("｜前{marker}《ぜんじゅ》"), 1),
        (marker.to_owned(), 0),
        (format!("{marker}字《じ》"), 0),
        (format!("{marker}《》"), 0),
        ("※［＃未知の字］《じゅ》".to_owned(), 0),
    ] {
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let document: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let claims: Vec<_> = document["meta"]["interpretation_facts"]
            .as_array()
            .into_iter()
            .flatten()
            .filter(|fact| fact["kind"] == "gaiji-ruby")
            .collect();
        assert_eq!(claims.len(), expected, "{body}");
        for claim in claims {
            let start = usize::try_from(claim["source_span"]["start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(claim["source_span"]["end"].as_u64().unwrap()).unwrap();
            assert!(&source[start..end] == marker || source[start..end] == body);
        }
    }
}
