//! Cross-crate JSON characterization for binary plain provenance recoveries.

use ab_aat::{aat_json_from_bytes, diagnostics_json_from_bytes};
use serde_json::{Value, json};

#[test]
fn accepted_plain_recoveries_pin_diagnostics_and_aat_json() {
    let cases = [
        "plain",
        "｜",
        "＃",
        "※",
        "「literal」",
        "〔literal〕",
        "［＃tail",
        "stray］",
        "※［＃［＃nested］］",
    ];
    let expected = [
        r#"{"aat":{"blocks":[{"content":[{"kind":"text","span":{"byte_end":5,"byte_start":0,"line_end":1,"line_start":1},"value":"plain"}],"kind":"paragraph","span":{"byte_end":5,"byte_start":0,"line_end":1,"line_start":1}}],"parse_complete":true,"warnings":[]},"diagnostics":{"data":[],"schemaVersion":3},"source":"plain"}"#,
        r#"{"aat":{"blocks":[{"content":[],"kind":"paragraph"}],"parse_complete":true,"warnings":[]},"diagnostics":{"data":[],"schemaVersion":3},"source":"｜"}"#,
        r#"{"aat":{"blocks":[{"content":[{"kind":"text","span":{"byte_end":3,"byte_start":0,"line_end":1,"line_start":1},"value":"＃"}],"kind":"paragraph","span":{"byte_end":3,"byte_start":0,"line_end":1,"line_start":1}}],"parse_complete":true,"warnings":[]},"diagnostics":{"data":[],"schemaVersion":3},"source":"＃"}"#,
        r#"{"aat":{"blocks":[{"content":[{"kind":"text","span":{"byte_end":3,"byte_start":0,"line_end":1,"line_start":1},"value":"※"}],"kind":"paragraph","span":{"byte_end":3,"byte_start":0,"line_end":1,"line_start":1}}],"parse_complete":true,"warnings":[]},"diagnostics":{"data":[],"schemaVersion":3},"source":"※"}"#,
        r#"{"aat":{"blocks":[{"content":[{"kind":"text","span":{"byte_end":13,"byte_start":0,"line_end":1,"line_start":1},"value":"「literal」"}],"kind":"paragraph","span":{"byte_end":13,"byte_start":0,"line_end":1,"line_start":1}}],"parse_complete":true,"warnings":[]},"diagnostics":{"data":[],"schemaVersion":3},"source":"「literal」"}"#,
        r#"{"aat":{"blocks":[{"content":[{"kind":"text","span":{"byte_end":13,"byte_start":0,"line_end":1,"line_start":1},"value":"〔literal〕"}],"kind":"paragraph","span":{"byte_end":13,"byte_start":0,"line_end":1,"line_start":1}}],"parse_complete":true,"warnings":[]},"diagnostics":{"data":[],"schemaVersion":3},"source":"〔literal〕"}"#,
        r#"{"aat":{"blocks":[{"content":[{"interpretation_problem":{"aspects":["content","structure","layout"],"code":"unparsed-source-gap","influence":{"kind":"document"},"kind":"uninterpreted-notation"},"kind":"raw","source":"［＃tail","span":{"byte_end":10,"byte_start":0,"line_end":1,"line_start":1},"x-provenance":"source-derived","x-source-marker-kind":"unparsed-source-gap"}],"kind":"paragraph","span":{"byte_end":10,"byte_start":0,"line_end":1,"line_start":1}}],"parse_complete":false,"warnings":[{"code":"unclosed-bracket","message":"unclosed_bracket","severity":"error","span":{"byte_end":3,"byte_start":0,"line_end":1,"line_start":1}}]},"diagnostics":{"data":[{"code":"unclosed-bracket","kind":"unclosed_bracket","severity":"error","source":"source","span":{"end":3,"start":0}}],"schemaVersion":3},"source":"［＃tail"}"#,
        r#"{"aat":{"blocks":[{"content":[{"kind":"text","span":{"byte_end":8,"byte_start":0,"line_end":1,"line_start":1},"value":"stray］"}],"kind":"paragraph","span":{"byte_end":8,"byte_start":0,"line_end":1,"line_start":1}}],"parse_complete":false,"warnings":[{"code":"unmatched-close","message":"unmatched_close","severity":"error","span":{"byte_end":8,"byte_start":5,"line_end":1,"line_start":1}}]},"diagnostics":{"data":[{"code":"unmatched-close","kind":"unmatched_close","severity":"error","source":"source","span":{"end":8,"start":5}}],"schemaVersion":3},"source":"stray］"}"#,
        r#"{"aat":{"blocks":[{"content":[{"interpretation_problem":{"aspects":["content","structure","layout"],"code":"unparsed-source-gap","influence":{"kind":"document"},"kind":"uninterpreted-notation"},"kind":"raw","source":"※","span":{"byte_end":3,"byte_start":0,"line_end":1,"line_start":1},"x-provenance":"source-derived","x-source-marker-kind":"unparsed-source-gap"},{"interpretation_problem":{"aspects":["content","structure","layout"],"code":"unknown-notation","influence":{"kind":"document"},"kind":"unknown-notation"},"kind":"raw","source":"［＃［＃nested］］","span":{"byte_end":27,"byte_start":3,"line_end":1,"line_start":1},"x-provenance":"parser-derived","x-source-marker-kind":"directive"}],"kind":"paragraph","span":{"byte_end":27,"byte_start":0,"line_end":1,"line_start":1}}],"parse_complete":true,"warnings":[]},"diagnostics":{"data":[],"schemaVersion":3},"source":"※［＃［＃nested］］"}"#,
    ];

    for (source, expected) in cases.into_iter().zip(expected) {
        let diagnostics: Value =
            serde_json::from_slice(&diagnostics_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let projection = json!({
            "source": source,
            "diagnostics": diagnostics,
            "aat": {
                "blocks": aat["blocks"],
                "parse_complete": aat["meta"]["parse_complete"],
                "warnings": aat["meta"]["warnings"],
            }
        });
        assert_eq!(serde_json::to_string(&projection).unwrap(), expected);
    }
}
