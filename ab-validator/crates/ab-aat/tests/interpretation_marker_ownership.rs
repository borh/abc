//! Positive interpretation claims name owned markers, not enclosing text ranges.
use ab_aat::aat_json_from_bytes;
use serde_json::Value;

fn claims(source: &str) -> (Value, Vec<&str>) {
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let markers = aat["meta"]["interpretation_facts"]
        .as_array()
        .unwrap()
        .iter()
        .map(|fact| {
            let span = &fact["source_span"];
            &source[usize::try_from(span["start"].as_u64().unwrap()).unwrap()
                ..usize::try_from(span["end"].as_u64().unwrap()).unwrap()]
        })
        .collect();
    (aat, markers)
}

#[test]
fn outer_formatting_never_claims_nested_unsupported_markers() {
    for marker in [
        "［＃「字」に白四角傍点］",
        "［＃「字」の部分はイタリック体］",
    ] {
        let source = format!("題\n作者\n\n［＃斜体］字{marker}［＃斜体終わり］\n\n底本：本\n");
        let (aat, markers) = claims(&source);
        assert_eq!(markers, ["［＃斜体］", "［＃斜体終わり］"], "{aat}");
        assert!(aat.to_string().contains("unknown-notation"));
    }
}

#[test]
fn nested_interpreted_styles_keep_their_own_marker_claims() {
    let source = "題\n作者\n\n［＃太字］字［＃「字」は斜体］［＃太字終わり］\n\n底本：本\n";
    let (_, markers) = claims(source);
    assert_eq!(
        markers,
        ["［＃太字］", "［＃「字」は斜体］", "［＃太字終わり］"]
    );
}

#[test]
fn ruby_gaiji_and_warichu_claim_only_their_lexical_occurrences() {
    let source = "題\n作者\n\n漢字《かんじ》｜漢字《かんじ》［＃割り注］※［＃歌記号、1-3-28］［＃割り注終わり］\n\n底本：本\n";
    let (_, markers) = claims(source);
    assert_eq!(
        markers,
        [
            "《かんじ》",
            "｜漢字《かんじ》",
            "［＃割り注］",
            "※［＃歌記号、1-3-28］",
            "［＃割り注終わり］"
        ]
    );
}
