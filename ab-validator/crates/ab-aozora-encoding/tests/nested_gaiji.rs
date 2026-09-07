//! Nested component references retain their containing glyph's distinct identity.
use ab_aozora_encoding::gaiji::{find_span, gaiji_resolutions, recognize_gaiji_body};

#[test]
fn a_numbered_page_column_line_is_provenance_not_a_glyph_code() {
    use ab_aozora_encoding::gaiji::is_page_line_shaped;
    let body = "「※［＃第4水準2-13-74］ 」の「斤」に代えて「りっとう」、132-中段-24";
    let parsed = recognize_gaiji_body(body).expect("source-stated substitution and locator");
    assert_eq!(parsed.mencode, Some("132-中段-24"));
    for reference in ["38-上段-1", "132-中段-24", "38-下段-1"] {
        assert!(!is_page_line_shaped(reference));
    }
    for unrelated in [
        "中段",
        "132-中段",
        "中段-24",
        "本文-中段-24",
        "132-中段-未定",
    ] {
        assert!(!is_page_line_shaped(unrelated));
    }
    assert!(recognize_gaiji_body("底本ルビは「もら」と誤記、175-上段-4").is_none());
    let source = format!("※［＃{body}］");
    let entries = gaiji_resolutions(&source);
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].resolved, None);
    assert_eq!(entries[0].mencode.as_deref(), Some("132-中段-24"));
}

#[test]
fn nested_component_code_does_not_resolve_the_containing_glyph() {
    let body = "「姉」の正字、「女＋※［＃第3水準1-85-57］のつくり」、252-下-27";
    let parsed = recognize_gaiji_body(body).expect("source-described glyph");
    assert_eq!(parsed.mencode, Some("252-下-27"));
    assert_eq!(
        parsed.description,
        body.strip_suffix("、252-下-27").unwrap()
    );
    let marker = format!("※［＃{body}］");
    let source = format!("前{marker}後");
    let entries = gaiji_resolutions(&source);
    assert_eq!(entries.len(), 1);
    let entry = &entries[0];
    assert_eq!(&source[entry.start..entry.end], marker);
    assert_eq!(entry.description, parsed.description);
    assert_eq!(entry.resolved, None);
    assert_eq!(entry.codepoint, None);
    for (offset, _) in marker.char_indices() {
        assert_eq!(
            find_span(&source, "前".len() + offset),
            Some((entry.start, entry.end))
        );
    }
}

#[test]
fn incomplete_components_and_unowned_nested_annotations_stay_unrecognized() {
    for body in [
        "「姉」の正字、「女＋※［＃第3水準1-85-57のつくり」、252-下-27",
        "「姉」の正字、「女＋［＃第3水準1-85-57］のつくり」、252-下-27",
        "「本文」は※［＃第3水準1-85-57］ではない",
    ] {
        assert!(recognize_gaiji_body(body).is_none(), "{body}");
    }
    let unclosed = "※［＃「女＋※［＃第3水準1-85-57］のつくり」、252-下-27";
    assert!(gaiji_resolutions(unclosed).is_empty());
    assert!(find_span(unclosed, 0).is_none());
}

#[test]
fn a_quoted_closing_glyph_is_not_the_reference_boundary() {
    let marker = "※［＃「］」、1-1-47］";
    let entries = gaiji_resolutions(marker);
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].end, marker.len());
    assert_eq!(entries[0].description, "］");
}
