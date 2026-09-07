//! Independent lexical boundaries distinguish outer glyphs from component markers.
use ab_source_syntax::{SourceMarkerKind, source_markers};

#[test]
fn nested_reference_owns_its_complete_source_extent() {
    let marker = "※［＃「姉」の正字、「女＋※［＃第3水準1-85-57］のつくり」、252-下-27］";
    let source = format!("前{marker}後");
    let markers = source_markers(&source);
    assert_eq!(markers.len(), 1);
    assert_eq!(markers[0].kind, SourceMarkerKind::GaijiFullwidth);
    assert_eq!(markers[0].raw, marker);
    assert_eq!(&source[markers[0].span.start..markers[0].span.end], marker);
    let children = source_markers(markers[0].body);
    assert_eq!(children.len(), 1);
    assert_eq!(children[0].raw, "※［＃第3水準1-85-57］");
}

#[test]
fn quoted_operands_do_not_turn_the_containing_note_into_a_glyph() {
    let source =
        "［＃「※［＃「女＋※［＃第3水準1-85-57］のつくり」、252-下-27］」は底本では「姉」］";
    let markers = source_markers(source);
    assert_eq!(markers.len(), 1);
    assert_eq!(markers[0].kind, SourceMarkerKind::CommandFullwidth);
    assert_eq!(markers[0].raw, source);
}

#[test]
fn an_unclosed_outer_glyph_is_not_certified_by_its_component_closer() {
    let markers = source_markers("※［＃「女＋※［＃第3水準1-85-57］のつくり」");
    assert_eq!(markers[0].kind, SourceMarkerKind::MalformedGaiji);
    let markers = source_markers("※［＃入力者注：初\n次］");
    assert_eq!(markers[0].kind, SourceMarkerKind::MalformedGaiji);
    let markers = source_markers("※［＃「女＋※［＃第3水準1-85-57］\nのつくり」］");
    assert_eq!(markers[0].kind, SourceMarkerKind::MalformedGaiji);
}
