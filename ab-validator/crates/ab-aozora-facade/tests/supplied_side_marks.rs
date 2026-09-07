//! Source-positioned marks retain their literal spelling without a reading claim.
use ab_aozora_facade::Document;

#[test]
fn supplied_mark_round_trips_without_becoming_a_ruby_reading() {
    let source = "おろちへ［＃「ちへ」の右に「）」］";
    let document = Document::new(source);
    let parsed = document.parse();
    assert_eq!(parsed.to_source(), source);
    let html = parsed.to_html();
    assert!(html.contains("data-supplied-mark=\"）\" data-place=\"right\""));
    assert!(html.contains(">ちへ</span>"));
    assert!(!html.contains("<ruby>"));
}
