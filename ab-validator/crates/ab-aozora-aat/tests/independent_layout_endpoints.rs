//! Co-applied presentation can end before its enclosing indentation.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

fn layout_blocks<'a>(value: &'a Value, blocks: &mut Vec<&'a Value>) {
    match value {
        Value::Object(fields) => {
            if value["kind"] == "layout_block" {
                blocks.push(value);
            }
            for child in fields.values() {
                layout_blocks(child, blocks);
            }
        }
        Value::Array(children) => {
            for child in children {
                layout_blocks(child, blocks);
            }
        }
        _ => {}
    }
}

#[test]
fn frame_ends_without_ending_its_enclosing_indentation() {
    let source = "［＃ここから１字下げ、罫囲みで］\n枠の中\n［＃ここで罫囲み終わり］\n字下げだけ\n［＃ここで字下げ終わり］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut blocks = Vec::new();
    layout_blocks(&aat, &mut blocks);
    let indent = blocks
        .iter()
        .find(|block| block["indent"] == 1)
        .unwrap_or_else(|| panic!("{aat}"));
    let frame = blocks
        .iter()
        .find(|block| block["formatting"]["kind"] == "keigakomi")
        .unwrap_or_else(|| panic!("{aat}"));
    assert!(indent.to_string().contains("字下げだけ"));
    assert!(!frame.to_string().contains("字下げだけ"));
    assert_eq!(frame["formatting"]["border"], "rule");
    assert!(!aat.to_string().contains("interpretation_problem"));
}

#[test]
fn completed_style_does_not_certify_an_unclosed_indentation() {
    let opener = "［＃ここから２字下げ、横組み右揃えで］";
    let closer = "［＃ここで横組み終わり］";
    let source = format!("{opener}\n横向き\n{closer}\n残り");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    assert!(aat.to_string().contains("interpretation_problem"));
    let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
    assert!(!facts.iter().any(|fact| fact["source_span"]["start"] == 0));
    assert!(
        facts
            .iter()
            .any(|fact| fact["source_span"]["start"] == source.find(closer).unwrap())
    );
    assert!(aat.to_string().contains("残り"));
}

#[test]
fn replacement_precedes_every_logical_scope_of_the_next_opener() {
    let next = "［＃ここから１字下げ、罫囲みで］";
    let source = format!(
        "［＃ここから２字下げ、横組み］\n最初\n［＃ここで横組み終わり］\n続き\n{next}\n次の枠\n［＃ここで罫囲み終わり］\n次の字下げ\n［＃ここで字下げ終わり］"
    );
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let first = &aat["blocks"][0];
    assert_eq!(first["indent"], 2, "{aat}");
    assert!(!first.to_string().contains("次の枠"));
    assert!(!first.to_string().contains("次の字下げ"));
    assert!(first["span"]["byte_end"].as_u64().unwrap() <= source.find(next).unwrap() as u64);
    assert_eq!(
        first["source_end"]["span"]["byte_start"],
        source.find(next).unwrap()
    );
    assert!(!aat.to_string().contains("interpretation_problem"));
}

#[test]
fn horizontal_closer_preserves_the_remaining_indentation() {
    let source = "［＃ここから２字下げ、横組み右揃えで］\n横向き\n［＃ここで横組み終わり］\n字下げだけ\n［＃ここで字下げ終わり］\n外側";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut blocks = Vec::new();
    layout_blocks(&aat, &mut blocks);
    let indent = blocks
        .iter()
        .find(|block| block["indent"] == 2)
        .unwrap_or_else(|| panic!("{aat}"));
    let horizontal = blocks
        .iter()
        .find(|block| block["direction"] == "horizontal")
        .unwrap();
    assert_eq!(horizontal["align"], "right");
    assert!(indent.get("align").is_none());
    assert!(indent.to_string().contains("字下げだけ"));
    assert!(!indent.to_string().contains("外側"));
    assert!(!horizontal.to_string().contains("字下げだけ"));
    let close = "［＃ここで横組み終わり］";
    assert_eq!(
        horizontal["span"]["byte_end"],
        source.find(close).unwrap() + close.len()
    );
    assert!(!aat.to_string().contains("interpretation_problem"));
    let document = ab_aozora_facade::Document::new(source);
    let tree = document.parse();
    let canonical = tree.to_source();
    assert_eq!(canonical.matches("ここから").count(), 1);
    assert_eq!(canonical.matches("横組み右揃えで").count(), 1);
    let reparsed = ab_aozora_facade::Document::new(canonical.as_str());
    assert_eq!(tree.to_html(), reparsed.parse().to_html());
    assert_eq!(canonical, reparsed.parse().to_source());
    let html = tree.to_html();
    let first_close = html.find("</div>").unwrap();
    let remaining = html.find("字下げだけ").unwrap();
    let outside = html.find("外側").unwrap();
    assert!(first_close < remaining);
    assert_eq!(html[..remaining].matches("</div>").count(), 1);
    assert_eq!(html[..outside].matches("</div>").count(), 2);
}
