//! Supplied figure identity and caption do not depend on numeric dimensions.
use serde_json::Value;

#[test]
fn unknown_dimensions_keep_independent_figure_facts() {
    for (dimensions, expected_aspects) in [
        ("横×縦", vec!["content", "structure"]),
        ("横640×縦480", vec!["content", "structure", "layout"]),
    ] {
        let marker = format!("［＃「王冠」のキャプション付きの図（crown.png、{dimensions}）入る］");
        let source = format!("題\n著者\n\n{marker}\n\n底本：本\n");
        let aat: Value =
            serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
        let figure_fact = facts
            .iter()
            .find(|fact| fact["kind"] == "illustration")
            .unwrap();
        assert_eq!(figure_fact["aspects"], serde_json::json!(expected_aspects));
        let span = &figure_fact["source_span"];
        let start = usize::try_from(span["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(span["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], marker);
    }
}
