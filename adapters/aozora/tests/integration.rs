use std::{
    path::PathBuf,
    process::{Command, Stdio},
};

fn adapter_bin() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target/debug/aozora-adapter");
    if !path.exists() {
        path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("target/release/aozora-adapter");
    }
    path
}

fn run_mode(source: &str, mode: &str) -> Vec<u8> {
    let mut child = Command::new(adapter_bin())
        .arg("--mode")
        .arg(mode)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("spawn aozora-adapter");
    {
        use std::io::Write;
        child
            .stdin
            .as_mut()
            .expect("stdin")
            .write_all(source.as_bytes())
            .expect("write source");
    }
    let output = child.wait_with_output().expect("adapter output");
    assert!(
        output.status.success(),
        "adapter failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}

fn run_aat(source: &str) -> serde_json::Value {
    serde_json::from_slice(&run_mode(source, "aat")).expect("AAT JSON")
}

fn paragraph_content(aat: &serde_json::Value) -> Vec<&serde_json::Value> {
    aat["blocks"][0]["content"]
        .as_array()
        .expect("paragraph content")
        .iter()
        .collect()
}

#[test]
fn version_mentions_upstream_aozora() {
    let output = Command::new(adapter_bin())
        .arg("--version")
        .output()
        .expect("version output");
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.starts_with("aozora-adapter 0.1.0 aozora "));
}

#[test]
fn emits_schema_valid_aat_for_core_constructs() {
    let source = [
        "｜青梅《おうめ》",
        "※［＃「口＋世」、U+546D］",
        "［＃返り点一］",
        "［＃ここから2字下げ］",
        "字下げ本文",
        "［＃ここで字下げ終わり］",
        "［＃改ページ］",
    ]
    .join("\n");
    let aat = run_aat(&source);

    assert_eq!(aat["version"], 1);
    assert_eq!(aat["meta"]["adapter"], "aozora");
    assert!(
        aat["meta"]["adapter_version"]
            .as_str()
            .unwrap()
            .contains("aozora ")
    );
    assert_eq!(aat["meta"]["parse_complete"], true);

    let text = serde_json::to_string(&aat).unwrap();
    assert!(text.contains(r#""kind":"ruby""#));
    assert!(text.contains(r#""kind":"gaiji""#));
    assert!(text.contains(r#""x-source-marker-kind":"kaeriten""#));
    assert!(text.contains(r#""kind":"jisage_block""#));
    assert!(text.contains(r#""x-break-kind":"page""#));

    let schema_text = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../data/aat-schema.json"),
    )
    .expect("schema");
    let schema: serde_json::Value = serde_json::from_str(&schema_text).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    validator.validate(&aat).expect("schema-valid AAT");
}

#[test]
fn preserves_visible_text_around_gaiji_in_source_order() {
    let aat = run_aat("耳朶を※［＃「口＋世」、U+546D］えて");
    let content = paragraph_content(&aat);
    let kinds = content
        .iter()
        .map(|node| node["kind"].as_str().unwrap())
        .collect::<Vec<_>>();

    assert_eq!(kinds, ["text", "gaiji", "text"]);
    assert_eq!(content[0]["value"], "耳朶を");
    assert_eq!(content[2]["value"], "えて");
}

#[test]
fn preserves_visible_text_after_ruby_in_source_order() {
    let aat = run_aat("｜青梅《おうめ》後");
    let content = paragraph_content(&aat);
    let kinds = content
        .iter()
        .map(|node| node["kind"].as_str().unwrap())
        .collect::<Vec<_>>();

    assert_eq!(kinds, ["ruby", "text"]);
    assert_eq!(content[0]["base"], "青梅");
    assert_eq!(content[0]["reading"], "おうめ");
    assert_eq!(content[1]["value"], "後");
}

#[test]
fn preserves_visible_text_around_ruby_in_source_order() {
    let aat = run_aat("前｜青梅《おうめ》後");
    let content = paragraph_content(&aat);
    let kinds = content
        .iter()
        .map(|node| node["kind"].as_str().unwrap())
        .collect::<Vec<_>>();

    assert_eq!(kinds, ["text", "ruby", "text"]);
    assert_eq!(content[0]["value"], "前");
    assert_eq!(content[1]["base"], "青梅");
    assert_eq!(content[1]["reading"], "おうめ");
    assert_eq!(content[2]["value"], "後");
}

#[test]
fn slices_constructs_in_upstream_sanitized_coordinate_space() {
    let source = "Title\r\n-------------------------------------------------------\r\n本文｜青梅《おうめ》後\r\n";
    let aat = run_aat(source);
    let content = paragraph_content(&aat);
    let kinds = content
        .iter()
        .map(|node| node["kind"].as_str().unwrap())
        .collect::<Vec<_>>();

    assert_eq!(kinds, ["text", "ruby", "text"]);
    assert_eq!(
        content[0]["value"],
        "Title\n\n-------------------------------------------------------\n本文"
    );
    assert_eq!(content[1]["base"], "青梅");
    assert_eq!(content[1]["reading"], "おうめ");
    assert_eq!(content[2]["value"], "後\n");
}

#[test]
fn normalizes_upstream_style_and_tcy_nodes_to_typed_aat() {
    let aat =
        run_aat("あた［＃「あた」に傍点］人物［＃「人物」は太字］昭和10［＃「10」は縦中横］年");
    let content = paragraph_content(&aat);
    let kinds = content
        .iter()
        .map(|node| node["kind"].as_str().unwrap())
        .collect::<Vec<_>>();

    assert_eq!(kinds, ["style", "style", "text", "tcy", "text"]);
    assert_eq!(content[0]["style_type"], "bouten");
    assert_eq!(content[0]["content"][0]["value"], "あた");
    assert_eq!(content[1]["style_type"], "bold");
    assert_eq!(content[1]["content"][0]["value"], "人物");
    assert_eq!(content[2]["value"], "昭和");
    assert_eq!(content[3]["content"][0]["value"], "10");
    assert_eq!(content[4]["value"], "年");
}

#[test]
fn html_mode_delegates_to_upstream_renderer() {
    let stdout = run_mode("｜青梅《おうめ》後", "html");
    let html = String::from_utf8(stdout).expect("html utf-8");

    assert!(html.contains("<ruby>青梅"));
    assert!(html.contains("<rt>おうめ</rt>"));
    assert!(html.contains("後"));
}
