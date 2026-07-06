use aozora_rs_adapter::aat_json_from_bytes;

#[test]
fn preserves_ruby_gaiji_fixture_output() {
    let input = include_bytes!("fixtures/ruby_gaiji.txt");
    let mut expected: serde_json::Value =
        serde_json::from_slice(include_bytes!("fixtures/ruby_gaiji.aat.json")).unwrap();
    let mut actual: serde_json::Value =
        serde_json::from_slice(&aat_json_from_bytes(input).unwrap()).unwrap();
    if let Some(meta) = expected
        .get_mut("meta")
        .and_then(serde_json::Value::as_object_mut)
    {
        meta.remove("metrics");
    }
    if let Some(meta) = actual
        .get_mut("meta")
        .and_then(serde_json::Value::as_object_mut)
    {
        meta.remove("metrics");
    }
    assert_eq!(actual, expected);
}
