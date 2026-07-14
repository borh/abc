use std::{fs, process::Command};

#[test]
fn materializes_exact_index_order_and_hashes() {
    let temp = tempfile::tempdir().unwrap();
    let corpus = temp.path().join("corpus");
    let output = temp.path().join("output");
    fs::create_dir_all(corpus.join("cards/1/files")).unwrap();
    fs::write(corpus.join("cards/1/files/a.txt"), b"alpha").unwrap();
    let index = temp.path().join("index.json");
    fs::write(
        &index,
        br#"{"works":[{"id":"work-a","txt_path":"cards/1/files/a.txt","features":[]}]}"#,
    )
    .unwrap();
    let status = Command::new(env!("CARGO_BIN_EXE_ab-materialize-study-inventory"))
        .args(["--index", index.to_str().unwrap()])
        .args(["--corpus", corpus.to_str().unwrap()])
        .args(["--output", output.to_str().unwrap()])
        .status()
        .unwrap();
    assert!(status.success());
    let inventory: serde_json::Value =
        serde_json::from_slice(&fs::read(output.join("inventory.json")).unwrap()).unwrap();
    assert_eq!(inventory["items"][0]["id"], "work-a");
    assert_eq!(
        fs::read(output.join("sources/work-a.txt")).unwrap(),
        b"alpha"
    );
}
