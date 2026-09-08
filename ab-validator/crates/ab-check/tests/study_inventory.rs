use std::{fs, process::Command};

use ab_check::encoding::hex_sha256;

/// The authoritative study-inventory identity: item id is `{work_id}-{sha12}`
/// (sha12 = first 12 hex of sha256(indexed_path)) and the content hash is
/// sha256(bytes). This is the scheme that produced the committed
/// `aozorabunko-source-snapshot` corpus hash; the Python `materialize_index`
/// smoke helper differs (id = raw work id, path = sha256(work_id))
/// and must not be mistaken for it.
#[test]
fn inventory_identity_is_the_authoritative_work_id_sha12_scheme() {
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
    let expected_id = format!("work-a-{}", &hex_sha256(b"cards/1/files/a.txt")[..12]);
    assert_eq!(inventory["items"][0]["id"].as_str().unwrap(), expected_id);
    assert_eq!(
        inventory["items"][0]["sha256"].as_str().unwrap(),
        format!("sha256:{}", hex_sha256(b"alpha"))
    );
    // Authoritative inventory carries no smoke marker.
    assert!(inventory.get("materializer").is_none());
}

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
    let item_id = inventory["items"][0]["id"].as_str().unwrap();
    assert!(item_id.starts_with("work-a-"));
    assert_eq!(
        fs::read(output.join(format!("sources/{item_id}.txt"))).unwrap(),
        b"alpha"
    );
}
