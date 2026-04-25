use std::{fs, path::Path, process::Command};

use ab_index::{
    features::FeatureDetector,
    index::{build_index, query_all, query_any, sample},
};

#[test]
fn builds_index_from_fixture_corpus() {
    let corpus = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/corpus");
    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let index = build_index(&corpus, &detector).unwrap();

    assert_eq!(index.works_count, 3);
    let ruby_ids = query_any(&index, &["ruby".to_owned()]);
    assert_eq!(ruby_ids, vec!["000001_1", "000003_3"]);
    let both = query_all(&index, &["ruby".to_owned(), "gaiji".to_owned()]);
    assert_eq!(both, vec!["000003_3"]);
    assert_eq!(
        sample(&index, 1, &["ruby".to_owned(), "gaiji".to_owned()]),
        vec!["000001_1"]
    );

    for work in &index.works {
        assert!(!work.txt_path.starts_with('/'));
        assert!(!work.txt_path.contains('\\'));
        if let Some(html_path) = &work.html_path {
            assert!(!html_path.starts_with('/'));
            assert!(!html_path.contains('\\'));
        }
    }
}

#[test]
fn cli_query_outputs_json_work_ids() {
    let corpus = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/corpus");
    let patterns = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../data/feature-patterns.toml");
    let index_path = std::env::temp_dir().join(format!("ab-index-{}.json", std::process::id()));

    let status = Command::new(env!("CARGO_BIN_EXE_ab-index"))
        .arg("--corpus")
        .arg(&corpus)
        .arg("--patterns")
        .arg(&patterns)
        .arg("--output")
        .arg(&index_path)
        .status()
        .unwrap();
    assert!(status.success());

    let output = Command::new(env!("CARGO_BIN_EXE_ab-index"))
        .arg("--index")
        .arg(&index_path)
        .arg("--query")
        .arg("ruby")
        .output()
        .unwrap();
    assert!(output.status.success());
    let ids: Vec<String> = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(ids, vec!["000001_1", "000003_3"]);

    let _ = fs::remove_file(index_path);
}
