use std::{fs, io::Write, path::Path, process::Command};

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

#[test]
fn indexes_first_text_entry_from_aozora_zip() {
    let root = std::env::temp_dir().join(format!("ab-index-zip-{}", std::process::id()));
    let files = root.join("cards/000148/files");
    fs::create_dir_all(&files).unwrap();
    let zip_path = files.join("799_ruby_19091.zip");
    write_zip_text(
        &zip_path,
        "799_ruby_19091.txt",
        "吾輩《わがはい》は猫である。",
    );

    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let index = build_index(&root, &detector).unwrap();

    assert_eq!(index.works_count, 1);
    assert_eq!(index.works[0].id, "000148_799");
    assert_eq!(
        index.works[0].txt_path,
        "cards/000148/files/799_ruby_19091.zip::799_ruby_19091.txt"
    );
    assert_eq!(query_any(&index, &["ruby".to_owned()]), vec!["000148_799"]);

    let _ = fs::remove_dir_all(root);
}

#[test]
fn indexes_misnamed_zip_with_txt_extension() {
    let root = std::env::temp_dir().join(format!("ab-index-misnamed-zip-{}", std::process::id()));
    let files = root.join("cards/001030/files");
    fs::create_dir_all(&files).unwrap();
    let zip_path = files.join("4812_ruby_14383.txt");
    write_zip_text(&zip_path, "utukusii_mura.txt", "美しい村《むら》");

    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let index = build_index(&root, &detector).unwrap();

    assert_eq!(index.works_count, 1);
    assert_eq!(
        index.works[0].txt_path,
        "cards/001030/files/4812_ruby_14383.txt::utukusii_mura.txt"
    );

    let _ = fs::remove_dir_all(root);
}

#[test]
fn skips_unreadable_zip_artifacts() {
    let root = std::env::temp_dir().join(format!("ab-index-bad-zip-{}", std::process::id()));
    let files = root.join("cards/000148/files");
    fs::create_dir_all(&files).unwrap();
    fs::write(files.join("broken.zip"), []).unwrap();

    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let index = build_index(&root, &detector).unwrap();

    assert_eq!(index.works_count, 0);

    let _ = fs::remove_dir_all(root);
}

fn write_zip_text(path: &Path, name: &str, text: &str) {
    let file = fs::File::create(path).unwrap();
    let mut zip = zip::ZipWriter::new(file);
    let options = zip::write::SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated);
    zip.start_file(name, options).unwrap();
    zip.write_all(text.as_bytes()).unwrap();
    zip.finish().unwrap();
}
