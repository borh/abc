use std::{collections::BTreeSet, fs, io::Write, path::Path, process::Command};

use ab_index::{
    features::FeatureDetector,
    index::{build_index, query_all, query_any, sample},
    syntax_coverage::{SyntaxCoverage, SyntaxStatus},
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
fn indexes_html_sibling_with_matching_source_work_prefix() {
    let root = std::env::temp_dir().join(format!("ab-index-html-sibling-{}", std::process::id()));
    let files = root.join("cards/000296/files");
    fs::create_dir_all(&files).unwrap();
    write_zip_text(&files.join("1864_ruby_61551.zip"), "old.txt", "古い本文");
    fs::write(files.join("1864_61590.html"), "<html>old</html>").unwrap();
    write_zip_text(
        &files.join("47149_ruby_27921.zip"),
        "gakkono_setsu.txt",
        "法律学［＃ここから割り注］注［＃ここで割り注終わり］",
    );
    fs::write(files.join("47149_27961.html"), "<html>current</html>").unwrap();

    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let index = build_index(&root, &detector).unwrap();

    let work = index
        .works
        .iter()
        .find(|work| work.id == "000296_47149")
        .unwrap();
    assert_eq!(
        work.html_path.as_deref(),
        Some("cards/000296/files/47149_27961.html")
    );

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
fn excludes_aozora_tools_reference_text_from_work_index() {
    let root = std::env::temp_dir().join(format!("ab-index-tools-{}", std::process::id()));
    let tools = root.join("tools");
    fs::create_dir_all(&tools).unwrap();
    write_zip_text(
        &tools.join("JISTABLE.zip"),
        "JISTABLE.TXT",
        "JIS漢字コード表 (JIS X 0208)\n1-1\tj-2121\n",
    );
    fs::write(tools.join("tools.html"), "<html>tools</html>").unwrap();

    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let index = build_index(&root, &detector).unwrap();

    assert_eq!(index.works_count, 0);
    assert!(index.works.is_empty());
    assert!(!index.by_feature.contains_key("jis_code"));

    let _ = fs::remove_dir_all(root);
}

#[test]
fn excludes_text_sources_outside_aozora_cards_files_tree() {
    let root = std::env::temp_dir().join(format!("ab-index-nonwork-{}", std::process::id()));
    let files = root.join("cards/000148/files");
    fs::create_dir_all(&files).unwrap();
    write_zip_text(
        &files.join("799_ruby_19091.zip"),
        "799_ruby_19091.txt",
        "吾輩《わがはい》は猫である。",
    );
    let reference = root.join("reference");
    fs::create_dir_all(&reference).unwrap();
    fs::write(reference.join("ruby_reference.txt"), "基準《きじゅん》資料").unwrap();

    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let index = build_index(&root, &detector).unwrap();

    assert_eq!(index.works_count, 1);
    assert_eq!(
        index.works[0].txt_path,
        "cards/000148/files/799_ruby_19091.zip::799_ruby_19091.txt"
    );
    assert_eq!(query_any(&index, &["ruby".to_owned()]), vec!["000148_799"]);

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

#[test]
fn syntax_coverage_matrix_declares_priority_one_rows() {
    let matrix = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/aozora-syntax-coverage.toml")
        .canonicalize()
        .unwrap();
    let coverage = SyntaxCoverage::from_toml(&matrix).unwrap();
    let priority_one = coverage
        .rows()
        .iter()
        .filter(|row| row.priority == 1)
        .map(|row| row.id.as_str())
        .collect::<BTreeSet<_>>();

    assert_eq!(
        priority_one,
        BTreeSet::from([
            "break.page_line",
            "emphasis.basic",
            "figure.image_caption",
            "gaiji.marker",
            "gaiji_ruby.inline_base",
            "heading.basic",
            "indentation.basic",
            "ruby.basic",
            "warichu.basic",
        ])
    );
}

#[test]
fn syntax_coverage_feature_keys_exist_in_feature_patterns() {
    let matrix = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/aozora-syntax-coverage.toml")
        .canonicalize()
        .unwrap();
    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let coverage = SyntaxCoverage::from_toml(&matrix).unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let feature_names = detector
        .feature_names()
        .into_iter()
        .collect::<BTreeSet<_>>();

    for row in coverage.rows().iter().filter(|row| row.priority == 1) {
        assert_eq!(row.status, SyntaxStatus::Partial);
        assert!(
            !row.feature_keys.is_empty(),
            "{} should map to at least one feature key",
            row.id
        );
        for key in &row.feature_keys {
            assert!(
                feature_names.contains(key.as_str()),
                "{} maps to unknown feature key {}",
                row.id,
                key
            );
        }
    }
}

#[test]
fn detects_real_kunten_fixture_spellings() {
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../tests/fixtures/kunten-source-excerpt.txt")
        .canonicalize()
        .unwrap();
    let text = fs::read_to_string(fixture).unwrap();
    let patterns = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../data/feature-patterns.toml")
        .canonicalize()
        .unwrap();
    let detector = FeatureDetector::from_toml(&patterns).unwrap();
    let detected = detector.detect(&text);

    let kaeriten = detected
        .get("kaeriten")
        .expect("real fixture must detect compact kaeriten markers");
    let okurigana = detected
        .get("okurigana")
        .expect("real fixture must detect compact okurigana markers");

    assert!(
        text.lines()
            .enumerate()
            .any(|(idx, line)| kaeriten.contains(&(idx + 1)) && line.contains("［＃レ］")),
        "kaeriten detector must match bare return-point marker ［＃レ］"
    );
    assert!(
        text.lines()
            .enumerate()
            .any(|(idx, line)| kaeriten.contains(&(idx + 1)) && line.contains("［＃一］")),
        "kaeriten detector must match bare return-point marker ［＃一］"
    );
    assert!(
        text.lines()
            .enumerate()
            .any(|(idx, line)| okurigana.contains(&(idx + 1)) && line.contains("［＃（ノ）］")),
        "okurigana detector must match parenthesized marker ［＃（ノ）］"
    );
    assert!(
        text.lines()
            .enumerate()
            .any(|(idx, line)| okurigana.contains(&(idx + 1)) && line.contains("［＃（ス）］")),
        "okurigana detector must match parenthesized marker ［＃（ス）］"
    );
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

#[test]
fn census_and_index_keep_all_text_members_and_explicit_failures() {
    let root = std::env::temp_dir().join(format!("ab-index-census-{}", std::process::id()));
    let files = root.join("cards/000001/files");
    fs::create_dir_all(&files).unwrap();
    let mut archive = zip::ZipWriter::new(fs::File::create(files.join("1.zip")).unwrap());
    for (name, bytes) in [
        ("first.txt", b"first".as_slice()),
        ("second.txt", b"second"),
        ("README.txt", b"notes"),
        ("image.png", b"png"),
    ] {
        archive
            .start_file(name, zip::write::SimpleFileOptions::default())
            .unwrap();
        archive.write_all(bytes).unwrap();
    }
    archive.finish().unwrap();
    fs::write(files.join("broken.zip"), b"not a zip").unwrap();
    write_zip_text(&files.join("2.html"), "third.txt", "third");
    let census = ab_index::index::source_census(&root).unwrap();
    assert_eq!(census.files.len(), 3);
    assert_eq!(census.files[0].members.len(), 4);
    assert_eq!(
        census.files[0]
            .members
            .iter()
            .filter(|m| m.role == "source-text")
            .count(),
        2
    );
    assert_eq!(census.files[0].members[2].role, "auxiliary-text");
    assert_eq!(census.files[0].members[3].role, "non-text");
    assert_eq!(census.files[1].container_format, "zip");
    assert!(census.files[2].error.is_some());
    let patterns = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../data/feature-patterns.toml");
    let index = build_index(&root, &FeatureDetector::from_toml(&patterns).unwrap()).unwrap();
    assert_eq!(index.works_count, 3);
    assert_ne!(index.works[0].txt_path, index.works[1].txt_path);
    fs::remove_dir_all(root).unwrap();
}
