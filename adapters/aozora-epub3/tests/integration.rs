//! Integration tests exercising the built mapper binary and the library API.
//!
//! The release binary is built once per test process via a `std::sync::Once`.

use std::path::PathBuf;
use std::process::Command;
use std::sync::Once;

static BUILD: Once = Once::new();

fn ensure_release_binary() {
    BUILD.call_once(|| {
        let status = Command::new("cargo")
            .args([
                "build",
                "--manifest-path",
                concat!(env!("CARGO_MANIFEST_DIR"), "/Cargo.toml"),
                "--release",
            ])
            .status()
            .expect("failed to run cargo build");
        assert!(status.success(), "cargo build --release failed");
    });
}

fn adapter_bin() -> PathBuf {
    ensure_release_binary();
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target/release/aozora-epub3-adapter");
    path
}

#[test]
fn version_prints_expected_format() {
    let output = Command::new(adapter_bin())
        .arg("--version")
        .output()
        .expect("failed to run adapter");
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(
        stdout.starts_with("aozora-epub3-adapter "),
        "unexpected version line: {stdout}"
    );
    assert!(
        stdout.contains("AozoraEpub3-JDK21"),
        "version should name the upstream parser: {stdout}"
    );
}

#[test]
fn mapper_emits_schema_shape_aat_for_ruby() {
    let xhtml = include_str!("fixtures/ruby_basic.xhtml");
    let input = aozora_epub3_adapter::MappingInput {
        source_bytes: b"test".to_vec(),
        xhtml_documents: vec![aozora_epub3_adapter::XhtmlDocument {
            bytes: xhtml.as_bytes().to_vec(),
            kind: aozora_epub3_adapter::XhtmlDocumentKind::BodySection,
        }],
        parser_failed: false,
        parser_error_message: None,
    };
    let aat = aozora_epub3_adapter::map_to_aat(&input).unwrap();
    assert_eq!(aat["meta"]["adapter"], "aozora-epub3");
    assert_eq!(aat["meta"]["parse_complete"], true);
    let para = &aat["blocks"][0];
    assert_eq!(para["kind"], "paragraph");
    let ruby = &para["content"][0];
    assert_eq!(ruby["kind"], "ruby");
    assert_eq!(ruby["base"], "吾輩");
    assert_eq!(ruby["reading"], "わがはい");
    assert_eq!(ruby["direction"], "right");
}

#[test]
fn html_mode_concatenates_body_and_colophon() {
    let body = include_str!("fixtures/paragraph.xhtml");
    let colophon = include_str!("fixtures/yoko.xhtml");
    let input = aozora_epub3_adapter::MappingInput {
        source_bytes: vec![],
        xhtml_documents: vec![
            aozora_epub3_adapter::XhtmlDocument {
                bytes: body.as_bytes().to_vec(),
                kind: aozora_epub3_adapter::XhtmlDocumentKind::BodySection,
            },
            aozora_epub3_adapter::XhtmlDocument {
                bytes: colophon.as_bytes().to_vec(),
                kind: aozora_epub3_adapter::XhtmlDocumentKind::Colophon,
            },
        ],
        parser_failed: false,
        parser_error_message: None,
    };
    let html = aozora_epub3_adapter::map_to_html(&input).unwrap();
    assert!(html.contains("一文字。"));
    assert!(html.contains("横組"));
    assert!(html.contains("section boundary"));
}

#[test]
fn parser_failed_flag_exits_code_two_and_emits_incomplete_aat() {
    let output = Command::new(adapter_bin())
        .args([
            "--mode",
            "aat",
            "--source",
            concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/fixtures/paragraph.xhtml"
            ),
            "--xhtml",
            concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests/fixtures/paragraph.xhtml"
            ),
            "--parser-failed",
            "--parser-error-file",
            "/dev/null",
        ])
        .output()
        .expect("failed to run adapter");
    assert_eq!(
        output.status.code(),
        Some(2),
        "parser-failed in aat mode must exit 2"
    );
    let aat: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("AAT JSON on stdout even on failure");
    assert_eq!(aat["meta"]["parse_complete"], false);
    assert_eq!(aat["meta"]["adapter"], "aozora-epub3");
}
