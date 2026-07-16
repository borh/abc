//! End-to-end classified-source capture and publication checks.

use std::{fs, str};

use ab_aozora_aat::{
    capture_generation_from_bytes, classified_source_ledger_from_bytes, verify_capture_generation,
};
use hegel::generators;
use serde_json::Value;

fn json(bytes: &[u8]) -> Value {
    serde_json::from_slice(bytes).expect("capture member is JSON")
}

#[test]
fn capture_rebases_bom_crlf_accent_and_excludes_inserted_bytes() {
    let source =
        b"\xef\xbb\xbfA\r\n------------------------------\r\n\xe3\x80\x94cafe'\xe3\x80\x95";
    let generation = capture_generation_from_bytes(source).expect("capture");
    let ledger = json(&generation.classified_source_ledger);
    let decoded = str::from_utf8(&generation.decoded_source).unwrap();

    assert!(!decoded.starts_with('\u{feff}'));
    assert!(decoded.contains("\r\n"));
    assert!(ledger["entries"].as_array().unwrap().iter().all(|entry| {
        let start = usize::try_from(entry["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(entry["end"].as_u64().unwrap()).unwrap();
        start < end && decoded.is_char_boundary(start) && decoded.is_char_boundary(end)
    }));

    let proofs = ledger["entries"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|entry| entry.get("normalization_proof"))
        .collect::<Vec<_>>();
    assert!(proofs.iter().any(|proof| proof["inverse_rule"] == "crlf"));
    assert!(
        proofs
            .iter()
            .any(|proof| proof["inverse_rule"] == "accent_decomposition")
    );

    // Decorative-rule isolation inserts a parser-only blank line. Every
    // published interval must still select real decoded-source bytes.
    for entry in ledger["entries"].as_array().unwrap() {
        let start = usize::try_from(entry["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(entry["end"].as_u64().unwrap()).unwrap();
        assert!(!decoded[start..end].is_empty());
    }
}

#[test]
fn bare_cr_normalization_has_exact_reversible_evidence() {
    let ledger = json(&classified_source_ledger_from_bytes(b"A\rB").unwrap());
    let entry = ledger["entries"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["construct_id"] == "bare_cr_normalization")
        .expect("bare CR normalization claim");
    assert_eq!(entry["start"], 1);
    assert_eq!(entry["end"], 2);
    assert_eq!(entry["normalization_proof"]["source_form"], "\r");
    assert_eq!(entry["normalization_proof"]["normalized_form"], "\n");
    assert_eq!(entry["normalization_proof"]["inverse_rule"], "bare_cr");
    assert_eq!(entry["target_identity"]["relation"], "emits");
}

#[test]
fn capture_distinguishes_semantic_opaque_recovery_and_destructive_pua() {
    let source = "本文｜青梅《おうめ》［＃未知］［＃tail\u{e001}";
    let ledger = json(&classified_source_ledger_from_bytes(source.as_bytes()).unwrap());
    let entries = ledger["entries"].as_array().unwrap();

    assert!(entries.iter().any(|entry| entry["construct_id"] == "ruby"));
    assert!(
        entries
            .iter()
            .any(|entry| entry["construct_id"] == "unknown_directive"
                && entry["disposition"] == "preserved_opaque")
    );
    assert!(
        entries
            .iter()
            .any(|entry| entry["construct_id"] == "recovered_verbatim")
    );
    let pua_start = source.find('\u{e001}').unwrap() as u64;
    assert!(!entries.iter().any(|entry| {
        entry["start"].as_u64().unwrap() <= pua_start && pua_start < entry["end"].as_u64().unwrap()
    }));
}

#[test]
fn generation_authenticates_members_without_cyclic_references() {
    let generation = capture_generation_from_bytes("本文\n".as_bytes()).unwrap();
    verify_capture_generation(&generation).unwrap();
    let manifest = json(&generation.manifest);
    let generation_ref = manifest["generation_ref"].as_str().unwrap();

    for member in [
        &generation.decoded_source,
        &generation.parser_output,
        &generation.raw_diagnostics,
        &generation.classified_source_ledger,
    ] {
        assert!(!String::from_utf8_lossy(member).contains(generation_ref));
    }

    for mutate in 0..5 {
        let mut changed = generation.clone();
        match mutate {
            0 => changed.decoded_source.push(b'!'),
            1 => changed.classified_source_ledger.push(b' '),
            2 => {
                let mut ledger = json(&changed.classified_source_ledger);
                ledger["policy_hash"] = Value::String(format!("sha256:{}", "0".repeat(64)));
                changed.classified_source_ledger = serde_json::to_vec(&ledger).unwrap();
            }
            3 => {
                let mut manifest = json(&changed.manifest);
                manifest["generation_ref"] = Value::String(format!("sha256:{}", "0".repeat(64)));
                changed.manifest = serde_json::to_vec(&manifest).unwrap();
            }
            4 => changed.parser_output.push(b' '),
            _ => unreachable!(),
        }
        verify_capture_generation(&changed).expect_err(&format!("mutation {mutate} must fail"));
    }
}

#[test]
fn publication_is_immutable_and_manifest_is_published_last() {
    let temp = tempfile::tempdir().unwrap();
    let generation = capture_generation_from_bytes("本文\n".as_bytes()).unwrap();
    let first = generation.publish(temp.path()).unwrap();
    let second = generation.publish(temp.path()).unwrap();
    assert_eq!(first, second);
    assert!(temp.path().join(&first.manifest.locator).is_file());

    let mut mixed = generation;
    mixed.decoded_source.push(b'!');
    mixed.publish(temp.path()).unwrap_err();

    let files = fs::read_dir(temp.path().join("sha256"))
        .unwrap()
        .flat_map(|prefix| fs::read_dir(prefix.unwrap().path()).unwrap())
        .count();
    assert_eq!(
        files, 5,
        "four members and one manifest are immutable blobs"
    );
}

#[test]
fn production_fixture_regenerates_byte_identically() {
    const ROOT: &str = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../../abc/test/fixtures/parser-rq/classified-source-capture"
    );
    let source = include_bytes!(concat!(
        "../../../../abc/test/fixtures/parser-rq/classified-source-capture/",
        "source.txt"
    ));
    let first = capture_generation_from_bytes(source).unwrap();
    let second = capture_generation_from_bytes(source).unwrap();
    assert_eq!(first, second);
    for (name, actual) in [
        ("decoded.txt", first.decoded_source.as_slice()),
        ("parser-output.json", first.parser_output.as_slice()),
        ("raw-diagnostics.json", first.raw_diagnostics.as_slice()),
        ("ledger.json", first.classified_source_ledger.as_slice()),
        ("generation.json", first.manifest.as_slice()),
    ] {
        assert_eq!(
            fs::read(format!("{ROOT}/{name}")).unwrap(),
            actual,
            "{name}"
        );
    }
}

#[hegel::test(test_cases = 100)]
fn every_generated_unicode_capture_has_valid_public_boundaries(tc: hegel::TestCase) {
    let source = tc.draw(generators::text().max_size(80));
    let ledger = json(&classified_source_ledger_from_bytes(source.as_bytes()).unwrap());
    for entry in ledger["entries"].as_array().unwrap() {
        let start = usize::try_from(entry["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(entry["end"].as_u64().unwrap()).unwrap();
        assert!(start < end);
        assert!(source.is_char_boundary(start));
        assert!(source.is_char_boundary(end));
    }
}
