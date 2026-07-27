//! The declared three-region partition, as recognition derives and publishes it.
//!
//! The declared three-region partition, and the body-projection measure taken
//! against it.
//!
//! `eligible_bytes` is the BODY region. The ledger's facts come from lexing the
//! body projection, so the whole-file denominator it replaced divided across a
//! coordinate boundary and could mean neither parser fidelity nor packaging
//! attribution. The header and tail are measured separately under `metadata`,
//! so no byte leaves the accounting -- they move to a different accounted
//! region, which is what distinguishes this from a denominator reduction.

use std::fs;
use std::path::{Path, PathBuf};

use ab_aozora_capture::capture_generation_from_bytes_for_identity_and_work;
use ab_parser_rq_source_accountability::{
    RecognitionInput, RecognitionStatus, analyze_recognition,
};
use serde_json::Value;

const POLICY: &[u8] =
    include_bytes!("../../../../abc/data/parser-rq-ab-aozora-classified-source-v1.json");

/// A source with all three regions non-empty: a legend-fenced header, a body,
/// and a colophon tail with a blank line before it.
const LEGEND_FENCED: &str = concat!(
    "はつ恋\n",
    "ツルゲーネフ\n",
    "\n",
    "-------------------------------------------------------\n",
    "【テキスト中に現れる記号について】\n",
    "《》：ルビ\n",
    "-------------------------------------------------------\n",
    "\n",
    "本文《ほんぶん》の一行目。\n",
    "本文の二行目。\n",
    "\n",
    "底本：「テスト全集」テスト書房\n",
);

fn identity_ref() -> String {
    format!("sha256:{}", "a".repeat(64))
}

fn temp(label: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "parser-rq-regions-{label}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    path
}

fn analyze(source: &str, root: &Path) -> ab_parser_rq_source_accountability::RecognitionAnalysis {
    let generation = capture_generation_from_bytes_for_identity_and_work(
        source.as_bytes(),
        &identity_ref(),
        "w",
    )
    .unwrap();
    let manifest: Value = serde_json::from_slice(&generation.manifest).unwrap();
    let locator = manifest["members"]["classified_source_ledger"]["artifact_ref"]
        .as_str()
        .unwrap()
        .to_owned();
    generation.publish(root).unwrap();
    analyze_recognition(RecognitionInput {
        decoded_source: generation.decoded_source.clone(),
        parser_output: generation.parser_output.clone(),
        raw_diagnostics: generation.raw_diagnostics.clone(),
        ledger_bytes: generation.classified_source_ledger.clone(),
        policy_bytes: POLICY.to_vec(),
        generation_manifest: generation.manifest.clone(),
        qualification_identity_ref: identity_ref(),
        work_id: "w".to_owned(),
        ledger_locator: locator,
    })
}

#[test]
fn the_published_regions_partition_the_decoded_source() {
    let root = temp("partition");
    let record = analyze(LEGEND_FENCED, &root).record;
    assert_eq!(record.status, RecognitionStatus::Ok, "{:?}", record.errors);
    let regions = record.regions.expect("regions are published");

    // Conservation, checkable from the published record alone. This is the
    // structure the whole partition exists to make assertable: every earlier
    // defect in this area shared the property that no invariant could catch it.
    assert_eq!(regions.header.start, 0);
    assert_eq!(regions.header.end, regions.body.start);
    assert_eq!(regions.body.end, regions.tail.start);
    assert_eq!(regions.tail.end, LEGEND_FENCED.len() as u64);
    let covered = (regions.header.end - regions.header.start)
        + (regions.body.end - regions.body.start)
        + (regions.tail.end - regions.tail.start);
    assert_eq!(covered, LEGEND_FENCED.len() as u64);

    // All three regions are non-empty for this shape, and they slice to what
    // they claim. The tail includes the blank line the body end trimmed --
    // that is the `[body_end, len)` derivation, not `[tail_start, len)`.
    let decoded = LEGEND_FENCED;
    let slice = |i: ab_parser_rq_source_accountability::RecognitionInterval| {
        &decoded[i.start as usize..i.end as usize]
    };
    assert!(slice(regions.header).contains("《》：ルビ"));
    assert_eq!(
        slice(regions.body),
        "本文《ほんぶん》の一行目。\n本文の二行目。"
    );
    assert_eq!(slice(regions.tail), "\n\n底本：「テスト全集」テスト書房\n");
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn eligibility_is_the_body_and_the_two_populations_still_sum_to_the_file() {
    let root = temp("body-denominator");
    let record = analyze(LEGEND_FENCED, &root).record;
    let regions = record.regions.unwrap();
    let metadata = record.metadata.as_ref().unwrap();

    assert_eq!(
        record.eligible_bytes.unwrap(),
        regions.body.end - regions.body.start,
        "the denominator is the body region"
    );
    assert!(
        record.eligible_bytes.unwrap() < LEGEND_FENCED.len() as u64,
        "and is a strict subset of the file, or the change is not observable"
    );

    // The cross-region identity: no byte is measured twice and none by
    // nothing. This is what makes the partition a partition rather than a
    // denominator reduction, and it is the check that can actually fail.
    assert_eq!(
        record.eligible_bytes.unwrap() + metadata.eligible_bytes,
        LEGEND_FENCED.len() as u64
    );
    assert_eq!(
        metadata.eligible_bytes,
        (regions.header.end - regions.header.start) + (regions.tail.end - regions.tail.start)
    );
    // Per-population conservation, restated from the whole-file assertions the
    // measure previously carried.
    assert_eq!(
        record.recognized_bytes.unwrap() + record.semantic_gap_bytes.unwrap(),
        record.eligible_bytes.unwrap()
    );
    assert_eq!(
        record.accounted_bytes.unwrap() + record.unaccounted_bytes.unwrap(),
        record.eligible_bytes.unwrap()
    );
    assert_eq!(
        metadata.accounted_bytes + metadata.unaccounted_bytes,
        metadata.eligible_bytes
    );
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn body_intervals_never_escape_the_body_region() {
    // Every published body interval lies inside the body. Facts do fall
    // outside it -- see the CRLF test below -- so this holds because the
    // measure intersects against the region, not because the producer happens
    // to stay inside it.
    let root = temp("containment");
    let crlf = LEGEND_FENCED.replace('\n', "\r\n");
    let record = analyze(&crlf, &root).record;
    let regions = record.regions.unwrap();
    for (label, intervals) in [
        ("recognized", record.recognized.as_ref().unwrap()),
        ("accounted", record.accounted.as_ref().unwrap()),
        ("semantic_gaps", record.semantic_gaps.as_ref().unwrap()),
        ("unaccounted", record.unaccounted.as_ref().unwrap()),
    ] {
        for interval in intervals {
            assert!(
                interval.start >= regions.body.start && interval.end <= regions.body.end,
                "{label} escaped the body: {interval:?}"
            );
        }
    }
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn crlf_sources_carry_facts_outside_the_body_and_lf_sources_do_not() {
    // Measured, and it corrects the premise recorded in
    // parser-rq-instrument-before-threshold c2, which says the header and tail
    // carry no entry of any kind. That holds for LF sources. It does not hold
    // for CRLF sources, which real Aozora files are: `sanitizer_entries` walks
    // the WHOLE sanitized text rather than the body, so every line ending in
    // the header and tail is already a `crlf_normalization` fact.
    //
    // So a body-region denominator must not assume the body contains every
    // accounted interval -- it does not, on any real CRLF work -- and a
    // metadata fact producer must not re-derive these newlines, or two
    // producers will double-count the same bytes.
    let root = temp("crlf");
    let crlf = LEGEND_FENCED.replace('\n', "\r\n");
    let record = analyze(&crlf, &root).record;
    assert_eq!(record.status, RecognitionStatus::Ok, "{:?}", record.errors);
    let regions = record.regions.unwrap();
    let metadata = record.metadata.as_ref().unwrap();
    assert!(
        metadata.accounted_bytes > 0,
        "CRLF header and tail line endings are accounted metadata facts"
    );
    for interval in &metadata.accounted {
        assert!(
            interval.end <= regions.header.end || interval.start >= regions.tail.start,
            "metadata facts lie in the metadata regions: {interval:?}"
        );
    }

    let lf_root = temp("lf");
    let lf = analyze(LEGEND_FENCED, &lf_root).record;
    assert_eq!(
        lf.metadata.as_ref().unwrap().accounted_bytes,
        0,
        "an LF source has no facts in its header or tail at all"
    );
    assert!(
        lf.metadata.as_ref().unwrap().eligible_bytes > 0,
        "though it still has header and tail bytes to attribute"
    );
    fs::remove_dir_all(root).unwrap();
    fs::remove_dir_all(lf_root).unwrap();
}

#[test]
fn the_published_regions_validate_against_the_live_abc_schema() {
    let root = temp("schema");
    let record = analyze(LEGEND_FENCED, &root).record;
    let schema_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(3)
        .unwrap()
        .join("abc/schemas/parser-rq-source-recognition-work.schema.json");
    let schema: Value = serde_json::from_slice(&fs::read(schema_path).unwrap()).unwrap();
    let instance = serde_json::to_value(&record).unwrap();
    assert!(instance.get("regions").is_some());
    let validator = jsonschema::validator_for(&schema).unwrap();
    let errors = validator
        .iter_errors(&instance)
        .map(|error| error.to_string())
        .collect::<Vec<_>>();
    assert!(errors.is_empty(), "schema errors: {errors:?}");
    fs::remove_dir_all(root).unwrap();
}
