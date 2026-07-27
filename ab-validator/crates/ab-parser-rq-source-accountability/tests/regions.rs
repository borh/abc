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

/// The same shape carrying every legend line form, plus a free-text remark of
/// the kind the transcriber writes and this producer must not claim.
const LEGEND_FENCED_WITH_REMARK: &str = concat!(
    "はつ恋\n",
    "ツルゲーネフ\n",
    "\n",
    "-------------------------------------------------------\n",
    "【テキスト中に現れる記号について】\n",
    "\n",
    "《》：ルビ\n",
    "（例）金森《かなもり》\n",
    "　　　（数字は、JIS X 0213の面区点番号）\n",
    "＊濁点付きの二倍の踊り字は「／″＼」\n",
    "-------------------------------------------------------\n",
    "\n",
    "本文《ほんぶん》の一行目。\n",
    "\n",
    "底本：「テスト全集」テスト書房\n",
);

/// A fenced pair with no heading. Nothing inside is claimed.
const FENCE_WITHOUT_HEADING: &str = concat!(
    "はつ恋\n",
    "\n",
    "-------------------------------------------------------\n",
    "《》：ルビ\n",
    "-------------------------------------------------------\n",
    "\n",
    "本文《ほんぶん》の一行目。\n",
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

/// Every classified line of the fixture, as `(source_form, construct, role)`.
fn classified(source: &str) -> Vec<(String, String, String)> {
    let generation = capture_generation_from_bytes_for_identity_and_work(
        source.as_bytes(),
        &identity_ref(),
        "w",
    )
    .unwrap();
    let ledger: Value = serde_json::from_slice(&generation.classified_source_ledger).unwrap();
    let mut rows = ledger["entries"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|entry| entry["construct_witness"].is_object())
        .filter(|entry| {
            let role = entry["source_role"].as_str().unwrap_or_default();
            role == "publication_metadata" || role == "editorial_legend"
        })
        .map(|entry| {
            (
                entry["construct_witness"]["source_form"]
                    .as_str()
                    .unwrap()
                    .to_owned(),
                entry["construct_id"].as_str().unwrap().to_owned(),
                entry["source_role"].as_str().unwrap().to_owned(),
                entry["start"].as_u64().unwrap(),
            )
        })
        .collect::<Vec<_>>();
    rows.sort_by_key(|row| row.3);
    rows.into_iter()
        .map(|(form, construct, role, _)| (form, construct, role))
        .collect()
}

/// The two `key：value` forms are told apart by region, never by shape.
///
/// `底本：「テスト全集」テスト書房` and `《》：ルビ` are the same shape --
/// non-empty key, fullwidth colon, value -- and mean entirely different things.
/// The colophon producer scans the tail; the legend producer scans the fenced
/// block in the header. Nothing about either line's own text distinguishes it,
/// so a producer that lost its region bound would silently relabel one as the
/// other, with the wrong role attached.
#[test]
fn the_two_key_value_forms_are_separated_by_region_and_not_by_shape() {
    let rows = classified(LEGEND_FENCED);
    let colophon = rows
        .iter()
        .find(|(form, ..)| form == "底本：「テスト全集」テスト書房")
        .expect("the colophon line is classified");
    assert_eq!(
        (colophon.1.as_str(), colophon.2.as_str()),
        ("publication_metadata_line", "publication_metadata")
    );

    let legend = rows
        .iter()
        .find(|(form, ..)| form == "《》：ルビ")
        .expect("the legend entry is classified");
    assert_eq!(
        (legend.1.as_str(), legend.2.as_str()),
        ("editorial_legend_entry", "editorial_legend")
    );
}

/// The legend block is classified by its fence, and only inside it.
///
/// The block is a closed pair of separator rules enclosing a `【...】` heading.
/// Each line form inside gets its own construct, and a line matching none of
/// them stays unattributed -- the transcriber's free-text remarks are prose,
/// not a typed form, and claiming them would be claiming to understand a
/// sentence.
///
/// The title and author lines sit outside the fence and are not claimed here.
/// They are bibliographic, not notation, and they need a producer of their own.
#[test]
fn the_fenced_legend_block_is_classified_line_form_by_line_form() {
    assert_eq!(
        classified(LEGEND_FENCED_WITH_REMARK),
        [
            (
                "-------------------------------------------------------".to_owned(),
                "editorial_separator_rule".to_owned(),
                "editorial_legend".to_owned()
            ),
            (
                "【テキスト中に現れる記号について】".to_owned(),
                "editorial_legend_heading".to_owned(),
                "editorial_legend".to_owned()
            ),
            (
                "《》：ルビ".to_owned(),
                "editorial_legend_entry".to_owned(),
                "editorial_legend".to_owned()
            ),
            (
                "（例）金森《かなもり》".to_owned(),
                "editorial_legend_example".to_owned(),
                "editorial_legend".to_owned()
            ),
            (
                "（数字は、JIS X 0213の面区点番号）".to_owned(),
                "editorial_legend_note".to_owned(),
                "editorial_legend".to_owned()
            ),
            (
                "-------------------------------------------------------".to_owned(),
                "editorial_separator_rule".to_owned(),
                "editorial_legend".to_owned()
            ),
            (
                "底本：「テスト全集」テスト書房".to_owned(),
                "publication_metadata_line".to_owned(),
                "publication_metadata".to_owned()
            ),
        ]
    );
}

/// A fence with no heading is a shape this producer has never seen.
///
/// Over a 597-work sample of the pinned corpus, every separator-fenced pair in
/// a header contained exactly one `【...】` heading; not one lacked it. Rather
/// than guess at an unfamiliar fenced block, the producer declines it entirely,
/// so those bytes stay unattributed and show up as a gap rather than as
/// confident nonsense.
#[test]
fn a_fenced_block_without_a_heading_is_declined_rather_than_guessed_at() {
    assert_eq!(
        classified(FENCE_WITHOUT_HEADING),
        [(
            "底本：「テスト全集」テスト書房".to_owned(),
            "publication_metadata_line".to_owned(),
            "publication_metadata".to_owned()
        )]
    );
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
        metadata.attributed_bytes + metadata.unattributed_bytes,
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
    //
    // What these facts must NOT do is attribute. They are true of every line
    // in the file whether or not anything understands the packaging, so this
    // test pins metadata attribution as invariant under line endings.
    let root = temp("crlf");
    let crlf = LEGEND_FENCED.replace('\n', "\r\n");
    let generation =
        capture_generation_from_bytes_for_identity_and_work(crlf.as_bytes(), &identity_ref(), "w")
            .unwrap();
    let ledger: Value = serde_json::from_slice(&generation.classified_source_ledger).unwrap();
    let record = analyze(&crlf, &root).record;
    assert_eq!(record.status, RecognitionStatus::Ok, "{:?}", record.errors);
    let regions = record.regions.unwrap();
    let metadata = record.metadata.as_ref().unwrap();

    // The facts are there: line-ending normalizations inside the header and
    // tail, which exist only because the sanitizer walks the whole text.
    let metadata_newline_facts = ledger["entries"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|entry| entry["construct_id"] == "crlf_normalization")
        .filter(|entry| {
            let start = entry["start"].as_u64().unwrap();
            let end = entry["end"].as_u64().unwrap();
            end <= regions.header.end || start >= regions.tail.start
        })
        .count();
    assert!(
        metadata_newline_facts > 0,
        "a CRLF source must carry line-ending facts in its header and tail"
    );

    // And they do not attribute. `structural_newline` is not in the policy's
    // `metadata_attributing_roles`, so a byte covered only by one of these
    // facts stays unattributed. This is the property that keeps the measure
    // about packaging: were these counted, a CRLF work would start with
    // several percent of attribution for free and the number would move when
    // line endings changed rather than when packaging became understood.
    let lf_root = temp("lf");
    let lf = analyze(LEGEND_FENCED, &lf_root).record;
    let lf_metadata = lf.metadata.as_ref().unwrap();
    assert_eq!(
        metadata.attributed_bytes, lf_metadata.attributed_bytes,
        "line endings changed metadata attribution"
    );
    assert!(metadata.attributed_bytes > 0, "the colophon is attributed");
    for interval in &metadata.attributed {
        assert!(
            interval.end <= regions.header.end || interval.start >= regions.tail.start,
            "metadata facts lie in the metadata regions: {interval:?}"
        );
    }
    assert!(
        lf_metadata.unattributed_bytes > 0,
        "metadata attribution must not be satisfiable by construction"
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
