use ab_aozora_aat::capture_generation_from_bytes;
use ab_parser_rq_source_accountability::{
    RecognitionInput, RecognitionInterval, RecognitionStatus, analyze_recognition,
};
use hegel::generators;
use serde_json::Value;

const POLICY: &[u8] =
    include_bytes!("../../../../abc/data/parser-rq-ab-aozora-classified-source-v1.json");

fn input(source: &str) -> RecognitionInput {
    let generation = capture_generation_from_bytes(source.as_bytes()).unwrap();
    let manifest: Value = serde_json::from_slice(&generation.manifest).unwrap();
    RecognitionInput {
        decoded_source: generation.decoded_source,
        parser_output: generation.parser_output,
        raw_diagnostics: generation.raw_diagnostics,
        ledger_bytes: generation.classified_source_ledger,
        policy_bytes: POLICY.to_vec(),
        generation_manifest: generation.manifest,
        qualification_identity_ref: manifest["qualification_identity_ref"]
            .as_str()
            .unwrap()
            .into(),
        work_id: manifest["work_id"].as_str().unwrap().into(),
        ledger_locator: "ledger.json".into(),
    }
}

fn covered(intervals: &[RecognitionInterval], byte: u64) -> bool {
    intervals
        .iter()
        .any(|interval| interval.start <= byte && byte < interval.end)
}

fn draw_source(tc: &hegel::TestCase) -> String {
    tc.draw(
        generators::vecs(generators::sampled_from(vec![
            "文",
            "｜",
            "※",
            "［＃未知］",
            "｜青梅《おうめ》",
            "\n",
            "\r\n",
        ]))
        .max_size(12),
    )
    .concat()
}

#[hegel::test(test_cases = 100)]
fn recognized_is_a_subset_of_accounted_by_independent_byte_oracle(tc: hegel::TestCase) {
    let source = draw_source(&tc);
    let record = analyze_recognition(input(&source)).record;
    assert_eq!(record.status, RecognitionStatus::Ok);
    let recognized = record.recognized.unwrap();
    let accounted = record.accounted.unwrap();
    for byte in 0..record.eligible_bytes.unwrap() {
        if covered(&recognized, byte) {
            assert!(covered(&accounted, byte));
        }
    }
}

#[hegel::test(test_cases = 100)]
fn both_complements_conserve_eligible_bytes(tc: hegel::TestCase) {
    let source = draw_source(&tc);
    let record = analyze_recognition(input(&source)).record;
    assert_eq!(record.status, RecognitionStatus::Ok);
    let eligible = record.eligible_bytes.unwrap();
    assert_eq!(
        record.recognized_bytes.unwrap() + record.semantic_gap_bytes.unwrap(),
        eligible
    );
    assert_eq!(
        record.accounted_bytes.unwrap() + record.unaccounted_bytes.unwrap(),
        eligible
    );
}
