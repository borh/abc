//! Like `detect_sentences` but uses `MlLogisticRegression` instead of
//! `HeuristicV1`. Reads JSONL `{sentence, label}` from stdin, runs the
//! character-only ML detector on each sentence, prints JSONL
//! `{sentence, gold, heuristic, agree}` (the `heuristic` field is the ML
//! verdict here; the name is kept identical to `detect_sentences` so the
//! same `recall_floor.clj` consumer works without changes).
//!
//! This is the ML train-accuracy ceiling measurement: the model was trained
//! on the same 300 records it is being evaluated on (see the Phase 2.5
//! measurement report for the upper-bound caveat). It is NOT a
//! generalization estimate.
//!
//! Usage:
//!   cargo run -p ab-ortho-detect --example detect_ml -- \
//!       data/ortho-gold/models/model-gold-300.bin \
//!       < data/ortho-gold/sentences-llm-300.jsonl > out.jsonl

use std::io::{BufRead, BufReader};
use std::path::Path;

use ab_ortho_detect::OrthoDetector;
use ab_ortho_detect::ml::MlLogisticRegression;
use ab_plaintext::sentence_split;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize)]
struct GoldRecord {
    sentence: String,
    label: String,
}

#[derive(Debug, Serialize)]
struct OutRecord<'a> {
    sentence: &'a str,
    gold: &'a str,
    heuristic: String,
    agree: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let model_path = std::env::args()
        .nth(1)
        .expect("usage: detect_ml <model.bin> < gold.jsonl");
    let detector = MlLogisticRegression::load(Path::new(&model_path))?;
    // `Box<dyn OrthoDetector>` works because the trait is object-safe
    // (`detect(&self, ...)`); `MlLogisticRegression` is `Send + Sync`.
    let det: Box<dyn OrthoDetector> = Box::new(detector);

    for line in BufReader::new(std::io::stdin()).lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let rec: GoldRecord = serde_json::from_str(&line)?;
        // `sentence_split` yields `Vec<SentenceSpan<'_>>` borrowing `rec.sentence`;
        // the borrows do not escape the loop body, so this is sound.
        let spans = sentence_split(&rec.sentence);
        let anns = det.detect(&spans);
        let verdict = if anns.is_empty() { "reject" } else { "accept" };
        let out = OutRecord {
            sentence: &rec.sentence,
            gold: &rec.label,
            heuristic: verdict.to_string(),
            agree: verdict == rec.label,
        };
        println!("{}", serde_json::to_string(&out)?);
    }
    Ok(())
}
