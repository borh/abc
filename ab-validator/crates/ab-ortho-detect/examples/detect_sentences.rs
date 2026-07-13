//! Reads JSONL `{sentence, label}` from stdin, runs HeuristicV1 on each
//! sentence, prints JSONL `{sentence, gold, heuristic, agree}`.
//!
//! Requires AB_VIBRATO_DICT to point at a unidic-cwj dictionary
//! (HeuristicV1 uses Vibrato for the proper-noun guard via OrthoTokenizer).
//!
//! Usage:
//!   cargo run --example detect_sentences < data/ortho-gold/sentences.jsonl > out.jsonl

use std::io::{BufRead, BufReader};
use std::sync::Arc;

use ab_ortho_detect::OrthoDetector;
use ab_ortho_detect::heuristic::{HeuristicConfig, HeuristicV1};
use ab_plaintext::sentence_split;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize)]
struct GoldRecord {
    sentence: String,
    label: String,
}

fn is_positive(label: &str) -> bool {
    label == "accept" || label == "normalize"
}

#[derive(Debug, Serialize)]
struct OutRecord<'a> {
    sentence: &'a str,
    gold: &'a str,
    heuristic: String,
    agree: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let vibrato = Arc::new(ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default()?);
    let detector = HeuristicV1::new(
        vibrato as Arc<dyn ab_ortho_detect::OrthoTokenizer>,
        HeuristicConfig::default(),
    );

    let stdin = BufReader::new(std::io::stdin());
    for line in stdin.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let rec: GoldRecord = serde_json::from_str(&line)?;
        // HeuristicV1::detect takes &[SentenceSpan]; the gold sentence may be
        // a single sentence or contain terminals. sentence_split handles both.
        let spans = sentence_split(&rec.sentence);
        let anns = detector.detect(&spans);
        let gold_pos = is_positive(&rec.label);
        let h_label = if anns.is_empty() { "reject" } else { "accept" };
        let pred_pos = h_label == "accept";
        let out = OutRecord {
            sentence: &rec.sentence,
            gold: &rec.label,
            heuristic: h_label.to_string(),
            agree: gold_pos == pred_pos,
        };
        println!("{}", serde_json::to_string(&out)?);
    }
    Ok(())
}
