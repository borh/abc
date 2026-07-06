//! Sweep katakana_ratio_threshold; for each value, compute recall+precision+F1
//! of HeuristicV1 against the gold set read from $ORTHO_GOLD_PATH.
use ab_ortho_detect::OrthoDetector;
use ab_ortho_detect::heuristic::{HeuristicConfig, HeuristicV1};
use ab_plaintext::sentence_split;
use serde::Deserialize;
use std::io::BufRead;
use std::sync::Arc;

#[derive(Deserialize)]
struct GoldRecord {
    sentence: String,
    label: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let gold_path =
        std::env::var("ORTHO_GOLD_PATH").expect("set ORTHO_GOLD_PATH to the labeled JSONL");
    let vibrato = Arc::new(ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default()?);
    let tokenizer: Arc<dyn ab_ortho_detect::OrthoTokenizer> = vibrato;
    let recs: Vec<GoldRecord> = std::io::BufReader::new(std::fs::File::open(&gold_path)?)
        .lines()
        .map_while(Result::ok)
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(&l).expect("jsonl"))
        .collect();
    println!("threshold | recall  precision  f1       | tp fn fp tn");
    println!("----------|----------------------------------");
    let mut best: Option<(f64, f64, f64, f64)> = None; // (threshold, recall, precision, f1)
    // Sweep over [0.25, 0.55] in 0.05 steps (7 values, i = 0..=6).
    let mut i = 0;
    while i <= 6 {
        let t = 0.25 + (i as f64) * 0.05;
        i += 1;
        let det = HeuristicV1::new(
            tokenizer.clone(),
            HeuristicConfig {
                katakana_ratio_threshold: t,
                ..Default::default()
            },
        );
        let (mut tp, mut fn_, mut fp, mut tn) = (0u32, 0u32, 0u32, 0u32);
        for rec in &recs {
            let spans = sentence_split(&rec.sentence);
            let anns = det.detect(&spans);
            let pred = if anns.is_empty() { "reject" } else { "accept" };
            match (pred, rec.label.as_str()) {
                ("accept", "accept") => tp += 1,
                ("reject", "accept") => fn_ += 1,
                ("accept", "reject") => fp += 1,
                ("reject", "reject") => tn += 1,
                _ => {}
            }
        }
        let recall = if tp + fn_ == 0 {
            0.0
        } else {
            tp as f64 / (tp + fn_) as f64
        };
        let precision = if tp + fp == 0 {
            0.0
        } else {
            tp as f64 / (tp + fp) as f64
        };
        let f1 = if recall + precision == 0.0 {
            0.0
        } else {
            2.0 * recall * precision / (recall + precision)
        };
        println!(
            "  {:.2}    | {:.4}   {:.4}    {:.4}  | {} {} {} {}",
            t, recall, precision, f1, tp, fn_, fp, tn
        );
        // Pick highest F1 subject to recall >= 0.85. Among ties prefer the
        // HIGHEST threshold (most conservative gate; minimizes FP risk on unseen
        // data and minimally deviates from the prior 0.5 default).
        if recall >= 0.85 && best.is_none_or(|b| f1 >= b.3) {
            best = Some((t, recall, precision, f1));
        }
    }
    println!();
    if let Some((t, r, p, f1)) = best {
        println!(
            "BEST (recall >= 0.85, max F1): threshold={:.2} recall={:.4} precision={:.4} f1={:.4}",
            t, r, p, f1
        );
    } else {
        eprintln!(
            "WARN: no threshold cleared recall >= 0.85 — escalation needed (see plan Task 4 EARLY EXIT B)"
        );
    }
    Ok(())
}
