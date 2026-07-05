mod io;

use clap::{Parser, Subcommand};
use linfa::dataset::Dataset;
use linfa::traits::{Fit, Predict};
use linfa_logistic::LogisticRegression;
use ndarray::{Array1, Array2};

use ab_ortho_detect::features::{FEATURE_NAMES, extract_char_features, features_to_vector};
use ab_ortho_detect::ml::{MlModel, model_hash};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Train a model on gold JSONL.
    Train {
        #[arg(long)]
        gold: std::path::PathBuf,
        #[arg(long)]
        out: std::path::PathBuf,
    },
    /// Run the required character-only-vs-full-feature ablation. In v1 (Branch B)
    /// both use the same character-only feature set since OOV/proper-noun are dead;
    /// the ablation documents this and is ready for re-evaluation.
    Ablate {
        #[arg(long)]
        gold: std::path::PathBuf,
        #[arg(long)]
        report: std::path::PathBuf,
    },
    /// Print the model_hash of a model file.
    Hash {
        #[arg(long)]
        model: std::path::PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.cmd {
        Cmd::Train { gold, out } => train(&gold, &out)?,
        Cmd::Ablate { gold, report } => ablate(&gold, &report)?,
        Cmd::Hash { model } => {
            let bytes = std::fs::read(&model)?;
            let m: MlModel = bincode::deserialize(&bytes)?;
            println!("{}", model_hash(&m));
        }
    }
    Ok(())
}

/// Build the feature matrix and label vector from gold records.
/// Labels are `i32` (`linfa-logistic` requires an `Ord` class type; `f64`
/// does not impl `Ord`). `1` = accept, `0` = reject.
fn build_xy(recs: &[io::GoldRecord]) -> (Array2<f64>, Array1<i32>) {
    let n = recs.len();
    let d = FEATURE_NAMES.len();
    let mut x = Array2::<f64>::zeros((n, d));
    let mut y = Array1::<i32>::zeros(n);
    for (i, rec) in recs.iter().enumerate() {
        let features = extract_char_features(&rec.sentence);
        let v = features_to_vector(&features);
        for (j, val) in v.iter().enumerate() {
            x[(i, j)] = *val;
        }
        y[i] = if rec.label == "accept" { 1 } else { 0 };
    }
    (x, y)
}

fn train(gold: &std::path::Path, out: &std::path::Path) -> anyhow::Result<()> {
    let recs = io::read_gold(gold)?;
    let n = recs.len();
    if n == 0 {
        anyhow::bail!("gold file {gold:?} has no records");
    }
    let (x, y) = build_xy(&recs);
    let dataset = Dataset::new(x, y);
    let model = LogisticRegression::default()
        .max_iterations(500)
        .fit(&dataset)?;
    let weights: Vec<f32> = model.params().iter().map(|w| *w as f32).collect();
    let intercept = model.intercept() as f32;
    let ml_model = MlModel {
        feature_names: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
        weights,
        intercept,
        threshold: 0.5,
    };
    let bytes = bincode::serialize(&ml_model)?;
    std::fs::write(out, &bytes)?;
    eprintln!("trained model_hash={} (n={})", model_hash(&ml_model), n);
    Ok(())
}

fn ablate(gold: &std::path::Path, report: &std::path::Path) -> anyhow::Result<()> {
    // v1 reality (Branch B): TokenFeatures (oov_count, oov_ratio,
    // proper_noun_char_ratio) are dead. Character-only IS the only viable
    // feature set in v1. The ablation documents this so a future full-feature
    // revisit has a baseline to beat.
    let recs = io::read_gold(gold)?;
    let n = recs.len();
    if n == 0 {
        anyhow::bail!("gold file {gold:?} has no records");
    }
    let (x, y) = build_xy(&recs);
    let dataset = Dataset::new(x, y);
    let model = LogisticRegression::default()
        .max_iterations(500)
        .fit(&dataset)?;
    // Train-set accuracy (small data, no hold-out split — documented caveat).
    // Use `Predict::predict` which returns the actual class labels directly,
    // avoiding any confusion about which class `predict_probabilities`
    // treats as "positive" (linfa-logistic picks the more-frequent class
    // as the positive label, which can flip the sign of the output).
    let records = dataset.records().view();
    let preds: Array1<i32> = model.predict(&records);
    let targets = dataset.targets();
    let mut correct = 0usize;
    for (p, t) in preds.iter().zip(targets.iter()) {
        if p == t {
            correct += 1;
        }
    }
    let acc = correct as f64 / n as f64;

    let pos = targets.iter().filter(|t| **t == 1).count();
    let neg = n - pos;

    let text = format!(
        "# Ortho-Detect ML Ablation (Phase 2)\n\n\
Date: 2026-07-05\nGold set: `{}` (n={}, accept={}, reject={})\n\n\
## Ablation: full-feature vs character-only\n\n\
**v1 reality (Branch B):** TokenFeatures (oov_count, oov_ratio,\n\
proper_noun_char_ratio) are dead — Vibrato's `LexType::Unknown` never\n\
fires on katakana prose because Unidic-CWJ has dictionary entries for\n\
katakana particles/copulas (see `reports/ortho-detect/2026-07-05-sudachi-baseline.md`).\n\
Therefore **character-only IS the only viable feature set in v1.** The\n\
Vibrato-coupling (`OrthoTokenizer` trait + `ortho_compat.rs`) is a no-op\n\
for ML purposes in v1 and can be DELETED if the ML detector becomes the\n\
default.\n\n\
**Train-set accuracy (small data, no hold-out split):** {:.4}\n\n\
## Bias caveat (read before trusting these numbers)\n\n\
This accuracy is against **bootstrap labels** (the Python\n\
`is_katakana_sentence` heuristic labeling itself, via\n\
aozora-corpus-generator — see `scripts/ortho-gold/bootstrap_label.py`).\n\
It measures whether the linear model can reproduce the heuristic's\n\
character cascade, NOT real-world detection quality. A perfectly-trained\n\
model on these labels can at best tie the heuristic; it cannot exceed it\n\
on the labeled distribution. Real evaluation requires the\n\
human-annotated gold set (Task 9 — the documented finishing input).\n\n\
## Recommended action\n\n\
- Delete `OrthoTokenizer` trait + `ortho_compat.rs` + double-dict-load\n\
  + `oov_*` config fields (investigation report items C1, C3, C4).\n\
- Keep `MlLogisticRegression` character-only as the canonical detector.\n\
- Re-run this ablation when TokenFeatures become live (requires exposing\n\
  Vibrato `LexType::Unknown` through the trait).\n",
        gold.display(),
        n,
        pos,
        neg,
        acc
    );
    std::fs::write(report, text)?;
    eprintln!(
        "wrote ablation report: {} (train acc {:.4})",
        report.display(),
        acc
    );
    Ok(())
}
