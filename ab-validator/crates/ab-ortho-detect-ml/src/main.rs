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
    /// k-fold cross-validation on a gold JSONL set. Trains k models,
    /// each leaving one strided fold out as the test set, and reports
    /// per-fold + mean recall/precision/F1/accuracy. Honest hold-out
    /// generalization estimate (vs the train-accuracy ceiling).
    CrossValidate {
        #[arg(long)]
        gold: std::path::PathBuf,
        #[arg(long, default_value = "5")]
        k: usize,
        #[arg(long)]
        report: std::path::PathBuf,
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
        Cmd::CrossValidate { gold, k, report } => cross_validate(&gold, k, &report)?,
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
        y[i] = if rec.is_positive() { 1 } else { 0 };
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

/// Per-fold metrics: (recall, precision, f1, accuracy, n_test).
type FoldMetrics = (f64, f64, f64, f64, usize);

fn cross_validate(
    gold: &std::path::Path,
    k: usize,
    report: &std::path::Path,
) -> anyhow::Result<()> {
    if k < 2 {
        anyhow::bail!("k must be >= 2 (got {k})");
    }
    let recs = io::read_gold(gold)?;
    let n = recs.len();
    if n == 0 {
        anyhow::bail!("gold file {gold:?} has no records");
    }
    if n < k {
        anyhow::bail!("gold file has n={n} records, need >= k={k}");
    }
    let (x_all, y_all) = build_xy(&recs);
    let d = FEATURE_NAMES.len();

    // Determine the global positive-class prevalence for the header note.
    let pos_total = y_all.iter().filter(|v| **v == 1).count();
    let neg_total = n - pos_total;

    // Strided fold assignment (deterministic — the sampler pre-shuffled the
    // gold file, so a strided split yields class-balanced folds). Test fold f
    // = indices where i % k == f.
    let mut per_fold: Vec<FoldMetrics> = Vec::with_capacity(k);
    let mut fold_assignments: Vec<usize> = Vec::with_capacity(k);

    for f in 0..k {
        let mut train_idx = Vec::new();
        let mut test_idx = Vec::new();
        for i in 0..n {
            if i % k == f {
                test_idx.push(i);
            } else {
                train_idx.push(i);
            }
        }
        let n_tr = train_idx.len();
        let n_te = test_idx.len();
        fold_assignments.push(n_te);

        let mut x_tr = Array2::<f64>::zeros((n_tr, d));
        let mut y_tr = Array1::<i32>::zeros(n_tr);
        for (row, &i) in train_idx.iter().enumerate() {
            for (col, val) in x_all.row(i).iter().enumerate() {
                x_tr[(row, col)] = *val;
            }
            y_tr[row] = y_all[i];
        }

        let mut x_te = Array2::<f64>::zeros((n_te, d));
        let mut y_te = Array1::<i32>::zeros(n_te);
        for (row, &i) in test_idx.iter().enumerate() {
            for (col, val) in x_all.row(i).iter().enumerate() {
                x_te[(row, col)] = *val;
            }
            y_te[row] = y_all[i];
        }

        let ds = linfa::Dataset::new(x_tr, y_tr);
        let model = LogisticRegression::default().max_iterations(500).fit(&ds)?;

        // Predict::predict returns the actual class labels (i32 here), NOT
        // probabilities-then-threshold. Using predict_probabilities >= 0.5
        // would invert on this data because linfa-logistic designates the
        // more-frequent class as positive (Phase 2 Task 6 fix).
        let preds_te: Array1<i32> = model.predict(&x_te);

        let (mut tp, mut fn_, mut fp, mut tn) = (0u32, 0u32, 0u32, 0u32);
        for (p, t) in preds_te.iter().zip(y_te.iter()) {
            match (*p, *t) {
                (1, 1) => tp += 1,
                (0, 1) => fn_ += 1,
                (1, 0) => fp += 1,
                (0, 0) => tn += 1,
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
        let accuracy = (tp + tn) as f64 / n_te as f64;
        per_fold.push((recall, precision, f1, accuracy, n_te));
    }

    let mean = |proj: fn(FoldMetrics) -> f64| -> f64 {
        per_fold.iter().map(|m| proj(*m)).sum::<f64>() / k as f64
    };
    let reduce = |proj: fn(FoldMetrics) -> f64, init: f64, f: fn(f64, f64) -> f64| -> f64 {
        per_fold.iter().map(|m| proj(*m)).fold(init, f)
    };
    let recall_of: fn(FoldMetrics) -> f64 = |m| m.0;
    let precision_of: fn(FoldMetrics) -> f64 = |m| m.1;
    let f1_of: fn(FoldMetrics) -> f64 = |m| m.2;
    let accuracy_of: fn(FoldMetrics) -> f64 = |m| m.3;

    let mean_recall = mean(recall_of);
    let mean_precision = mean(precision_of);
    let mean_f1 = mean(f1_of);
    let mean_accuracy = mean(accuracy_of);
    let min_recall = reduce(recall_of, f64::INFINITY, f64::min);
    let max_recall = reduce(recall_of, f64::NEG_INFINITY, f64::max);

    let mut rows = String::new();
    for (f, m) in per_fold.iter().enumerate() {
        rows.push_str(&format!(
            "| {} | {:.4} | {:.4} | {:.4} | {:.4} | {} |\n",
            f + 1,
            m.0,
            m.1,
            m.2,
            m.3,
            m.4
        ));
    }

    let gates_clears = mean_recall >= 0.85;
    let text = format!(
        "# Ortho-Detect Phase 2.5 — ML k-Fold Cross-Validation\n\
\n\
Date: 2026-07-05\n\
Gold set: `{}` (n={}, LLM-labeled, accept={}, reject={})\n\
k = {} (strided fold assignment; deterministic — the sampler pre-shuffled the gold file, so no separate shuffle is needed; each record appears in exactly one test fold)\n\
Model: `linfa_logistic::LogisticRegression` (char-only features via `extract_char_features` + `features_to_vector`; same train config as `{}` train: `max_iterations=500`, label encoding `1=accept / 0=reject` as `i32`).\n\
\n\
## Per-fold results (held-out test fold)\n\
\n\
| fold | recall | precision | f1 | accuracy | n_test |\n\
|---:|---:|---:|---:|---:|---:|\n\
{}\
\n\
| metric | mean | min | max |\n\
|---|---:|---:|---:|\n\
| recall | {:.4} | {:.4} | {:.4} |\n\
| precision | {:.4} | — | — |\n\
| f1 | {:.4} | — | — |\n\
| accuracy | {:.4} | — | — |\n\
\n\
## Method note (Phase 2 Task 6 fix applied)\n\
\n\
Evaluation uses `Predict::predict`, which returns the actual class labels directly. **Do NOT** use `predict_probabilities >= 0.5`: linfa-logistic's `label_classes` designates the *more-frequent* class as positive, so thresholded probabilities invert on this data (correct impl shows ~0.9x recall; a buggy probabilities-threshold impl shows ~0.05 recall — the inversion signature). Labels are `Array1<i32>` (`1=accept, 0=reject`); linfa-logistic requires `Ord` labels, ruling out `f64`.\n\
\n\
## Honest generalization vs train-accuracy ceiling\n\
\n\
- Train-accuracy recall on the full 300 LLM labels (Phase 2.5 Task 5/6 trainer, no hold-out): **0.958** — this is a *ceiling*, not generalization.\n\
- Hold-out mean recall (5-fold CV, this report): **{:.4}** (min {:.4}, max {:.4}).\n\
\n\
The ~{} percentage-point gap between the ceiling and the hold-out mean is the train/test optimism on this feature set.\n\
\n\
## Phase 2.5 Task 7 gate\n\
\n\
Promote `--ortho-detect ml` from EXPERIMENTAL to stable **only if mean recall >= 0.85**.\n\
\n\
- Mean recall = **{:.4}**\n\
- Gate (>= 0.85): **{}**\n\
\n\
",
        gold.display(),
        n,
        pos_total,
        neg_total,
        k,
        env!("CARGO_PKG_NAME"),
        rows,
        mean_recall,
        min_recall,
        max_recall,
        mean_precision,
        mean_f1,
        mean_accuracy,
        mean_recall,
        min_recall,
        max_recall,
        ((0.958 - mean_recall) * 100.0).round() as i64,
        mean_recall,
        if gates_clears { "PASS" } else { "FAIL" },
    );
    std::fs::write(report, text)?;
    eprintln!(
        "k={} CV on n={}: mean recall={:.4} (min {:.4}, max {:.4}), mean precision={:.4}, mean f1={:.4}, mean acc={:.4}; gate(>=0.85)={}",
        k,
        n,
        mean_recall,
        min_recall,
        max_recall,
        mean_precision,
        mean_f1,
        mean_accuracy,
        if gates_clears { "PASS" } else { "FAIL" },
    );
    Ok(())
}
