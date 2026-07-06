//! Calibration tooling for the interestingness ranker (spec §Calibration
//! Plan): ranking comparison (Kendall τ-b, overlap), pooled blind labeling
//! export, and label scoring (p@k, nDCG@k). Pure cores with thin IO shells;
//! ranking artifacts are the serialized `InterestingSummary` JSONs.
//! One responsibility per submodule: compare / label_export / label_score.

use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use anyhow::{Context, Result};

use crate::summary::InterestingSummary;

mod compare;
mod label_export;

pub use compare::{RankingComparison, run_compare_rankings};
pub use label_export::{ExportLabelsOptions, ExportSummary, run_export_labels};

/// Reads a ranking artifact (a serialized `InterestingSummary` JSON), shared
/// by ranking comparison (this module) and label scoring (a later task).
pub fn read_ranking(path: &Path) -> Result<InterestingSummary> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    serde_json::from_reader(BufReader::new(file))
        .with_context(|| format!("failed to parse ranking JSON {}", path.display()))
}
