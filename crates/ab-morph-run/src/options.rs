use std::path::{Path, PathBuf};

use clap::ValueEnum;
use serde::{Deserialize, Serialize};

use crate::warehouse::schema::{RunAnalyzerRow, WarehousePaths, WarehouseTable};

/// Controls how analysis and comparison output rows are serialized.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OutputProfile {
    Full,
    Compact,
}

/// Controls which warehouse tables are written.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WarehouseProfile {
    Full,
    Triage,
}

const WAREHOUSE_TRIAGE_TABLES: &[WarehouseTable] = &[
    WarehouseTable::Runs,
    WarehouseTable::RunAnalyzers,
    WarehouseTable::Sources,
    WarehouseTable::Analyses,
    WarehouseTable::Morphemes,
    WarehouseTable::NwayRegions,
    WarehouseTable::NwayRegionAnalyzers,
    WarehouseTable::FeaturePatternCounts,
    WarehouseTable::Errors,
];

const WAREHOUSE_TRIAGE_MERGED_DATA_TABLES: &[WarehouseTable] = &[
    WarehouseTable::Sources,
    WarehouseTable::Analyses,
    WarehouseTable::Morphemes,
    WarehouseTable::NwayRegions,
    WarehouseTable::NwayRegionAnalyzers,
    WarehouseTable::FeaturePatternCounts,
    WarehouseTable::Errors,
];

impl WarehouseProfile {
    pub(crate) fn tables(self) -> &'static [WarehouseTable] {
        match self {
            Self::Full => WarehouseTable::ALL,
            Self::Triage => WAREHOUSE_TRIAGE_TABLES,
        }
    }

    pub(crate) fn merged_data_tables(self) -> &'static [WarehouseTable] {
        match self {
            Self::Full => WarehouseTable::MERGED_DATA,
            Self::Triage => WAREHOUSE_TRIAGE_MERGED_DATA_TABLES,
        }
    }
}

impl OutputProfile {
    #[must_use]
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            OutputProfile::Full => "full",
            OutputProfile::Compact => "compact",
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SerialRunOptions<'a> {
    pub(crate) analyses_output: Option<&'a Path>,
    pub(crate) comparisons_output: Option<&'a Path>,
    pub(crate) errors_output: Option<&'a Path>,
    pub(crate) resume: bool,
    pub(crate) output_profile: OutputProfile,
    pub(crate) examples_output: Option<&'a Path>,
    pub(crate) max_examples_per_comparison: usize,
    pub(crate) nway_output: Option<&'a Path>,
    pub(crate) nway_pattern_counts_output: Option<&'a Path>,
    pub(crate) max_nway_examples_per_text: usize,
    pub(crate) collect_string_stats: bool,
    pub(crate) warehouse: Option<WarehouseRunOptions>,
    pub(crate) progress: Option<SerialProgress>,
}

#[derive(Debug, Clone)]
pub(crate) struct WarehouseRunOptions {
    pub(crate) paths: WarehousePaths,
    pub(crate) input_mode: &'static str,
    pub(crate) input_path: String,
    pub(crate) analyzer_rows: Vec<RunAnalyzerRow>,
    pub(crate) warehouse_profile: WarehouseProfile,
}

#[derive(Debug, Clone)]
pub(crate) struct SerialProgress {
    pub(crate) label: String,
    pub(crate) total: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct WarehouseParallelOptions {
    pub(crate) warehouse_dir: PathBuf,
    pub(crate) run_id: String,
    pub(crate) jobs: usize,
    pub(crate) input_mode: &'static str,
    pub(crate) input_path: String,
    pub(crate) analyzer_rows: Vec<RunAnalyzerRow>,
    pub(crate) warehouse_profile: WarehouseProfile,
}
