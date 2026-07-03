use std::path::{Path, PathBuf};
use std::sync::Arc;

pub const SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WarehouseTable {
    Runs,
    RunAnalyzers,
    Sources,
    Analyses,
    Morphemes,
    MorphemeFeatures,
    NwayRegions,
    NwayRegionAnalyzers,
    NwayFeatureDiffs,
    FeaturePatternCounts,
    Errors,
}

impl WarehouseTable {
    pub const ALL: &'static [Self] = &[
        Self::Runs,
        Self::RunAnalyzers,
        Self::Sources,
        Self::Analyses,
        Self::Morphemes,
        Self::MorphemeFeatures,
        Self::NwayRegions,
        Self::NwayRegionAnalyzers,
        Self::NwayFeatureDiffs,
        Self::FeaturePatternCounts,
        Self::Errors,
    ];

    pub const MERGED_DATA: &'static [Self] = &[
        Self::Sources,
        Self::Analyses,
        Self::Morphemes,
        Self::MorphemeFeatures,
        Self::NwayRegions,
        Self::NwayRegionAnalyzers,
        Self::NwayFeatureDiffs,
        Self::FeaturePatternCounts,
        Self::Errors,
    ];

    #[must_use]
    pub fn file_name(self) -> &'static str {
        match self {
            Self::Runs => "runs.parquet",
            Self::RunAnalyzers => "run_analyzers.parquet",
            Self::Sources => "sources.parquet",
            Self::Analyses => "analyses.parquet",
            Self::Morphemes => "morphemes.parquet",
            Self::MorphemeFeatures => "morpheme_features.parquet",
            Self::NwayRegions => "nway_regions.parquet",
            Self::NwayRegionAnalyzers => "nway_region_analyzers.parquet",
            Self::NwayFeatureDiffs => "nway_feature_diffs.parquet",
            Self::FeaturePatternCounts => "feature_pattern_counts.parquet",
            Self::Errors => "errors.parquet",
        }
    }

    #[cfg(test)]
    pub fn column_names(self) -> &'static [&'static str] {
        match self {
            Self::Runs => &[
                "schema_version",
                "run_id",
                "created_at_utc",
                "input_mode",
                "input_path",
                "source_count",
                "analyzer_count",
                "error_count",
            ],
            Self::RunAnalyzers => &["run_id", "analyzer_id", "analyzer_arg", "analyzer_family"],
            Self::Sources => &[
                "run_id",
                "source_id",
                "text_id",
                "aat_path",
                "source_bytes",
                "source_chars",
            ],
            Self::Analyses => &[
                "run_id",
                "source_id",
                "text_id",
                "analyzer_id",
                "morpheme_count",
            ],
            Self::Morphemes => &[
                "run_id",
                "source_id",
                "text_id",
                "analyzer_id",
                "morpheme_index",
                "byte_start",
                "byte_end",
                "char_start",
                "char_end",
                "surface",
            ],
            Self::MorphemeFeatures => &[
                "run_id",
                "source_id",
                "text_id",
                "analyzer_id",
                "morpheme_index",
                "feature_key",
                "feature_value",
            ],
            Self::NwayRegions => &[
                "run_id",
                "source_id",
                "text_id",
                "region_index",
                "byte_start",
                "byte_end",
                "char_start",
                "char_end",
                "is_nonempty_whitespace",
                "is_agreement",
                "has_coverage_mismatch",
                "has_segmentation_disagreement",
                "has_feature_disagreement",
            ],
            Self::NwayRegionAnalyzers => &[
                "run_id",
                "source_id",
                "text_id",
                "region_index",
                "analyzer_id",
                "covers_exactly",
                "morpheme_start",
                "morpheme_end",
                "surfaces",
            ],
            Self::NwayFeatureDiffs => &[
                "run_id",
                "source_id",
                "text_id",
                "region_index",
                "feature_key",
                "scope_type",
                "scope_position",
                "scope_surface",
                "feature_value",
                "analyzer_id",
            ],
            Self::FeaturePatternCounts => &[
                "kind",
                "feature_profile",
                "feature_key",
                "is_nonempty_whitespace",
                "pattern",
                "examples",
                "source_count",
                "text_count",
                "sample_source_ids",
                "sample_text_ids",
                "script_categories",
            ],
            Self::Errors => &[
                "run_id",
                "source_id",
                "text_id",
                "analyzer_id",
                "stage",
                "error_code",
                "message",
            ],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WarehousePaths {
    pub warehouse_dir: PathBuf,
    pub run_id: String,
    pub staging_dir: PathBuf,
    pub final_dir: PathBuf,
}

impl WarehousePaths {
    pub fn new(warehouse_dir: impl AsRef<Path>, run_id: impl Into<String>) -> Self {
        let warehouse_dir = warehouse_dir.as_ref().to_path_buf();
        let run_id = run_id.into();
        let staging_dir =
            warehouse_dir
                .join(".staging")
                .join(format!("{}.{}", run_id, std::process::id()));
        let final_dir = warehouse_dir.join("runs").join(&run_id);
        Self {
            warehouse_dir,
            run_id,
            staging_dir,
            final_dir,
        }
    }

    #[must_use]
    pub fn staging_table_path(&self, table: WarehouseTable) -> PathBuf {
        self.staging_dir.join(table.file_name())
    }

    #[cfg(test)]
    pub fn final_table_path(&self, table: WarehouseTable) -> PathBuf {
        self.final_dir.join(table.file_name())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RunRow {
    pub schema_version: u32,
    pub run_id: String,
    pub created_at_utc: String,
    pub input_mode: String,
    pub input_path: String,
    pub source_count: u64,
    pub analyzer_count: u64,
    pub error_count: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RunAnalyzerRow {
    pub run_id: String,
    pub analyzer_id: String,
    pub analyzer_arg: String,
    pub analyzer_family: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRow {
    pub run_id: String,
    pub source_id: String,
    pub text_id: String,
    pub aat_path: String,
    pub source_bytes: u64,
    pub source_chars: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisRow {
    pub run_id: String,
    pub source_id: String,
    pub text_id: String,
    pub analyzer_id: String,
    pub morpheme_count: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MorphemeRow {
    pub run_id: Arc<str>,
    pub source_id: Arc<str>,
    pub text_id: Arc<str>,
    pub analyzer_id: Arc<str>,
    pub morpheme_index: u64,
    pub byte_start: u64,
    pub byte_end: u64,
    pub char_start: u64,
    pub char_end: u64,
    pub surface: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MorphemeFeatureRow {
    pub run_id: Arc<str>,
    pub source_id: Arc<str>,
    pub text_id: Arc<str>,
    pub analyzer_id: Arc<str>,
    pub morpheme_index: u64,
    pub feature_key: String,
    pub feature_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NwayRegionRow {
    pub run_id: String,
    pub source_id: String,
    pub text_id: String,
    pub region_index: u64,
    pub byte_start: u64,
    pub byte_end: u64,
    pub char_start: u64,
    pub char_end: u64,
    pub is_nonempty_whitespace: bool,
    pub is_agreement: bool,
    pub has_coverage_mismatch: bool,
    pub has_segmentation_disagreement: bool,
    pub has_feature_disagreement: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NwayRegionAnalyzerRow {
    pub run_id: String,
    pub source_id: String,
    pub text_id: String,
    pub region_index: u64,
    pub analyzer_id: String,
    pub covers_exactly: bool,
    pub morpheme_start: u64,
    pub morpheme_end: u64,
    pub surfaces: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NwayFeatureDiffRow {
    pub run_id: String,
    pub source_id: String,
    pub text_id: String,
    pub region_index: u64,
    pub feature_key: String,
    pub scope_type: String,
    pub scope_position: Option<u64>,
    pub scope_surface: Option<String>,
    pub feature_value: Option<String>,
    pub analyzer_id: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FeaturePatternCountRow {
    pub kind: String,
    pub feature_profile: String,
    pub feature_key: String,
    pub is_nonempty_whitespace: bool,
    pub pattern: String,
    pub examples: u64,
    pub source_count: u64,
    pub text_count: u64,
    pub sample_source_ids: String,
    pub sample_text_ids: String,
    pub script_categories: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorRow {
    pub run_id: String,
    pub source_id: Option<String>,
    pub text_id: Option<String>,
    pub analyzer_id: Option<String>,
    pub stage: String,
    pub error_code: String,
    pub message: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn warehouse_paths_stage_then_publish_by_run_id() {
        let paths = WarehousePaths::new("scratch/morph-warehouse", "run-a");

        assert_eq!(
            paths.final_table_path(WarehouseTable::Morphemes),
            PathBuf::from("scratch/morph-warehouse/runs/run-a/morphemes.parquet")
        );
        assert!(
            paths
                .staging_table_path(WarehouseTable::Morphemes)
                .display()
                .to_string()
                .starts_with("scratch/morph-warehouse/.staging/run-a.")
        );
    }

    #[test]
    fn every_table_has_a_parquet_file_name() {
        let names: Vec<_> = WarehouseTable::ALL
            .iter()
            .map(|table| table.file_name())
            .collect();

        assert_eq!(names.len(), 11);
        assert!(names.iter().all(|name| name.ends_with(".parquet")));
        assert!(names.contains(&"nway_region_analyzers.parquet"));
        assert!(!names.contains(&"nway_segmentation_groups.parquet"));
    }

    #[test]
    fn every_table_has_documented_columns() {
        for table in WarehouseTable::ALL {
            assert!(
                !table.column_names().is_empty(),
                "missing columns for {:?}",
                table
            );
        }
        assert!(
            WarehouseTable::NwayRegionAnalyzers
                .column_names()
                .contains(&"surfaces")
        );
        assert!(
            WarehouseTable::NwayRegions
                .column_names()
                .contains(&"is_nonempty_whitespace")
        );
    }
}
