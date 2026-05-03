use std::path::{Path, PathBuf};

pub(crate) const SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WarehouseTable {
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
    pub(crate) const ALL: &'static [Self] = &[
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

    pub(crate) const MERGED_DATA: &'static [Self] = &[
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

    pub(crate) fn file_name(self) -> &'static str {
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
    pub(crate) fn column_names(self) -> &'static [&'static str] {
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
pub(crate) struct WarehousePaths {
    pub(crate) warehouse_dir: PathBuf,
    pub(crate) run_id: String,
    pub(crate) staging_dir: PathBuf,
    pub(crate) final_dir: PathBuf,
}

impl WarehousePaths {
    pub(crate) fn new(warehouse_dir: impl AsRef<Path>, run_id: impl Into<String>) -> Self {
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

    pub(crate) fn staging_table_path(&self, table: WarehouseTable) -> PathBuf {
        self.staging_dir.join(table.file_name())
    }

    #[cfg(test)]
    pub(crate) fn final_table_path(&self, table: WarehouseTable) -> PathBuf {
        self.final_dir.join(table.file_name())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RunRow {
    pub(crate) schema_version: u32,
    pub(crate) run_id: String,
    pub(crate) created_at_utc: String,
    pub(crate) input_mode: String,
    pub(crate) input_path: String,
    pub(crate) source_count: u64,
    pub(crate) analyzer_count: u64,
    pub(crate) error_count: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RunAnalyzerRow {
    pub(crate) run_id: String,
    pub(crate) analyzer_id: String,
    pub(crate) analyzer_arg: String,
    pub(crate) analyzer_family: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SourceRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) aat_path: String,
    pub(crate) source_bytes: u64,
    pub(crate) source_chars: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct AnalysisRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) analyzer_id: String,
    pub(crate) morpheme_count: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MorphemeRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) analyzer_id: String,
    pub(crate) morpheme_index: u64,
    pub(crate) byte_start: u64,
    pub(crate) byte_end: u64,
    pub(crate) char_start: u64,
    pub(crate) char_end: u64,
    pub(crate) surface: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MorphemeFeatureRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) analyzer_id: String,
    pub(crate) morpheme_index: u64,
    pub(crate) feature_key: String,
    pub(crate) feature_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NwayRegionRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) region_index: u64,
    pub(crate) byte_start: u64,
    pub(crate) byte_end: u64,
    pub(crate) char_start: u64,
    pub(crate) char_end: u64,
    pub(crate) is_nonempty_whitespace: bool,
    pub(crate) is_agreement: bool,
    pub(crate) has_coverage_mismatch: bool,
    pub(crate) has_segmentation_disagreement: bool,
    pub(crate) has_feature_disagreement: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NwayRegionAnalyzerRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) region_index: u64,
    pub(crate) analyzer_id: String,
    pub(crate) covers_exactly: bool,
    pub(crate) morpheme_start: u64,
    pub(crate) morpheme_end: u64,
    pub(crate) surfaces: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NwayFeatureDiffRow {
    pub(crate) run_id: String,
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) region_index: u64,
    pub(crate) feature_key: String,
    pub(crate) scope_type: String,
    pub(crate) scope_position: Option<u64>,
    pub(crate) scope_surface: Option<String>,
    pub(crate) feature_value: Option<String>,
    pub(crate) analyzer_id: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FeaturePatternCountRow {
    pub(crate) kind: String,
    pub(crate) feature_profile: String,
    pub(crate) feature_key: String,
    pub(crate) is_nonempty_whitespace: bool,
    pub(crate) pattern: String,
    pub(crate) examples: u64,
    pub(crate) source_count: u64,
    pub(crate) text_count: u64,
    pub(crate) sample_source_ids: String,
    pub(crate) sample_text_ids: String,
    pub(crate) script_categories: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ErrorRow {
    pub(crate) run_id: String,
    pub(crate) source_id: Option<String>,
    pub(crate) text_id: Option<String>,
    pub(crate) analyzer_id: Option<String>,
    pub(crate) stage: String,
    pub(crate) error_code: String,
    pub(crate) message: String,
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
