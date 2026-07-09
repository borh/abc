use std::path::Path;

pub use super::summary_body::{
    materialize_warehouse_core_feature_pattern_counts, summarize_warehouse_errors,
    summarize_warehouse_nway, summarize_warehouse_nway_patterns, summarize_warehouse_pairwise,
    summarize_warehouse_pattern_examples, summarize_warehouse_regions,
};
use super::{WarehousePatternExampleOptions, WarehousePatternOptions, WarehouseRegionOptions};

pub fn warehouse_feature_pattern_counts_available(
    run_dir: &Path,
    options: &WarehousePatternOptions,
) -> bool {
    super::summary_body::warehouse_feature_pattern_counts_available(run_dir, options)
}

pub fn warehouse_feature_pattern_counts_duckdb_sql(
    run_dir: &Path,
    options: &WarehousePatternOptions,
) -> String {
    super::summary_body::warehouse_feature_pattern_counts_duckdb_sql(run_dir, options)
}

pub fn warehouse_pattern_duckdb_sql(run_dir: &Path, options: &WarehousePatternOptions) -> String {
    // Shape only matters when the SQL actually reads nway_feature_diffs;
    // on detection failure (e.g. table absent) keep the v3 SQL so the
    // missing-file error still surfaces from DuckDB itself, exactly as
    // before shape detection existed.
    let shape = super::summary_body::nway_feature_diffs_shape(run_dir)
        .unwrap_or(super::summary_body::FeatureDiffsShape::CollapsedAnalyzers);
    super::summary_body::warehouse_pattern_duckdb_sql(run_dir, options, shape)
}

pub fn warehouse_region_examples_duckdb_sql(
    run_dir: &Path,
    options: &WarehouseRegionOptions,
) -> String {
    let shape = super::summary_body::nway_feature_diffs_shape(run_dir)
        .unwrap_or(super::summary_body::FeatureDiffsShape::CollapsedAnalyzers);
    super::summary_body::warehouse_region_examples_duckdb_sql(run_dir, options, shape)
}

pub fn warehouse_pattern_examples_duckdb_sql(
    run_dir: &Path,
    options: &WarehousePatternExampleOptions,
) -> String {
    let shape = super::summary_body::nway_feature_diffs_shape(run_dir)
        .unwrap_or(super::summary_body::FeatureDiffsShape::CollapsedAnalyzers);
    super::summary_body::warehouse_pattern_examples_duckdb_sql(run_dir, options, shape)
}

pub fn duckdb_temp_dir(run_dir: &Path) -> std::path::PathBuf {
    super::summary_body::duckdb_temp_dir(run_dir)
}
