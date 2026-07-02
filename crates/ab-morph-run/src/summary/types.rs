use std::collections::BTreeSet;

use clap::ValueEnum;
use serde::Serialize;

use crate::nway::{NwayFeatureScopeRow, NwayFeatureValueGroupRow, NwaySegmentationGroupRow};
use crate::script::ScriptCategory;

pub(crate) const WAREHOUSE_CORE_FEATURE_KEYS: &[&str] = &["pos1", "pos2", "pos3", "pos4"];

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum CompactSummaryGroupBy {
    SourceId,
    TextId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum CompactSummarySort {
    BoundaryF1,
    SegmentationRegions,
    LexicalSegmentationRegions,
    WhitespaceSegmentationRegions,
    FeatureDifferences,
    LexicalFeatureDifferences,
    WhitespaceFeatureDifferences,
    CoverageMismatchRegions,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum CompactExampleFilter {
    All,
    WhitespaceOnly,
    LexicalOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum CompactExampleSummarySort {
    Examples,
    WhitespaceExamples,
    LexicalExamples,
    SegmentationExamples,
    FeatureDiffExamples,
    CoverageExamples,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum CompactDifferenceKindFilter {
    All,
    Segmentation,
    Feature,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum NwaySummarySort {
    RegionsWithSegmentationDisagreement,
    RegionsWithFeatureDisagreement,
    RegionsWithCoverageMismatch,
    VariableBoundaryCount,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum NwayPatternKind {
    Segmentation,
    Feature,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum WarehouseFeatureProfile {
    Raw,
    Core,
    Schema,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SummaryExclusions {
    pub source_ids: BTreeSet<String>,
    pub text_ids: BTreeSet<String>,
}

impl SummaryExclusions {
    pub fn from_values(
        source_ids: impl IntoIterator<Item = String>,
        text_ids: impl IntoIterator<Item = String>,
    ) -> Self {
        Self {
            source_ids: source_ids.into_iter().collect(),
            text_ids: text_ids.into_iter().collect(),
        }
    }

    pub(crate) fn excludes(&self, source_id: &str, text_id: &str) -> bool {
        self.source_ids.contains(source_id) || self.text_ids.contains(text_id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactSummaryOptions {
    pub group_by: CompactSummaryGroupBy,
    pub sort_by: CompactSummarySort,
    pub script_category: Option<ScriptCategory>,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactExampleSummaryOptions {
    pub group_by: CompactSummaryGroupBy,
    pub filter: CompactExampleFilter,
    pub script_category: Option<ScriptCategory>,
    pub sort_by: CompactExampleSummarySort,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactDifferenceSummaryOptions {
    pub filter: CompactExampleFilter,
    pub script_category: Option<ScriptCategory>,
    pub kind: CompactDifferenceKindFilter,
    pub feature_key: Option<String>,
    pub excluded_feature_values: BTreeSet<String>,
    pub one_to_one_lexical_features: bool,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NwaySummaryOptions {
    pub group_by: CompactSummaryGroupBy,
    pub sort_by: NwaySummarySort,
    pub script_category: Option<ScriptCategory>,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NwayPatternOptions {
    pub kind: NwayPatternKind,
    pub feature_key: Option<String>,
    pub script_category: Option<ScriptCategory>,
    pub excluded_feature_values: BTreeSet<String>,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WarehousePatternOptions {
    pub kind: NwayPatternKind,
    pub feature_key: Option<String>,
    pub feature_profile: WarehouseFeatureProfile,
    pub text_filter: WarehouseTextFilter,
    pub excluded_feature_values: BTreeSet<String>,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WarehousePatternExampleOptions {
    pub kind: NwayPatternKind,
    pub pattern: String,
    pub feature_key: Option<String>,
    pub feature_profile: WarehouseFeatureProfile,
    pub text_filter: WarehouseTextFilter,
    pub excluded_feature_values: BTreeSet<String>,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum WarehouseRegionKind {
    All,
    Segmentation,
    Feature,
    Coverage,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum WarehouseTextFilter {
    All,
    WhitespaceOnly,
    LexicalOnly,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WarehouseRegionOptions {
    pub kind: WarehouseRegionKind,
    pub text_filter: WarehouseTextFilter,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum WarehouseErrorGroupBy {
    ErrorCode,
    Stage,
    Analyzer,
    SourceId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WarehouseErrorSummaryOptions {
    pub group_by: WarehouseErrorGroupBy,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum WarehousePairwiseSort {
    SegmentationRegions,
    FeatureRegions,
    CoverageRegions,
    VariableBoundaryCount,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WarehousePairwiseSummaryOptions {
    pub sort_by: WarehousePairwiseSort,
    pub text_filter: WarehouseTextFilter,
    pub exclusions: SummaryExclusions,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompactSummaryRow {
    pub key: String,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
    pub script_categories: Vec<String>,
    pub comparisons: usize,
    pub worst_boundary_f1: Option<f64>,
    pub total_segmentation_regions: usize,
    pub total_whitespace_segmentation_regions: usize,
    pub total_lexical_segmentation_regions: usize,
    pub total_feature_difference_regions: usize,
    pub total_whitespace_feature_difference_regions: usize,
    pub total_lexical_feature_difference_regions: usize,
    pub total_coverage_mismatch_regions: usize,
    pub max_segmentation_regions: usize,
    pub max_feature_difference_regions: usize,
    pub max_coverage_mismatch_regions: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompactExampleSummaryRow {
    pub key: String,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
    pub script_categories: Vec<String>,
    pub examples: usize,
    pub whitespace_examples: usize,
    pub lexical_examples: usize,
    pub segmentation_examples: usize,
    pub feature_diff_examples: usize,
    pub coverage_examples: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompactDifferenceSummaryRow {
    pub kind: String,
    pub from_analyzer: String,
    pub to_analyzer: String,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
    pub script_categories: Vec<String>,
    pub examples: usize,
    pub region_kind: Option<String>,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
    pub feature_key: Option<String>,
    pub feature_from: Option<String>,
    pub feature_to: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct NwaySummaryRow {
    pub key: String,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
    pub script_categories: Vec<String>,
    pub rows: usize,
    pub analyzer_count: usize,
    pub regions: usize,
    pub agreement_regions: usize,
    pub regions_with_feature_disagreement: usize,
    pub regions_with_segmentation_disagreement: usize,
    pub regions_with_coverage_mismatch: usize,
    pub whitespace_regions: usize,
    pub lexical_regions: usize,
    pub unanimous_boundary_count: usize,
    pub variable_boundary_count: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct NwayPatternRow {
    pub kind: String,
    pub pattern: String,
    pub examples: usize,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
    pub script_categories: Vec<String>,
    pub segmentation_groups: Vec<NwaySegmentationGroupRow>,
    pub feature_key: Option<String>,
    pub feature_scope: Option<NwayFeatureScopeRow>,
    pub feature_values: Vec<NwayFeatureValueGroupRow>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct WarehouseRegionExampleRow {
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
    pub analyzers: Vec<WarehouseRegionAnalyzerExampleRow>,
    pub feature_diffs: Vec<WarehouseFeatureDiffExampleRow>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct WarehouseRegionAnalyzerExampleRow {
    pub analyzer_id: String,
    pub covers_exactly: bool,
    pub morpheme_start: u64,
    pub morpheme_end: u64,
    pub surfaces: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct WarehouseFeatureDiffExampleRow {
    pub feature_key: String,
    pub scope_type: String,
    pub scope_position: Option<u64>,
    pub scope_surface: Option<String>,
    pub feature_value: Option<String>,
    pub analyzer_id: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct WarehouseErrorSummaryRow {
    pub key: String,
    pub errors: usize,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
    pub analyzer_ids: Vec<String>,
    pub stages: Vec<String>,
    pub error_codes: Vec<String>,
    pub sample_messages: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct WarehousePairwiseSummaryRow {
    pub source_id: String,
    pub text_id: String,
    pub from_analyzer: String,
    pub to_analyzer: String,
    pub regions: usize,
    pub segmentation_regions: usize,
    pub feature_regions: usize,
    pub coverage_regions: usize,
    pub unanimous_boundary_count: usize,
    pub variable_boundary_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WarehousePairwiseKey {
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) from_analyzer: String,
    pub(crate) to_analyzer: String,
}
