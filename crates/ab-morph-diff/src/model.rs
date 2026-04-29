use std::collections::BTreeMap;
use std::ops::Range;

use serde::Serialize;

pub type AnalyzerId = String;
pub type TextId = String;
pub type FeatureKey = String;
pub type FeatureMap = BTreeMap<FeatureKey, Option<String>>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Morpheme {
    pub surface: String,
    pub byte_span: Range<usize>,
    pub char_span: Range<usize>,
    pub features: FeatureMap,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Analysis {
    pub analyzer: AnalyzerId,
    pub text_id: TextId,
    pub source_text: String,
    pub morphemes: Vec<Morpheme>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Region {
    OneToOne(AlignedMorpheme),
    Segmentation(SegmentationDiff),
    CoverageMismatch(CoverageMismatch),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AlignedMorpheme {
    pub text_span: Range<usize>,
    pub from_index: usize,
    pub to_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SegmentationDiff {
    pub text_span: Range<usize>,
    pub from_indices: Range<usize>,
    pub to_indices: Range<usize>,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
    pub kind: SegmentationKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum SegmentationKind {
    Split,
    Merge,
    Resegment,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CoverageMismatch {
    pub text_span: Range<usize>,
    pub from_indices: Range<usize>,
    pub to_indices: Range<usize>,
    pub reason: CoverageMismatchKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum CoverageMismatchKind {
    MissingFrom,
    MissingTo,
    UnequalCoverage,
    InvalidInput,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FeatureDiff {
    pub region_index: usize,
    pub text_span: Range<usize>,
    pub surface: String,
    pub from_index: usize,
    pub to_index: usize,
    pub changed: BTreeMap<FeatureKey, ChangedValue>,
    pub same_context: BTreeMap<FeatureKey, Option<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ChangedValue {
    pub from: Option<String>,
    pub to: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Comparison {
    pub from_analyzer: AnalyzerId,
    pub to_analyzer: AnalyzerId,
    pub text_id: TextId,
    pub regions: Vec<Region>,
    pub feature_diffs: Vec<FeatureDiff>,
    pub stats: ComparisonStats,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompactComparison {
    pub from_analyzer: AnalyzerId,
    pub to_analyzer: AnalyzerId,
    pub text_id: TextId,
    pub stats: ComparisonStats,
    pub examples: Vec<CompactComparisonExample>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CompactComparisonExample {
    pub region_index: usize,
    pub kind: CompactExampleKind,
    pub text_span: Range<usize>,
    pub from_indices: Range<usize>,
    pub to_indices: Range<usize>,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
    pub feature_changes: Option<Vec<CompactFeatureChange>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum CompactExampleKind {
    Split,
    Merge,
    Resegment,
    CoverageMissingFrom,
    CoverageMissingTo,
    CoverageUnequal,
    CoverageInvalidInput,
    FeatureDiff,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CompactFeatureChange {
    pub key: FeatureKey,
    pub from: Option<String>,
    pub to: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ComparisonStats {
    pub from_morphemes: usize,
    pub to_morphemes: usize,
    pub one_to_one_regions: usize,
    pub one_to_one_with_feature_differences: usize,
    pub segmentation_regions: usize,
    pub coverage_mismatch_regions: usize,
    pub split_regions: usize,
    pub merge_regions: usize,
    pub resegment_regions: usize,
    pub from_morphemes_in_segmentation: usize,
    pub to_morphemes_in_segmentation: usize,
    pub boundary_precision: Option<f64>,
    pub boundary_recall: Option<f64>,
    pub boundary_f1: Option<f64>,
}
