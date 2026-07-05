use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::ops::Range;
use std::sync::Arc;

use serde::ser::SerializeMap;
use serde::{Deserialize, Serialize, Serializer};

pub type AnalyzerId = String;
pub type TextId = String;
pub type FeatureKey = Arc<str>;
pub type FeatureValue = Arc<str>;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AnalyzerWarning {
    pub analyzer_id: String,
    pub text_id: TextId,
    pub stage: String,
    pub message: String,
    pub count: usize,
    pub first_byte_offset: usize,
    pub hard_limit_bytes: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FeatureMap {
    entries: Vec<(FeatureKey, Option<FeatureValue>)>,
}

impl FeatureMap {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            entries: Vec::with_capacity(capacity),
        }
    }

    #[must_use]
    pub fn insert(
        &mut self,
        key: FeatureKey,
        value: Option<FeatureValue>,
    ) -> Option<Option<FeatureValue>> {
        match self
            .entries
            .binary_search_by(|(existing, _)| existing.as_ref().cmp(key.as_ref()))
        {
            Ok(index) => Some(std::mem::replace(&mut self.entries[index].1, value)),
            Err(index) => {
                self.entries.insert(index, (key, value));
                None
            }
        }
    }

    #[must_use]
    pub fn get(&self, key: impl AsRef<str>) -> Option<&Option<FeatureValue>> {
        let key = key.as_ref();
        self.entries
            .binary_search_by(|(existing, _)| existing.as_ref().cmp(key))
            .ok()
            .map(|index| &self.entries[index].1)
    }

    pub fn keys(&self) -> impl Iterator<Item = &FeatureKey> {
        self.entries.iter().map(|(key, _)| key)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&FeatureKey, &Option<FeatureValue>)> {
        self.entries.iter().map(|(key, value)| (key, value))
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl Serialize for FeatureMap {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.entries.len()))?;
        for (key, value) in &self.entries {
            map.serialize_entry(key, value)?;
        }
        map.end()
    }
}

impl FromIterator<(FeatureKey, Option<FeatureValue>)> for FeatureMap {
    fn from_iter<T: IntoIterator<Item = (FeatureKey, Option<FeatureValue>)>>(iter: T) -> Self {
        let mut map = Self::new();
        for (key, value) in iter {
            let _ = map.insert(key, value);
        }
        map
    }
}

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
    pub warnings: Vec<AnalyzerWarning>,
    /// Orthographic normalizations applied before tokenization.
    pub ortho_annotations: Option<Vec<ab_ortho_detect::OrthoAnnotation>>,
    /// Maps normalized-text byte ranges to original-text byte ranges.
    pub ortho_offset_map: Option<ab_ortho_detect::OffsetMap>,
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
    pub same_context: BTreeMap<FeatureKey, Option<FeatureValue>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ChangedValue {
    pub from: Option<FeatureValue>,
    pub to: Option<FeatureValue>,
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
    pub from: Option<FeatureValue>,
    pub to: Option<FeatureValue>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct NwayComparison {
    pub text_id: TextId,
    pub analyzers: Vec<AnalyzerId>,
    pub regions: Vec<NwayRegion>,
    pub stats: NwayStats,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct NwayRegion {
    pub region_index: usize,
    pub text_span: Range<usize>,
    pub per_analyzer: Vec<NwayAnalyzerRegion>,
    pub segmentation_groups: Vec<NwaySegmentationGroup>,
    pub feature_groups: Vec<NwayFeatureGroup>,
}

impl NwayRegion {
    #[must_use]
    pub fn has_coverage_mismatch(&self) -> bool {
        self.per_analyzer
            .iter()
            .any(|region| !region.covers_exactly)
    }

    #[must_use]
    pub fn has_segmentation_disagreement(&self) -> bool {
        self.segmentation_groups.len() > 1
    }

    #[must_use]
    pub fn has_feature_disagreement(&self) -> bool {
        self.feature_groups
            .iter()
            .any(|group| group.values.len() > 1)
    }

    #[must_use]
    pub fn is_agreement(&self) -> bool {
        !self.has_coverage_mismatch()
            && !self.has_segmentation_disagreement()
            && !self.has_feature_disagreement()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct NwayAnalyzerRegion {
    pub analyzer: AnalyzerId,
    pub indices: Range<usize>,
    pub surfaces: Vec<String>,
    pub covers_exactly: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct NwaySegmentationGroup {
    pub surfaces: Vec<String>,
    pub analyzers: Vec<AnalyzerId>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct NwayFeatureGroup {
    pub key: FeatureKey,
    pub scope: NwayFeatureScope,
    pub values: Vec<NwayFeatureValueGroup>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum NwayFeatureScope {
    WholeRegion,
    TokenPosition { position: usize },
    Surface { surface: String },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct NwayFeatureValueGroup {
    pub value: Option<FeatureValue>,
    pub analyzers: Vec<AnalyzerId>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct NwayStats {
    pub analyzers: usize,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn feature_strings_are_interned_but_serialize_as_strings() {
        let first_key: FeatureKey = "pos1".into();
        let second_key: FeatureKey = "pos1".into();
        let first_value: FeatureValue = "名詞".into();
        let second_value: FeatureValue = "名詞".into();

        assert_eq!(first_key, second_key);
        assert_eq!(first_value, second_value);
        assert_eq!(serde_json::to_string(&first_key).unwrap(), "\"pos1\"");
        assert_eq!(serde_json::to_string(&first_value).unwrap(), "\"名詞\"");
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ComparisonStats {
    pub from_morphemes: usize,
    pub to_morphemes: usize,
    pub one_to_one_regions: usize,
    pub one_to_one_with_feature_differences: usize,
    pub segmentation_regions: usize,
    pub whitespace_segmentation_regions: usize,
    pub lexical_segmentation_regions: usize,
    pub coverage_mismatch_regions: usize,
    pub split_regions: usize,
    pub merge_regions: usize,
    pub resegment_regions: usize,
    pub whitespace_feature_diff_regions: usize,
    pub lexical_feature_diff_regions: usize,
    pub from_morphemes_in_segmentation: usize,
    pub to_morphemes_in_segmentation: usize,
    pub boundary_precision: Option<f64>,
    pub boundary_recall: Option<f64>,
    pub boundary_f1: Option<f64>,
}
