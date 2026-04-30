use std::ops::Range;

use ab_morph_diff::{
    NwayComparison, NwayFeatureGroup, NwayFeatureScope, NwayRegion, NwaySegmentationGroup,
};
use serde::{Deserialize, Serialize};

use crate::script::{ScriptCategory, classify_text};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct NwayComparisonRow {
    pub source_id: String,
    pub text_id: String,
    pub source_script_category: ScriptCategory,
    pub analyzers: Vec<String>,
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
    pub examples: Vec<NwayExampleRegionRow>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct NwayExampleRegionRow {
    pub region_index: usize,
    pub char_start: usize,
    pub char_end: usize,
    pub source_excerpt: String,
    pub per_analyzer_surfaces: Vec<NwayAnalyzerSurfacesRow>,
    pub segmentation_groups: Vec<NwaySegmentationGroupRow>,
    pub feature_groups: Vec<NwayFeatureGroupRow>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct NwayAnalyzerSurfacesRow {
    pub analyzer: String,
    pub surfaces: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct NwaySegmentationGroupRow {
    pub surfaces: Vec<String>,
    pub analyzers: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) struct NwayFeatureGroupRow {
    pub key: String,
    pub scope: NwayFeatureScopeRow,
    pub values: Vec<NwayFeatureValueGroupRow>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum NwayFeatureScopeRow {
    WholeRegion,
    TokenPosition { position: usize },
    Surface { surface: String },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct NwayFeatureValueGroupRow {
    pub value: Option<String>,
    pub analyzers: Vec<String>,
}

pub(crate) fn row_from_comparison(
    source_id: String,
    source_text: &str,
    comparison: &NwayComparison,
    max_examples: usize,
) -> NwayComparisonRow {
    NwayComparisonRow {
        source_id,
        text_id: comparison.text_id.clone(),
        source_script_category: classify_text(source_text),
        analyzers: comparison.analyzers.clone(),
        analyzer_count: comparison.stats.analyzers,
        regions: comparison.stats.regions,
        agreement_regions: comparison.stats.agreement_regions,
        regions_with_feature_disagreement: comparison.stats.regions_with_feature_disagreement,
        regions_with_segmentation_disagreement: comparison
            .stats
            .regions_with_segmentation_disagreement,
        regions_with_coverage_mismatch: comparison.stats.regions_with_coverage_mismatch,
        whitespace_regions: comparison.stats.whitespace_regions,
        lexical_regions: comparison.stats.lexical_regions,
        unanimous_boundary_count: comparison.stats.unanimous_boundary_count,
        variable_boundary_count: comparison.stats.variable_boundary_count,
        examples: select_nway_example_regions(comparison, max_examples)
            .into_iter()
            .map(|region| example_region_row(source_text, region))
            .collect(),
    }
}

fn select_nway_example_regions(
    comparison: &NwayComparison,
    max_examples: usize,
) -> Vec<&NwayRegion> {
    comparison
        .regions
        .iter()
        .filter(|region| !region.is_agreement())
        .take(max_examples)
        .collect()
}

fn example_region_row(source_text: &str, region: &NwayRegion) -> NwayExampleRegionRow {
    NwayExampleRegionRow {
        region_index: region.region_index,
        char_start: region.text_span.start,
        char_end: region.text_span.end,
        source_excerpt: excerpt(source_text, &region.text_span),
        per_analyzer_surfaces: region
            .per_analyzer
            .iter()
            .map(|entry| NwayAnalyzerSurfacesRow {
                analyzer: entry.analyzer.clone(),
                surfaces: entry.surfaces.clone(),
            })
            .collect(),
        segmentation_groups: region
            .segmentation_groups
            .iter()
            .cloned()
            .map(segmentation_group_row)
            .collect(),
        feature_groups: region
            .feature_groups
            .iter()
            .filter(|group| group.values.len() > 1)
            .cloned()
            .map(feature_group_row)
            .collect(),
    }
}

fn segmentation_group_row(group: NwaySegmentationGroup) -> NwaySegmentationGroupRow {
    NwaySegmentationGroupRow {
        surfaces: group.surfaces,
        analyzers: group.analyzers,
    }
}

fn feature_group_row(group: NwayFeatureGroup) -> NwayFeatureGroupRow {
    NwayFeatureGroupRow {
        key: group.key,
        scope: match group.scope {
            NwayFeatureScope::WholeRegion => NwayFeatureScopeRow::WholeRegion,
            NwayFeatureScope::TokenPosition { position } => {
                NwayFeatureScopeRow::TokenPosition { position }
            }
            NwayFeatureScope::Surface { surface } => NwayFeatureScopeRow::Surface { surface },
        },
        values: group
            .values
            .into_iter()
            .map(|value| NwayFeatureValueGroupRow {
                value: value.value,
                analyzers: value.analyzers,
            })
            .collect(),
    }
}

fn excerpt(source_text: &str, span: &Range<usize>) -> String {
    source_text
        .chars()
        .skip(span.start)
        .take(span.end - span.start)
        .collect()
}
