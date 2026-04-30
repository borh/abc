use std::ops::Range;

#[cfg(test)]
use ab_morph_diff::NwayComparison;
use ab_morph_diff::{
    Analysis, MorphDiffError, NwayFeatureGroup, NwayFeatureScope, NwayRegion,
    NwaySegmentationGroup, NwayStats, visit_nway_regions_with_source_text,
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

#[cfg(test)]
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

pub(crate) fn row_from_analyses(
    source_id: String,
    source_text: &str,
    analyses: &[Analysis],
    max_examples: usize,
) -> Result<NwayComparisonRow, MorphDiffError> {
    let mut examples = Vec::new();
    let stats = visit_nway_regions_with_source_text(analyses, source_text, &[], |region| {
        if examples.len() < max_examples && !region.is_agreement() {
            examples.push(example_region_row(source_text, region));
        }
    })?;

    Ok(row_from_parts(
        source_id,
        source_text,
        analyses,
        stats,
        examples,
    ))
}

fn row_from_parts(
    source_id: String,
    source_text: &str,
    analyses: &[Analysis],
    stats: NwayStats,
    examples: Vec<NwayExampleRegionRow>,
) -> NwayComparisonRow {
    NwayComparisonRow {
        source_id,
        text_id: analyses
            .first()
            .map(|analysis| analysis.text_id.clone())
            .unwrap_or_default(),
        source_script_category: classify_text(source_text),
        analyzers: analyses
            .iter()
            .map(|analysis| analysis.analyzer.clone())
            .collect(),
        analyzer_count: stats.analyzers,
        regions: stats.regions,
        agreement_regions: stats.agreement_regions,
        regions_with_feature_disagreement: stats.regions_with_feature_disagreement,
        regions_with_segmentation_disagreement: stats.regions_with_segmentation_disagreement,
        regions_with_coverage_mismatch: stats.regions_with_coverage_mismatch,
        whitespace_regions: stats.whitespace_regions,
        lexical_regions: stats.lexical_regions,
        unanimous_boundary_count: stats.unanimous_boundary_count,
        variable_boundary_count: stats.variable_boundary_count,
        examples,
    }
}

#[cfg(test)]
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

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use ab_morph_diff::{Analysis, FeatureMap, Morpheme, compare_nway_with_source_text};

    use super::*;

    fn features(values: &[(&str, Option<&str>)]) -> FeatureMap {
        values
            .iter()
            .map(|(key, value)| (key.to_string(), value.map(str::to_owned)))
            .collect::<BTreeMap<_, _>>()
    }

    fn morpheme(
        source: &str,
        surface: &str,
        start: usize,
        end: usize,
        features: FeatureMap,
    ) -> Morpheme {
        Morpheme {
            surface: surface.to_owned(),
            byte_span: byte_offset(source, start)..byte_offset(source, end),
            char_span: start..end,
            features,
        }
    }

    fn byte_offset(source: &str, char_index: usize) -> usize {
        source
            .char_indices()
            .nth(char_index)
            .map(|(index, _)| index)
            .unwrap_or(source.len())
    }

    fn analysis(analyzer: &str, source: &str, morphemes: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "text-a".to_owned(),
            source_text: source.to_owned(),
            morphemes,
        }
    }

    #[test]
    fn streaming_row_matches_full_nway_row() {
        let source = "今日晴れ";
        let analyses = vec![
            analysis(
                "vibrato",
                source,
                vec![
                    morpheme(source, "今日", 0, 2, features(&[("pos1", Some("名詞"))])),
                    morpheme(source, "晴れ", 2, 4, features(&[("pos1", Some("名詞"))])),
                ],
            ),
            analysis(
                "sudachi-a",
                source,
                vec![
                    morpheme(source, "今", 0, 1, features(&[("pos1", Some("名詞"))])),
                    morpheme(source, "日", 1, 2, features(&[("pos1", Some("名詞"))])),
                    morpheme(source, "晴れ", 2, 4, features(&[("pos1", Some("動詞"))])),
                ],
            ),
            analysis(
                "sudachi-c",
                source,
                vec![
                    morpheme(source, "今日", 0, 2, features(&[("pos1", Some("名詞"))])),
                    morpheme(source, "晴れ", 2, 4, features(&[("pos1", Some("動詞"))])),
                ],
            ),
        ];
        let full = compare_nway_with_source_text(&analyses, source, &[]).unwrap();
        let full_row = row_from_comparison("source-a".to_owned(), source, &full, 10);
        let streaming_row =
            row_from_analyses("source-a".to_owned(), source, &analyses, 10).unwrap();

        assert_eq!(
            serde_json::to_value(streaming_row).unwrap(),
            serde_json::to_value(full_row).unwrap()
        );
    }
}
