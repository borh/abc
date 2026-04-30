use std::ops::Range;
use std::path::Path;

use ab_morph_diff::{
    Analysis, ChangedValue, CompactComparison, CompactComparisonExample, CompactExampleKind,
    CompactFeatureChange, Comparison, CoverageMismatchKind, FeatureDiff, Region, SegmentationKind,
};
use serde::{Deserialize, Serialize};

use crate::script::{ScriptCategory, classify_text};

pub(crate) fn source_id_from_aat_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("unknown")
        .to_owned()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct AnalysisSummaryRow {
    pub source_id: String,
    pub text_id: String,
    pub analyzer: String,
    pub morpheme_count: usize,
    pub source_bytes: usize,
    pub source_chars: usize,
}

impl AnalysisSummaryRow {
    pub(crate) fn from_analysis(source_id: String, analysis: &Analysis) -> Self {
        Self {
            source_id,
            text_id: analysis.text_id.clone(),
            analyzer: analysis.analyzer.clone(),
            morpheme_count: analysis.morphemes.len(),
            source_bytes: analysis.source_text.len(),
            source_chars: analysis.source_text.chars().count(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct ComparisonSummaryRow {
    pub source_id: String,
    pub text_id: String,
    #[serde(default)]
    pub source_script_category: ScriptCategory,
    pub from_analyzer: String,
    pub to_analyzer: String,
    pub from_morphemes: usize,
    pub to_morphemes: usize,
    pub one_to_one_regions: usize,
    pub one_to_one_with_feature_differences: usize,
    pub segmentation_regions: usize,
    #[serde(default)]
    pub whitespace_segmentation_regions: usize,
    #[serde(default)]
    pub lexical_segmentation_regions: usize,
    pub coverage_mismatch_regions: usize,
    pub split_regions: usize,
    pub merge_regions: usize,
    pub resegment_regions: usize,
    #[serde(default)]
    pub whitespace_feature_diff_regions: usize,
    #[serde(default)]
    pub lexical_feature_diff_regions: usize,
    pub from_morphemes_in_segmentation: usize,
    pub to_morphemes_in_segmentation: usize,
    pub boundary_precision: Option<f64>,
    pub boundary_recall: Option<f64>,
    pub boundary_f1: Option<f64>,
}

impl ComparisonSummaryRow {
    #[cfg(test)]
    pub(crate) fn from_comparison(source_id: String, comparison: &Comparison) -> Self {
        let stats = &comparison.stats;
        Self {
            source_id,
            text_id: comparison.text_id.clone(),
            source_script_category: ScriptCategory::Other,
            from_analyzer: comparison.from_analyzer.clone(),
            to_analyzer: comparison.to_analyzer.clone(),
            from_morphemes: stats.from_morphemes,
            to_morphemes: stats.to_morphemes,
            one_to_one_regions: stats.one_to_one_regions,
            one_to_one_with_feature_differences: stats.one_to_one_with_feature_differences,
            segmentation_regions: stats.segmentation_regions,
            whitespace_segmentation_regions: stats.whitespace_segmentation_regions,
            lexical_segmentation_regions: stats.lexical_segmentation_regions,
            coverage_mismatch_regions: stats.coverage_mismatch_regions,
            split_regions: stats.split_regions,
            merge_regions: stats.merge_regions,
            resegment_regions: stats.resegment_regions,
            whitespace_feature_diff_regions: stats.whitespace_feature_diff_regions,
            lexical_feature_diff_regions: stats.lexical_feature_diff_regions,
            from_morphemes_in_segmentation: stats.from_morphemes_in_segmentation,
            to_morphemes_in_segmentation: stats.to_morphemes_in_segmentation,
            boundary_precision: stats.boundary_precision,
            boundary_recall: stats.boundary_recall,
            boundary_f1: stats.boundary_f1,
        }
    }

    pub(crate) fn from_compact_comparison(
        source_id: String,
        comparison: &CompactComparison,
        source_text: &str,
    ) -> Self {
        let stats = &comparison.stats;
        Self {
            source_id,
            text_id: comparison.text_id.clone(),
            source_script_category: classify_text(source_text),
            from_analyzer: comparison.from_analyzer.clone(),
            to_analyzer: comparison.to_analyzer.clone(),
            from_morphemes: stats.from_morphemes,
            to_morphemes: stats.to_morphemes,
            one_to_one_regions: stats.one_to_one_regions,
            one_to_one_with_feature_differences: stats.one_to_one_with_feature_differences,
            segmentation_regions: stats.segmentation_regions,
            whitespace_segmentation_regions: stats.whitespace_segmentation_regions,
            lexical_segmentation_regions: stats.lexical_segmentation_regions,
            coverage_mismatch_regions: stats.coverage_mismatch_regions,
            split_regions: stats.split_regions,
            merge_regions: stats.merge_regions,
            resegment_regions: stats.resegment_regions,
            whitespace_feature_diff_regions: stats.whitespace_feature_diff_regions,
            lexical_feature_diff_regions: stats.lexical_feature_diff_regions,
            from_morphemes_in_segmentation: stats.from_morphemes_in_segmentation,
            to_morphemes_in_segmentation: stats.to_morphemes_in_segmentation,
            boundary_precision: stats.boundary_precision,
            boundary_recall: stats.boundary_recall,
            boundary_f1: stats.boundary_f1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct ComparisonExampleRow {
    pub source_id: String,
    pub text_id: String,
    pub from_analyzer: String,
    pub to_analyzer: String,
    pub region_index: usize,
    pub kind: String,
    pub byte_start: usize,
    pub byte_end: usize,
    pub char_start: usize,
    pub char_end: usize,
    pub source_excerpt: String,
    pub whitespace_only: bool,
    pub script_category: ScriptCategory,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
    pub feature_changes: Option<Vec<FeatureChangeRow>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct FeatureChangeRow {
    pub key: String,
    pub from: Option<String>,
    pub to: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct RunManifest {
    pub version: u32,
    pub output_profile: String,
    pub analyzer_args: Vec<String>,
    pub jobs: usize,
    pub input_mode: String,
    pub input_path: String,
    pub input_file_count: usize,
    pub analyses_output: String,
    pub comparisons_output: Option<String>,
    pub examples_output: Option<String>,
    pub errors_output: Option<String>,
}

pub(crate) fn example_rows_from_comparison(
    source_id: String,
    source_text: &str,
    comparison: &Comparison,
    analyses: &[Analysis],
    max_examples: usize,
) -> Vec<ComparisonExampleRow> {
    if max_examples == 0 {
        return Vec::new();
    }

    let from = analyses
        .iter()
        .find(|analysis| analysis.analyzer == comparison.from_analyzer);
    let to = analyses
        .iter()
        .find(|analysis| analysis.analyzer == comparison.to_analyzer);
    let mut rows = Vec::new();

    for (region_index, region) in comparison.regions.iter().enumerate() {
        if rows.len() >= max_examples {
            return rows;
        }
        if let Some(row) = example_row_from_region(
            &source_id,
            source_text,
            comparison,
            from,
            to,
            region_index,
            region,
        ) {
            rows.push(row);
        }
    }

    for feature_diff in &comparison.feature_diffs {
        if rows.len() >= max_examples {
            return rows;
        }
        rows.push(example_row_from_feature_diff(
            &source_id,
            source_text,
            comparison,
            from,
            to,
            feature_diff,
        ));
    }

    rows
}

pub(crate) fn example_rows_from_compact_comparison(
    source_id: String,
    source_text: &str,
    comparison: &CompactComparison,
) -> Vec<ComparisonExampleRow> {
    comparison
        .examples
        .iter()
        .map(|example| {
            example_row_from_compact_example(&source_id, source_text, comparison, example)
        })
        .collect()
}

fn example_row_from_compact_example(
    source_id: &str,
    source_text: &str,
    comparison: &CompactComparison,
    example: &CompactComparisonExample,
) -> ComparisonExampleRow {
    let byte_span = byte_span_from_char_span(source_text, &example.text_span);
    let source_excerpt = excerpt(source_text, &example.text_span);
    ComparisonExampleRow {
        source_id: source_id.to_owned(),
        text_id: comparison.text_id.clone(),
        from_analyzer: comparison.from_analyzer.clone(),
        to_analyzer: comparison.to_analyzer.clone(),
        region_index: example.region_index,
        kind: compact_kind(example.kind).to_owned(),
        byte_start: byte_span.start,
        byte_end: byte_span.end,
        char_start: example.text_span.start,
        char_end: example.text_span.end,
        whitespace_only: is_whitespace_only(&source_excerpt),
        script_category: classify_text(&source_excerpt),
        source_excerpt,
        from_surfaces: example.from_surfaces.clone(),
        to_surfaces: example.to_surfaces.clone(),
        feature_changes: example
            .feature_changes
            .as_ref()
            .map(|changes| compact_feature_changes(changes)),
    }
}

fn example_row_from_region(
    source_id: &str,
    source_text: &str,
    comparison: &Comparison,
    from: Option<&Analysis>,
    to: Option<&Analysis>,
    region_index: usize,
    region: &Region,
) -> Option<ComparisonExampleRow> {
    match region {
        Region::OneToOne(_) => None,
        Region::Segmentation(diff) => {
            let byte_span = byte_span_from_char_span(source_text, &diff.text_span);
            let source_excerpt = excerpt(source_text, &diff.text_span);
            Some(ComparisonExampleRow {
                source_id: source_id.to_owned(),
                text_id: comparison.text_id.clone(),
                from_analyzer: comparison.from_analyzer.clone(),
                to_analyzer: comparison.to_analyzer.clone(),
                region_index,
                kind: segmentation_kind(diff.kind).to_owned(),
                byte_start: byte_span.start,
                byte_end: byte_span.end,
                char_start: diff.text_span.start,
                char_end: diff.text_span.end,
                whitespace_only: is_whitespace_only(&source_excerpt),
                script_category: classify_text(&source_excerpt),
                source_excerpt,
                from_surfaces: diff.from_surfaces.clone(),
                to_surfaces: diff.to_surfaces.clone(),
                feature_changes: None,
            })
        }
        Region::CoverageMismatch(mismatch) => {
            let byte_span = byte_span_from_char_span(source_text, &mismatch.text_span);
            let source_excerpt = excerpt(source_text, &mismatch.text_span);
            Some(ComparisonExampleRow {
                source_id: source_id.to_owned(),
                text_id: comparison.text_id.clone(),
                from_analyzer: comparison.from_analyzer.clone(),
                to_analyzer: comparison.to_analyzer.clone(),
                region_index,
                kind: coverage_kind(mismatch.reason).to_owned(),
                byte_start: byte_span.start,
                byte_end: byte_span.end,
                char_start: mismatch.text_span.start,
                char_end: mismatch.text_span.end,
                whitespace_only: is_whitespace_only(&source_excerpt),
                script_category: classify_text(&source_excerpt),
                source_excerpt,
                from_surfaces: surfaces(from, mismatch.from_indices.clone()),
                to_surfaces: surfaces(to, mismatch.to_indices.clone()),
                feature_changes: None,
            })
        }
    }
}

fn example_row_from_feature_diff(
    source_id: &str,
    source_text: &str,
    comparison: &Comparison,
    from: Option<&Analysis>,
    to: Option<&Analysis>,
    feature_diff: &FeatureDiff,
) -> ComparisonExampleRow {
    let byte_span = byte_span_from_char_span(source_text, &feature_diff.text_span);
    let source_excerpt = excerpt(source_text, &feature_diff.text_span);
    ComparisonExampleRow {
        source_id: source_id.to_owned(),
        text_id: comparison.text_id.clone(),
        from_analyzer: comparison.from_analyzer.clone(),
        to_analyzer: comparison.to_analyzer.clone(),
        region_index: feature_diff.region_index,
        kind: "feature_diff".to_owned(),
        byte_start: byte_span.start,
        byte_end: byte_span.end,
        char_start: feature_diff.text_span.start,
        char_end: feature_diff.text_span.end,
        whitespace_only: is_whitespace_only(&source_excerpt),
        script_category: classify_text(&source_excerpt),
        source_excerpt,
        from_surfaces: surface_at(from, feature_diff.from_index),
        to_surfaces: surface_at(to, feature_diff.to_index),
        feature_changes: Some(feature_changes(&feature_diff.changed)),
    }
}

fn feature_changes(
    changed: &std::collections::BTreeMap<String, ChangedValue>,
) -> Vec<FeatureChangeRow> {
    changed
        .iter()
        .map(|(key, value)| FeatureChangeRow {
            key: key.clone(),
            from: value.from.clone(),
            to: value.to.clone(),
        })
        .collect()
}

fn compact_feature_changes(changed: &[CompactFeatureChange]) -> Vec<FeatureChangeRow> {
    changed
        .iter()
        .map(|value| FeatureChangeRow {
            key: value.key.clone(),
            from: value.from.clone(),
            to: value.to.clone(),
        })
        .collect()
}

fn surfaces(analysis: Option<&Analysis>, range: Range<usize>) -> Vec<String> {
    let Some(analysis) = analysis else {
        return Vec::new();
    };
    analysis.morphemes[range]
        .iter()
        .map(|morpheme| morpheme.surface.clone())
        .collect()
}

fn surface_at(analysis: Option<&Analysis>, index: usize) -> Vec<String> {
    analysis
        .and_then(|analysis| analysis.morphemes.get(index))
        .map(|morpheme| vec![morpheme.surface.clone()])
        .unwrap_or_default()
}

fn excerpt(source_text: &str, span: &Range<usize>) -> String {
    source_text
        .get(byte_span_from_char_span(source_text, span))
        .unwrap_or("")
        .to_owned()
}

pub(crate) fn is_whitespace_only(text: &str) -> bool {
    !text.is_empty() && text.chars().all(char::is_whitespace)
}

fn byte_span_from_char_span(source_text: &str, span: &Range<usize>) -> Range<usize> {
    char_to_byte(source_text, span.start)..char_to_byte(source_text, span.end)
}

fn char_to_byte(source_text: &str, char_offset: usize) -> usize {
    source_text
        .char_indices()
        .nth(char_offset)
        .map(|(byte_offset, _)| byte_offset)
        .unwrap_or(source_text.len())
}

fn segmentation_kind(kind: SegmentationKind) -> &'static str {
    match kind {
        SegmentationKind::Split => "split",
        SegmentationKind::Merge => "merge",
        SegmentationKind::Resegment => "resegment",
    }
}

fn coverage_kind(kind: CoverageMismatchKind) -> &'static str {
    match kind {
        CoverageMismatchKind::MissingFrom => "coverage_missing_from",
        CoverageMismatchKind::MissingTo => "coverage_missing_to",
        CoverageMismatchKind::UnequalCoverage => "coverage_unequal",
        CoverageMismatchKind::InvalidInput => "coverage_invalid_input",
    }
}

fn compact_kind(kind: CompactExampleKind) -> &'static str {
    match kind {
        CompactExampleKind::Split => "split",
        CompactExampleKind::Merge => "merge",
        CompactExampleKind::Resegment => "resegment",
        CompactExampleKind::CoverageMissingFrom => "coverage_missing_from",
        CompactExampleKind::CoverageMissingTo => "coverage_missing_to",
        CompactExampleKind::CoverageUnequal => "coverage_unequal",
        CompactExampleKind::CoverageInvalidInput => "coverage_invalid_input",
        CompactExampleKind::FeatureDiff => "feature_diff",
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use ab_morph_diff::{
        Analysis, ChangedValue, Comparison, ComparisonStats, FeatureDiff, FeatureMap, Morpheme,
        Region, SegmentationDiff, SegmentationKind, compare_pair_compact_with_source_text,
        compare_pair_with_source_text,
    };

    use super::*;

    #[test]
    fn source_id_uses_aat_file_stem_without_hash_suffix() {
        let path = Path::new("aats/aozora-rs-adapter/000013_542-f2b43aeff7df.json");
        assert_eq!(source_id_from_aat_path(path), "000013_542-f2b43aeff7df");
    }

    #[test]
    fn source_id_preserves_distinct_duplicate_logical_ids() {
        let first = source_id_from_aat_path(Path::new("000013_542-f2b43aeff7df.json"));
        let second = source_id_from_aat_path(Path::new("000013_542-9dc5cea740ac.json"));
        assert_ne!(first, second);
    }

    #[test]
    fn builds_compact_rows_without_source_text_or_regions() {
        let analysis = Analysis {
            analyzer: "vibrato".to_owned(),
            text_id: "t1".to_owned(),
            source_text: "今日".to_owned(),
            morphemes: vec![Morpheme {
                surface: "今日".to_owned(),
                byte_span: 0..6,
                char_span: 0..2,
                features: FeatureMap::new(),
            }],
        };
        let comparison = Comparison {
            text_id: "t1".to_owned(),
            from_analyzer: "vibrato".to_owned(),
            to_analyzer: "sudachi-c".to_owned(),
            regions: Vec::new(),
            feature_diffs: Vec::new(),
            stats: ComparisonStats {
                from_morphemes: 1,
                to_morphemes: 1,
                one_to_one_regions: 1,
                one_to_one_with_feature_differences: 0,
                segmentation_regions: 0,
                whitespace_segmentation_regions: 0,
                lexical_segmentation_regions: 0,
                coverage_mismatch_regions: 0,
                split_regions: 0,
                merge_regions: 0,
                resegment_regions: 0,
                whitespace_feature_diff_regions: 0,
                lexical_feature_diff_regions: 0,
                from_morphemes_in_segmentation: 0,
                to_morphemes_in_segmentation: 0,
                boundary_precision: Some(1.0),
                boundary_recall: Some(1.0),
                boundary_f1: Some(1.0),
            },
        };

        let analysis_row = AnalysisSummaryRow::from_analysis("source-a".to_owned(), &analysis);
        let comparison_row =
            ComparisonSummaryRow::from_comparison("source-a".to_owned(), &comparison);
        let encoded = serde_json::to_string(&comparison_row).unwrap();

        assert_eq!(analysis_row.source_id, "source-a");
        assert_eq!(analysis_row.text_id, "t1");
        assert_eq!(analysis_row.analyzer, "vibrato");
        assert_eq!(analysis_row.morpheme_count, 1);
        assert_eq!(comparison_row.coverage_mismatch_regions, 0);
        assert_eq!(comparison_row.boundary_f1, Some(1.0));
        let encoded: serde_json::Value = serde_json::from_str(&encoded).unwrap();
        assert!(encoded.get("source_text").is_none());
        assert!(encoded.get("regions").is_none());
    }

    #[test]
    fn example_rows_are_limited_by_budget() {
        let comparison = fixture_comparison_with_segmentation_regions(12);
        let analyses = fixture_analyses();
        let rows = example_rows_from_comparison(
            "source-a".to_owned(),
            "abcdefghijklmnop",
            &comparison,
            &analyses,
            3,
        );

        assert_eq!(rows.len(), 3);
        assert_eq!(rows[0].region_index, 0);
        assert_eq!(rows[1].region_index, 1);
        assert_eq!(rows[2].region_index, 2);
        assert!(rows.iter().all(|row| row.source_id == "source-a"));
        assert!(rows.iter().all(|row| !row.source_excerpt.is_empty()));
    }

    #[test]
    fn example_rows_convert_char_spans_to_byte_spans_for_multibyte_text() {
        let comparison = Comparison {
            text_id: "t1".to_owned(),
            from_analyzer: "from".to_owned(),
            to_analyzer: "to".to_owned(),
            regions: vec![Region::Segmentation(SegmentationDiff {
                text_span: 0..2,
                from_indices: 0..1,
                to_indices: 0..2,
                from_surfaces: vec!["今日".to_owned()],
                to_surfaces: vec!["今".to_owned(), "日".to_owned()],
                kind: SegmentationKind::Split,
            })],
            feature_diffs: Vec::new(),
            stats: stats(1, 2, 0, 1),
        };

        let rows = example_rows_from_comparison(
            "source-a".to_owned(),
            "今日は晴れです。",
            &comparison,
            &fixture_analyses(),
            10,
        );

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].byte_start, 0);
        assert_eq!(rows[0].byte_end, 6);
        assert_eq!(rows[0].char_start, 0);
        assert_eq!(rows[0].char_end, 2);
        assert_eq!(rows[0].source_excerpt, "今日");
    }

    #[test]
    fn example_rows_mark_whitespace_only_spans() {
        let comparison = Comparison {
            text_id: "t1".to_owned(),
            from_analyzer: "from".to_owned(),
            to_analyzer: "to".to_owned(),
            regions: vec![Region::Segmentation(SegmentationDiff {
                text_span: 0..2,
                from_indices: 0..1,
                to_indices: 0..2,
                from_surfaces: vec!["\n\u{3000}".to_owned()],
                to_surfaces: vec!["\n".to_owned(), "\u{3000}".to_owned()],
                kind: SegmentationKind::Split,
            })],
            feature_diffs: Vec::new(),
            stats: stats(1, 2, 0, 1),
        };

        let rows = example_rows_from_comparison(
            "source-a".to_owned(),
            "\n\u{3000}本文",
            &comparison,
            &fixture_analyses(),
            10,
        );

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].source_excerpt, "\n\u{3000}");
        assert!(rows[0].whitespace_only);
    }

    #[test]
    fn feature_diff_examples_include_changed_payload() {
        let mut changed = BTreeMap::new();
        changed.insert(
            "pos".to_owned(),
            ChangedValue {
                from: Some("名詞".to_owned()),
                to: Some("動詞".to_owned()),
            },
        );
        let comparison = Comparison {
            text_id: "t1".to_owned(),
            from_analyzer: "from".to_owned(),
            to_analyzer: "to".to_owned(),
            regions: Vec::new(),
            feature_diffs: vec![FeatureDiff {
                region_index: 0,
                text_span: 0..6,
                surface: "今日".to_owned(),
                from_index: 0,
                to_index: 0,
                changed,
                same_context: BTreeMap::new(),
            }],
            stats: stats(1, 1, 1, 0),
        };
        let rows = example_rows_from_comparison(
            "source-a".to_owned(),
            "今日は晴れです。",
            &comparison,
            &fixture_analyses(),
            10,
        );

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].kind, "feature_diff");
        assert_eq!(rows[0].feature_changes.as_ref().unwrap()[0].key, "pos");
    }

    #[test]
    fn compact_streaming_examples_match_full_compact_examples() {
        let source = "今日明日";
        let from = analysis_with_morphemes(
            "from",
            source,
            vec![
                m(source, "今日", 0, 2, FeatureMap::new()),
                m(source, "明日", 2, 4, features(&[("pos", Some("名詞"))])),
            ],
        );
        let to = analysis_with_morphemes(
            "to",
            source,
            vec![
                m(source, "今", 0, 1, FeatureMap::new()),
                m(source, "日", 1, 2, FeatureMap::new()),
                m(source, "明日", 2, 4, features(&[("pos", Some("副詞"))])),
            ],
        );
        let full = compare_pair_with_source_text(&from, &to, source, &[]).unwrap();
        let compact = compare_pair_compact_with_source_text(&from, &to, source, &[], 10).unwrap();

        let full_rows = example_rows_from_comparison(
            "source-a".to_owned(),
            source,
            &full,
            &[from.clone(), to.clone()],
            10,
        );
        let compact_rows =
            example_rows_from_compact_comparison("source-a".to_owned(), source, &compact);

        assert_eq!(compact_rows, full_rows);
    }

    fn fixture_comparison_with_segmentation_regions(count: usize) -> Comparison {
        let regions = (0..count)
            .map(|index| {
                Region::Segmentation(SegmentationDiff {
                    text_span: index..index + 3,
                    from_indices: 0..1,
                    to_indices: 0..1,
                    from_surfaces: vec!["今".to_owned()],
                    to_surfaces: vec!["今日".to_owned()],
                    kind: SegmentationKind::Split,
                })
            })
            .collect::<Vec<_>>();
        Comparison {
            text_id: "t1".to_owned(),
            from_analyzer: "from".to_owned(),
            to_analyzer: "to".to_owned(),
            regions,
            feature_diffs: Vec::new(),
            stats: stats(1, 1, 0, count),
        }
    }

    fn fixture_analyses() -> Vec<Analysis> {
        vec![analysis("from"), analysis("to")]
    }

    fn analysis(analyzer: &str) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "t1".to_owned(),
            source_text: "今日は晴れです。".to_owned(),
            morphemes: vec![Morpheme {
                surface: "今日".to_owned(),
                byte_span: 0..6,
                char_span: 0..2,
                features: FeatureMap::new(),
            }],
        }
    }

    fn analysis_with_morphemes(
        analyzer: &str,
        source_text: &str,
        morphemes: Vec<Morpheme>,
    ) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "t1".to_owned(),
            source_text: source_text.to_owned(),
            morphemes,
        }
    }

    fn m(
        source_text: &str,
        surface: &str,
        start: usize,
        end: usize,
        features: FeatureMap,
    ) -> Morpheme {
        let byte_start = source_text
            .char_indices()
            .nth(start)
            .map(|(index, _)| index)
            .unwrap_or(source_text.len());
        let byte_end = source_text
            .char_indices()
            .nth(end)
            .map(|(index, _)| index)
            .unwrap_or(source_text.len());
        Morpheme {
            surface: surface.to_owned(),
            byte_span: byte_start..byte_end,
            char_span: start..end,
            features,
        }
    }

    fn features(values: &[(&str, Option<&str>)]) -> FeatureMap {
        values
            .iter()
            .map(|(key, value)| (key.to_string(), value.map(str::to_owned)))
            .collect()
    }

    fn stats(
        from_morphemes: usize,
        to_morphemes: usize,
        one_to_one: usize,
        segmentation: usize,
    ) -> ComparisonStats {
        ComparisonStats {
            from_morphemes,
            to_morphemes,
            one_to_one_regions: one_to_one,
            one_to_one_with_feature_differences: 0,
            segmentation_regions: segmentation,
            whitespace_segmentation_regions: 0,
            lexical_segmentation_regions: segmentation,
            coverage_mismatch_regions: 0,
            split_regions: segmentation,
            merge_regions: 0,
            resegment_regions: 0,
            whitespace_feature_diff_regions: 0,
            lexical_feature_diff_regions: 0,
            from_morphemes_in_segmentation: segmentation,
            to_morphemes_in_segmentation: segmentation,
            boundary_precision: Some(1.0),
            boundary_recall: Some(1.0),
            boundary_f1: Some(1.0),
        }
    }
}
