use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::compact::{ComparisonSummaryRow, is_whitespace_only};
use crate::output::for_each_jsonl_or_zst_line;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactSummaryGroupBy {
    SourceId,
    TextId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactExampleFilter {
    All,
    WhitespaceOnly,
    LexicalOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactExampleSummarySort {
    Examples,
    WhitespaceExamples,
    LexicalExamples,
    SegmentationExamples,
    FeatureDiffExamples,
    CoverageExamples,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactSummaryOptions {
    pub group_by: CompactSummaryGroupBy,
    pub sort_by: CompactSummarySort,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactExampleSummaryOptions {
    pub group_by: CompactSummaryGroupBy,
    pub filter: CompactExampleFilter,
    pub sort_by: CompactExampleSummarySort,
    pub limit: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompactSummaryRow {
    pub key: String,
    pub source_ids: Vec<String>,
    pub text_ids: Vec<String>,
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
    pub examples: usize,
    pub whitespace_examples: usize,
    pub lexical_examples: usize,
    pub segmentation_examples: usize,
    pub feature_diff_examples: usize,
    pub coverage_examples: usize,
}

#[derive(Debug, Default)]
struct Accumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    comparisons: usize,
    worst_boundary_f1: Option<f64>,
    saw_null_boundary_f1: bool,
    total_segmentation_regions: usize,
    total_whitespace_segmentation_regions: usize,
    total_lexical_segmentation_regions: usize,
    total_feature_difference_regions: usize,
    total_whitespace_feature_difference_regions: usize,
    total_lexical_feature_difference_regions: usize,
    total_coverage_mismatch_regions: usize,
    max_segmentation_regions: usize,
    max_feature_difference_regions: usize,
    max_coverage_mismatch_regions: usize,
}

#[derive(Debug, Default)]
struct ExampleAccumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    examples: usize,
    whitespace_examples: usize,
    lexical_examples: usize,
    segmentation_examples: usize,
    feature_diff_examples: usize,
    coverage_examples: usize,
}

#[derive(Debug, Deserialize)]
struct ExampleSummaryInputRow {
    source_id: String,
    text_id: String,
    kind: String,
    source_excerpt: String,
    #[serde(default)]
    whitespace_only: Option<bool>,
}

pub fn summarize_compact_comparisons(
    comparisons_path: &Path,
    options: CompactSummaryOptions,
) -> Result<Vec<CompactSummaryRow>> {
    let mut groups = BTreeMap::<String, Accumulator>::new();
    for_each_jsonl_or_zst_line(comparisons_path, |line| {
        let row: ComparisonSummaryRow = serde_json::from_str(line)?;
        let key = match options.group_by {
            CompactSummaryGroupBy::SourceId => row.source_id.clone(),
            CompactSummaryGroupBy::TextId => row.text_id.clone(),
        };
        groups.entry(key).or_default().push(row);
        Ok(())
    })?;

    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| compare_rows(left, right, options.sort_by));
    rows.truncate(options.limit);
    Ok(rows)
}

pub fn summarize_compact_examples(
    examples_path: &Path,
    options: CompactExampleSummaryOptions,
) -> Result<Vec<CompactExampleSummaryRow>> {
    let mut groups = BTreeMap::<String, ExampleAccumulator>::new();
    for_each_jsonl_or_zst_line(examples_path, |line| {
        let row: ExampleSummaryInputRow = serde_json::from_str(line)?;
        let whitespace_only = row
            .whitespace_only
            .unwrap_or_else(|| is_whitespace_only(&row.source_excerpt));
        if !example_filter_matches(options.filter, whitespace_only) {
            return Ok(());
        }
        let key = match options.group_by {
            CompactSummaryGroupBy::SourceId => row.source_id.clone(),
            CompactSummaryGroupBy::TextId => row.text_id.clone(),
        };
        groups.entry(key).or_default().push(row, whitespace_only);
        Ok(())
    })?;

    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| compare_example_rows(left, right, options.sort_by));
    rows.truncate(options.limit);
    Ok(rows)
}

impl Accumulator {
    fn push(&mut self, row: ComparisonSummaryRow) {
        self.source_ids.insert(row.source_id);
        self.text_ids.insert(row.text_id);
        self.comparisons += 1;
        match row.boundary_f1 {
            Some(value) => {
                self.worst_boundary_f1 =
                    Some(self.worst_boundary_f1.map_or(value, |old| old.min(value)));
            }
            None => self.saw_null_boundary_f1 = true,
        }
        self.total_segmentation_regions += row.segmentation_regions;
        self.total_whitespace_segmentation_regions += row.whitespace_segmentation_regions;
        self.total_lexical_segmentation_regions += row.lexical_segmentation_regions;
        self.total_feature_difference_regions += row.one_to_one_with_feature_differences;
        self.total_whitespace_feature_difference_regions += row.whitespace_feature_diff_regions;
        self.total_lexical_feature_difference_regions += row.lexical_feature_diff_regions;
        self.total_coverage_mismatch_regions += row.coverage_mismatch_regions;
        self.max_segmentation_regions = self.max_segmentation_regions.max(row.segmentation_regions);
        self.max_feature_difference_regions = self
            .max_feature_difference_regions
            .max(row.one_to_one_with_feature_differences);
        self.max_coverage_mismatch_regions = self
            .max_coverage_mismatch_regions
            .max(row.coverage_mismatch_regions);
    }

    fn into_row(self, key: String) -> CompactSummaryRow {
        CompactSummaryRow {
            key,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            comparisons: self.comparisons,
            worst_boundary_f1: if self.saw_null_boundary_f1 {
                None
            } else {
                self.worst_boundary_f1
            },
            total_segmentation_regions: self.total_segmentation_regions,
            total_whitespace_segmentation_regions: self.total_whitespace_segmentation_regions,
            total_lexical_segmentation_regions: self.total_lexical_segmentation_regions,
            total_feature_difference_regions: self.total_feature_difference_regions,
            total_whitespace_feature_difference_regions: self
                .total_whitespace_feature_difference_regions,
            total_lexical_feature_difference_regions: self.total_lexical_feature_difference_regions,
            total_coverage_mismatch_regions: self.total_coverage_mismatch_regions,
            max_segmentation_regions: self.max_segmentation_regions,
            max_feature_difference_regions: self.max_feature_difference_regions,
            max_coverage_mismatch_regions: self.max_coverage_mismatch_regions,
        }
    }
}

impl ExampleAccumulator {
    fn push(&mut self, row: ExampleSummaryInputRow, whitespace_only: bool) {
        self.source_ids.insert(row.source_id);
        self.text_ids.insert(row.text_id);
        self.examples += 1;
        if whitespace_only {
            self.whitespace_examples += 1;
        } else {
            self.lexical_examples += 1;
        }
        if is_segmentation_example(&row.kind) {
            self.segmentation_examples += 1;
        } else if row.kind == "feature_diff" {
            self.feature_diff_examples += 1;
        } else if row.kind.starts_with("coverage_") {
            self.coverage_examples += 1;
        }
    }

    fn into_row(self, key: String) -> CompactExampleSummaryRow {
        CompactExampleSummaryRow {
            key,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            examples: self.examples,
            whitespace_examples: self.whitespace_examples,
            lexical_examples: self.lexical_examples,
            segmentation_examples: self.segmentation_examples,
            feature_diff_examples: self.feature_diff_examples,
            coverage_examples: self.coverage_examples,
        }
    }
}

fn compare_rows(
    left: &CompactSummaryRow,
    right: &CompactSummaryRow,
    sort_by: CompactSummarySort,
) -> std::cmp::Ordering {
    match sort_by {
        CompactSummarySort::BoundaryF1 => compare_boundary_f1(left, right),
        CompactSummarySort::SegmentationRegions => right
            .total_segmentation_regions
            .cmp(&left.total_segmentation_regions)
            .then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::LexicalSegmentationRegions => right
            .total_lexical_segmentation_regions
            .cmp(&left.total_lexical_segmentation_regions)
            .then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::WhitespaceSegmentationRegions => right
            .total_whitespace_segmentation_regions
            .cmp(&left.total_whitespace_segmentation_regions)
            .then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::FeatureDifferences => right
            .total_feature_difference_regions
            .cmp(&left.total_feature_difference_regions)
            .then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::LexicalFeatureDifferences => right
            .total_lexical_feature_difference_regions
            .cmp(&left.total_lexical_feature_difference_regions)
            .then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::WhitespaceFeatureDifferences => right
            .total_whitespace_feature_difference_regions
            .cmp(&left.total_whitespace_feature_difference_regions)
            .then_with(|| left.key.cmp(&right.key)),
        CompactSummarySort::CoverageMismatchRegions => right
            .total_coverage_mismatch_regions
            .cmp(&left.total_coverage_mismatch_regions)
            .then_with(|| left.key.cmp(&right.key)),
    }
}

fn compare_example_rows(
    left: &CompactExampleSummaryRow,
    right: &CompactExampleSummaryRow,
    sort_by: CompactExampleSummarySort,
) -> std::cmp::Ordering {
    let left_value = example_sort_value(left, sort_by);
    let right_value = example_sort_value(right, sort_by);
    right_value
        .cmp(&left_value)
        .then_with(|| left.key.cmp(&right.key))
}

fn example_sort_value(row: &CompactExampleSummaryRow, sort_by: CompactExampleSummarySort) -> usize {
    match sort_by {
        CompactExampleSummarySort::Examples => row.examples,
        CompactExampleSummarySort::WhitespaceExamples => row.whitespace_examples,
        CompactExampleSummarySort::LexicalExamples => row.lexical_examples,
        CompactExampleSummarySort::SegmentationExamples => row.segmentation_examples,
        CompactExampleSummarySort::FeatureDiffExamples => row.feature_diff_examples,
        CompactExampleSummarySort::CoverageExamples => row.coverage_examples,
    }
}

fn example_filter_matches(filter: CompactExampleFilter, whitespace_only: bool) -> bool {
    match filter {
        CompactExampleFilter::All => true,
        CompactExampleFilter::WhitespaceOnly => whitespace_only,
        CompactExampleFilter::LexicalOnly => !whitespace_only,
    }
}

fn is_segmentation_example(kind: &str) -> bool {
    matches!(kind, "split" | "merge" | "resegment")
}

fn compare_boundary_f1(left: &CompactSummaryRow, right: &CompactSummaryRow) -> std::cmp::Ordering {
    match (left.worst_boundary_f1, right.worst_boundary_f1) {
        (None, Some(_)) => std::cmp::Ordering::Less,
        (Some(_), None) => std::cmp::Ordering::Greater,
        (None, None) => left.key.cmp(&right.key),
        (Some(left), Some(right)) => left
            .partial_cmp(&right)
            .unwrap_or(std::cmp::Ordering::Equal),
    }
    .then_with(|| left.key.cmp(&right.key))
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;

    #[test]
    fn boundary_f1_sort_puts_null_first_then_lowest_numeric() {
        let dir = temp_dir("boundary");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("comparisons.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":11,"one_to_one_regions":8,"one_to_one_with_feature_differences":1,"segmentation_regions":2,"coverage_mismatch_regions":0,"split_regions":1,"merge_regions":1,"resegment_regions":0,"from_morphemes_in_segmentation":2,"to_morphemes_in_segmentation":3,"boundary_precision":0.9,"boundary_recall":0.8,"boundary_f1":0.847}"#, "\n",
                r#"{"source_id":"src-b","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":10,"one_to_one_regions":10,"one_to_one_with_feature_differences":0,"segmentation_regions":0,"coverage_mismatch_regions":0,"split_regions":0,"merge_regions":0,"resegment_regions":0,"from_morphemes_in_segmentation":0,"to_morphemes_in_segmentation":0,"boundary_precision":1.0,"boundary_recall":1.0,"boundary_f1":1.0}"#, "\n",
                r#"{"source_id":"src-c","text_id":"t2","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":5,"to_morphemes":5,"one_to_one_regions":5,"one_to_one_with_feature_differences":0,"segmentation_regions":0,"coverage_mismatch_regions":0,"split_regions":0,"merge_regions":0,"resegment_regions":0,"from_morphemes_in_segmentation":0,"to_morphemes_in_segmentation":0,"boundary_precision":null,"boundary_recall":null,"boundary_f1":null}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_comparisons(
            &path,
            CompactSummaryOptions {
                group_by: CompactSummaryGroupBy::SourceId,
                sort_by: CompactSummarySort::BoundaryF1,
                limit: 2,
            },
        )
        .unwrap();

        assert_eq!(
            rows.iter().map(|row| row.key.as_str()).collect::<Vec<_>>(),
            vec!["src-c", "src-a"]
        );
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_can_group_duplicate_text_ids_explicitly() {
        let dir = temp_dir("text-group");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("comparisons.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"same","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":11,"one_to_one_regions":8,"one_to_one_with_feature_differences":2,"segmentation_regions":3,"coverage_mismatch_regions":1,"split_regions":1,"merge_regions":1,"resegment_regions":1,"from_morphemes_in_segmentation":3,"to_morphemes_in_segmentation":4,"boundary_precision":0.9,"boundary_recall":0.8,"boundary_f1":0.847}"#, "\n",
                r#"{"source_id":"src-b","text_id":"same","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":12,"to_morphemes":13,"one_to_one_regions":9,"one_to_one_with_feature_differences":4,"segmentation_regions":5,"coverage_mismatch_regions":0,"split_regions":3,"merge_regions":1,"resegment_regions":1,"from_morphemes_in_segmentation":5,"to_morphemes_in_segmentation":6,"boundary_precision":0.95,"boundary_recall":0.9,"boundary_f1":0.924}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_comparisons(
            &path,
            CompactSummaryOptions {
                group_by: CompactSummaryGroupBy::TextId,
                sort_by: CompactSummarySort::SegmentationRegions,
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].key, "same");
        assert_eq!(rows[0].source_ids, vec!["src-a", "src-b"]);
        assert_eq!(rows[0].comparisons, 2);
        assert_eq!(rows[0].total_segmentation_regions, 8);
        assert_eq!(rows[0].max_segmentation_regions, 5);
        assert_eq!(rows[0].total_feature_difference_regions, 6);
        assert_eq!(rows[0].total_coverage_mismatch_regions, 1);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_can_sort_by_lexical_segmentation_regions() {
        let dir = temp_dir("lexical-sort");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("comparisons.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"whitespace-heavy","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":11,"one_to_one_regions":8,"one_to_one_with_feature_differences":1,"segmentation_regions":10,"whitespace_segmentation_regions":9,"lexical_segmentation_regions":1,"coverage_mismatch_regions":0,"split_regions":1,"merge_regions":9,"resegment_regions":0,"whitespace_feature_diff_regions":1,"lexical_feature_diff_regions":0,"from_morphemes_in_segmentation":10,"to_morphemes_in_segmentation":11,"boundary_precision":0.9,"boundary_recall":0.8,"boundary_f1":0.847}"#, "\n",
                r#"{"source_id":"lexical-heavy","text_id":"t2","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":11,"one_to_one_regions":8,"one_to_one_with_feature_differences":1,"segmentation_regions":4,"whitespace_segmentation_regions":0,"lexical_segmentation_regions":4,"coverage_mismatch_regions":0,"split_regions":4,"merge_regions":0,"resegment_regions":0,"whitespace_feature_diff_regions":0,"lexical_feature_diff_regions":1,"from_morphemes_in_segmentation":4,"to_morphemes_in_segmentation":8,"boundary_precision":0.9,"boundary_recall":0.8,"boundary_f1":0.847}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_comparisons(
            &path,
            CompactSummaryOptions {
                group_by: CompactSummaryGroupBy::SourceId,
                sort_by: CompactSummarySort::LexicalSegmentationRegions,
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows[0].key, "lexical-heavy");
        assert_eq!(rows[0].total_lexical_segmentation_regions, 4);
        assert_eq!(rows[1].key, "whitespace-heavy");
        assert_eq!(rows[1].total_whitespace_segmentation_regions, 9);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_examples_can_isolate_whitespace_only_rows() {
        let dir = temp_dir("example-whitespace");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("examples.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":0,"kind":"merge","byte_start":0,"byte_end":2,"char_start":0,"char_end":2,"source_excerpt":"\n　","from_surfaces":["\n","　"],"to_surfaces":["\n　"],"feature_changes":null}"#, "\n",
                r#"{"source_id":"src-a","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":1,"kind":"split","byte_start":2,"byte_end":8,"char_start":2,"char_end":4,"source_excerpt":"今日","from_surfaces":["今日"],"to_surfaces":["今","日"],"feature_changes":null,"whitespace_only":false}"#, "\n",
                r#"{"source_id":"src-b","text_id":"t2","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":0,"kind":"feature_diff","byte_start":0,"byte_end":6,"char_start":0,"char_end":2,"source_excerpt":"明日","from_surfaces":["明日"],"to_surfaces":["明日"],"feature_changes":[{"key":"pos","from":"名詞","to":"副詞"}],"whitespace_only":false}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_examples(
            &path,
            CompactExampleSummaryOptions {
                group_by: CompactSummaryGroupBy::SourceId,
                filter: CompactExampleFilter::WhitespaceOnly,
                sort_by: CompactExampleSummarySort::Examples,
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].key, "src-a");
        assert_eq!(rows[0].examples, 1);
        assert_eq!(rows[0].whitespace_examples, 1);
        assert_eq!(rows[0].lexical_examples, 0);
        assert_eq!(rows[0].segmentation_examples, 1);
        assert_eq!(rows[0].feature_diff_examples, 0);

        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-summary-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
