use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::io;
#[cfg(test)]
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::sync::Arc;

use anyhow::{Context, Result, bail};
use arrow_array::{
    Array, ArrayRef, BooleanArray, ListArray, RecordBatch, StringArray, UInt64Array,
};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::ArrowWriter;
use parquet::arrow::arrow_reader::{ParquetRecordBatchReader, ParquetRecordBatchReaderBuilder};
use parquet::basic::{Compression, ZstdLevel};
use parquet::file::properties::WriterProperties;
use serde::Deserialize;

use super::types::*;
use crate::compact::{ComparisonSummaryRow, is_whitespace_only};
use crate::nway::{
    NwayComparisonRow, NwayFeatureScopeRow, NwayFeatureValueGroupRow, NwayPatternCountOutputRow,
    NwayPatternCountRow, NwaySegmentationGroupRow,
};
use crate::output::for_each_jsonl_or_zst_line;
use crate::script::{ScriptCategory, classify_text};
use crate::warehouse::schema::WarehouseTable;

#[derive(Debug, Default)]
pub(crate) struct Accumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    script_categories: BTreeSet<ScriptCategory>,
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
pub(crate) struct ExampleAccumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    script_categories: BTreeSet<ScriptCategory>,
    examples: usize,
    whitespace_examples: usize,
    lexical_examples: usize,
    segmentation_examples: usize,
    feature_diff_examples: usize,
    coverage_examples: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct DifferenceKey {
    pub(crate) kind: String,
    pub(crate) from_analyzer: String,
    pub(crate) to_analyzer: String,
    pub(crate) region_kind: Option<String>,
    pub(crate) from_surfaces: Vec<String>,
    pub(crate) to_surfaces: Vec<String>,
    pub(crate) feature_key: Option<String>,
    pub(crate) feature_from: Option<String>,
    pub(crate) feature_to: Option<String>,
}

#[derive(Debug, Default)]
pub(crate) struct DifferenceAccumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    script_categories: BTreeSet<ScriptCategory>,
    examples: usize,
}

#[derive(Debug, Default)]
pub(crate) struct NwayAccumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    script_categories: BTreeSet<ScriptCategory>,
    rows: usize,
    analyzer_count: usize,
    regions: usize,
    agreement_regions: usize,
    regions_with_feature_disagreement: usize,
    regions_with_segmentation_disagreement: usize,
    regions_with_coverage_mismatch: usize,
    whitespace_regions: usize,
    lexical_regions: usize,
    unanimous_boundary_count: usize,
    variable_boundary_count: usize,
}

#[derive(Debug, Deserialize)]
pub(crate) struct NwaySummaryInputRow {
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) source_script_category: ScriptCategory,
    pub(crate) analyzer_count: usize,
    pub(crate) regions: usize,
    pub(crate) agreement_regions: usize,
    pub(crate) regions_with_feature_disagreement: usize,
    pub(crate) regions_with_segmentation_disagreement: usize,
    pub(crate) regions_with_coverage_mismatch: usize,
    pub(crate) whitespace_regions: usize,
    pub(crate) lexical_regions: usize,
    pub(crate) unanimous_boundary_count: usize,
    pub(crate) variable_boundary_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct NwayPatternKey {
    pub(crate) kind: String,
    pub(crate) segmentation_groups: Vec<NwaySegmentationGroupRow>,
    pub(crate) feature_key: Option<String>,
    pub(crate) feature_scope: Option<NwayFeatureScopeRow>,
    pub(crate) feature_values: Vec<NwayFeatureValueGroupRow>,
}

#[derive(Debug, Default)]
pub(crate) struct NwayPatternAccumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    script_categories: BTreeSet<ScriptCategory>,
    examples: usize,
}

#[derive(Debug, Default)]
pub(crate) struct WarehousePatternAccumulator {
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    examples: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WarehouseRegionKey {
    pub(super) run_id: String,
    pub(super) source_id: String,
    pub(super) text_id: String,
    pub(super) region_index: u64,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct WarehouseRegionFlags {
    pub(super) byte_start: u64,
    pub(super) byte_end: u64,
    pub(super) char_start: u64,
    pub(super) char_end: u64,
    pub(super) is_nonempty_whitespace: bool,
    pub(super) is_agreement: bool,
    pub(super) has_coverage_mismatch: bool,
    pub(super) has_segmentation_disagreement: bool,
    pub(super) has_feature_disagreement: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct WarehouseRegionAnalyzerFact {
    pub(super) key: WarehouseRegionKey,
    pub(super) analyzer_id: String,
    pub(super) covers_exactly: bool,
    pub(super) morpheme_start: u64,
    pub(super) morpheme_end: u64,
    pub(super) surfaces: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WarehouseFeatureGroupKey {
    pub(super) region: WarehouseRegionKey,
    pub(super) feature_key: String,
    pub(super) scope_type: String,
    pub(super) scope_position: Option<u64>,
    pub(super) scope_surface: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct WarehouseFeatureDiffFact {
    pub(super) key: WarehouseFeatureGroupKey,
    pub(super) feature_value: Option<String>,
    pub(super) analyzer_id: String,
}

#[derive(Debug, Clone)]
pub(crate) struct WarehouseErrorFact {
    source_id: Option<String>,
    text_id: Option<String>,
    analyzer_id: Option<String>,
    stage: String,
    error_code: String,
    message: String,
}

#[derive(Debug, Default)]
pub(crate) struct WarehouseErrorAccumulator {
    errors: usize,
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
    analyzer_ids: BTreeSet<String>,
    stages: BTreeSet<String>,
    error_codes: BTreeSet<String>,
    sample_messages: Vec<String>,
}

#[derive(Debug, Default)]
pub(crate) struct WarehousePairwiseAccumulator {
    regions: usize,
    segmentation_regions: usize,
    feature_region_keys: BTreeSet<WarehouseRegionKey>,
    coverage_regions: usize,
    unanimous_boundary_count: usize,
    variable_boundary_count: usize,
}

#[derive(Debug, Deserialize)]
pub(crate) struct ExampleSummaryInputRow {
    pub(crate) source_id: String,
    pub(crate) text_id: String,
    pub(crate) from_analyzer: String,
    pub(crate) to_analyzer: String,
    pub(crate) kind: String,
    pub(crate) source_excerpt: String,
    #[serde(default)]
    pub(crate) from_surfaces: Vec<String>,
    #[serde(default)]
    pub(crate) to_surfaces: Vec<String>,
    #[serde(default)]
    pub(crate) feature_changes: Option<Vec<FeatureChangeInputRow>>,
    #[serde(default)]
    pub(crate) whitespace_only: Option<bool>,
    #[serde(default)]
    pub(crate) script_category: Option<ScriptCategory>,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct FeatureChangeInputRow {
    pub(crate) key: String,
    pub(crate) from: Option<String>,
    pub(crate) to: Option<String>,
}

/// Summarizes compact comparisons by source or text ID.
///
/// # Errors
///
/// Returns an error when comparison input lines cannot be read or parsed as JSON.
pub fn summarize_compact_comparisons(
    comparisons_path: &Path,
    options: CompactSummaryOptions,
) -> Result<Vec<CompactSummaryRow>> {
    let mut groups = BTreeMap::<String, Accumulator>::new();
    for_each_jsonl_or_zst_line(comparisons_path, |line| {
        let row: ComparisonSummaryRow = serde_json::from_str(line)?;
        if options.exclusions.excludes(&row.source_id, &row.text_id) {
            return Ok(());
        }
        if options
            .script_category
            .is_some_and(|category| row.source_script_category != category)
        {
            return Ok(());
        }
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

/// Summarizes compact example rows by source or text ID.
///
/// # Errors
///
/// Returns an error when comparison input lines cannot be read or parsed as JSON.
pub fn summarize_compact_examples(
    examples_path: &Path,
    options: CompactExampleSummaryOptions,
) -> Result<Vec<CompactExampleSummaryRow>> {
    let mut groups = BTreeMap::<String, ExampleAccumulator>::new();
    for_each_jsonl_or_zst_line(examples_path, |line| {
        let row: ExampleSummaryInputRow = serde_json::from_str(line)?;
        if options.exclusions.excludes(&row.source_id, &row.text_id) {
            return Ok(());
        }
        let whitespace_only = row
            .whitespace_only
            .unwrap_or_else(|| is_whitespace_only(&row.source_excerpt));
        let script_category = row
            .script_category
            .unwrap_or_else(|| classify_text(&row.source_excerpt));
        if options
            .script_category
            .is_some_and(|category| script_category != category)
        {
            return Ok(());
        }
        if !example_filter_matches(options.filter, whitespace_only) {
            return Ok(());
        }
        let key = match options.group_by {
            CompactSummaryGroupBy::SourceId => row.source_id.clone(),
            CompactSummaryGroupBy::TextId => row.text_id.clone(),
        };
        groups
            .entry(key)
            .or_default()
            .push(row, whitespace_only, script_category);
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

/// Summarizes compact differences into grouped difference rows.
///
/// # Errors
///
/// Returns an error when comparison input lines cannot be read or parsed as JSON.
pub fn summarize_compact_differences(
    examples_path: &Path,
    options: CompactDifferenceSummaryOptions,
) -> Result<Vec<CompactDifferenceSummaryRow>> {
    let mut groups = BTreeMap::<DifferenceKey, DifferenceAccumulator>::new();
    for_each_jsonl_or_zst_line(examples_path, |line| {
        let row: ExampleSummaryInputRow = serde_json::from_str(line)?;
        if options.exclusions.excludes(&row.source_id, &row.text_id) {
            return Ok(());
        }
        let whitespace_only = row
            .whitespace_only
            .unwrap_or_else(|| is_whitespace_only(&row.source_excerpt));
        let script_category = row
            .script_category
            .unwrap_or_else(|| classify_text(&row.source_excerpt));
        if options
            .script_category
            .is_some_and(|category| script_category != category)
        {
            return Ok(());
        }
        if !example_filter_matches(options.filter, whitespace_only) {
            return Ok(());
        }
        if options.one_to_one_lexical_features && (whitespace_only || row.kind != "feature_diff") {
            return Ok(());
        }

        if !options.one_to_one_lexical_features
            && matches!(
                options.kind,
                CompactDifferenceKindFilter::All | CompactDifferenceKindFilter::Segmentation
            )
            && is_segmentation_example(&row.kind)
        {
            let key = DifferenceKey {
                kind: "segmentation".to_owned(),
                from_analyzer: row.from_analyzer.clone(),
                to_analyzer: row.to_analyzer.clone(),
                region_kind: Some(row.kind.clone()),
                from_surfaces: row.from_surfaces.clone(),
                to_surfaces: row.to_surfaces.clone(),
                feature_key: None,
                feature_from: None,
                feature_to: None,
            };
            groups.entry(key).or_default().push(&row, script_category);
        }

        if matches!(
            options.kind,
            CompactDifferenceKindFilter::All | CompactDifferenceKindFilter::Feature
        ) && row.kind == "feature_diff"
        {
            for change in row.feature_changes.as_deref().unwrap_or(&[]) {
                if options
                    .feature_key
                    .as_ref()
                    .is_some_and(|wanted| change.key != *wanted)
                {
                    continue;
                }
                if feature_change_has_excluded_value(change, &options.excluded_feature_values) {
                    continue;
                }
                let key = DifferenceKey {
                    kind: "feature".to_owned(),
                    from_analyzer: row.from_analyzer.clone(),
                    to_analyzer: row.to_analyzer.clone(),
                    region_kind: None,
                    from_surfaces: Vec::new(),
                    to_surfaces: Vec::new(),
                    feature_key: Some(change.key.clone()),
                    feature_from: change.from.clone(),
                    feature_to: change.to.clone(),
                };
                groups.entry(key).or_default().push(&row, script_category);
            }
        }

        Ok(())
    })?;

    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| {
        right
            .examples
            .cmp(&left.examples)
            .then_with(|| left.kind.cmp(&right.kind))
            .then_with(|| left.feature_key.cmp(&right.feature_key))
            .then_with(|| left.region_kind.cmp(&right.region_kind))
            .then_with(|| left.from_surfaces.cmp(&right.from_surfaces))
            .then_with(|| left.to_surfaces.cmp(&right.to_surfaces))
            .then_with(|| left.feature_from.cmp(&right.feature_from))
            .then_with(|| left.feature_to.cmp(&right.feature_to))
    });
    rows.truncate(options.limit);
    Ok(rows)
}

/// Summarizes N-way comparison rows.
///
/// # Errors
///
/// Returns an error when N-way input lines cannot be read or parsed as JSON.
pub fn summarize_nway(
    nway_path: &Path,
    options: NwaySummaryOptions,
) -> Result<Vec<NwaySummaryRow>> {
    let mut groups = BTreeMap::<String, NwayAccumulator>::new();
    for_each_jsonl_or_zst_line(nway_path, |line| {
        let row: NwaySummaryInputRow = serde_json::from_str(line)?;
        if options.exclusions.excludes(&row.source_id, &row.text_id) {
            return Ok(());
        }
        if options
            .script_category
            .is_some_and(|category| row.source_script_category != category)
        {
            return Ok(());
        }
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
    rows.sort_by(|left, right| compare_nway_rows(left, right, options.sort_by));
    rows.truncate(options.limit);
    Ok(rows)
}

/// Summarizes N-way pattern rows from compact outputs.
///
/// # Errors
///
/// Returns an error when pattern rows cannot be read or parsed as JSON.
pub fn summarize_nway_patterns(
    nway_path: &Path,
    options: NwayPatternOptions,
) -> Result<Vec<NwayPatternRow>> {
    let mut groups = BTreeMap::<NwayPatternKey, NwayPatternAccumulator>::new();
    for_each_jsonl_or_zst_line(nway_path, |line| {
        let row: NwayComparisonRow = serde_json::from_str(line)?;
        if options.exclusions.excludes(&row.source_id, &row.text_id) {
            return Ok(());
        }
        if options
            .script_category
            .is_some_and(|category| row.source_script_category != category)
        {
            return Ok(());
        }
        if !row.pattern_counts.is_empty() {
            for pattern_count in &row.pattern_counts {
                push_nway_pattern_count(&mut groups, &row, pattern_count, &options);
            }
            return Ok(());
        }
        for example in &row.examples {
            match options.kind {
                NwayPatternKind::Segmentation => {
                    if example.segmentation_groups.len() <= 1 {
                        continue;
                    }
                    let mut segmentation_groups = example.segmentation_groups.clone();
                    canonicalize_segmentation_groups(&mut segmentation_groups);
                    let key = NwayPatternKey {
                        kind: "segmentation".to_owned(),
                        segmentation_groups,
                        feature_key: None,
                        feature_scope: None,
                        feature_values: Vec::new(),
                    };
                    groups
                        .entry(key)
                        .or_default()
                        .push(&row, row.source_script_category);
                }
                NwayPatternKind::Feature => {
                    for feature_group in &example.feature_groups {
                        if options
                            .feature_key
                            .as_ref()
                            .is_some_and(|wanted| *wanted != feature_group.key)
                        {
                            continue;
                        }
                        if feature_group.values.len() <= 1 {
                            continue;
                        }
                        let mut values = filtered_feature_values(
                            &feature_group.values,
                            &options.excluded_feature_values,
                        );
                        if values.len() <= 1 {
                            continue;
                        }
                        canonicalize_feature_values(&mut values);
                        let key = NwayPatternKey {
                            kind: "feature".to_owned(),
                            segmentation_groups: Vec::new(),
                            feature_key: Some(feature_group.key.clone()),
                            feature_scope: Some(feature_group.scope.clone()),
                            feature_values: values,
                        };
                        groups.entry(key).or_default().push_count(
                            &row,
                            row.source_script_category,
                            1,
                        );
                    }
                }
            }
        }
        Ok(())
    })?;
    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| {
        right
            .examples
            .cmp(&left.examples)
            .then_with(|| left.pattern.cmp(&right.pattern))
    });
    rows.truncate(options.limit);
    Ok(rows)
}

/// Summarizes N-way pattern-count output rows.
///
/// # Errors
///
/// Returns an error when pattern-count rows cannot be read or parsed as JSON.
pub fn summarize_nway_pattern_counts(
    pattern_counts_path: &Path,
    options: NwayPatternOptions,
) -> Result<Vec<NwayPatternRow>> {
    let mut groups = BTreeMap::<NwayPatternKey, NwayPatternAccumulator>::new();
    for_each_jsonl_or_zst_line(pattern_counts_path, |line| {
        let row: NwayPatternCountOutputRow = serde_json::from_str(line)?;
        if options.exclusions.excludes(&row.source_id, &row.text_id) {
            return Ok(());
        }
        if options
            .script_category
            .is_some_and(|category| row.source_script_category != category)
        {
            return Ok(());
        }
        push_nway_pattern_output_count(&mut groups, &row, &options);
        Ok(())
    })?;
    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| {
        right
            .examples
            .cmp(&left.examples)
            .then_with(|| left.pattern.cmp(&right.pattern))
    });
    rows.truncate(options.limit);
    Ok(rows)
}

/// Summarizes N-way results from warehouse output.
///
/// # Errors
///
/// Returns an error when required warehouse tables are missing or unreadable.
pub fn summarize_warehouse_nway(
    run_dir: &Path,
    options: NwaySummaryOptions,
) -> Result<Vec<NwaySummaryRow>> {
    if options.script_category.is_some() {
        bail!(
            "warehouse N-way summaries do not support --script-category in phase 1; warehouse facts do not store script categories"
        );
    }
    let analyzer_counts = read_warehouse_analysis_counts(run_dir)?;
    let boundary_counts = read_warehouse_boundary_counts(run_dir)?;
    let mut groups = BTreeMap::<String, NwayAccumulator>::new();
    for (region, flags) in read_warehouse_region_flags(run_dir)? {
        if options
            .exclusions
            .excludes(&region.source_id, &region.text_id)
        {
            continue;
        }
        let key = match options.group_by {
            CompactSummaryGroupBy::SourceId => region.source_id.clone(),
            CompactSummaryGroupBy::TextId => region.text_id.clone(),
        };
        groups
            .entry(key)
            .or_default()
            .push_warehouse_region(&region, flags);
    }
    for accumulator in groups.values_mut() {
        let source_ids = accumulator.source_ids.iter().cloned().collect::<Vec<_>>();
        for source_id in source_ids {
            accumulator.analyzer_count = accumulator
                .analyzer_count
                .max(analyzer_counts.get(&source_id).copied().unwrap_or_default());
            if let Some((unanimous, variable)) = boundary_counts.get(&source_id).copied() {
                accumulator.unanimous_boundary_count += unanimous;
                accumulator.variable_boundary_count += variable;
            }
        }
    }
    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| compare_nway_rows(left, right, options.sort_by));
    rows.truncate(options.limit);
    Ok(rows)
}

/// Summarizes warehouse region-level rows.
///
/// # Errors
///
/// Returns an error when warehouse region facts cannot be loaded.
pub fn summarize_warehouse_regions(
    run_dir: &Path,
    options: WarehouseRegionOptions,
) -> Result<Vec<WarehouseRegionExampleRow>> {
    let mut analyzers_by_region =
        BTreeMap::<WarehouseRegionKey, Vec<WarehouseRegionAnalyzerFact>>::new();
    for fact in read_warehouse_region_analyzers(run_dir)? {
        analyzers_by_region
            .entry(fact.key.clone())
            .or_default()
            .push(fact);
    }
    for analyzers in analyzers_by_region.values_mut() {
        analyzers.sort_by(|left, right| left.analyzer_id.cmp(&right.analyzer_id));
    }

    let mut features_by_region =
        BTreeMap::<WarehouseRegionKey, Vec<WarehouseFeatureDiffFact>>::new();
    for fact in read_warehouse_feature_diffs(run_dir)? {
        features_by_region
            .entry(fact.key.region.clone())
            .or_default()
            .push(fact);
    }

    let mut rows = Vec::new();
    for (region, flags) in read_warehouse_region_flags(run_dir)? {
        if options
            .exclusions
            .excludes(&region.source_id, &region.text_id)
        {
            continue;
        }
        if !warehouse_region_kind_matches(options.kind, flags) {
            continue;
        }
        if !warehouse_text_filter_matches(options.text_filter, flags) {
            continue;
        }
        let analyzers = analyzers_by_region
            .remove(&region)
            .unwrap_or_default()
            .into_iter()
            .map(|fact| WarehouseRegionAnalyzerExampleRow {
                analyzer_id: fact.analyzer_id,
                covers_exactly: fact.covers_exactly,
                morpheme_start: fact.morpheme_start,
                morpheme_end: fact.morpheme_end,
                surfaces: fact.surfaces,
            })
            .collect();
        let mut feature_diffs = features_by_region
            .remove(&region)
            .unwrap_or_default()
            .into_iter()
            .map(|fact| WarehouseFeatureDiffExampleRow {
                feature_key: fact.key.feature_key,
                scope_type: fact.key.scope_type,
                scope_position: fact.key.scope_position,
                scope_surface: fact.key.scope_surface,
                feature_value: fact.feature_value,
                analyzer_id: fact.analyzer_id,
            })
            .collect::<Vec<_>>();
        feature_diffs.sort_by(|left, right| {
            left.feature_key
                .cmp(&right.feature_key)
                .then_with(|| left.scope_type.cmp(&right.scope_type))
                .then_with(|| left.scope_position.cmp(&right.scope_position))
                .then_with(|| left.scope_surface.cmp(&right.scope_surface))
                .then_with(|| left.feature_value.cmp(&right.feature_value))
                .then_with(|| left.analyzer_id.cmp(&right.analyzer_id))
        });
        rows.push(WarehouseRegionExampleRow {
            source_id: region.source_id,
            text_id: region.text_id,
            region_index: region.region_index,
            byte_start: flags.byte_start,
            byte_end: flags.byte_end,
            char_start: flags.char_start,
            char_end: flags.char_end,
            is_nonempty_whitespace: flags.is_nonempty_whitespace,
            is_agreement: flags.is_agreement,
            has_coverage_mismatch: flags.has_coverage_mismatch,
            has_segmentation_disagreement: flags.has_segmentation_disagreement,
            has_feature_disagreement: flags.has_feature_disagreement,
            analyzers,
            feature_diffs,
        });
        if rows.len() >= options.limit {
            break;
        }
    }
    Ok(rows)
}

/// Summarizes warehouse error rows.
///
/// # Errors
///
/// Returns an error when warehouse error facts cannot be loaded.
pub fn summarize_warehouse_errors(
    run_dir: &Path,
    options: WarehouseErrorSummaryOptions,
) -> Result<Vec<WarehouseErrorSummaryRow>> {
    let mut groups = BTreeMap::<String, WarehouseErrorAccumulator>::new();
    for fact in read_warehouse_errors(run_dir)? {
        if options.exclusions.excludes(
            fact.source_id.as_deref().unwrap_or_default(),
            fact.text_id.as_deref().unwrap_or_default(),
        ) {
            continue;
        }
        let key = match options.group_by {
            WarehouseErrorGroupBy::ErrorCode => fact.error_code.clone(),
            WarehouseErrorGroupBy::Stage => fact.stage.clone(),
            WarehouseErrorGroupBy::Analyzer => fact
                .analyzer_id
                .clone()
                .unwrap_or_else(|| "<none>".to_owned()),
            WarehouseErrorGroupBy::SourceId => fact
                .source_id
                .clone()
                .unwrap_or_else(|| "<none>".to_owned()),
        };
        groups.entry(key).or_default().push(fact);
    }
    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| {
        right
            .errors
            .cmp(&left.errors)
            .then_with(|| left.key.cmp(&right.key))
    });
    rows.truncate(options.limit);
    Ok(rows)
}

/// Summarizes pairwise agreement metrics from warehouse rows.
///
/// # Errors
///
/// Returns an error when warehouse boundary and feature facts cannot be loaded.
pub fn summarize_warehouse_pairwise(
    run_dir: &Path,
    options: WarehousePairwiseSummaryOptions,
) -> Result<Vec<WarehousePairwiseSummaryRow>> {
    let source_text_ids = read_warehouse_source_text_ids(run_dir)?;
    let boundary_sets = read_warehouse_boundary_sets(run_dir)?;
    let mut groups = BTreeMap::<WarehousePairwiseKey, WarehousePairwiseAccumulator>::new();

    for (source_id, analyzer_sets) in &boundary_sets {
        let text_id = source_text_ids
            .get(source_id)
            .cloned()
            .unwrap_or_else(|| source_id.clone());
        if options.exclusions.excludes(source_id, &text_id) {
            continue;
        }
        let analyzers = analyzer_sets.keys().cloned().collect::<Vec<_>>();
        for left_index in 0..analyzers.len() {
            for right_index in (left_index + 1)..analyzers.len() {
                let key = warehouse_pairwise_key(
                    source_id,
                    &text_id,
                    &analyzers[left_index],
                    &analyzers[right_index],
                );
                let accumulator = groups.entry(key).or_default();
                let left_boundaries = &analyzer_sets[&analyzers[left_index]];
                let right_boundaries = &analyzer_sets[&analyzers[right_index]];
                let all_boundaries = left_boundaries
                    .union(right_boundaries)
                    .copied()
                    .collect::<BTreeSet<_>>();
                let unanimous = all_boundaries
                    .iter()
                    .filter(|boundary| {
                        left_boundaries.contains(boundary) && right_boundaries.contains(boundary)
                    })
                    .count();
                accumulator.unanimous_boundary_count += unanimous;
                accumulator.variable_boundary_count +=
                    all_boundaries.len().saturating_sub(unanimous);
            }
        }
    }

    let mut analyzers_by_region =
        BTreeMap::<WarehouseRegionKey, Vec<WarehouseRegionAnalyzerFact>>::new();
    for fact in read_warehouse_region_analyzers(run_dir)? {
        if options
            .exclusions
            .excludes(&fact.key.source_id, &fact.key.text_id)
        {
            continue;
        }
        analyzers_by_region
            .entry(fact.key.clone())
            .or_default()
            .push(fact);
    }
    let regions = read_warehouse_region_flags(run_dir)?;
    for (region, mut analyzers) in analyzers_by_region {
        let Some(flags) = regions.get(&region).copied() else {
            continue;
        };
        if !warehouse_text_filter_matches(options.text_filter, flags) {
            continue;
        }
        analyzers.sort_by(|left, right| left.analyzer_id.cmp(&right.analyzer_id));
        for left_index in 0..analyzers.len() {
            for right_index in (left_index + 1)..analyzers.len() {
                let left = &analyzers[left_index];
                let right = &analyzers[right_index];
                let accumulator = groups
                    .entry(warehouse_pairwise_key(
                        &region.source_id,
                        &region.text_id,
                        &left.analyzer_id,
                        &right.analyzer_id,
                    ))
                    .or_default();
                accumulator.regions += 1;
                if flags.has_coverage_mismatch || !left.covers_exactly || !right.covers_exactly {
                    accumulator.coverage_regions += 1;
                }
                if flags.has_segmentation_disagreement
                    && left.covers_exactly
                    && right.covers_exactly
                    && left.surfaces != right.surfaces
                {
                    accumulator.segmentation_regions += 1;
                }
            }
        }
    }

    let mut feature_groups =
        BTreeMap::<WarehouseFeatureGroupKey, BTreeMap<String, Option<String>>>::new();
    for fact in read_warehouse_feature_diffs(run_dir)? {
        if options
            .exclusions
            .excludes(&fact.key.region.source_id, &fact.key.region.text_id)
        {
            continue;
        }
        if !regions
            .get(&fact.key.region)
            .copied()
            .is_some_and(|flags| warehouse_text_filter_matches(options.text_filter, flags))
        {
            continue;
        }
        feature_groups
            .entry(fact.key)
            .or_default()
            .insert(fact.analyzer_id, fact.feature_value);
    }
    for (feature, values_by_analyzer) in feature_groups {
        let analyzers = values_by_analyzer.keys().cloned().collect::<Vec<_>>();
        for left_index in 0..analyzers.len() {
            for right_index in (left_index + 1)..analyzers.len() {
                let left = &analyzers[left_index];
                let right = &analyzers[right_index];
                if values_by_analyzer[left] == values_by_analyzer[right] {
                    continue;
                }
                groups
                    .entry(warehouse_pairwise_key(
                        &feature.region.source_id,
                        &feature.region.text_id,
                        left,
                        right,
                    ))
                    .or_default()
                    .feature_region_keys
                    .insert(feature.region.clone());
            }
        }
    }

    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| compare_warehouse_pairwise_rows(left, right, options.sort_by));
    rows.truncate(options.limit);
    Ok(rows)
}

/// Summarizes warehouse N-way pattern rows.
///
/// # Errors
///
/// Returns an error when required warehouse data is unavailable or unparsable.
pub fn summarize_warehouse_nway_patterns(
    run_dir: &Path,
    options: WarehousePatternOptions,
) -> Result<Vec<NwayPatternRow>> {
    if warehouse_feature_pattern_counts_available(run_dir, &options) {
        return summarize_materialized_warehouse_feature_pattern_counts(run_dir, &options);
    }
    let regions = read_warehouse_region_flags(run_dir)?;
    let groups = match options.kind {
        NwayPatternKind::Segmentation => {
            summarize_warehouse_segmentation_patterns(run_dir, &regions, &options)?
        }
        NwayPatternKind::Feature => {
            summarize_warehouse_feature_patterns(run_dir, &regions, &options)?
        }
    };
    let mut rows = groups
        .into_iter()
        .map(|(key, accumulator)| accumulator.into_row(key))
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| {
        right
            .examples
            .cmp(&left.examples)
            .then_with(|| left.pattern.cmp(&right.pattern))
    });
    rows.truncate(options.limit);
    Ok(rows)
}

fn summarize_materialized_warehouse_feature_pattern_counts(
    run_dir: &Path,
    options: &WarehousePatternOptions,
) -> Result<Vec<NwayPatternRow>> {
    let mut groups = BTreeMap::<
        (String, String),
        (usize, BTreeSet<String>, BTreeSet<String>, BTreeSet<String>),
    >::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::FeaturePatternCounts)? {
        let kind = string_column(&batch, 0)?;
        let feature_profile = string_column(&batch, 1)?;
        let feature_key = string_column(&batch, 2)?;
        let is_nonempty_whitespace = bool_column(&batch, 3)?;
        let pattern = string_column(&batch, 4)?;
        let examples = u64_column(&batch, 5)?;
        let sample_source_ids = string_column(&batch, 8)?;
        let sample_text_ids = string_column(&batch, 9)?;
        let script_categories = string_column(&batch, 10)?;
        for row in 0..batch.num_rows() {
            let is_nonempty_whitespace = is_nonempty_whitespace.value(row);
            if kind.value(row) != "feature"
                || feature_profile.value(row) != "core"
                || !warehouse_text_filter_matches_nonempty_whitespace(
                    options.text_filter,
                    is_nonempty_whitespace,
                )
                || options
                    .feature_key
                    .as_ref()
                    .is_some_and(|wanted| feature_key.value(row) != wanted)
            {
                continue;
            }
            let entry = groups
                .entry((
                    feature_key.value(row).to_owned(),
                    pattern.value(row).to_owned(),
                ))
                .or_default();
            entry.0 += examples.value(row) as usize;
            entry
                .1
                .extend(split_materialized_sample_ids(sample_source_ids.value(row)));
            entry
                .2
                .extend(split_materialized_sample_ids(sample_text_ids.value(row)));
            entry
                .3
                .extend(split_materialized_sample_ids(script_categories.value(row)));
        }
    }
    let mut rows = groups
        .into_iter()
        .map(
            |((feature_key, pattern), (examples, source_ids, text_ids, script_categories))| {
                NwayPatternRow {
                    kind: "feature".to_owned(),
                    pattern,
                    examples,
                    source_ids: source_ids.into_iter().collect(),
                    text_ids: text_ids.into_iter().collect(),
                    script_categories: script_categories.into_iter().collect(),
                    segmentation_groups: Vec::new(),
                    feature_key: Some(feature_key),
                    feature_scope: None,
                    feature_values: Vec::new(),
                }
            },
        )
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| {
        right
            .examples
            .cmp(&left.examples)
            .then_with(|| left.pattern.cmp(&right.pattern))
    });
    rows.truncate(options.limit);
    Ok(rows)
}

fn split_materialized_sample_ids(value: &str) -> Vec<String> {
    value
        .split(',')
        .filter(|part| !part.is_empty())
        .map(str::to_owned)
        .collect()
}

/// Writes materialized feature-pattern parquet artifacts for the warehouse.
///
/// # Errors
///
/// Returns an error when existing files cannot be removed, directories cannot be
/// created, or the materialization process fails.
pub fn materialize_warehouse_core_feature_pattern_counts(
    run_dir: &Path,
    feature_key: Option<&str>,
) -> Result<bool> {
    let output_path = run_dir.join(WarehouseTable::FeaturePatternCounts.file_name());
    if output_path.exists() && !output_path.is_dir() {
        fs::remove_file(&output_path)
            .with_context(|| format!("failed to remove {}", output_path.display()))?;
    }
    if feature_key.is_none() && output_path.exists() {
        fs::remove_dir_all(&output_path)
            .with_context(|| format!("failed to remove {}", output_path.display()))?;
    }
    fs::create_dir_all(&output_path)
        .with_context(|| format!("failed to create {}", output_path.display()))?;
    let feature_keys = feature_key
        .map(|key| vec![key.to_owned()])
        .unwrap_or_else(|| {
            WAREHOUSE_CORE_FEATURE_KEYS
                .iter()
                .map(|key| (*key).to_owned())
                .collect()
        });
    let mut used_duckdb = false;
    for feature_key in &feature_keys {
        let part_path = output_path.join(format!("{feature_key}.parquet"));
        if part_path.exists() {
            fs::remove_file(&part_path)
                .with_context(|| format!("failed to remove {}", part_path.display()))?;
        }
        let sql = materialize_core_feature_pattern_counts_duckdb_sql(
            run_dir,
            &part_path,
            feature_key.as_str(),
        );
        if !run_duckdb_statement(run_dir, sql, "warehouse feature pattern materialization")? {
            break;
        }
        used_duckdb = true;
    }
    if used_duckdb {
        return Ok(true);
    }

    let mut accumulator = FeaturePatternMaterializer::default();
    for (regions, features) in paired_region_feature_part_paths(run_dir)? {
        materialize_core_feature_pattern_counts_part(
            &regions,
            &features,
            &feature_keys,
            &mut accumulator,
        )
        .with_context(|| {
            format!(
                "failed to materialize feature patterns from {} and {}",
                regions.display(),
                features.display()
            )
        })?;
    }
    write_feature_pattern_count_parts(&output_path, accumulator)?;
    Ok(true)
}

#[cfg(test)]
fn write_merged_pattern_tsv<'a, W: Write>(
    chunks: impl IntoIterator<Item = &'a str>,
    limit: usize,
    writer: &mut W,
) -> Result<()> {
    super::write::write_merged_pattern_tsv(chunks, limit, writer)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct MaterializedRegionKey {
    source_id: String,
    text_id: String,
    region_index: u64,
}

#[derive(Debug, Clone)]
struct MaterializedRegionRow {
    key: MaterializedRegionKey,
    is_nonempty_whitespace: bool,
}

#[derive(Debug, Clone)]
struct MaterializedFeatureRow {
    region: MaterializedRegionKey,
    feature_key: String,
    scope_type: String,
    scope_position: Option<u64>,
    scope_surface: Option<String>,
    feature_value: Option<String>,
    analyzer_id: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct MaterializedFeatureGroupKey {
    feature_key: String,
    scope_type: String,
    scope_position: Option<u64>,
    scope_surface: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct MaterializedPatternKey {
    feature_key: String,
    is_nonempty_whitespace: bool,
    pattern: String,
}

#[derive(Debug, Default)]
struct MaterializedPatternAccumulator {
    examples: u64,
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
}

#[derive(Debug, Default)]
struct FeaturePatternMaterializer {
    patterns: BTreeMap<MaterializedPatternKey, MaterializedPatternAccumulator>,
}

fn materialize_core_feature_pattern_counts_part(
    regions_path: &Path,
    features_path: &Path,
    feature_keys: &[String],
    accumulator: &mut FeaturePatternMaterializer,
) -> Result<()> {
    let feature_keys = feature_keys
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let mut regions = MaterializedRegionIter::new(regions_path)?;
    let mut current_region = regions.next_row()?;
    let mut features = MaterializedFeatureIter::new(features_path)?;
    let mut current_features = Vec::new();
    let mut current_key = None::<MaterializedRegionKey>;

    while let Some(feature) = features.next_row()? {
        if !feature_keys.contains(feature.feature_key.as_str())
            || !warehouse_feature_key_in_profile(
                &feature.feature_key,
                WarehouseFeatureProfile::Core,
            )
        {
            continue;
        }
        match &current_key {
            Some(key) if *key == feature.region => current_features.push(feature),
            Some(_) => {
                flush_materialized_region_features(
                    &mut current_region,
                    &mut regions,
                    current_key.take().expect("current key exists"),
                    &mut current_features,
                    accumulator,
                )?;
                current_key = Some(feature.region.clone());
                current_features.push(feature);
            }
            None => {
                current_key = Some(feature.region.clone());
                current_features.push(feature);
            }
        }
    }

    if let Some(key) = current_key {
        flush_materialized_region_features(
            &mut current_region,
            &mut regions,
            key,
            &mut current_features,
            accumulator,
        )?;
    }
    Ok(())
}

fn flush_materialized_region_features(
    current_region: &mut Option<MaterializedRegionRow>,
    regions: &mut MaterializedRegionIter,
    feature_region: MaterializedRegionKey,
    features: &mut Vec<MaterializedFeatureRow>,
    accumulator: &mut FeaturePatternMaterializer,
) -> Result<()> {
    while current_region
        .as_ref()
        .is_some_and(|region| region.key < feature_region)
    {
        *current_region = regions.next_row()?;
    }
    let Some(region) = current_region.as_ref() else {
        features.clear();
        return Ok(());
    };
    if region.key != feature_region {
        features.clear();
        return Ok(());
    }

    let mut by_group = BTreeMap::<MaterializedFeatureGroupKey, Vec<MaterializedFeatureRow>>::new();
    for feature in features.drain(..) {
        by_group
            .entry(MaterializedFeatureGroupKey {
                feature_key: feature.feature_key.clone(),
                scope_type: feature.scope_type.clone(),
                scope_position: feature.scope_position,
                scope_surface: feature.scope_surface.clone(),
            })
            .or_default()
            .push(feature);
    }

    for (group, facts) in by_group {
        if let Some(pattern) = materialized_feature_pattern(&group, &facts) {
            let key = MaterializedPatternKey {
                feature_key: group.feature_key,
                is_nonempty_whitespace: region.is_nonempty_whitespace,
                pattern,
            };
            let entry = accumulator.patterns.entry(key).or_default();
            entry.examples += 1;
            entry.source_ids.insert(feature_region.source_id.clone());
            entry.text_ids.insert(feature_region.text_id.clone());
        }
    }
    Ok(())
}

fn materialized_feature_pattern(
    group: &MaterializedFeatureGroupKey,
    facts: &[MaterializedFeatureRow],
) -> Option<String> {
    let mut by_value = BTreeMap::<Option<String>, Vec<String>>::new();
    for fact in facts {
        by_value
            .entry(fact.feature_value.clone())
            .or_default()
            .push(fact.analyzer_id.clone());
    }
    if by_value.len() <= 1 {
        return None;
    }
    let values = by_value
        .into_iter()
        .map(|(value, mut analyzers)| {
            analyzers.sort();
            format!("{}=>{}", value.unwrap_or_default(), analyzers.join("+"))
        })
        .collect::<Vec<_>>()
        .join(" ; ");
    Some(format!(
        "{} {} {}",
        group.feature_key,
        materialized_scope_label(group),
        values
    ))
}

fn materialized_scope_label(group: &MaterializedFeatureGroupKey) -> String {
    match group.scope_type.as_str() {
        "whole_region" => "whole_region".to_owned(),
        "token_position" => format!(
            "token_position:{}",
            group.scope_position.unwrap_or_default()
        ),
        "surface" => format!(
            "surface:{}",
            group.scope_surface.as_deref().unwrap_or_default()
        ),
        other => other.to_owned(),
    }
}

fn write_feature_pattern_count_parts(
    output_dir: &Path,
    materializer: FeaturePatternMaterializer,
) -> Result<()> {
    let mut rows_by_feature = BTreeMap::<String, Vec<_>>::new();
    for row in materializer.patterns {
        rows_by_feature
            .entry(row.0.feature_key.clone())
            .or_default()
            .push(row);
    }
    for (feature_key, rows) in rows_by_feature {
        write_feature_pattern_count_part(output_dir, &feature_key, &rows)?;
    }
    Ok(())
}

fn write_feature_pattern_count_part(
    output_dir: &Path,
    feature_key: &str,
    rows: &[(MaterializedPatternKey, MaterializedPatternAccumulator)],
) -> Result<()> {
    let path = output_dir.join(format!("{feature_key}.parquet"));
    let mut writer = ArrowWriter::try_new(
        File::create(&path).with_context(|| format!("failed to create {}", path.display()))?,
        feature_pattern_counts_schema(),
        Some(
            WriterProperties::builder()
                .set_compression(Compression::ZSTD(
                    ZstdLevel::try_new(3).expect("valid zstd level"),
                ))
                .build(),
        ),
    )?;
    for chunk in rows.chunks(50_000) {
        let batch = RecordBatch::try_new(
            feature_pattern_counts_schema(),
            vec![
                Arc::new(StringArray::from_iter_values(
                    chunk.iter().map(|_| "feature"),
                )) as ArrayRef,
                Arc::new(StringArray::from_iter_values(chunk.iter().map(|_| "core"))) as ArrayRef,
                Arc::new(StringArray::from_iter_values(
                    chunk.iter().map(|(key, _)| key.feature_key.as_str()),
                )) as ArrayRef,
                Arc::new(BooleanArray::from_iter(
                    chunk
                        .iter()
                        .map(|(key, _)| Some(key.is_nonempty_whitespace)),
                )) as ArrayRef,
                Arc::new(StringArray::from_iter_values(
                    chunk.iter().map(|(key, _)| key.pattern.as_str()),
                )) as ArrayRef,
                Arc::new(UInt64Array::from_iter_values(
                    chunk.iter().map(|(_, acc)| acc.examples),
                )) as ArrayRef,
                Arc::new(UInt64Array::from_iter_values(
                    chunk.iter().map(|(_, acc)| acc.source_ids.len() as u64),
                )) as ArrayRef,
                Arc::new(UInt64Array::from_iter_values(
                    chunk.iter().map(|(_, acc)| acc.text_ids.len() as u64),
                )) as ArrayRef,
                Arc::new(StringArray::from_iter_values(
                    chunk
                        .iter()
                        .map(|(_, acc)| materialized_sample_ids(&acc.source_ids)),
                )) as ArrayRef,
                Arc::new(StringArray::from_iter_values(
                    chunk
                        .iter()
                        .map(|(_, acc)| materialized_sample_ids(&acc.text_ids)),
                )) as ArrayRef,
                Arc::new(StringArray::from_iter_values(chunk.iter().map(|_| ""))) as ArrayRef,
            ],
        )?;
        writer.write(&batch)?;
    }
    writer.close()?;
    Ok(())
}

fn materialized_sample_ids(ids: &BTreeSet<String>) -> String {
    let mut sample = ids.iter().take(5).cloned().collect::<Vec<_>>().join(",");
    if ids.len() > 5 {
        sample.push_str(&format!(",...+{}", ids.len() - 5));
    }
    sample
}

fn feature_pattern_counts_schema() -> Arc<Schema> {
    Arc::new(Schema::new(vec![
        Field::new("kind", DataType::Utf8, false),
        Field::new("feature_profile", DataType::Utf8, false),
        Field::new("feature_key", DataType::Utf8, false),
        Field::new("is_nonempty_whitespace", DataType::Boolean, false),
        Field::new("pattern", DataType::Utf8, false),
        Field::new("examples", DataType::UInt64, false),
        Field::new("source_count", DataType::UInt64, false),
        Field::new("text_count", DataType::UInt64, false),
        Field::new("sample_source_ids", DataType::Utf8, false),
        Field::new("sample_text_ids", DataType::Utf8, false),
        Field::new("script_categories", DataType::Utf8, false),
    ]))
}

struct MaterializedRegionIter {
    reader: ParquetRecordBatchReader,
    batch: Option<RecordBatch>,
    row: usize,
}

impl MaterializedRegionIter {
    fn new(path: &Path) -> Result<Self> {
        Ok(Self {
            reader: ParquetRecordBatchReaderBuilder::try_new(
                File::open(path).with_context(|| format!("failed to open {}", path.display()))?,
            )?
            .build()?,
            batch: None,
            row: 0,
        })
    }

    fn next_row(&mut self) -> Result<Option<MaterializedRegionRow>> {
        loop {
            if let Some(batch) = self.batch.as_ref()
                && self.row < batch.num_rows()
            {
                let row = self.row;
                self.row += 1;
                return Ok(Some(MaterializedRegionRow {
                    key: MaterializedRegionKey {
                        source_id: batch_string_value(batch, "source_id", row)?,
                        text_id: batch_string_value(batch, "text_id", row)?,
                        region_index: batch_u64_value(batch, "region_index", row)?,
                    },
                    is_nonempty_whitespace: batch_bool_value(batch, "is_nonempty_whitespace", row)?,
                }));
            }
            match self.reader.next() {
                Some(batch) => {
                    self.batch = Some(batch?);
                    self.row = 0;
                }
                None => return Ok(None),
            }
        }
    }
}

struct MaterializedFeatureIter {
    reader: ParquetRecordBatchReader,
    batch: Option<RecordBatch>,
    row: usize,
}

impl MaterializedFeatureIter {
    fn new(path: &Path) -> Result<Self> {
        Ok(Self {
            reader: ParquetRecordBatchReaderBuilder::try_new(
                File::open(path).with_context(|| format!("failed to open {}", path.display()))?,
            )?
            .build()?,
            batch: None,
            row: 0,
        })
    }

    fn next_row(&mut self) -> Result<Option<MaterializedFeatureRow>> {
        loop {
            if let Some(batch) = self.batch.as_ref()
                && self.row < batch.num_rows()
            {
                let row = self.row;
                self.row += 1;
                return Ok(Some(MaterializedFeatureRow {
                    region: MaterializedRegionKey {
                        source_id: batch_string_value(batch, "source_id", row)?,
                        text_id: batch_string_value(batch, "text_id", row)?,
                        region_index: batch_u64_value(batch, "region_index", row)?,
                    },
                    feature_key: batch_string_value(batch, "feature_key", row)?,
                    scope_type: batch_string_value(batch, "scope_type", row)?,
                    scope_position: batch_nullable_u64_value(batch, "scope_position", row)?,
                    scope_surface: batch_nullable_string_value(batch, "scope_surface", row)?,
                    feature_value: batch_nullable_string_value(batch, "feature_value", row)?,
                    analyzer_id: batch_string_value(batch, "analyzer_id", row)?,
                }));
            }
            match self.reader.next() {
                Some(batch) => {
                    self.batch = Some(batch?);
                    self.row = 0;
                }
                None => return Ok(None),
            }
        }
    }
}

fn paired_region_feature_part_paths(
    run_dir: &Path,
) -> Result<Vec<(std::path::PathBuf, std::path::PathBuf)>> {
    let region_paths = parquet_part_paths_for_table(run_dir, WarehouseTable::NwayRegions)?;
    let feature_paths = parquet_part_paths_for_table(run_dir, WarehouseTable::NwayFeatureDiffs)?;
    if region_paths.len() != feature_paths.len() {
        bail!(
            "cannot stream feature pattern counts: nway_regions has {} part(s), nway_feature_diffs has {} part(s)",
            region_paths.len(),
            feature_paths.len()
        );
    }
    Ok(region_paths.into_iter().zip(feature_paths).collect())
}

fn parquet_part_paths_for_table(
    run_dir: &Path,
    table: WarehouseTable,
) -> Result<Vec<std::path::PathBuf>> {
    let path = run_dir.join(table.file_name());
    if path.is_dir() {
        let mut paths = fs::read_dir(&path)
            .with_context(|| format!("failed to read {}", path.display()))?
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<std::result::Result<Vec<_>, _>>()?;
        paths.retain(|path| path.extension().is_some_and(|ext| ext == "parquet"));
        paths.sort();
        return Ok(paths);
    }
    Ok(vec![path])
}

fn batch_string_value(batch: &RecordBatch, name: &str, row: usize) -> Result<String> {
    let index = batch.schema().index_of(name)?;
    Ok(string_column(batch, index)?.value(row).to_owned())
}

fn batch_nullable_string_value(
    batch: &RecordBatch,
    name: &str,
    row: usize,
) -> Result<Option<String>> {
    let index = batch.schema().index_of(name)?;
    Ok(nullable_string_value(string_column(batch, index)?, row))
}

fn batch_u64_value(batch: &RecordBatch, name: &str, row: usize) -> Result<u64> {
    let index = batch.schema().index_of(name)?;
    Ok(u64_column(batch, index)?.value(row))
}

fn batch_nullable_u64_value(batch: &RecordBatch, name: &str, row: usize) -> Result<Option<u64>> {
    let index = batch.schema().index_of(name)?;
    Ok(nullable_u64_value(u64_column(batch, index)?, row))
}

fn batch_bool_value(batch: &RecordBatch, name: &str, row: usize) -> Result<bool> {
    let index = batch.schema().index_of(name)?;
    Ok(bool_column(batch, index)?.value(row))
}

fn run_duckdb_statement(run_dir: &Path, sql: String, context: &str) -> Result<bool> {
    let duckdb_bin = std::env::var("AB_DUCKDB_BIN")
        .unwrap_or_else(|_| std::env::var("DUCKDB").unwrap_or_else(|_| String::from("duckdb")));

    fs::create_dir_all(duckdb_temp_dir(run_dir)).with_context(|| {
        format!(
            "failed to create DuckDB temp directory for {}",
            run_dir.display()
        )
    })?;
    let output = match Command::new(&duckdb_bin).arg("-c").arg(sql).output() {
        Ok(output) => output,
        Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(false),
        Err(error) => return Err(error).context("failed to run duckdb"),
    };
    if !output.status.success() {
        bail!(
            "duckdb {context} failed with status {}: stderr={} stdout={}",
            output.status,
            String::from_utf8_lossy(&output.stderr),
            String::from_utf8_lossy(&output.stdout)
        );
    }
    Ok(true)
}

pub(crate) fn warehouse_pattern_duckdb_sql(
    run_dir: &Path,
    options: &WarehousePatternOptions,
) -> String {
    let regions = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegions);
    let analyzers = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegionAnalyzers);
    let features = duckdb_table_path_literal(run_dir, WarehouseTable::NwayFeatureDiffs);
    let region_filter = warehouse_duckdb_region_filter(options);
    let source_exclusion = sql_not_in_clause("source_id", &options.exclusions.source_ids);
    let text_exclusion = sql_not_in_clause("text_id", &options.exclusions.text_ids);
    let limit = options.limit;
    if options.kind == NwayPatternKind::Feature
        && options.feature_profile == WarehouseFeatureProfile::Core
        && options.feature_key.is_none()
    {
        let excluded_values = sql_not_in_clause("feature_value", &options.excluded_feature_values);
        let branches = WAREHOUSE_CORE_FEATURE_KEYS
            .iter()
            .map(|feature_key| {
                format!(
                    "({})",
                    warehouse_feature_pattern_select_sql(
                        &regions,
                        &features,
                        region_filter,
                        &source_exclusion,
                        &text_exclusion,
                        &format!("feature_key = {}", sql_literal(feature_key)),
                        &excluded_values,
                        "TRUE",
                        limit,
                    )
                )
            })
            .collect::<Vec<_>>()
            .join("\nUNION ALL\n");
        let body = format!(
            r#"
SELECT *
FROM (
{branches}
) AS core_patterns
ORDER BY examples DESC, pattern
LIMIT {limit}
"#
        );
        return duckdb_copy_sql(run_dir, &body);
    }
    let body = match options.kind {
        NwayPatternKind::Segmentation => format!(
            r#"
WITH regions AS (
    SELECT source_id, text_id, region_index
    FROM read_parquet({regions})
    WHERE has_segmentation_disagreement
      AND {region_filter}
      AND {source_exclusion}
      AND {text_exclusion}
),
surface_groups AS (
    SELECT
        a.source_id,
        a.text_id,
        a.region_index,
        a.surfaces,
        list(a.analyzer_id ORDER BY a.analyzer_id) AS analyzers,
        array_to_string(a.surfaces, '|') AS surface_text
    FROM read_parquet({analyzers}) AS a
    JOIN regions AS r USING (source_id, text_id, region_index)
    GROUP BY a.source_id, a.text_id, a.region_index, a.surfaces
),
patterns AS (
    SELECT
        'segmentation' AS kind,
        source_id,
        text_id,
        region_index,
        string_agg(array_to_string(analyzers, '+') || ':[' || surface_text || ']', ' ; ' ORDER BY surface_text, array_to_string(analyzers, '+')) AS pattern
    FROM surface_groups
    GROUP BY source_id, text_id, region_index
    HAVING count(*) > 1
)
{final_select}
"#,
            final_select = warehouse_pattern_final_select(limit)
        ),
        NwayPatternKind::Feature => {
            let feature_filter = options.feature_key.as_ref().map_or_else(
                || "TRUE".to_owned(),
                |feature_key| format!("feature_key = {}", sql_literal(feature_key)),
            );
            let excluded_values =
                sql_not_in_clause("feature_value", &options.excluded_feature_values);
            let profile_filter = warehouse_feature_profile_filter(options.feature_profile);
            if options.feature_profile == WarehouseFeatureProfile::Schema {
                format!(
                    r#"
WITH regions AS (
    SELECT source_id, text_id, region_index
    FROM read_parquet({regions})
    WHERE {region_filter}
      AND {source_exclusion}
      AND {text_exclusion}
),
filtered_feature_rows AS (
    SELECT
        f.*,
        {schema_expr} AS analyzer_schema_id
    FROM read_parquet({features}) AS f
    JOIN regions AS r USING (source_id, text_id, region_index)
    WHERE {feature_filter}
      AND {excluded_values}
),
schema_value_counts AS (
    SELECT
        source_id,
        text_id,
        region_index,
        feature_key,
        scope_type,
        scope_position,
        scope_surface,
        analyzer_schema_id
    FROM filtered_feature_rows
    GROUP BY source_id, text_id, region_index, feature_key, scope_type, scope_position, scope_surface, analyzer_schema_id
    HAVING count(DISTINCT feature_value) > 1
       AND count(DISTINCT analyzer_id) > 1
),
feature_values AS (
    SELECT
        f.source_id,
        f.text_id,
        f.region_index,
        f.feature_key,
        f.scope_type,
        f.scope_position,
        f.scope_surface,
        f.feature_value,
        list(f.analyzer_id ORDER BY f.analyzer_id) AS analyzers
    FROM filtered_feature_rows AS f
    JOIN schema_value_counts AS s
      ON f.source_id = s.source_id
     AND f.text_id = s.text_id
     AND f.region_index = s.region_index
     AND f.feature_key = s.feature_key
     AND f.scope_type = s.scope_type
     AND coalesce(f.scope_position, -1) = coalesce(s.scope_position, -1)
     AND coalesce(f.scope_surface, '') = coalesce(s.scope_surface, '')
     AND f.analyzer_schema_id = s.analyzer_schema_id
    GROUP BY f.source_id, f.text_id, f.region_index, f.feature_key, f.scope_type, f.scope_position, f.scope_surface, f.feature_value
),
patterns AS (
    SELECT
        'feature' AS kind,
        source_id,
        text_id,
        region_index,
        feature_key || ' ' ||
            CASE
                WHEN scope_type = 'whole_region' THEN 'whole_region'
                WHEN scope_type = 'token_position' THEN 'token_position:' || CAST(scope_position AS VARCHAR)
                ELSE 'surface:' || scope_surface
            END || ' ' ||
            string_agg(coalesce(feature_value, '') || '=>' || array_to_string(analyzers, '+'), ' ; ' ORDER BY feature_value NULLS FIRST, array_to_string(analyzers, '+')) AS pattern
    FROM feature_values
    GROUP BY source_id, text_id, region_index, feature_key, scope_type, scope_position, scope_surface
    HAVING count(*) > 1
)
{final_select}
"#,
                    schema_expr = warehouse_analyzer_schema_sql("f.analyzer_id"),
                    final_select = warehouse_pattern_final_select(limit)
                )
            } else {
                warehouse_feature_pattern_select_sql(
                    &regions,
                    &features,
                    region_filter,
                    &source_exclusion,
                    &text_exclusion,
                    &feature_filter,
                    &excluded_values,
                    profile_filter,
                    limit,
                )
            }
        }
    };
    duckdb_copy_sql(run_dir, &body)
}

pub(crate) fn warehouse_feature_pattern_counts_available(
    run_dir: &Path,
    options: &WarehousePatternOptions,
) -> bool {
    options.kind == NwayPatternKind::Feature
        && options.feature_profile == WarehouseFeatureProfile::Core
        && options.exclusions.source_ids.is_empty()
        && options.exclusions.text_ids.is_empty()
        && options.excluded_feature_values.is_empty()
        && parquet_part_paths_for_table(run_dir, WarehouseTable::FeaturePatternCounts)
            .is_ok_and(|paths| !paths.is_empty())
}

pub(crate) fn warehouse_feature_pattern_counts_duckdb_sql(
    run_dir: &Path,
    options: &WarehousePatternOptions,
) -> String {
    let counts = duckdb_table_path_literal(run_dir, WarehouseTable::FeaturePatternCounts);
    let feature_filter = options.feature_key.as_ref().map_or_else(
        || "TRUE".to_owned(),
        |feature_key| format!("feature_key = {}", sql_literal(feature_key)),
    );
    let text_filter = warehouse_feature_pattern_count_text_filter(options.text_filter);
    let limit = options.limit;
    let body = format!(
        r#"
SELECT
    kind,
    sum(examples) AS examples,
    sum(source_count) AS source_count,
    sum(text_count) AS text_count,
    any_value(sample_source_ids) AS sample_source_ids,
    any_value(sample_text_ids) AS sample_text_ids,
    any_value(script_categories) AS script_categories,
    pattern
FROM read_parquet({counts})
WHERE feature_profile = 'core'
  AND {feature_filter}
  AND {text_filter}
GROUP BY kind, pattern
ORDER BY examples DESC, pattern
LIMIT {limit}
"#
    );
    duckdb_copy_sql(run_dir, &body)
}

fn materialize_core_feature_pattern_counts_duckdb_sql(
    run_dir: &Path,
    output_path: &Path,
    feature_key: &str,
) -> String {
    let regions = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegions);
    let features = duckdb_table_path_literal(run_dir, WarehouseTable::NwayFeatureDiffs);
    let output = sql_literal(&output_path.display().to_string());
    let feature_key = sql_literal(feature_key);
    let body = format!(
        r#"
WITH regions AS (
    SELECT source_id, text_id, region_index, is_nonempty_whitespace
    FROM read_parquet({regions})
    WHERE has_feature_disagreement
),
feature_values AS (
    SELECT
        f.source_id,
        f.text_id,
        f.region_index,
        r.is_nonempty_whitespace,
        f.feature_key,
        f.scope_type,
        f.scope_position,
        f.scope_surface,
        f.feature_value,
        list(f.analyzer_id ORDER BY f.analyzer_id) AS analyzers
    FROM read_parquet({features}) AS f
    JOIN regions AS r USING (source_id, text_id, region_index)
    WHERE feature_key = {feature_key}
    GROUP BY f.source_id, f.text_id, f.region_index, r.is_nonempty_whitespace, f.feature_key, f.scope_type, f.scope_position, f.scope_surface, f.feature_value
),
patterns AS (
    SELECT
        'feature' AS kind,
        'core' AS feature_profile,
        source_id,
        text_id,
        region_index,
        is_nonempty_whitespace,
        feature_key,
        feature_key || ' ' ||
            CASE
                WHEN scope_type = 'whole_region' THEN 'whole_region'
                WHEN scope_type = 'token_position' THEN 'token_position:' || CAST(scope_position AS VARCHAR)
                ELSE 'surface:' || scope_surface
            END || ' ' ||
            string_agg(coalesce(feature_value, '') || '=>' || array_to_string(analyzers, '+'), ' ; ' ORDER BY feature_value NULLS FIRST, array_to_string(analyzers, '+')) AS pattern
    FROM feature_values
    GROUP BY source_id, text_id, region_index, is_nonempty_whitespace, feature_key, scope_type, scope_position, scope_surface
    HAVING count(*) > 1
)
SELECT
    kind,
    feature_profile,
    feature_key,
    is_nonempty_whitespace,
    pattern,
    CAST(count(*) AS UBIGINT) AS examples,
    CAST(count(DISTINCT source_id) AS UBIGINT) AS source_count,
    CAST(count(DISTINCT text_id) AS UBIGINT) AS text_count,
    array_to_string(list_slice(list_sort(list_distinct(list(source_id))), 1, 5), ',') ||
        CASE
            WHEN count(DISTINCT source_id) > 5 THEN ',...+' || CAST(count(DISTINCT source_id) - 5 AS VARCHAR)
            ELSE ''
        END AS sample_source_ids,
    array_to_string(list_slice(list_sort(list_distinct(list(text_id))), 1, 5), ',') ||
        CASE
            WHEN count(DISTINCT text_id) > 5 THEN ',...+' || CAST(count(DISTINCT text_id) - 5 AS VARCHAR)
            ELSE ''
        END AS sample_text_ids,
    '' AS script_categories
FROM patterns
GROUP BY kind, feature_profile, feature_key, is_nonempty_whitespace, pattern
"#
    );
    format!(
        "{}\nCOPY ({body}) TO {output} (FORMAT PARQUET, COMPRESSION ZSTD);",
        duckdb_settings_sql(run_dir)
    )
}

#[allow(clippy::too_many_arguments)]
fn warehouse_feature_pattern_select_sql(
    regions: &str,
    features: &str,
    region_filter: &str,
    source_exclusion: &str,
    text_exclusion: &str,
    feature_filter: &str,
    excluded_values: &str,
    profile_filter: &str,
    limit: usize,
) -> String {
    format!(
        r#"
WITH regions AS (
    SELECT source_id, text_id, region_index
    FROM read_parquet({regions})
    WHERE {region_filter}
      AND {source_exclusion}
      AND {text_exclusion}
),
feature_values AS (
    SELECT
        f.source_id,
        f.text_id,
        f.region_index,
        f.feature_key,
        f.scope_type,
        f.scope_position,
        f.scope_surface,
        f.feature_value,
        list(f.analyzer_id ORDER BY f.analyzer_id) AS analyzers
    FROM read_parquet({features}) AS f
    JOIN regions AS r USING (source_id, text_id, region_index)
    WHERE {feature_filter}
      AND {excluded_values}
      AND {profile_filter}
    GROUP BY f.source_id, f.text_id, f.region_index, f.feature_key, f.scope_type, f.scope_position, f.scope_surface, f.feature_value
),
patterns AS (
    SELECT
        'feature' AS kind,
        source_id,
        text_id,
        region_index,
        feature_key || ' ' ||
            CASE
                WHEN scope_type = 'whole_region' THEN 'whole_region'
                WHEN scope_type = 'token_position' THEN 'token_position:' || CAST(scope_position AS VARCHAR)
                ELSE 'surface:' || scope_surface
            END || ' ' ||
            string_agg(coalesce(feature_value, '') || '=>' || array_to_string(analyzers, '+'), ' ; ' ORDER BY feature_value NULLS FIRST, array_to_string(analyzers, '+')) AS pattern
    FROM feature_values
    GROUP BY source_id, text_id, region_index, feature_key, scope_type, scope_position, scope_surface
    HAVING count(*) > 1
)
{final_select}
"#,
        final_select = warehouse_pattern_final_select(limit)
    )
}

fn warehouse_pattern_final_select(limit: usize) -> String {
    format!(
        r#"
SELECT
    kind,
    count(*) AS examples,
    count(DISTINCT source_id) AS source_count,
    count(DISTINCT text_id) AS text_count,
    array_to_string(list_slice(list_sort(list_distinct(list(source_id))), 1, 5), ',') ||
        CASE
            WHEN count(DISTINCT source_id) > 5 THEN ',...+' || CAST(count(DISTINCT source_id) - 5 AS VARCHAR)
            ELSE ''
        END AS sample_source_ids,
    array_to_string(list_slice(list_sort(list_distinct(list(text_id))), 1, 5), ',') ||
        CASE
            WHEN count(DISTINCT text_id) > 5 THEN ',...+' || CAST(count(DISTINCT text_id) - 5 AS VARCHAR)
            ELSE ''
        END AS sample_text_ids,
    NULL::VARCHAR AS script_categories,
    pattern
FROM patterns
GROUP BY kind, pattern
ORDER BY examples DESC, pattern
LIMIT {limit}
"#
    )
}

fn warehouse_feature_pattern_count_text_filter(filter: WarehouseTextFilter) -> &'static str {
    match filter {
        WarehouseTextFilter::All => "TRUE",
        WarehouseTextFilter::WhitespaceOnly => "is_nonempty_whitespace",
        WarehouseTextFilter::LexicalOnly => "NOT is_nonempty_whitespace",
    }
}

fn warehouse_duckdb_region_filter(options: &WarehousePatternOptions) -> &'static str {
    match options.text_filter {
        WarehouseTextFilter::All => "TRUE",
        WarehouseTextFilter::WhitespaceOnly => "is_nonempty_whitespace",
        WarehouseTextFilter::LexicalOnly => "NOT is_nonempty_whitespace",
    }
}

pub(crate) fn warehouse_region_examples_duckdb_sql(
    run_dir: &Path,
    options: &WarehouseRegionOptions,
) -> String {
    let regions = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegions);
    let analyzers = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegionAnalyzers);
    let features = duckdb_table_path_literal(run_dir, WarehouseTable::NwayFeatureDiffs);
    let kind_filter = warehouse_region_kind_filter(options.kind);
    let text_filter = warehouse_text_filter_sql(options.text_filter);
    let source_exclusion = sql_not_in_clause("source_id", &options.exclusions.source_ids);
    let text_exclusion = sql_not_in_clause("text_id", &options.exclusions.text_ids);
    let limit = options.limit;
    let body = format!(
        r#"
WITH selected_regions AS (
    SELECT
        source_id,
        text_id,
        region_index,
        byte_start,
        byte_end,
        char_start,
        char_end,
        is_nonempty_whitespace,
        is_agreement,
        has_coverage_mismatch,
        has_segmentation_disagreement,
        has_feature_disagreement
    FROM read_parquet({regions})
    WHERE {kind_filter}
      AND {text_filter}
      AND {source_exclusion}
      AND {text_exclusion}
    ORDER BY source_id, text_id, region_index
    LIMIT {limit}
),
analyzer_rows AS (
    SELECT
        a.source_id,
        a.text_id,
        a.region_index,
        string_agg(
            a.analyzer_id || ':[' || array_to_string(a.surfaces, '|') || ']',
            ' ; '
            ORDER BY a.analyzer_id
        ) AS analyzers
    FROM read_parquet({analyzers}) AS a
    JOIN selected_regions AS r USING (source_id, text_id, region_index)
    GROUP BY a.source_id, a.text_id, a.region_index
),
feature_diffs AS (
    SELECT
        f.source_id,
        f.text_id,
        f.region_index,
        string_agg(
            f.analyzer_id || ':' || f.feature_key || ':' ||
                CASE
                    WHEN f.scope_type = 'whole_region' THEN 'whole_region'
                    WHEN f.scope_type = 'token_position' THEN 'token_position:' || CAST(f.scope_position AS VARCHAR)
                    ELSE 'surface:' || f.scope_surface
                END || '=' || coalesce(f.feature_value, '<null>'),
            ' ; '
            ORDER BY f.feature_key, f.scope_type, f.scope_position, f.scope_surface, f.feature_value NULLS FIRST, f.analyzer_id
        ) AS feature_diffs
    FROM read_parquet({features}) AS f
    JOIN selected_regions AS r USING (source_id, text_id, region_index)
    GROUP BY f.source_id, f.text_id, f.region_index
)
SELECT
    r.source_id,
    r.text_id,
    r.region_index,
    r.char_start || '..' || r.char_end AS char_span,
    r.byte_start || '..' || r.byte_end AS byte_span,
    r.is_nonempty_whitespace,
    r.is_agreement,
    r.has_segmentation_disagreement,
    r.has_feature_disagreement,
    r.has_coverage_mismatch,
    coalesce(a.analyzers, '') AS analyzers,
    coalesce(f.feature_diffs, '') AS feature_diffs
FROM selected_regions AS r
LEFT JOIN analyzer_rows AS a USING (source_id, text_id, region_index)
LEFT JOIN feature_diffs AS f USING (source_id, text_id, region_index)
ORDER BY r.source_id, r.text_id, r.region_index
"#
    );
    duckdb_copy_sql(run_dir, &body)
}

pub(crate) fn warehouse_pattern_examples_duckdb_sql(
    run_dir: &Path,
    options: &WarehousePatternExampleOptions,
) -> String {
    let regions = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegions);
    let analyzers = duckdb_table_path_literal(run_dir, WarehouseTable::NwayRegionAnalyzers);
    let features = duckdb_table_path_literal(run_dir, WarehouseTable::NwayFeatureDiffs);
    let text_filter = warehouse_text_filter_sql(options.text_filter);
    let source_exclusion = sql_not_in_clause("source_id", &options.exclusions.source_ids);
    let text_exclusion = sql_not_in_clause("text_id", &options.exclusions.text_ids);
    let pattern = sql_literal(&options.pattern);
    let limit = options.limit;
    let pattern_ctes = match options.kind {
        NwayPatternKind::Segmentation => format!(
            r#"
base_regions AS (
    SELECT source_id, text_id, region_index
    FROM read_parquet({regions})
    WHERE has_segmentation_disagreement
      AND {text_filter}
      AND {source_exclusion}
      AND {text_exclusion}
),
surface_groups AS (
    SELECT
        a.source_id,
        a.text_id,
        a.region_index,
        a.surfaces,
        list(a.analyzer_id ORDER BY a.analyzer_id) AS analyzers,
        array_to_string(a.surfaces, '|') AS surface_text
    FROM read_parquet({analyzers}) AS a
    JOIN base_regions AS r USING (source_id, text_id, region_index)
    GROUP BY a.source_id, a.text_id, a.region_index, a.surfaces
),
patterns AS (
    SELECT
        source_id,
        text_id,
        region_index,
        string_agg(array_to_string(analyzers, '+') || ':[' || surface_text || ']', ' ; ' ORDER BY surface_text, array_to_string(analyzers, '+')) AS pattern
    FROM surface_groups
    GROUP BY source_id, text_id, region_index
    HAVING count(*) > 1
)"#
        ),
        NwayPatternKind::Feature => {
            let feature_filter = options.feature_key.as_ref().map_or_else(
                || "TRUE".to_owned(),
                |feature_key| format!("feature_key = {}", sql_literal(feature_key)),
            );
            let excluded_values =
                sql_not_in_clause("feature_value", &options.excluded_feature_values);
            let profile_filter = warehouse_feature_profile_filter(options.feature_profile);
            if options.feature_profile == WarehouseFeatureProfile::Schema {
                format!(
                    r#"
base_regions AS (
    SELECT source_id, text_id, region_index
    FROM read_parquet({regions})
    WHERE {text_filter}
      AND {source_exclusion}
      AND {text_exclusion}
),
filtered_feature_rows AS (
    SELECT
        f.*,
        {schema_expr} AS analyzer_schema_id
    FROM read_parquet({features}) AS f
    JOIN base_regions AS r USING (source_id, text_id, region_index)
    WHERE {feature_filter}
      AND {excluded_values}
),
schema_value_counts AS (
    SELECT
        source_id,
        text_id,
        region_index,
        feature_key,
        scope_type,
        scope_position,
        scope_surface,
        analyzer_schema_id
    FROM filtered_feature_rows
    GROUP BY source_id, text_id, region_index, feature_key, scope_type, scope_position, scope_surface, analyzer_schema_id
    HAVING count(DISTINCT feature_value) > 1
       AND count(DISTINCT analyzer_id) > 1
),
feature_values AS (
    SELECT
        f.source_id,
        f.text_id,
        f.region_index,
        f.feature_key,
        f.scope_type,
        f.scope_position,
        f.scope_surface,
        f.feature_value,
        list(f.analyzer_id ORDER BY f.analyzer_id) AS analyzers
    FROM filtered_feature_rows AS f
    JOIN schema_value_counts AS s
      ON f.source_id = s.source_id
     AND f.text_id = s.text_id
     AND f.region_index = s.region_index
     AND f.feature_key = s.feature_key
     AND f.scope_type = s.scope_type
     AND coalesce(f.scope_position, -1) = coalesce(s.scope_position, -1)
     AND coalesce(f.scope_surface, '') = coalesce(s.scope_surface, '')
     AND f.analyzer_schema_id = s.analyzer_schema_id
    GROUP BY f.source_id, f.text_id, f.region_index, f.feature_key, f.scope_type, f.scope_position, f.scope_surface, f.feature_value
),
patterns AS (
    SELECT
        source_id,
        text_id,
        region_index,
        feature_key || ' ' ||
            CASE
                WHEN scope_type = 'whole_region' THEN 'whole_region'
                WHEN scope_type = 'token_position' THEN 'token_position:' || CAST(scope_position AS VARCHAR)
                ELSE 'surface:' || scope_surface
            END || ' ' ||
            string_agg(coalesce(feature_value, '') || '=>' || array_to_string(analyzers, '+'), ' ; ' ORDER BY feature_value NULLS FIRST, array_to_string(analyzers, '+')) AS pattern
    FROM feature_values
    GROUP BY source_id, text_id, region_index, feature_key, scope_type, scope_position, scope_surface
    HAVING count(*) > 1
)"#,
                    schema_expr = warehouse_analyzer_schema_sql("f.analyzer_id")
                )
            } else {
                format!(
                    r#"
base_regions AS (
    SELECT source_id, text_id, region_index
    FROM read_parquet({regions})
    WHERE {text_filter}
      AND {source_exclusion}
      AND {text_exclusion}
),
feature_values AS (
    SELECT
        f.source_id,
        f.text_id,
        f.region_index,
        f.feature_key,
        f.scope_type,
        f.scope_position,
        f.scope_surface,
        f.feature_value,
        list(f.analyzer_id ORDER BY f.analyzer_id) AS analyzers
    FROM read_parquet({features}) AS f
    JOIN base_regions AS r USING (source_id, text_id, region_index)
    WHERE {feature_filter}
      AND {excluded_values}
      AND {profile_filter}
    GROUP BY f.source_id, f.text_id, f.region_index, f.feature_key, f.scope_type, f.scope_position, f.scope_surface, f.feature_value
),
patterns AS (
    SELECT
        source_id,
        text_id,
        region_index,
        feature_key || ' ' ||
            CASE
                WHEN scope_type = 'whole_region' THEN 'whole_region'
                WHEN scope_type = 'token_position' THEN 'token_position:' || CAST(scope_position AS VARCHAR)
                ELSE 'surface:' || scope_surface
            END || ' ' ||
            string_agg(coalesce(feature_value, '') || '=>' || array_to_string(analyzers, '+'), ' ; ' ORDER BY feature_value NULLS FIRST, array_to_string(analyzers, '+')) AS pattern
    FROM feature_values
    GROUP BY source_id, text_id, region_index, feature_key, scope_type, scope_position, scope_surface
    HAVING count(*) > 1
)"#
                )
            }
        }
    };
    let body = format!(
        r#"
WITH {pattern_ctes},
selected_regions AS (
    SELECT
        r.source_id,
        r.text_id,
        r.region_index,
        r.byte_start,
        r.byte_end,
        r.char_start,
        r.char_end,
        r.is_nonempty_whitespace,
        r.is_agreement,
        r.has_coverage_mismatch,
        r.has_segmentation_disagreement,
        r.has_feature_disagreement
    FROM patterns AS p
    JOIN read_parquet({regions}) AS r USING (source_id, text_id, region_index)
    WHERE p.pattern = {pattern}
    ORDER BY r.source_id, r.text_id, r.region_index
    LIMIT {limit}
),
analyzer_rows AS (
    SELECT
        a.source_id,
        a.text_id,
        a.region_index,
        string_agg(
            a.analyzer_id || ':[' || array_to_string(a.surfaces, '|') || ']',
            ' ; '
            ORDER BY a.analyzer_id
        ) AS analyzers
    FROM read_parquet({analyzers}) AS a
    JOIN selected_regions AS r USING (source_id, text_id, region_index)
    GROUP BY a.source_id, a.text_id, a.region_index
),
feature_diffs AS (
    SELECT
        f.source_id,
        f.text_id,
        f.region_index,
        string_agg(
            f.analyzer_id || ':' || f.feature_key || ':' ||
                CASE
                    WHEN f.scope_type = 'whole_region' THEN 'whole_region'
                    WHEN f.scope_type = 'token_position' THEN 'token_position:' || CAST(f.scope_position AS VARCHAR)
                    ELSE 'surface:' || f.scope_surface
                END || '=' || coalesce(f.feature_value, '<null>'),
            ' ; '
            ORDER BY f.feature_key, f.scope_type, f.scope_position, f.scope_surface, f.feature_value NULLS FIRST, f.analyzer_id
        ) AS feature_diffs
    FROM read_parquet({features}) AS f
    JOIN selected_regions AS r USING (source_id, text_id, region_index)
    GROUP BY f.source_id, f.text_id, f.region_index
)
SELECT
    r.source_id,
    r.text_id,
    r.region_index,
    r.char_start || '..' || r.char_end AS char_span,
    r.byte_start || '..' || r.byte_end AS byte_span,
    r.is_nonempty_whitespace,
    r.is_agreement,
    r.has_segmentation_disagreement,
    r.has_feature_disagreement,
    r.has_coverage_mismatch,
    coalesce(a.analyzers, '') AS analyzers,
    coalesce(f.feature_diffs, '') AS feature_diffs
FROM selected_regions AS r
LEFT JOIN analyzer_rows AS a USING (source_id, text_id, region_index)
LEFT JOIN feature_diffs AS f USING (source_id, text_id, region_index)
ORDER BY r.source_id, r.text_id, r.region_index
"#
    );
    duckdb_copy_sql(run_dir, &body)
}

fn warehouse_region_kind_filter(kind: WarehouseRegionKind) -> &'static str {
    match kind {
        WarehouseRegionKind::All => "NOT is_agreement",
        WarehouseRegionKind::Segmentation => "has_segmentation_disagreement",
        WarehouseRegionKind::Feature => "has_feature_disagreement",
        WarehouseRegionKind::Coverage => "has_coverage_mismatch",
    }
}

fn warehouse_text_filter_sql(filter: WarehouseTextFilter) -> &'static str {
    match filter {
        WarehouseTextFilter::All => "TRUE",
        WarehouseTextFilter::WhitespaceOnly => "is_nonempty_whitespace",
        WarehouseTextFilter::LexicalOnly => "NOT is_nonempty_whitespace",
    }
}

fn warehouse_feature_profile_filter(profile: WarehouseFeatureProfile) -> &'static str {
    match profile {
        WarehouseFeatureProfile::Raw | WarehouseFeatureProfile::Schema => "TRUE",
        WarehouseFeatureProfile::Core => "feature_key IN ('pos1', 'pos2', 'pos3', 'pos4')",
    }
}

pub(super) fn warehouse_feature_key_in_profile(feature_key: &str, profile: WarehouseFeatureProfile) -> bool {
    match profile {
        WarehouseFeatureProfile::Raw | WarehouseFeatureProfile::Schema => true,
        WarehouseFeatureProfile::Core => matches!(feature_key, "pos1" | "pos2" | "pos3" | "pos4"),
    }
}

pub(super) fn warehouse_feature_facts_for_profile(
    profile: WarehouseFeatureProfile,
    facts: Vec<WarehouseFeatureDiffFact>,
) -> Vec<WarehouseFeatureDiffFact> {
    if profile != WarehouseFeatureProfile::Schema {
        return facts;
    }

    let mut by_schema = BTreeMap::<String, Vec<WarehouseFeatureDiffFact>>::new();
    for fact in facts {
        by_schema
            .entry(warehouse_analyzer_schema_id(&fact.analyzer_id).to_owned())
            .or_default()
            .push(fact);
    }

    let mut selected = Vec::new();
    for schema_facts in by_schema.into_values() {
        let analyzer_count = schema_facts
            .iter()
            .map(|fact| fact.analyzer_id.as_str())
            .collect::<BTreeSet<_>>()
            .len();
        let value_count = schema_facts
            .iter()
            .map(|fact| fact.feature_value.as_deref())
            .collect::<BTreeSet<_>>()
            .len();
        if analyzer_count > 1 && value_count > 1 {
            selected.extend(schema_facts);
        }
    }
    selected
}

fn warehouse_analyzer_schema_id(analyzer_id: &str) -> &str {
    if analyzer_id.starts_with("vibrato:") {
        "unidic"
    } else if analyzer_id.starts_with("sudachi-") {
        "sudachi"
    } else {
        analyzer_id
    }
}

fn warehouse_analyzer_schema_sql(analyzer_expr: &str) -> String {
    format!(
        "CASE
            WHEN {analyzer_expr} LIKE 'vibrato:%' THEN 'unidic'
            WHEN {analyzer_expr} LIKE 'sudachi-%' THEN 'sudachi'
            ELSE {analyzer_expr}
        END"
    )
}

fn sql_not_in_clause(column: &str, values: &BTreeSet<String>) -> String {
    if values.is_empty() {
        return "TRUE".to_owned();
    }
    let values = values
        .iter()
        .map(|value| sql_literal(value))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{column} NOT IN ({values})")
}

fn duckdb_table_path_literal(run_dir: &Path, table: WarehouseTable) -> String {
    let path = run_dir.join(table.file_name());
    let path = if path.is_dir() {
        path.join("*.parquet")
    } else {
        path
    };
    sql_literal(&path.display().to_string())
}

fn duckdb_copy_sql(run_dir: &Path, body: &str) -> String {
    format!(
        "{}\nCOPY ({body}) TO STDOUT (HEADER, DELIMITER '\t');",
        duckdb_settings_sql(run_dir)
    )
}

fn duckdb_settings_sql(run_dir: &Path) -> String {
    format!(
        "SET temp_directory = {};\nSET threads = 4;\nSET preserve_insertion_order = false;\nSET memory_limit = '16GB';",
        sql_literal(&duckdb_temp_dir(run_dir).display().to_string())
    )
}

pub(crate) fn duckdb_temp_dir(run_dir: &Path) -> std::path::PathBuf {
    run_dir
        .parent()
        .and_then(Path::parent)
        .unwrap_or(run_dir)
        .join(".duckdb_tmp")
}

fn sql_literal(value: &str) -> String {
    format!("'{}'", value.replace('\'', "''"))
}

/// Summarizes warehouse pattern example rows.
///
/// # Errors
///
/// Returns an error when warehouse region and analyzer facts cannot be loaded.
pub fn summarize_warehouse_pattern_examples(
    run_dir: &Path,
    options: WarehousePatternExampleOptions,
) -> Result<Vec<WarehouseRegionExampleRow>> {
    let regions = read_warehouse_region_flags(run_dir)?;
    let mut analyzers_by_region =
        BTreeMap::<WarehouseRegionKey, Vec<WarehouseRegionAnalyzerFact>>::new();
    for fact in read_warehouse_region_analyzers(run_dir)? {
        analyzers_by_region
            .entry(fact.key.clone())
            .or_default()
            .push(fact);
    }
    for analyzers in analyzers_by_region.values_mut() {
        analyzers.sort_by(|left, right| left.analyzer_id.cmp(&right.analyzer_id));
    }

    match options.kind {
        NwayPatternKind::Segmentation => summarize_warehouse_segmentation_pattern_examples(
            &regions,
            analyzers_by_region,
            options,
        ),
        NwayPatternKind::Feature => summarize_warehouse_feature_pattern_examples(
            run_dir,
            &regions,
            analyzers_by_region,
            options,
        ),
    }
}

fn summarize_warehouse_segmentation_pattern_examples(
    regions: &BTreeMap<WarehouseRegionKey, WarehouseRegionFlags>,
    analyzers_by_region: BTreeMap<WarehouseRegionKey, Vec<WarehouseRegionAnalyzerFact>>,
    options: WarehousePatternExampleOptions,
) -> Result<Vec<WarehouseRegionExampleRow>> {
    let mut rows = Vec::new();
    for (region, analyzers) in analyzers_by_region {
        let Some(flags) = regions.get(&region).copied() else {
            continue;
        };
        if options
            .exclusions
            .excludes(&region.source_id, &region.text_id)
            || !flags.has_segmentation_disagreement
            || !warehouse_text_filter_matches(options.text_filter, flags)
        {
            continue;
        }
        let Some(key) = warehouse_segmentation_pattern_key(&analyzers) else {
            continue;
        };
        if nway_pattern_display(&key) != options.pattern {
            continue;
        }
        rows.push(warehouse_region_example_row(
            region,
            flags,
            analyzers,
            Vec::new(),
        ));
        if rows.len() >= options.limit {
            break;
        }
    }
    Ok(rows)
}

fn summarize_warehouse_feature_pattern_examples(
    run_dir: &Path,
    regions: &BTreeMap<WarehouseRegionKey, WarehouseRegionFlags>,
    analyzers_by_region: BTreeMap<WarehouseRegionKey, Vec<WarehouseRegionAnalyzerFact>>,
    options: WarehousePatternExampleOptions,
) -> Result<Vec<WarehouseRegionExampleRow>> {
    let mut by_feature = BTreeMap::<WarehouseFeatureGroupKey, Vec<WarehouseFeatureDiffFact>>::new();
    for fact in read_warehouse_feature_diffs(run_dir)? {
        if options
            .exclusions
            .excludes(&fact.key.region.source_id, &fact.key.region.text_id)
        {
            continue;
        }
        if options
            .feature_key
            .as_ref()
            .is_some_and(|wanted| fact.key.feature_key != *wanted)
        {
            continue;
        }
        if !warehouse_feature_key_in_profile(&fact.key.feature_key, options.feature_profile) {
            continue;
        }
        if fact
            .feature_value
            .as_ref()
            .is_some_and(|value| options.excluded_feature_values.contains(value))
        {
            continue;
        }
        by_feature.entry(fact.key.clone()).or_default().push(fact);
    }

    let mut rows = Vec::new();
    for (feature, facts) in by_feature {
        let Some(flags) = regions.get(&feature.region).copied() else {
            continue;
        };
        if !warehouse_text_filter_matches(options.text_filter, flags) {
            continue;
        }
        let facts = warehouse_feature_facts_for_profile(options.feature_profile, facts);
        if facts.is_empty() {
            continue;
        }
        let key = warehouse_feature_pattern_key(&feature, &facts)?;
        if nway_pattern_display(&key) != options.pattern {
            continue;
        }
        rows.push(warehouse_region_example_row(
            feature.region.clone(),
            flags,
            analyzers_by_region
                .get(&feature.region)
                .cloned()
                .unwrap_or_default(),
            facts,
        ));
        if rows.len() >= options.limit {
            break;
        }
    }
    Ok(rows)
}

fn summarize_warehouse_segmentation_patterns(
    run_dir: &Path,
    regions: &BTreeMap<WarehouseRegionKey, WarehouseRegionFlags>,
    options: &WarehousePatternOptions,
) -> Result<BTreeMap<NwayPatternKey, WarehousePatternAccumulator>> {
    let mut by_region = BTreeMap::<WarehouseRegionKey, Vec<WarehouseRegionAnalyzerFact>>::new();
    for fact in read_warehouse_region_analyzers(run_dir)? {
        if options
            .exclusions
            .excludes(&fact.key.source_id, &fact.key.text_id)
        {
            continue;
        }
        if !regions.get(&fact.key).is_some_and(|flags| {
            flags.has_segmentation_disagreement
                && warehouse_text_filter_matches(options.text_filter, *flags)
        }) {
            continue;
        }
        by_region.entry(fact.key.clone()).or_default().push(fact);
    }

    let mut groups = BTreeMap::<NwayPatternKey, WarehousePatternAccumulator>::new();
    for (region, facts) in by_region {
        if let Some(key) = warehouse_segmentation_pattern_key(&facts) {
            groups.entry(key).or_default().push_region(&region, 1);
        }
    }
    Ok(groups)
}

fn summarize_warehouse_feature_patterns(
    run_dir: &Path,
    regions: &BTreeMap<WarehouseRegionKey, WarehouseRegionFlags>,
    options: &WarehousePatternOptions,
) -> Result<BTreeMap<NwayPatternKey, WarehousePatternAccumulator>> {
    let mut by_feature = BTreeMap::<WarehouseFeatureGroupKey, Vec<WarehouseFeatureDiffFact>>::new();
    for fact in read_warehouse_feature_diffs(run_dir)? {
        if options
            .exclusions
            .excludes(&fact.key.region.source_id, &fact.key.region.text_id)
        {
            continue;
        }
        if !regions
            .get(&fact.key.region)
            .is_some_and(|flags| warehouse_text_filter_matches(options.text_filter, *flags))
        {
            continue;
        }
        if options
            .feature_key
            .as_ref()
            .is_some_and(|wanted| fact.key.feature_key != *wanted)
        {
            continue;
        }
        if !warehouse_feature_key_in_profile(&fact.key.feature_key, options.feature_profile) {
            continue;
        }
        if fact
            .feature_value
            .as_ref()
            .is_some_and(|value| options.excluded_feature_values.contains(value))
        {
            continue;
        }
        by_feature.entry(fact.key.clone()).or_default().push(fact);
    }

    let mut groups = BTreeMap::<NwayPatternKey, WarehousePatternAccumulator>::new();
    for (feature, facts) in by_feature {
        let facts = warehouse_feature_facts_for_profile(options.feature_profile, facts);
        if facts.is_empty() {
            continue;
        }
        let key = warehouse_feature_pattern_key(&feature, &facts)?;
        if key.feature_values.len() > 1 {
            groups
                .entry(key)
                .or_default()
                .push_region(&feature.region, 1);
        }
    }
    Ok(groups)
}

impl Accumulator {
    pub(super) fn push(&mut self, row: ComparisonSummaryRow) {
        self.source_ids.insert(row.source_id);
        self.text_ids.insert(row.text_id);
        self.script_categories.insert(row.source_script_category);
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

    pub(super) fn into_row(self, key: String) -> CompactSummaryRow {
        CompactSummaryRow {
            key,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            script_categories: self
                .script_categories
                .into_iter()
                .map(ScriptCategory::as_str)
                .map(str::to_owned)
                .collect(),
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
    pub(super) fn push(
        &mut self,
        row: ExampleSummaryInputRow,
        whitespace_only: bool,
        script_category: ScriptCategory,
    ) {
        self.source_ids.insert(row.source_id);
        self.text_ids.insert(row.text_id);
        self.script_categories.insert(script_category);
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

    pub(super) fn into_row(self, key: String) -> CompactExampleSummaryRow {
        CompactExampleSummaryRow {
            key,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            script_categories: self
                .script_categories
                .into_iter()
                .map(ScriptCategory::as_str)
                .map(str::to_owned)
                .collect(),
            examples: self.examples,
            whitespace_examples: self.whitespace_examples,
            lexical_examples: self.lexical_examples,
            segmentation_examples: self.segmentation_examples,
            feature_diff_examples: self.feature_diff_examples,
            coverage_examples: self.coverage_examples,
        }
    }
}

impl DifferenceAccumulator {
    pub(super) fn push(&mut self, row: &ExampleSummaryInputRow, script_category: ScriptCategory) {
        self.source_ids.insert(row.source_id.clone());
        self.text_ids.insert(row.text_id.clone());
        self.script_categories.insert(script_category);
        self.examples += 1;
    }

    pub(super) fn into_row(self, key: DifferenceKey) -> CompactDifferenceSummaryRow {
        CompactDifferenceSummaryRow {
            kind: key.kind,
            from_analyzer: key.from_analyzer,
            to_analyzer: key.to_analyzer,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            script_categories: self
                .script_categories
                .into_iter()
                .map(ScriptCategory::as_str)
                .map(str::to_owned)
                .collect(),
            examples: self.examples,
            region_kind: key.region_kind,
            from_surfaces: key.from_surfaces,
            to_surfaces: key.to_surfaces,
            feature_key: key.feature_key,
            feature_from: key.feature_from,
            feature_to: key.feature_to,
        }
    }
}

impl NwayAccumulator {
    pub(super) fn push(&mut self, row: NwaySummaryInputRow) {
        self.source_ids.insert(row.source_id);
        self.text_ids.insert(row.text_id);
        self.script_categories.insert(row.source_script_category);
        self.rows += 1;
        self.analyzer_count = self.analyzer_count.max(row.analyzer_count);
        self.regions += row.regions;
        self.agreement_regions += row.agreement_regions;
        self.regions_with_feature_disagreement += row.regions_with_feature_disagreement;
        self.regions_with_segmentation_disagreement += row.regions_with_segmentation_disagreement;
        self.regions_with_coverage_mismatch += row.regions_with_coverage_mismatch;
        self.whitespace_regions += row.whitespace_regions;
        self.lexical_regions += row.lexical_regions;
        self.unanimous_boundary_count += row.unanimous_boundary_count;
        self.variable_boundary_count += row.variable_boundary_count;
    }

    pub(super) fn push_warehouse_region(
        &mut self,
        region: &WarehouseRegionKey,
        flags: WarehouseRegionFlags,
    ) {
        self.source_ids.insert(region.source_id.clone());
        self.text_ids.insert(region.text_id.clone());
        self.rows = self.source_ids.len();
        self.regions += 1;
        self.agreement_regions += usize::from(flags.is_agreement);
        self.regions_with_feature_disagreement += usize::from(flags.has_feature_disagreement);
        self.regions_with_segmentation_disagreement +=
            usize::from(flags.has_segmentation_disagreement);
        self.regions_with_coverage_mismatch += usize::from(flags.has_coverage_mismatch);
        if flags.is_nonempty_whitespace {
            self.whitespace_regions += 1;
        } else {
            self.lexical_regions += 1;
        }
    }

    pub(super) fn into_row(self, key: String) -> NwaySummaryRow {
        NwaySummaryRow {
            key,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            script_categories: self
                .script_categories
                .into_iter()
                .map(ScriptCategory::as_str)
                .map(str::to_owned)
                .collect(),
            rows: self.rows,
            analyzer_count: self.analyzer_count,
            regions: self.regions,
            agreement_regions: self.agreement_regions,
            regions_with_feature_disagreement: self.regions_with_feature_disagreement,
            regions_with_segmentation_disagreement: self.regions_with_segmentation_disagreement,
            regions_with_coverage_mismatch: self.regions_with_coverage_mismatch,
            whitespace_regions: self.whitespace_regions,
            lexical_regions: self.lexical_regions,
            unanimous_boundary_count: self.unanimous_boundary_count,
            variable_boundary_count: self.variable_boundary_count,
        }
    }
}

impl NwayPatternAccumulator {
    pub(super) fn push(&mut self, row: &NwayComparisonRow, script_category: ScriptCategory) {
        self.push_count(row, script_category, 1);
    }

    pub(super) fn push_count(
        &mut self,
        row: &NwayComparisonRow,
        script_category: ScriptCategory,
        count: usize,
    ) {
        self.push_count_values(
            row.source_id.clone(),
            row.text_id.clone(),
            script_category,
            count,
        );
    }

    pub(super) fn push_count_values(
        &mut self,
        source_id: String,
        text_id: String,
        script_category: ScriptCategory,
        count: usize,
    ) {
        self.source_ids.insert(source_id);
        self.text_ids.insert(text_id);
        self.script_categories.insert(script_category);
        self.examples += count;
    }

    pub(super) fn into_row(self, key: NwayPatternKey) -> NwayPatternRow {
        let pattern = nway_pattern_display(&key);
        NwayPatternRow {
            kind: key.kind,
            pattern,
            examples: self.examples,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            script_categories: self
                .script_categories
                .into_iter()
                .map(ScriptCategory::as_str)
                .map(str::to_owned)
                .collect(),
            segmentation_groups: key.segmentation_groups,
            feature_key: key.feature_key,
            feature_scope: key.feature_scope,
            feature_values: key.feature_values,
        }
    }
}

impl WarehousePatternAccumulator {
    pub(super) fn push_region(&mut self, region: &WarehouseRegionKey, count: usize) {
        self.source_ids.insert(region.source_id.clone());
        self.text_ids.insert(region.text_id.clone());
        self.examples += count;
    }

    pub(super) fn into_row(self, key: NwayPatternKey) -> NwayPatternRow {
        NwayPatternRow {
            kind: key.kind.clone(),
            pattern: nway_pattern_display(&key),
            examples: self.examples,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            script_categories: Vec::new(),
            segmentation_groups: key.segmentation_groups,
            feature_key: key.feature_key,
            feature_scope: key.feature_scope,
            feature_values: key.feature_values,
        }
    }
}

impl WarehouseErrorAccumulator {
    pub(super) fn push(&mut self, fact: WarehouseErrorFact) {
        self.errors += 1;
        if let Some(source_id) = fact.source_id {
            self.source_ids.insert(source_id);
        }
        if let Some(text_id) = fact.text_id {
            self.text_ids.insert(text_id);
        }
        if let Some(analyzer_id) = fact.analyzer_id {
            self.analyzer_ids.insert(analyzer_id);
        }
        self.stages.insert(fact.stage);
        self.error_codes.insert(fact.error_code);
        if self.sample_messages.len() < 3 && !self.sample_messages.contains(&fact.message) {
            self.sample_messages.push(fact.message);
        }
    }

    pub(super) fn into_row(self, key: String) -> WarehouseErrorSummaryRow {
        WarehouseErrorSummaryRow {
            key,
            errors: self.errors,
            source_ids: self.source_ids.into_iter().collect(),
            text_ids: self.text_ids.into_iter().collect(),
            analyzer_ids: self.analyzer_ids.into_iter().collect(),
            stages: self.stages.into_iter().collect(),
            error_codes: self.error_codes.into_iter().collect(),
            sample_messages: self.sample_messages,
        }
    }
}

impl WarehousePairwiseAccumulator {
    pub(super) fn into_row(self, key: WarehousePairwiseKey) -> WarehousePairwiseSummaryRow {
        WarehousePairwiseSummaryRow {
            source_id: key.source_id,
            text_id: key.text_id,
            from_analyzer: key.from_analyzer,
            to_analyzer: key.to_analyzer,
            regions: self.regions,
            segmentation_regions: self.segmentation_regions,
            feature_regions: self.feature_region_keys.len(),
            coverage_regions: self.coverage_regions,
            unanimous_boundary_count: self.unanimous_boundary_count,
            variable_boundary_count: self.variable_boundary_count,
        }
    }
}

fn warehouse_pairwise_key(
    source_id: &str,
    text_id: &str,
    left_analyzer: &str,
    right_analyzer: &str,
) -> WarehousePairwiseKey {
    let (from_analyzer, to_analyzer) = if left_analyzer <= right_analyzer {
        (left_analyzer.to_owned(), right_analyzer.to_owned())
    } else {
        (right_analyzer.to_owned(), left_analyzer.to_owned())
    };
    WarehousePairwiseKey {
        source_id: source_id.to_owned(),
        text_id: text_id.to_owned(),
        from_analyzer,
        to_analyzer,
    }
}

fn compare_warehouse_pairwise_rows(
    left: &WarehousePairwiseSummaryRow,
    right: &WarehousePairwiseSummaryRow,
    sort_by: WarehousePairwiseSort,
) -> std::cmp::Ordering {
    let ordering = match sort_by {
        WarehousePairwiseSort::SegmentationRegions => {
            left.segmentation_regions.cmp(&right.segmentation_regions)
        }
        WarehousePairwiseSort::FeatureRegions => left.feature_regions.cmp(&right.feature_regions),
        WarehousePairwiseSort::CoverageRegions => {
            left.coverage_regions.cmp(&right.coverage_regions)
        }
        WarehousePairwiseSort::VariableBoundaryCount => left
            .variable_boundary_count
            .cmp(&right.variable_boundary_count),
    };
    ordering
        .reverse()
        .then_with(|| left.source_id.cmp(&right.source_id))
        .then_with(|| left.from_analyzer.cmp(&right.from_analyzer))
        .then_with(|| left.to_analyzer.cmp(&right.to_analyzer))
}

pub(super) fn read_warehouse_region_flags(
    run_dir: &Path,
) -> Result<BTreeMap<WarehouseRegionKey, WarehouseRegionFlags>> {
    let mut regions = BTreeMap::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::NwayRegions)? {
        let run_id = string_column(&batch, 0)?;
        let source_id = string_column(&batch, 1)?;
        let text_id = string_column(&batch, 2)?;
        let region_index = u64_column(&batch, 3)?;
        let byte_start = u64_column(&batch, 4)?;
        let byte_end = u64_column(&batch, 5)?;
        let char_start = u64_column(&batch, 6)?;
        let char_end = u64_column(&batch, 7)?;
        let is_nonempty_whitespace = bool_column(&batch, 8)?;
        let is_agreement = bool_column(&batch, 9)?;
        let has_coverage_mismatch = bool_column(&batch, 10)?;
        let has_segmentation_disagreement = bool_column(&batch, 11)?;
        let has_feature_disagreement = bool_column(&batch, 12)?;
        for row in 0..batch.num_rows() {
            regions.insert(
                WarehouseRegionKey {
                    run_id: run_id.value(row).to_owned(),
                    source_id: source_id.value(row).to_owned(),
                    text_id: text_id.value(row).to_owned(),
                    region_index: region_index.value(row),
                },
                WarehouseRegionFlags {
                    byte_start: byte_start.value(row),
                    byte_end: byte_end.value(row),
                    char_start: char_start.value(row),
                    char_end: char_end.value(row),
                    is_nonempty_whitespace: is_nonempty_whitespace.value(row),
                    is_agreement: is_agreement.value(row),
                    has_coverage_mismatch: has_coverage_mismatch.value(row),
                    has_segmentation_disagreement: has_segmentation_disagreement.value(row),
                    has_feature_disagreement: has_feature_disagreement.value(row),
                },
            );
        }
    }
    Ok(regions)
}

fn read_warehouse_errors(run_dir: &Path) -> Result<Vec<WarehouseErrorFact>> {
    let mut facts = Vec::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::Errors)? {
        let source_id = string_column(&batch, 1)?;
        let text_id = string_column(&batch, 2)?;
        let analyzer_id = string_column(&batch, 3)?;
        let stage = string_column(&batch, 4)?;
        let error_code = string_column(&batch, 5)?;
        let message = string_column(&batch, 6)?;
        for row in 0..batch.num_rows() {
            facts.push(WarehouseErrorFact {
                source_id: nullable_string_value(source_id, row),
                text_id: nullable_string_value(text_id, row),
                analyzer_id: nullable_string_value(analyzer_id, row),
                stage: stage.value(row).to_owned(),
                error_code: error_code.value(row).to_owned(),
                message: message.value(row).to_owned(),
            });
        }
    }
    Ok(facts)
}

fn read_warehouse_analysis_counts(run_dir: &Path) -> Result<BTreeMap<String, usize>> {
    let mut counts = BTreeMap::<String, usize>::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::Analyses)? {
        let source_id = string_column(&batch, 1)?;
        for row in 0..batch.num_rows() {
            *counts.entry(source_id.value(row).to_owned()).or_default() += 1;
        }
    }
    Ok(counts)
}

fn read_warehouse_boundary_counts(run_dir: &Path) -> Result<BTreeMap<String, (usize, usize)>> {
    let boundary_sets = read_warehouse_boundary_sets(run_dir)?;
    Ok(boundary_sets
        .into_iter()
        .map(|(source_id, analyzer_sets)| {
            let mut all_boundaries = BTreeSet::new();
            for set in analyzer_sets.values() {
                all_boundaries.extend(set.iter().copied());
            }
            let unanimous = all_boundaries
                .iter()
                .filter(|boundary| analyzer_sets.values().all(|set| set.contains(boundary)))
                .count();
            let variable = all_boundaries.len().saturating_sub(unanimous);
            (source_id, (unanimous, variable))
        })
        .collect())
}

fn read_warehouse_boundary_sets(
    run_dir: &Path,
) -> Result<BTreeMap<String, BTreeMap<String, BTreeSet<usize>>>> {
    let source_chars = read_warehouse_source_chars(run_dir)?;
    let mut boundary_sets = BTreeMap::<String, BTreeMap<String, BTreeSet<usize>>>::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::Morphemes)? {
        let source_id = string_column(&batch, 1)?;
        let analyzer_id = string_column(&batch, 3)?;
        let char_start = u64_column(&batch, 7)?;
        let char_end = u64_column(&batch, 8)?;
        for row in 0..batch.num_rows() {
            let source_id_value = source_id.value(row);
            let Some(source_len) = source_chars.get(source_id_value).copied() else {
                continue;
            };
            let set = boundary_sets
                .entry(source_id_value.to_owned())
                .or_default()
                .entry(analyzer_id.value(row).to_owned())
                .or_default();
            for boundary in [char_start.value(row) as usize, char_end.value(row) as usize] {
                if boundary != 0 && boundary != source_len {
                    set.insert(boundary);
                }
            }
        }
    }
    Ok(boundary_sets)
}

fn read_warehouse_source_chars(run_dir: &Path) -> Result<BTreeMap<String, usize>> {
    let mut source_chars = BTreeMap::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::Sources)? {
        let source_id = string_column(&batch, 1)?;
        let chars = u64_column(&batch, 5)?;
        for row in 0..batch.num_rows() {
            source_chars.insert(source_id.value(row).to_owned(), chars.value(row) as usize);
        }
    }
    Ok(source_chars)
}

fn read_warehouse_source_text_ids(run_dir: &Path) -> Result<BTreeMap<String, String>> {
    let mut source_text_ids = BTreeMap::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::Sources)? {
        let source_id = string_column(&batch, 1)?;
        let text_id = string_column(&batch, 2)?;
        for row in 0..batch.num_rows() {
            source_text_ids.insert(
                source_id.value(row).to_owned(),
                text_id.value(row).to_owned(),
            );
        }
    }
    Ok(source_text_ids)
}

pub(super) fn read_warehouse_region_analyzers(run_dir: &Path) -> Result<Vec<WarehouseRegionAnalyzerFact>> {
    let mut facts = Vec::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::NwayRegionAnalyzers)? {
        let run_id = string_column(&batch, 0)?;
        let source_id = string_column(&batch, 1)?;
        let text_id = string_column(&batch, 2)?;
        let region_index = u64_column(&batch, 3)?;
        let analyzer_id = string_column(&batch, 4)?;
        let covers_exactly = bool_column(&batch, 5)?;
        let morpheme_start = u64_column(&batch, 6)?;
        let morpheme_end = u64_column(&batch, 7)?;
        let surfaces = list_string_column(&batch, 8)?;
        for row in 0..batch.num_rows() {
            facts.push(WarehouseRegionAnalyzerFact {
                key: WarehouseRegionKey {
                    run_id: run_id.value(row).to_owned(),
                    source_id: source_id.value(row).to_owned(),
                    text_id: text_id.value(row).to_owned(),
                    region_index: region_index.value(row),
                },
                analyzer_id: analyzer_id.value(row).to_owned(),
                covers_exactly: covers_exactly.value(row),
                morpheme_start: morpheme_start.value(row),
                morpheme_end: morpheme_end.value(row),
                surfaces: list_string_value(surfaces, row)?,
            });
        }
    }
    Ok(facts)
}

pub(super) fn read_warehouse_feature_diffs(run_dir: &Path) -> Result<Vec<WarehouseFeatureDiffFact>> {
    let mut facts = Vec::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::NwayFeatureDiffs)? {
        let run_id = string_column(&batch, 0)?;
        let source_id = string_column(&batch, 1)?;
        let text_id = string_column(&batch, 2)?;
        let region_index = u64_column(&batch, 3)?;
        let feature_key = string_column(&batch, 4)?;
        let scope_type = string_column(&batch, 5)?;
        let scope_position = u64_column(&batch, 6)?;
        let scope_surface = string_column(&batch, 7)?;
        let feature_value = string_column(&batch, 8)?;
        let analyzer_id = string_column(&batch, 9)?;
        for row in 0..batch.num_rows() {
            facts.push(WarehouseFeatureDiffFact {
                key: WarehouseFeatureGroupKey {
                    region: WarehouseRegionKey {
                        run_id: run_id.value(row).to_owned(),
                        source_id: source_id.value(row).to_owned(),
                        text_id: text_id.value(row).to_owned(),
                        region_index: region_index.value(row),
                    },
                    feature_key: feature_key.value(row).to_owned(),
                    scope_type: scope_type.value(row).to_owned(),
                    scope_position: nullable_u64_value(scope_position, row),
                    scope_surface: nullable_string_value(scope_surface, row),
                },
                feature_value: nullable_string_value(feature_value, row),
                analyzer_id: analyzer_id.value(row).to_owned(),
            });
        }
    }
    Ok(facts)
}

fn warehouse_feature_scope_row(feature: &WarehouseFeatureGroupKey) -> Result<NwayFeatureScopeRow> {
    match feature.scope_type.as_str() {
        "whole_region" => Ok(NwayFeatureScopeRow::WholeRegion),
        "token_position" => Ok(NwayFeatureScopeRow::TokenPosition {
            position: feature
                .scope_position
                .context("token_position scope missing scope_position")?
                as usize,
        }),
        "surface" => Ok(NwayFeatureScopeRow::Surface {
            surface: feature
                .scope_surface
                .clone()
                .context("surface scope missing scope_surface")?,
        }),
        other => bail!("unknown warehouse feature scope_type `{other}`"),
    }
}

pub(super) fn warehouse_segmentation_pattern_key(
    facts: &[WarehouseRegionAnalyzerFact],
) -> Option<NwayPatternKey> {
    let mut surface_groups = BTreeMap::<Vec<String>, Vec<String>>::new();
    for fact in facts {
        surface_groups
            .entry(fact.surfaces.clone())
            .or_default()
            .push(fact.analyzer_id.clone());
    }
    let mut segmentation_groups = surface_groups
        .into_iter()
        .map(|(surfaces, mut analyzers)| {
            analyzers.sort();
            NwaySegmentationGroupRow {
                surfaces,
                analyzers,
            }
        })
        .collect::<Vec<_>>();
    canonicalize_segmentation_groups(&mut segmentation_groups);
    (segmentation_groups.len() > 1).then(|| NwayPatternKey {
        kind: "segmentation".to_owned(),
        segmentation_groups,
        feature_key: None,
        feature_scope: None,
        feature_values: Vec::new(),
    })
}

pub(super) fn warehouse_feature_pattern_key(
    feature: &WarehouseFeatureGroupKey,
    facts: &[WarehouseFeatureDiffFact],
) -> Result<NwayPatternKey> {
    let mut values_by_analyzer = BTreeMap::<Option<String>, Vec<String>>::new();
    for fact in facts {
        values_by_analyzer
            .entry(fact.feature_value.clone())
            .or_default()
            .push(fact.analyzer_id.clone());
    }
    let mut values = values_by_analyzer
        .into_iter()
        .map(|(value, mut analyzers)| {
            analyzers.sort();
            NwayFeatureValueGroupRow { value, analyzers }
        })
        .collect::<Vec<_>>();
    canonicalize_feature_values(&mut values);
    Ok(NwayPatternKey {
        kind: "feature".to_owned(),
        segmentation_groups: Vec::new(),
        feature_key: Some(feature.feature_key.clone()),
        feature_scope: Some(warehouse_feature_scope_row(feature)?),
        feature_values: values,
    })
}

fn warehouse_region_example_row(
    region: WarehouseRegionKey,
    flags: WarehouseRegionFlags,
    analyzers: Vec<WarehouseRegionAnalyzerFact>,
    feature_facts: Vec<WarehouseFeatureDiffFact>,
) -> WarehouseRegionExampleRow {
    let analyzers = analyzers
        .into_iter()
        .map(|fact| WarehouseRegionAnalyzerExampleRow {
            analyzer_id: fact.analyzer_id,
            covers_exactly: fact.covers_exactly,
            morpheme_start: fact.morpheme_start,
            morpheme_end: fact.morpheme_end,
            surfaces: fact.surfaces,
        })
        .collect();
    let mut feature_diffs = feature_facts
        .into_iter()
        .map(|fact| WarehouseFeatureDiffExampleRow {
            feature_key: fact.key.feature_key,
            scope_type: fact.key.scope_type,
            scope_position: fact.key.scope_position,
            scope_surface: fact.key.scope_surface,
            feature_value: fact.feature_value,
            analyzer_id: fact.analyzer_id,
        })
        .collect::<Vec<_>>();
    feature_diffs.sort_by(|left, right| {
        left.feature_key
            .cmp(&right.feature_key)
            .then_with(|| left.scope_type.cmp(&right.scope_type))
            .then_with(|| left.scope_position.cmp(&right.scope_position))
            .then_with(|| left.scope_surface.cmp(&right.scope_surface))
            .then_with(|| left.feature_value.cmp(&right.feature_value))
            .then_with(|| left.analyzer_id.cmp(&right.analyzer_id))
    });
    WarehouseRegionExampleRow {
        source_id: region.source_id,
        text_id: region.text_id,
        region_index: region.region_index,
        byte_start: flags.byte_start,
        byte_end: flags.byte_end,
        char_start: flags.char_start,
        char_end: flags.char_end,
        is_nonempty_whitespace: flags.is_nonempty_whitespace,
        is_agreement: flags.is_agreement,
        has_coverage_mismatch: flags.has_coverage_mismatch,
        has_segmentation_disagreement: flags.has_segmentation_disagreement,
        has_feature_disagreement: flags.has_feature_disagreement,
        analyzers,
        feature_diffs,
    }
}

fn warehouse_region_kind_matches(kind: WarehouseRegionKind, flags: WarehouseRegionFlags) -> bool {
    match kind {
        WarehouseRegionKind::All => !flags.is_agreement,
        WarehouseRegionKind::Segmentation => flags.has_segmentation_disagreement,
        WarehouseRegionKind::Feature => flags.has_feature_disagreement,
        WarehouseRegionKind::Coverage => flags.has_coverage_mismatch,
    }
}

fn warehouse_text_filter_matches(filter: WarehouseTextFilter, flags: WarehouseRegionFlags) -> bool {
    warehouse_text_filter_matches_nonempty_whitespace(filter, flags.is_nonempty_whitespace)
}

fn warehouse_text_filter_matches_nonempty_whitespace(
    filter: WarehouseTextFilter,
    is_nonempty_whitespace: bool,
) -> bool {
    match filter {
        WarehouseTextFilter::All => true,
        WarehouseTextFilter::WhitespaceOnly => is_nonempty_whitespace,
        WarehouseTextFilter::LexicalOnly => !is_nonempty_whitespace,
    }
}

pub(super) fn read_warehouse_table(run_dir: &Path, table: WarehouseTable) -> Result<Vec<RecordBatch>> {
    let path = run_dir.join(table.file_name());
    if path.is_dir() {
        let mut paths = fs::read_dir(&path)
            .with_context(|| format!("failed to read {}", path.display()))?
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<std::result::Result<Vec<_>, _>>()
            .with_context(|| format!("failed to read {}", path.display()))?;
        paths.retain(|path| {
            path.extension()
                .is_some_and(|extension| extension == "parquet")
        });
        paths.sort();
        let mut batches = Vec::new();
        for part in paths {
            batches.extend(read_warehouse_parquet_file(&part)?);
        }
        return Ok(batches);
    }
    read_warehouse_parquet_file(&path)
}

pub(super) fn read_warehouse_parquet_file(path: &Path) -> Result<Vec<RecordBatch>> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .with_context(|| format!("failed to read parquet metadata from {}", path.display()))?
        .build()
        .with_context(|| format!("failed to build parquet reader for {}", path.display()))?;
    reader
        .collect::<std::result::Result<Vec<_>, _>>()
        .with_context(|| format!("failed to read {}", path.display()))
}

pub(super) fn string_column(batch: &RecordBatch, index: usize) -> Result<&StringArray> {
    batch
        .column(index)
        .as_any()
        .downcast_ref::<StringArray>()
        .with_context(|| format!("column {index} is not a StringArray"))
}

pub(super) fn u64_column(batch: &RecordBatch, index: usize) -> Result<&UInt64Array> {
    batch
        .column(index)
        .as_any()
        .downcast_ref::<UInt64Array>()
        .with_context(|| format!("column {index} is not a UInt64Array"))
}

fn bool_column(batch: &RecordBatch, index: usize) -> Result<&BooleanArray> {
    batch
        .column(index)
        .as_any()
        .downcast_ref::<BooleanArray>()
        .with_context(|| format!("column {index} is not a BooleanArray"))
}

fn list_string_column(batch: &RecordBatch, index: usize) -> Result<&ListArray> {
    batch
        .column(index)
        .as_any()
        .downcast_ref::<ListArray>()
        .with_context(|| format!("column {index} is not a ListArray"))
}

fn nullable_string_value(array: &StringArray, row: usize) -> Option<String> {
    (!array.is_null(row)).then(|| array.value(row).to_owned())
}

fn nullable_u64_value(array: &UInt64Array, row: usize) -> Option<u64> {
    (!array.is_null(row)).then(|| array.value(row))
}

fn list_string_value(array: &ListArray, row: usize) -> Result<Vec<String>> {
    if array.is_null(row) {
        return Ok(Vec::new());
    }
    let values = array.value(row);
    let values = values
        .as_any()
        .downcast_ref::<StringArray>()
        .context("list value is not a StringArray")?;
    Ok((0..values.len())
        .map(|index| values.value(index).to_owned())
        .collect())
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

fn compare_nway_rows(
    left: &NwaySummaryRow,
    right: &NwaySummaryRow,
    sort_by: NwaySummarySort,
) -> std::cmp::Ordering {
    let left_value = nway_sort_value(left, sort_by);
    let right_value = nway_sort_value(right, sort_by);
    right_value
        .cmp(&left_value)
        .then_with(|| left.key.cmp(&right.key))
}

fn nway_sort_value(row: &NwaySummaryRow, sort_by: NwaySummarySort) -> usize {
    match sort_by {
        NwaySummarySort::RegionsWithSegmentationDisagreement => {
            row.regions_with_segmentation_disagreement
        }
        NwaySummarySort::RegionsWithFeatureDisagreement => row.regions_with_feature_disagreement,
        NwaySummarySort::RegionsWithCoverageMismatch => row.regions_with_coverage_mismatch,
        NwaySummarySort::VariableBoundaryCount => row.variable_boundary_count,
    }
}

pub(super) fn canonicalize_segmentation_groups(groups: &mut [NwaySegmentationGroupRow]) {
    for group in groups.iter_mut() {
        group.analyzers.sort();
    }
    groups.sort();
}

pub(super) fn canonicalize_feature_values(values: &mut [NwayFeatureValueGroupRow]) {
    for value in values.iter_mut() {
        value.analyzers.sort();
    }
    values.sort();
}

pub(super) fn nway_pattern_display(key: &NwayPatternKey) -> String {
    match key.kind.as_str() {
        "segmentation" => key
            .segmentation_groups
            .iter()
            .map(|group| {
                format!(
                    "{}:[{}]",
                    group.analyzers.join("+"),
                    group
                        .surfaces
                        .iter()
                        .map(|surface| escape_pattern_value(surface))
                        .collect::<Vec<_>>()
                        .join("|")
                )
            })
            .collect::<Vec<_>>()
            .join(" ; "),
        "feature" => {
            let values = key
                .feature_values
                .iter()
                .map(|value| {
                    format!(
                        "{}=>{}",
                        value
                            .value
                            .as_deref()
                            .map(escape_pattern_value)
                            .unwrap_or_default(),
                        value.analyzers.join("+")
                    )
                })
                .collect::<Vec<_>>()
                .join(" ; ");
            format!(
                "{} {} {}",
                key.feature_key.as_deref().unwrap_or(""),
                key.feature_scope
                    .as_ref()
                    .map(scope_display)
                    .unwrap_or_default(),
                values
            )
        }
        _ => String::new(),
    }
}

pub(super) fn escape_pattern_value(value: &str) -> String {
    value
        .chars()
        .flat_map(|ch| match ch {
            '\n' => "\\n".chars().collect::<Vec<_>>(),
            '\r' => "\\r".chars().collect::<Vec<_>>(),
            '\t' => "\\t".chars().collect::<Vec<_>>(),
            '\\' => "\\\\".chars().collect::<Vec<_>>(),
            other => vec![other],
        })
        .collect()
}

pub(super) fn scope_display(scope: &NwayFeatureScopeRow) -> String {
    match scope {
        NwayFeatureScopeRow::WholeRegion => "whole_region".to_owned(),
        NwayFeatureScopeRow::TokenPosition { position } => format!("token_position:{position}"),
        NwayFeatureScopeRow::Surface { surface } => {
            format!("surface:{}", escape_pattern_value(surface))
        }
    }
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

pub(super) fn example_filter_matches(filter: CompactExampleFilter, whitespace_only: bool) -> bool {
    match filter {
        CompactExampleFilter::All => true,
        CompactExampleFilter::WhitespaceOnly => whitespace_only,
        CompactExampleFilter::LexicalOnly => !whitespace_only,
    }
}

pub(super) fn feature_change_has_excluded_value(
    change: &FeatureChangeInputRow,
    excluded_values: &BTreeSet<String>,
) -> bool {
    value_is_excluded(change.from.as_deref(), excluded_values)
        || value_is_excluded(change.to.as_deref(), excluded_values)
}

pub(super) fn value_is_excluded(value: Option<&str>, excluded_values: &BTreeSet<String>) -> bool {
    value.is_some_and(|value| excluded_values.contains(value))
}

pub(super) fn filtered_feature_values(
    values: &[NwayFeatureValueGroupRow],
    excluded_values: &BTreeSet<String>,
) -> Vec<NwayFeatureValueGroupRow> {
    values
        .iter()
        .filter(|value| !value_is_excluded(value.value.as_deref(), excluded_values))
        .cloned()
        .collect()
}

pub(super) fn push_nway_pattern_count(
    groups: &mut BTreeMap<NwayPatternKey, NwayPatternAccumulator>,
    row: &NwayComparisonRow,
    pattern_count: &NwayPatternCountRow,
    options: &NwayPatternOptions,
) {
    if pattern_count.count == 0 {
        return;
    }

    match (options.kind, pattern_count.kind.as_str()) {
        (NwayPatternKind::Segmentation, "segmentation") => {
            let mut segmentation_groups = pattern_count.segmentation_groups.clone();
            if segmentation_groups.len() <= 1 {
                return;
            }
            canonicalize_segmentation_groups(&mut segmentation_groups);
            let key = NwayPatternKey {
                kind: "segmentation".to_owned(),
                segmentation_groups,
                feature_key: None,
                feature_scope: None,
                feature_values: Vec::new(),
            };
            groups.entry(key).or_default().push_count(
                row,
                row.source_script_category,
                pattern_count.count,
            );
        }
        (NwayPatternKind::Feature, "feature") => {
            if options
                .feature_key
                .as_ref()
                .is_some_and(|wanted| pattern_count.feature_key.as_deref() != Some(wanted))
            {
                return;
            }
            let mut values = filtered_feature_values(
                &pattern_count.feature_values,
                &options.excluded_feature_values,
            );
            if values.len() <= 1 {
                return;
            }
            canonicalize_feature_values(&mut values);
            let key = NwayPatternKey {
                kind: "feature".to_owned(),
                segmentation_groups: Vec::new(),
                feature_key: pattern_count.feature_key.clone(),
                feature_scope: pattern_count.feature_scope.clone(),
                feature_values: values,
            };
            groups.entry(key).or_default().push_count(
                row,
                row.source_script_category,
                pattern_count.count,
            );
        }
        _ => {}
    }
}

pub(super) fn push_nway_pattern_output_count(
    groups: &mut BTreeMap<NwayPatternKey, NwayPatternAccumulator>,
    row: &NwayPatternCountOutputRow,
    options: &NwayPatternOptions,
) {
    if row.count == 0 {
        return;
    }

    match (options.kind, row.kind.as_str()) {
        (NwayPatternKind::Segmentation, "segmentation") => {
            let mut segmentation_groups = row.segmentation_groups.clone();
            if segmentation_groups.len() <= 1 {
                return;
            }
            canonicalize_segmentation_groups(&mut segmentation_groups);
            let key = NwayPatternKey {
                kind: "segmentation".to_owned(),
                segmentation_groups,
                feature_key: None,
                feature_scope: None,
                feature_values: Vec::new(),
            };
            groups.entry(key).or_default().push_count_values(
                row.source_id.clone(),
                row.text_id.clone(),
                row.source_script_category,
                row.count,
            );
        }
        (NwayPatternKind::Feature, "feature") => {
            if options
                .feature_key
                .as_ref()
                .is_some_and(|wanted| row.feature_key.as_deref() != Some(wanted))
            {
                return;
            }
            let mut values =
                filtered_feature_values(&row.feature_values, &options.excluded_feature_values);
            if values.len() <= 1 {
                return;
            }
            canonicalize_feature_values(&mut values);
            let key = NwayPatternKey {
                kind: "feature".to_owned(),
                segmentation_groups: Vec::new(),
                feature_key: row.feature_key.clone(),
                feature_scope: row.feature_scope.clone(),
                feature_values: values,
            };
            groups.entry(key).or_default().push_count_values(
                row.source_id.clone(),
                row.text_id.clone(),
                row.source_script_category,
                row.count,
            );
        }
        _ => {}
    }
}

pub(super) fn is_segmentation_example(kind: &str) -> bool {
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
                script_category: None,
                exclusions: SummaryExclusions::default(),
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
                script_category: None,
                exclusions: SummaryExclusions::default(),
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
                script_category: None,
                exclusions: SummaryExclusions::default(),
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
    fn summarize_comparisons_can_filter_by_script_category() {
        let dir = temp_dir("script-filter");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("comparisons.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"japanese","text_id":"t1","source_script_category":"japanese","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":11,"one_to_one_regions":8,"one_to_one_with_feature_differences":1,"segmentation_regions":4,"whitespace_segmentation_regions":0,"lexical_segmentation_regions":4,"coverage_mismatch_regions":0,"split_regions":4,"merge_regions":0,"resegment_regions":0,"whitespace_feature_diff_regions":0,"lexical_feature_diff_regions":1,"from_morphemes_in_segmentation":4,"to_morphemes_in_segmentation":8,"boundary_precision":0.9,"boundary_recall":0.8,"boundary_f1":0.847}"#, "\n",
                r#"{"source_id":"jis-table","text_id":"JISTABLE","source_script_category":"latin-code","from_analyzer":"vibrato","to_analyzer":"sudachi-c","from_morphemes":10,"to_morphemes":11,"one_to_one_regions":8,"one_to_one_with_feature_differences":1,"segmentation_regions":40,"whitespace_segmentation_regions":0,"lexical_segmentation_regions":40,"coverage_mismatch_regions":0,"split_regions":4,"merge_regions":36,"resegment_regions":0,"whitespace_feature_diff_regions":0,"lexical_feature_diff_regions":1,"from_morphemes_in_segmentation":40,"to_morphemes_in_segmentation":40,"boundary_precision":0.9,"boundary_recall":0.8,"boundary_f1":0.847}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_comparisons(
            &path,
            CompactSummaryOptions {
                group_by: CompactSummaryGroupBy::SourceId,
                sort_by: CompactSummarySort::LexicalSegmentationRegions,
                script_category: Some(ScriptCategory::Japanese),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].key, "japanese");
        assert_eq!(rows[0].script_categories, vec!["japanese"]);

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
                script_category: None,
                sort_by: CompactExampleSummarySort::Examples,
                exclusions: SummaryExclusions::default(),
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

    #[test]
    fn summarize_examples_can_filter_by_script_category() {
        let dir = temp_dir("example-script-filter");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("examples.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":0,"kind":"merge","byte_start":0,"byte_end":3,"char_start":0,"char_end":3,"source_excerpt":"JIS","whitespace_only":false,"script_category":"latin-code","from_surfaces":["J","I","S"],"to_surfaces":["JIS"],"feature_changes":null}"#, "\n",
                r#"{"source_id":"src-b","text_id":"t2","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":0,"kind":"merge","byte_start":0,"byte_end":12,"char_start":0,"char_end":4,"source_excerpt":"徳川時代","whitespace_only":false,"script_category":"japanese","from_surfaces":["徳川","時代"],"to_surfaces":["徳川時代"],"feature_changes":null}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_examples(
            &path,
            CompactExampleSummaryOptions {
                group_by: CompactSummaryGroupBy::SourceId,
                filter: CompactExampleFilter::LexicalOnly,
                script_category: Some(ScriptCategory::Japanese),
                sort_by: CompactExampleSummarySort::Examples,
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].key, "src-b");
        assert_eq!(rows[0].script_categories, vec!["japanese"]);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_differences_groups_segmentation_patterns_and_feature_transitions() {
        let dir = temp_dir("difference-summary");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("examples.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":0,"kind":"split","byte_start":0,"byte_end":6,"char_start":0,"char_end":2,"source_excerpt":"今日","whitespace_only":false,"script_category":"japanese","from_surfaces":["今日"],"to_surfaces":["今","日"],"feature_changes":null}"#, "\n",
                r#"{"source_id":"src-b","text_id":"t2","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":0,"kind":"split","byte_start":0,"byte_end":6,"char_start":0,"char_end":2,"source_excerpt":"今日","whitespace_only":false,"script_category":"japanese","from_surfaces":["今日"],"to_surfaces":["今","日"],"feature_changes":null}"#, "\n",
                r#"{"source_id":"src-c","text_id":"t3","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":0,"kind":"merge","byte_start":0,"byte_end":2,"char_start":0,"char_end":2,"source_excerpt":"\n　","whitespace_only":true,"script_category":"whitespace","from_surfaces":["\n","　"],"to_surfaces":["\n　"],"feature_changes":null}"#, "\n",
                r#"{"source_id":"src-a","text_id":"t1","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":1,"kind":"feature_diff","byte_start":6,"byte_end":12,"char_start":2,"char_end":4,"source_excerpt":"明日","whitespace_only":false,"script_category":"japanese","from_surfaces":["明日"],"to_surfaces":["明日"],"feature_changes":[{"key":"pos1","from":"名詞","to":"副詞"},{"key":"reading","from":"アス","to":"ミョウニチ"}]}"#, "\n",
                r#"{"source_id":"src-b","text_id":"t2","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":1,"kind":"feature_diff","byte_start":6,"byte_end":12,"char_start":2,"char_end":4,"source_excerpt":"明日","whitespace_only":false,"script_category":"japanese","from_surfaces":["明日"],"to_surfaces":["明日"],"feature_changes":[{"key":"pos1","from":"名詞","to":"副詞"}]}"#, "\n",
                r#"{"source_id":"src-d","text_id":"t4","from_analyzer":"vibrato","to_analyzer":"sudachi-c","region_index":1,"kind":"feature_diff","byte_start":6,"byte_end":12,"char_start":2,"char_end":4,"source_excerpt":"昨日","whitespace_only":false,"script_category":"japanese","from_surfaces":["昨日"],"to_surfaces":["昨日"],"feature_changes":[{"key":"pos1","from":"名詞","to":"副詞"}]}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_differences(
            &path,
            CompactDifferenceSummaryOptions {
                filter: CompactExampleFilter::LexicalOnly,
                script_category: Some(ScriptCategory::Japanese),
                kind: CompactDifferenceKindFilter::All,
                feature_key: None,
                excluded_feature_values: BTreeSet::new(),
                one_to_one_lexical_features: false,
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows[0].kind, "feature");
        assert_eq!(rows[0].feature_key.as_deref(), Some("pos1"));
        assert_eq!(rows[0].feature_from.as_deref(), Some("名詞"));
        assert_eq!(rows[0].feature_to.as_deref(), Some("副詞"));
        assert_eq!(rows[0].examples, 3);
        assert_eq!(rows[0].source_ids, vec!["src-a", "src-b", "src-d"]);

        assert_eq!(rows[1].kind, "segmentation");
        assert_eq!(rows[1].region_kind.as_deref(), Some("split"));
        assert_eq!(rows[1].from_surfaces, vec!["今日"]);
        assert_eq!(rows[1].to_surfaces, vec!["今", "日"]);
        assert_eq!(rows[1].examples, 2);

        assert!(
            rows.iter()
                .all(|row| row.script_categories == vec!["japanese"])
        );
        assert!(
            !rows
                .iter()
                .any(|row| row.region_kind.as_deref() == Some("merge"))
        );

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_nway_groups_exact_region_counts() {
        let dir = temp_dir("nway-summary");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("nway.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"t1","source_script_category":"japanese","analyzers":["vibrato","sudachi-a","sudachi-c"],"analyzer_count":3,"regions":10,"agreement_regions":6,"regions_with_feature_disagreement":1,"regions_with_segmentation_disagreement":3,"regions_with_coverage_mismatch":0,"whitespace_regions":1,"lexical_regions":9,"unanimous_boundary_count":8,"variable_boundary_count":2,"examples":[]}"#, "\n",
                r#"{"source_id":"src-b","text_id":"t2","source_script_category":"japanese","analyzers":["vibrato","sudachi-a","sudachi-c"],"analyzer_count":3,"regions":10,"agreement_regions":2,"regions_with_feature_disagreement":2,"regions_with_segmentation_disagreement":6,"regions_with_coverage_mismatch":0,"whitespace_regions":0,"lexical_regions":10,"unanimous_boundary_count":4,"variable_boundary_count":6,"examples":[]}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_nway(
            &path,
            NwaySummaryOptions {
                group_by: CompactSummaryGroupBy::SourceId,
                sort_by: NwaySummarySort::RegionsWithSegmentationDisagreement,
                script_category: Some(ScriptCategory::Japanese),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows[0].key, "src-b");
        assert_eq!(rows[0].regions_with_segmentation_disagreement, 6);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_nway_patterns_groups_segmentation_partitions_and_feature_values() {
        let dir = temp_dir("nway-patterns");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("nway.jsonl");
        let row = r#"{"source_id":"src-a","text_id":"t1","source_script_category":"japanese","analyzers":["vibrato","sudachi-a","sudachi-c"],"analyzer_count":3,"regions":1,"agreement_regions":0,"regions_with_feature_disagreement":1,"regions_with_segmentation_disagreement":1,"regions_with_coverage_mismatch":0,"whitespace_regions":0,"lexical_regions":1,"unanimous_boundary_count":0,"variable_boundary_count":1,"examples":[{"region_index":0,"char_start":0,"char_end":2,"source_excerpt":"今日","per_analyzer_surfaces":[{"analyzer":"vibrato","surfaces":["今日"]},{"analyzer":"sudachi-a","surfaces":["今日"]},{"analyzer":"sudachi-c","surfaces":["今","日"]}],"segmentation_groups":[{"surfaces":["今日"],"analyzers":["sudachi-a","vibrato"]},{"surfaces":["今","日"],"analyzers":["sudachi-c"]}],"feature_groups":[{"key":"pos1","scope":{"kind":"whole_region"},"values":[{"value":"名詞","analyzers":["sudachi-a","vibrato"]},{"value":"空白","analyzers":["sudachi-c"]}]}]}]}"#;
        fs::write(&path, format!("{row}\n{}\n", row.replace("src-a", "src-b"))).unwrap();

        let segmentation = summarize_nway_patterns(
            &path,
            NwayPatternOptions {
                kind: NwayPatternKind::Segmentation,
                feature_key: None,
                script_category: Some(ScriptCategory::Japanese),
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();
        assert_eq!(segmentation.len(), 1);
        assert_eq!(segmentation[0].examples, 2);
        assert_eq!(segmentation[0].segmentation_groups.len(), 2);

        let feature = summarize_nway_patterns(
            &path,
            NwayPatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: Some("pos1".to_owned()),
                script_category: Some(ScriptCategory::Japanese),
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();
        assert_eq!(feature.len(), 1);
        assert_eq!(feature[0].examples, 2);
        assert_eq!(feature[0].feature_key.as_deref(), Some("pos1"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_differences_can_exclude_feature_values_and_focus_one_to_one_lexical() {
        let dir = temp_dir("difference-feature-filters");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("examples.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"txt-a","from_analyzer":"vibrato","to_analyzer":"sudachi-a","kind":"feature_diff","source_excerpt":"猫","from_surfaces":["猫"],"to_surfaces":["猫"],"feature_changes":[{"key":"pos1","from":"名詞","to":"空白"}],"whitespace_only":false,"script_category":"japanese"}"#, "\n",
                r#"{"source_id":"src-b","text_id":"txt-b","from_analyzer":"vibrato","to_analyzer":"sudachi-a","kind":"feature_diff","source_excerpt":"走る","from_surfaces":["走る"],"to_surfaces":["走る"],"feature_changes":[{"key":"pos1","from":"動詞","to":"名詞"}],"whitespace_only":false,"script_category":"japanese"}"#, "\n",
                r#"{"source_id":"src-c","text_id":"txt-c","from_analyzer":"vibrato","to_analyzer":"sudachi-a","kind":"split","source_excerpt":"犬","from_surfaces":["犬"],"to_surfaces":["犬"],"feature_changes":null,"whitespace_only":false,"script_category":"japanese"}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_compact_differences(
            &path,
            CompactDifferenceSummaryOptions {
                filter: CompactExampleFilter::All,
                script_category: None,
                kind: CompactDifferenceKindFilter::All,
                feature_key: Some("pos1".to_owned()),
                excluded_feature_values: BTreeSet::from(["空白".to_owned()]),
                one_to_one_lexical_features: true,
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].kind, "feature");
        assert_eq!(rows[0].feature_from.as_deref(), Some("動詞"));
        assert_eq!(rows[0].feature_to.as_deref(), Some("名詞"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_nway_patterns_prefers_exact_pattern_counts() {
        let dir = temp_dir("nway-patterns-exact");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("nway.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"txt-a","source_script_category":"japanese","analyzers":["vibrato","sudachi-a"],"analyzer_count":2,"regions":7,"agreement_regions":0,"regions_with_feature_disagreement":0,"regions_with_segmentation_disagreement":7,"regions_with_coverage_mismatch":0,"whitespace_regions":0,"lexical_regions":7,"unanimous_boundary_count":0,"variable_boundary_count":7,"pattern_counts":[{"kind":"segmentation","count":7,"segmentation_groups":[{"surfaces":["今日"],"analyzers":["vibrato"]},{"surfaces":["今","日"],"analyzers":["sudachi-a"]}],"feature_key":null,"feature_scope":null,"feature_values":[]}],"examples":[]}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_nway_patterns(
            &path,
            NwayPatternOptions {
                kind: NwayPatternKind::Segmentation,
                feature_key: None,
                excluded_feature_values: BTreeSet::new(),
                script_category: None,
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].examples, 7);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_nway_pattern_counts_reads_narrow_exact_rows() {
        let dir = temp_dir("nway-pattern-counts");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("nway-pattern-counts.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"txt-a","source_script_category":"japanese","kind":"segmentation","count":7,"segmentation_groups":[{"surfaces":["今日"],"analyzers":["vibrato"]},{"surfaces":["今","日"],"analyzers":["sudachi-a"]}],"feature_key":null,"feature_scope":null,"feature_values":[]}"#, "\n",
                r#"{"source_id":"src-b","text_id":"txt-b","source_script_category":"japanese","kind":"segmentation","count":5,"segmentation_groups":[{"surfaces":["今日"],"analyzers":["vibrato"]},{"surfaces":["今","日"],"analyzers":["sudachi-a"]}],"feature_key":null,"feature_scope":null,"feature_values":[]}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_nway_pattern_counts(
            &path,
            NwayPatternOptions {
                kind: NwayPatternKind::Segmentation,
                feature_key: None,
                excluded_feature_values: BTreeSet::new(),
                script_category: Some(ScriptCategory::Japanese),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].examples, 12);
        assert_eq!(rows[0].source_ids, vec!["src-a", "src-b"]);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn summarize_warehouse_patterns_reads_segmentation_disagreement_facts() {
        use crate::warehouse::schema::{
            NwayRegionAnalyzerRow, NwayRegionRow, RunRow, WarehousePaths,
        };
        use crate::warehouse::writer::WarehouseWriter;

        let root = temp_dir("warehouse-patterns");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: crate::warehouse::schema::SCHEMA_VERSION,
                run_id: "run-a".to_owned(),
                created_at_utc: "2026-05-01T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 2,
                error_count: 0,
            }])
            .unwrap();
        writer
            .append_nway_regions(&[
                NwayRegionRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    byte_start: 0,
                    byte_end: 6,
                    char_start: 0,
                    char_end: 2,
                    is_nonempty_whitespace: false,
                    is_agreement: false,
                    has_coverage_mismatch: false,
                    has_segmentation_disagreement: true,
                    has_feature_disagreement: false,
                },
                NwayRegionRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    byte_start: 6,
                    byte_end: 9,
                    char_start: 2,
                    char_end: 3,
                    is_nonempty_whitespace: false,
                    is_agreement: true,
                    has_coverage_mismatch: false,
                    has_segmentation_disagreement: false,
                    has_feature_disagreement: false,
                },
                NwayRegionRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 2,
                    byte_start: 9,
                    byte_end: 10,
                    char_start: 3,
                    char_end: 4,
                    is_nonempty_whitespace: true,
                    is_agreement: false,
                    has_coverage_mismatch: false,
                    has_segmentation_disagreement: true,
                    has_feature_disagreement: false,
                },
            ])
            .unwrap();
        writer
            .append_nway_region_analyzers(&[
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    analyzer_id: "vibrato".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 0,
                    morpheme_end: 1,
                    surfaces: vec!["今日".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    analyzer_id: "sudachi-c".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 0,
                    morpheme_end: 2,
                    surfaces: vec!["今".to_owned(), "日".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    analyzer_id: "vibrato".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 1,
                    morpheme_end: 2,
                    surfaces: vec!["は".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    analyzer_id: "sudachi-c".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 2,
                    morpheme_end: 3,
                    surfaces: vec!["は".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 2,
                    analyzer_id: "vibrato".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 2,
                    morpheme_end: 3,
                    surfaces: vec![" ".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 2,
                    analyzer_id: "sudachi-c".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 3,
                    morpheme_end: 5,
                    surfaces: vec!["".to_owned(), " ".to_owned()],
                },
            ])
            .unwrap();
        writer.finalize().unwrap();

        let rows = summarize_warehouse_nway_patterns(
            &paths.final_dir,
            WarehousePatternOptions {
                kind: NwayPatternKind::Segmentation,
                feature_key: None,
                feature_profile: WarehouseFeatureProfile::Raw,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].examples, 1);
        assert_eq!(rows[0].source_ids, vec!["source-a"]);
        assert!(rows[0].pattern.contains("vibrato:[今日]"));
        assert!(rows[0].pattern.contains("sudachi-c:[今|日]"));

        let examples = summarize_warehouse_pattern_examples(
            &paths.final_dir,
            WarehousePatternExampleOptions {
                kind: NwayPatternKind::Segmentation,
                pattern: rows[0].pattern.clone(),
                feature_key: None,
                feature_profile: WarehouseFeatureProfile::Raw,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(examples.len(), 1);
        assert_eq!(examples[0].region_index, 0);
        assert_eq!(examples[0].analyzers.len(), 2);
        assert!(examples[0].feature_diffs.is_empty());

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn summarize_warehouse_patterns_reads_feature_disagreement_facts() {
        use crate::warehouse::schema::{
            NwayFeatureDiffRow, NwayRegionAnalyzerRow, NwayRegionRow, RunRow, WarehousePaths,
        };
        use crate::warehouse::writer::WarehouseWriter;

        let root = temp_dir("warehouse-feature-patterns");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: crate::warehouse::schema::SCHEMA_VERSION,
                run_id: "run-a".to_owned(),
                created_at_utc: "2026-05-01T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 2,
                error_count: 0,
            }])
            .unwrap();
        writer
            .append_nway_regions(&[NwayRegionRow {
                run_id: "run-a".to_owned(),
                source_id: "source-a".to_owned(),
                text_id: "work-a".to_owned(),
                region_index: 0,
                byte_start: 0,
                byte_end: 6,
                char_start: 0,
                char_end: 2,
                is_nonempty_whitespace: false,
                is_agreement: false,
                has_coverage_mismatch: false,
                has_segmentation_disagreement: false,
                has_feature_disagreement: true,
            }])
            .unwrap();
        writer
            .append_nway_region_analyzers(&[
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    analyzer_id: "vibrato".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 0,
                    morpheme_end: 1,
                    surfaces: vec!["今日".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    analyzer_id: "sudachi-c".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 0,
                    morpheme_end: 1,
                    surfaces: vec!["今日".to_owned()],
                },
            ])
            .unwrap();
        writer
            .append_nway_feature_diffs(&[
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "pos1".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("名詞".to_owned()),
                    analyzer_id: "vibrato".to_owned(),
                },
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "pos1".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("空白".to_owned()),
                    analyzer_id: "sudachi-c".to_owned(),
                },
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "lemma".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("今日".to_owned()),
                    analyzer_id: "vibrato".to_owned(),
                },
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "lemma".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("きょう".to_owned()),
                    analyzer_id: "sudachi-c".to_owned(),
                },
            ])
            .unwrap();
        writer.finalize().unwrap();

        let rows = summarize_warehouse_nway_patterns(
            &paths.final_dir,
            WarehousePatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: Some("pos1".to_owned()),
                feature_profile: WarehouseFeatureProfile::Raw,
                text_filter: WarehouseTextFilter::All,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].examples, 1);
        assert_eq!(rows[0].feature_key, Some("pos1".to_owned()));
        assert!(rows[0].pattern.contains("名詞=>vibrato"));
        assert!(rows[0].pattern.contains("空白=>sudachi-c"));

        let examples = summarize_warehouse_pattern_examples(
            &paths.final_dir,
            WarehousePatternExampleOptions {
                kind: NwayPatternKind::Feature,
                pattern: rows[0].pattern.clone(),
                feature_key: Some("pos1".to_owned()),
                feature_profile: WarehouseFeatureProfile::Raw,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(examples.len(), 1);
        assert_eq!(examples[0].region_index, 0);
        assert_eq!(examples[0].analyzers.len(), 2);
        assert_eq!(examples[0].feature_diffs.len(), 2);
        assert!(
            examples[0]
                .feature_diffs
                .iter()
                .all(|feature| feature.feature_key == "pos1")
        );

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn warehouse_pattern_duckdb_sql_escapes_literals_and_preserves_surface_boundaries() {
        let segmentation_sql = warehouse_pattern_duckdb_sql(
            Path::new("scratch/warehouse/runs/run's"),
            &WarehousePatternOptions {
                kind: NwayPatternKind::Segmentation,
                feature_key: None,
                feature_profile: WarehouseFeatureProfile::Raw,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::from_values(
                    ["src'1".to_owned()],
                    ["text'1".to_owned()],
                ),
                limit: 7,
            },
        );

        assert!(
            segmentation_sql
                .contains("read_parquet('scratch/warehouse/runs/run''s/nway_regions.parquet')")
        );
        assert!(segmentation_sql.contains("source_id NOT IN ('src''1')"));
        assert!(segmentation_sql.contains("text_id NOT IN ('text''1')"));
        assert!(segmentation_sql.contains("NOT is_nonempty_whitespace"));
        assert!(segmentation_sql.contains("array_to_string(a.surfaces, '|')"));
        assert!(segmentation_sql.contains("LIMIT 7"));

        let feature_sql = warehouse_pattern_duckdb_sql(
            Path::new("scratch/warehouse/runs/run's"),
            &WarehousePatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: Some("pos'1".to_owned()),
                feature_profile: WarehouseFeatureProfile::Raw,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: ["空'白".to_owned()].into_iter().collect(),
                exclusions: SummaryExclusions::default(),
                limit: 7,
            },
        );

        assert!(feature_sql.contains("feature_key = 'pos''1'"));
        assert!(feature_sql.contains("feature_value NOT IN ('空''白')"));
    }

    #[test]
    fn warehouse_region_examples_duckdb_sql_limits_before_large_joins() {
        let sql = warehouse_region_examples_duckdb_sql(
            Path::new("scratch/warehouse/runs/run's"),
            &WarehouseRegionOptions {
                kind: WarehouseRegionKind::Feature,
                text_filter: WarehouseTextFilter::LexicalOnly,
                exclusions: SummaryExclusions::from_values(
                    ["src'1".to_owned()],
                    ["text'1".to_owned()],
                ),
                limit: 11,
            },
        );

        assert!(sql.contains("read_parquet('scratch/warehouse/runs/run''s/nway_regions.parquet')"));
        assert!(sql.contains(
            "read_parquet('scratch/warehouse/runs/run''s/nway_region_analyzers.parquet')"
        ));
        assert!(
            sql.contains(
                "read_parquet('scratch/warehouse/runs/run''s/nway_feature_diffs.parquet')"
            )
        );
        assert!(sql.contains("has_feature_disagreement"));
        assert!(sql.contains("NOT is_nonempty_whitespace"));
        assert!(sql.contains("source_id NOT IN ('src''1')"));
        assert!(sql.contains("text_id NOT IN ('text''1')"));
        assert!(sql.contains("LIMIT 11"));
        assert!(
            sql.find("LIMIT 11").unwrap()
                < sql
                    .find(
                        "read_parquet('scratch/warehouse/runs/run''s/nway_feature_diffs.parquet')"
                    )
                    .unwrap()
        );
    }

    #[test]
    fn warehouse_pattern_examples_duckdb_sql_filters_by_pattern_and_feature_values() {
        let sql = warehouse_pattern_examples_duckdb_sql(
            Path::new("scratch/warehouse/runs/run's"),
            &WarehousePatternExampleOptions {
                kind: NwayPatternKind::Feature,
                pattern: "pos'1 whole_region 名詞=>vibrato ; 空白=>sudachi-c".to_owned(),
                feature_key: Some("pos'1".to_owned()),
                feature_profile: WarehouseFeatureProfile::Raw,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: ["空'白".to_owned()].into_iter().collect(),
                exclusions: SummaryExclusions::default(),
                limit: 3,
            },
        );

        assert!(sql.contains("pattern = 'pos''1 whole_region 名詞=>vibrato ; 空白=>sudachi-c'"));
        assert!(sql.contains("feature_key = 'pos''1'"));
        assert!(sql.contains("feature_value NOT IN ('空''白')"));
        assert!(sql.contains("LIMIT 3"));
        assert!(sql.contains("feature_diffs"));
    }

    #[test]
    fn warehouse_duckdb_sql_uses_globs_for_partitioned_tables() {
        let root = temp_dir("warehouse-duckdb-partitions");
        let run_dir = root.join("runs").join("run-a");
        fs::create_dir_all(run_dir.join(WarehouseTable::NwayRegions.file_name())).unwrap();
        fs::create_dir_all(run_dir.join(WarehouseTable::NwayRegionAnalyzers.file_name())).unwrap();
        fs::create_dir_all(run_dir.join(WarehouseTable::NwayFeatureDiffs.file_name())).unwrap();

        let sql = warehouse_region_examples_duckdb_sql(
            &run_dir,
            &WarehouseRegionOptions {
                kind: WarehouseRegionKind::All,
                text_filter: WarehouseTextFilter::All,
                exclusions: SummaryExclusions::default(),
                limit: 1,
            },
        );

        assert!(sql.contains("nway_regions.parquet/*.parquet"));
        assert!(sql.contains("nway_region_analyzers.parquet/*.parquet"));
        assert!(sql.contains("nway_feature_diffs.parquet/*.parquet"));

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn warehouse_duckdb_sql_sets_disk_temp_directory_and_memory_bound() {
        let sql = warehouse_pattern_duckdb_sql(
            Path::new("scratch/warehouse/runs/run's"),
            &WarehousePatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: Some("pos1".to_owned()),
                feature_profile: WarehouseFeatureProfile::Raw,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 5,
            },
        );

        assert!(sql.starts_with("SET temp_directory = "));
        assert!(sql.contains("scratch/warehouse/.duckdb_tmp"));
        assert!(sql.contains("SET threads = 4;"));
        assert!(sql.contains("SET preserve_insertion_order = false;"));
        assert!(sql.contains("SET memory_limit = '16GB';"));
        assert!(sql.contains("COPY ("));
    }

    #[test]
    fn warehouse_feature_profile_core_limits_feature_keys_in_duckdb_sql() {
        let sql = warehouse_pattern_duckdb_sql(
            Path::new("scratch/warehouse/runs/run-a"),
            &WarehousePatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: None,
                feature_profile: WarehouseFeatureProfile::Core,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 5,
            },
        );

        assert!(sql.contains("UNION ALL"));
        assert!(sql.contains("feature_key = 'pos1'"));
        assert!(sql.contains("feature_key = 'pos2'"));
        assert!(sql.contains("feature_key = 'pos3'"));
        assert!(sql.contains("feature_key = 'pos4'"));
        assert!(!sql.contains("feature_key IN ('pos1', 'pos2', 'pos3', 'pos4')"));
    }

    #[test]
    fn merged_pattern_tsv_orders_rows_globally_by_example_count() {
        let pos1 = concat!(
            "kind\texamples\tsource_count\ttext_count\tsample_source_ids\tsample_text_ids\tscript_categories\tpattern\n",
            "feature\t10\t1\t1\ts1\tt1\t\tpos1 whole_region A=>x ; B=>y\n",
            "feature\t8\t1\t1\ts2\tt2\t\tpos1 whole_region C=>x ; D=>y\n",
        );
        let pos2 = concat!(
            "kind\texamples\tsource_count\ttext_count\tsample_source_ids\tsample_text_ids\tscript_categories\tpattern\n",
            "feature\t12\t1\t1\ts3\tt3\t\tpos2 whole_region E=>x ; F=>y\n",
            "feature\t7\t1\t1\ts4\tt4\t\tpos2 whole_region G=>x ; H=>y\n",
        );
        let mut output = Vec::new();

        write_merged_pattern_tsv([pos1, pos2], 3, &mut output).unwrap();

        let output = String::from_utf8(output).unwrap();
        let lines = output.lines().collect::<Vec<_>>();
        assert_eq!(lines.len(), 4);
        assert!(lines[1].contains("feature\t12"));
        assert!(lines[2].contains("feature\t10"));
        assert!(lines[3].contains("feature\t8"));
    }

    #[test]
    fn warehouse_feature_pattern_counts_sql_reads_materialized_counts() {
        let sql = warehouse_feature_pattern_counts_duckdb_sql(
            Path::new("scratch/warehouse/runs/run-a"),
            &WarehousePatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: Some("pos1".to_owned()),
                feature_profile: WarehouseFeatureProfile::Core,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 12,
            },
        );

        assert!(sql.contains("feature_pattern_counts.parquet"));
        assert!(sql.contains("feature_profile = 'core'"));
        assert!(sql.contains("feature_key = 'pos1'"));
        assert!(sql.contains("NOT is_nonempty_whitespace"));
        assert!(sql.contains("LIMIT 12"));
    }

    #[test]
    fn materialize_core_feature_pattern_counts_sql_writes_partitionable_table() {
        let sql = materialize_core_feature_pattern_counts_duckdb_sql(
            Path::new("scratch/warehouse/runs/run-a"),
            Path::new("scratch/warehouse/runs/run-a/feature_pattern_counts.parquet"),
            "pos1",
        );

        assert!(sql.contains("COPY ("));
        assert!(sql.contains("feature_profile"));
        assert!(sql.contains("'core'"));
        assert!(sql.contains("feature_key = 'pos1'"));
        assert!(sql.contains("is_nonempty_whitespace"));
        assert!(sql.contains("TO 'scratch/warehouse/runs/run-a/feature_pattern_counts.parquet'"));
    }

    #[test]
    fn materialized_core_feature_patterns_are_used_without_raw_feature_diffs() {
        use crate::warehouse::schema::{NwayFeatureDiffRow, NwayRegionRow, RunRow, WarehousePaths};
        use crate::warehouse::writer::WarehouseWriter;

        let root = temp_dir("warehouse-materialized-feature-patterns");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: crate::warehouse::schema::SCHEMA_VERSION,
                run_id: "run-a".to_owned(),
                created_at_utc: "2026-05-01T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 2,
                error_count: 0,
            }])
            .unwrap();
        writer
            .append_nway_regions(&[NwayRegionRow {
                run_id: "run-a".to_owned(),
                source_id: "source-a".to_owned(),
                text_id: "work-a".to_owned(),
                region_index: 0,
                byte_start: 0,
                byte_end: 6,
                char_start: 0,
                char_end: 2,
                is_nonempty_whitespace: false,
                is_agreement: false,
                has_coverage_mismatch: false,
                has_segmentation_disagreement: false,
                has_feature_disagreement: true,
            }])
            .unwrap();
        writer
            .append_nway_feature_diffs(&[
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "pos1".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("名詞".to_owned()),
                    analyzer_id: "vibrato".to_owned(),
                },
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "pos1".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("空白".to_owned()),
                    analyzer_id: "sudachi-c".to_owned(),
                },
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "lemma".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("今日".to_owned()),
                    analyzer_id: "vibrato".to_owned(),
                },
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "lemma".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("きょう".to_owned()),
                    analyzer_id: "sudachi-c".to_owned(),
                },
            ])
            .unwrap();
        writer.finalize().unwrap();

        materialize_warehouse_core_feature_pattern_counts(&paths.final_dir, Some("pos1")).unwrap();
        let raw_features = paths.final_dir.join("nway_feature_diffs.parquet");
        if raw_features.is_dir() {
            fs::remove_dir_all(&raw_features).unwrap();
        } else {
            fs::remove_file(&raw_features).unwrap();
        }

        let rows = summarize_warehouse_nway_patterns(
            &paths.final_dir,
            WarehousePatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: Some("pos1".to_owned()),
                feature_profile: WarehouseFeatureProfile::Core,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].examples, 1);
        assert_eq!(rows[0].feature_key, Some("pos1".to_owned()));
        assert!(rows[0].pattern.contains("名詞=>vibrato"));
        assert!(rows[0].pattern.contains("空白=>sudachi-c"));
        assert!(!rows[0].pattern.contains("lemma"));

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn warehouse_feature_profile_schema_requires_same_schema_disagreement() {
        let sql = warehouse_pattern_duckdb_sql(
            Path::new("scratch/warehouse/runs/run-a"),
            &WarehousePatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: None,
                feature_profile: WarehouseFeatureProfile::Schema,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 5,
            },
        );

        assert!(sql.contains("schema_value_counts"));
        assert!(sql.contains("HAVING count(DISTINCT feature_value) > 1"));
        assert!(sql.contains("analyzer_schema_id"));
    }

    #[test]
    fn summarize_warehouse_nway_reads_region_and_boundary_facts() {
        use crate::warehouse::schema::{
            AnalysisRow, MorphemeRow, NwayRegionRow, RunRow, SourceRow, WarehousePaths,
        };
        use crate::warehouse::writer::WarehouseWriter;

        let root = temp_dir("warehouse-nway-summary");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: crate::warehouse::schema::SCHEMA_VERSION,
                run_id: "run-a".to_owned(),
                created_at_utc: "2026-05-01T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 2,
                error_count: 0,
            }])
            .unwrap();
        writer
            .append_sources(&[SourceRow {
                run_id: "run-a".to_owned(),
                source_id: "source-a".to_owned(),
                text_id: "work-a".to_owned(),
                aat_path: "scratch/source-a.json".to_owned(),
                source_bytes: 6,
                source_chars: 2,
            }])
            .unwrap();
        writer
            .append_analyses(&[
                AnalysisRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    analyzer_id: "vibrato".to_owned(),
                    morpheme_count: 1,
                },
                AnalysisRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    analyzer_id: "sudachi-c".to_owned(),
                    morpheme_count: 2,
                },
            ])
            .unwrap();
        writer
            .append_morphemes(&[
                MorphemeRow {
                    run_id: "run-a".into(),
                    source_id: "source-a".into(),
                    text_id: "work-a".into(),
                    analyzer_id: "vibrato".into(),
                    morpheme_index: 0,
                    byte_start: 0,
                    byte_end: 6,
                    char_start: 0,
                    char_end: 2,
                    surface: "今日".to_owned(),
                },
                MorphemeRow {
                    run_id: "run-a".into(),
                    source_id: "source-a".into(),
                    text_id: "work-a".into(),
                    analyzer_id: "sudachi-c".into(),
                    morpheme_index: 0,
                    byte_start: 0,
                    byte_end: 3,
                    char_start: 0,
                    char_end: 1,
                    surface: "今".to_owned(),
                },
                MorphemeRow {
                    run_id: "run-a".into(),
                    source_id: "source-a".into(),
                    text_id: "work-a".into(),
                    analyzer_id: "sudachi-c".into(),
                    morpheme_index: 1,
                    byte_start: 3,
                    byte_end: 6,
                    char_start: 1,
                    char_end: 2,
                    surface: "日".to_owned(),
                },
            ])
            .unwrap();
        writer
            .append_nway_regions(&[NwayRegionRow {
                run_id: "run-a".to_owned(),
                source_id: "source-a".to_owned(),
                text_id: "work-a".to_owned(),
                region_index: 0,
                byte_start: 0,
                byte_end: 6,
                char_start: 0,
                char_end: 2,
                is_nonempty_whitespace: false,
                is_agreement: false,
                has_coverage_mismatch: false,
                has_segmentation_disagreement: true,
                has_feature_disagreement: false,
            }])
            .unwrap();
        writer.finalize().unwrap();

        let rows = summarize_warehouse_nway(
            &paths.final_dir,
            NwaySummaryOptions {
                group_by: CompactSummaryGroupBy::SourceId,
                sort_by: NwaySummarySort::RegionsWithSegmentationDisagreement,
                script_category: None,
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].key, "source-a");
        assert_eq!(rows[0].rows, 1);
        assert_eq!(rows[0].analyzer_count, 2);
        assert_eq!(rows[0].regions, 1);
        assert_eq!(rows[0].regions_with_segmentation_disagreement, 1);
        assert_eq!(rows[0].lexical_regions, 1);
        assert_eq!(rows[0].unanimous_boundary_count, 0);
        assert_eq!(rows[0].variable_boundary_count, 1);

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn summarize_warehouse_pairwise_derives_pair_facts_from_nway_tables() {
        use crate::warehouse::schema::{
            AnalysisRow, MorphemeRow, NwayFeatureDiffRow, NwayRegionAnalyzerRow, NwayRegionRow,
            RunRow, SourceRow, WarehousePaths,
        };
        use crate::warehouse::writer::WarehouseWriter;

        let root = temp_dir("warehouse-pairwise-summary");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: crate::warehouse::schema::SCHEMA_VERSION,
                run_id: "run-a".to_owned(),
                created_at_utc: "2026-05-01T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 2,
                error_count: 0,
            }])
            .unwrap();
        writer
            .append_sources(&[SourceRow {
                run_id: "run-a".to_owned(),
                source_id: "source-a".to_owned(),
                text_id: "work-a".to_owned(),
                aat_path: "scratch/source-a.json".to_owned(),
                source_bytes: 6,
                source_chars: 2,
            }])
            .unwrap();
        writer
            .append_analyses(&[
                AnalysisRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    analyzer_id: "vibrato".to_owned(),
                    morpheme_count: 1,
                },
                AnalysisRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    analyzer_id: "sudachi-c".to_owned(),
                    morpheme_count: 2,
                },
            ])
            .unwrap();
        writer
            .append_morphemes(&[
                MorphemeRow {
                    run_id: "run-a".into(),
                    source_id: "source-a".into(),
                    text_id: "work-a".into(),
                    analyzer_id: "vibrato".into(),
                    morpheme_index: 0,
                    byte_start: 0,
                    byte_end: 6,
                    char_start: 0,
                    char_end: 2,
                    surface: "今日".to_owned(),
                },
                MorphemeRow {
                    run_id: "run-a".into(),
                    source_id: "source-a".into(),
                    text_id: "work-a".into(),
                    analyzer_id: "sudachi-c".into(),
                    morpheme_index: 0,
                    byte_start: 0,
                    byte_end: 3,
                    char_start: 0,
                    char_end: 1,
                    surface: "今".to_owned(),
                },
                MorphemeRow {
                    run_id: "run-a".into(),
                    source_id: "source-a".into(),
                    text_id: "work-a".into(),
                    analyzer_id: "sudachi-c".into(),
                    morpheme_index: 1,
                    byte_start: 3,
                    byte_end: 6,
                    char_start: 1,
                    char_end: 2,
                    surface: "日".to_owned(),
                },
            ])
            .unwrap();
        writer
            .append_nway_regions(&[
                NwayRegionRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    byte_start: 0,
                    byte_end: 6,
                    char_start: 0,
                    char_end: 2,
                    is_nonempty_whitespace: false,
                    is_agreement: false,
                    has_coverage_mismatch: false,
                    has_segmentation_disagreement: true,
                    has_feature_disagreement: true,
                },
                NwayRegionRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    byte_start: 6,
                    byte_end: 7,
                    char_start: 2,
                    char_end: 3,
                    is_nonempty_whitespace: true,
                    is_agreement: false,
                    has_coverage_mismatch: false,
                    has_segmentation_disagreement: true,
                    has_feature_disagreement: false,
                },
            ])
            .unwrap();
        writer
            .append_nway_region_analyzers(&[
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    analyzer_id: "vibrato".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 0,
                    morpheme_end: 1,
                    surfaces: vec!["今日".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    analyzer_id: "sudachi-c".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 0,
                    morpheme_end: 2,
                    surfaces: vec!["今".to_owned(), "日".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    analyzer_id: "vibrato".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 1,
                    morpheme_end: 2,
                    surfaces: vec![" ".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    analyzer_id: "sudachi-c".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 2,
                    morpheme_end: 4,
                    surfaces: vec!["".to_owned(), " ".to_owned()],
                },
            ])
            .unwrap();
        writer
            .append_nway_feature_diffs(&[
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "pos1".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("名詞".to_owned()),
                    analyzer_id: "vibrato".to_owned(),
                },
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "pos1".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("空白".to_owned()),
                    analyzer_id: "sudachi-c".to_owned(),
                },
            ])
            .unwrap();
        writer.finalize().unwrap();

        let rows = summarize_warehouse_pairwise(
            &paths.final_dir,
            WarehousePairwiseSummaryOptions {
                sort_by: WarehousePairwiseSort::SegmentationRegions,
                text_filter: WarehouseTextFilter::LexicalOnly,
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].source_id, "source-a");
        assert_eq!(rows[0].text_id, "work-a");
        assert_eq!(rows[0].from_analyzer, "sudachi-c");
        assert_eq!(rows[0].to_analyzer, "vibrato");
        assert_eq!(rows[0].regions, 1);
        assert_eq!(rows[0].segmentation_regions, 1);
        assert_eq!(rows[0].feature_regions, 1);
        assert_eq!(rows[0].coverage_regions, 0);
        assert_eq!(rows[0].unanimous_boundary_count, 0);
        assert_eq!(rows[0].variable_boundary_count, 1);

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn summarize_warehouse_regions_returns_bounded_concrete_evidence() {
        use crate::warehouse::schema::{
            NwayFeatureDiffRow, NwayRegionAnalyzerRow, NwayRegionRow, RunRow, WarehousePaths,
        };
        use crate::warehouse::writer::WarehouseWriter;

        let root = temp_dir("warehouse-regions");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: crate::warehouse::schema::SCHEMA_VERSION,
                run_id: "run-a".to_owned(),
                created_at_utc: "2026-05-01T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 2,
                error_count: 0,
            }])
            .unwrap();
        writer
            .append_nway_regions(&[
                NwayRegionRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    byte_start: 0,
                    byte_end: 6,
                    char_start: 0,
                    char_end: 2,
                    is_nonempty_whitespace: false,
                    is_agreement: false,
                    has_coverage_mismatch: false,
                    has_segmentation_disagreement: true,
                    has_feature_disagreement: true,
                },
                NwayRegionRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    byte_start: 6,
                    byte_end: 7,
                    char_start: 2,
                    char_end: 3,
                    is_nonempty_whitespace: true,
                    is_agreement: false,
                    has_coverage_mismatch: false,
                    has_segmentation_disagreement: true,
                    has_feature_disagreement: false,
                },
            ])
            .unwrap();
        writer
            .append_nway_region_analyzers(&[
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    analyzer_id: "vibrato".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 0,
                    morpheme_end: 1,
                    surfaces: vec!["今日".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    analyzer_id: "sudachi-c".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 0,
                    morpheme_end: 2,
                    surfaces: vec!["今".to_owned(), "日".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    analyzer_id: "vibrato".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 1,
                    morpheme_end: 2,
                    surfaces: vec![" ".to_owned()],
                },
                NwayRegionAnalyzerRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 1,
                    analyzer_id: "sudachi-c".to_owned(),
                    covers_exactly: true,
                    morpheme_start: 2,
                    morpheme_end: 4,
                    surfaces: vec!["".to_owned(), " ".to_owned()],
                },
            ])
            .unwrap();
        writer
            .append_nway_feature_diffs(&[
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "pos1".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("名詞".to_owned()),
                    analyzer_id: "vibrato".to_owned(),
                },
                NwayFeatureDiffRow {
                    run_id: "run-a".to_owned(),
                    source_id: "source-a".to_owned(),
                    text_id: "work-a".to_owned(),
                    region_index: 0,
                    feature_key: "pos1".to_owned(),
                    scope_type: "whole_region".to_owned(),
                    scope_position: None,
                    scope_surface: None,
                    feature_value: Some("空白".to_owned()),
                    analyzer_id: "sudachi-c".to_owned(),
                },
            ])
            .unwrap();
        writer.finalize().unwrap();

        let rows = summarize_warehouse_regions(
            &paths.final_dir,
            WarehouseRegionOptions {
                kind: WarehouseRegionKind::Segmentation,
                text_filter: WarehouseTextFilter::LexicalOnly,
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].source_id, "source-a");
        assert_eq!(rows[0].region_index, 0);
        assert_eq!(rows[0].analyzers.len(), 2);
        assert_eq!(rows[0].feature_diffs.len(), 2);
        assert!(
            rows[0]
                .analyzers
                .iter()
                .any(|row| row.analyzer_id == "sudachi-c" && row.surfaces == vec!["今", "日"])
        );

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn summarize_warehouse_errors_groups_error_facts() {
        use crate::warehouse::schema::{ErrorRow, WarehousePaths};
        use crate::warehouse::writer::WarehouseWriter;

        let root = temp_dir("warehouse-errors");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_errors(&[
                ErrorRow {
                    run_id: "run-a".to_owned(),
                    source_id: Some("source-a".to_owned()),
                    text_id: Some("work-a".to_owned()),
                    analyzer_id: Some("sudachi-c".to_owned()),
                    stage: "analyze".to_owned(),
                    error_code: "analyze_failed".to_owned(),
                    message: "input too long".to_owned(),
                },
                ErrorRow {
                    run_id: "run-a".to_owned(),
                    source_id: Some("source-b".to_owned()),
                    text_id: Some("work-b".to_owned()),
                    analyzer_id: Some("sudachi-c".to_owned()),
                    stage: "analyze".to_owned(),
                    error_code: "analyze_failed".to_owned(),
                    message: "input too long".to_owned(),
                },
                ErrorRow {
                    run_id: "run-a".to_owned(),
                    source_id: Some("source-c".to_owned()),
                    text_id: Some("work-c".to_owned()),
                    analyzer_id: None,
                    stage: "read_aat".to_owned(),
                    error_code: "read_aat_failed".to_owned(),
                    message: "bad json".to_owned(),
                },
            ])
            .unwrap();
        writer.finalize().unwrap();

        let rows = summarize_warehouse_errors(
            &paths.final_dir,
            WarehouseErrorSummaryOptions {
                group_by: WarehouseErrorGroupBy::ErrorCode,
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].key, "analyze_failed");
        assert_eq!(rows[0].errors, 2);
        assert_eq!(rows[0].source_ids, vec!["source-a", "source-b"]);
        assert_eq!(rows[0].analyzer_ids, vec!["sudachi-c"]);
        assert_eq!(rows[0].sample_messages, vec!["input too long"]);

        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn nway_summaries_can_exclude_outlier_text_ids() {
        let dir = temp_dir("nway-outlier-exclusion");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("nway.jsonl");
        fs::write(
            &path,
            concat!(
                r#"{"source_id":"src-a","text_id":"JISTABLE","source_script_category":"other","analyzers":["vibrato","sudachi-a"],"analyzer_count":2,"regions":10,"agreement_regions":0,"regions_with_feature_disagreement":0,"regions_with_segmentation_disagreement":10,"regions_with_coverage_mismatch":0,"whitespace_regions":0,"lexical_regions":10,"unanimous_boundary_count":0,"variable_boundary_count":10,"examples":[]}"#, "\n",
                r#"{"source_id":"src-b","text_id":"normal","source_script_category":"japanese","analyzers":["vibrato","sudachi-a"],"analyzer_count":2,"regions":3,"agreement_regions":3,"regions_with_feature_disagreement":0,"regions_with_segmentation_disagreement":0,"regions_with_coverage_mismatch":0,"whitespace_regions":0,"lexical_regions":3,"unanimous_boundary_count":2,"variable_boundary_count":0,"examples":[]}"#, "\n",
            ),
        )
        .unwrap();

        let rows = summarize_nway(
            &path,
            NwaySummaryOptions {
                group_by: CompactSummaryGroupBy::TextId,
                sort_by: NwaySummarySort::RegionsWithSegmentationDisagreement,
                script_category: None,
                exclusions: SummaryExclusions {
                    source_ids: BTreeSet::new(),
                    text_ids: BTreeSet::from(["JISTABLE".to_owned()]),
                },
                limit: 10,
            },
        )
        .unwrap();

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].text_ids, vec!["normal"]);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn nway_pattern_display_escapes_control_characters() {
        let key = NwayPatternKey {
            kind: "segmentation".to_owned(),
            segmentation_groups: vec![NwaySegmentationGroupRow {
                surfaces: vec!["\n".to_owned(), "\t".to_owned()],
                analyzers: vec!["a".to_owned()],
            }],
            feature_key: None,
            feature_scope: None,
            feature_values: Vec::new(),
        };

        assert_eq!(nway_pattern_display(&key), r#"a:[\n|\t]"#);
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
