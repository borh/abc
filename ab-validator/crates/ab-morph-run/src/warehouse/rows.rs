use std::ops::Range;

#[cfg(test)]
use ab_morph_diff::MorphDiffError;
use ab_morph_diff::{Analysis, NwayFeatureScope, NwayRegion, visit_nway_regions_with_source_text};
use ab_warehouse::schema::{
    AnalysisRow, MorphemeRow, NwayRegionAnalyzerRow, NwayRegionRow, ProjectionSpanRow, SourceRow,
};
#[cfg(test)]
use ab_warehouse::schema::{MorphemeFeatureRow, NwayFeatureDiffRow};
use ab_warehouse::writer::{MorphemeFeaturesColumns, NwayFeatureDiffsColumns};
use anyhow::Result as AnyhowResult;
#[cfg(test)]
use arrow_array::{Array, ListArray, RecordBatch, StringArray, UInt64Array};

use crate::WarehouseFeaturePatternAccumulator;

/// Reference `Vec<Row>` fact-table bundle, retained for this module's own
/// unit tests and the differential tests in `lib.rs` that characterize the
/// direct-column production path ([`NwayFactBatch`]) against it. Production
/// code (`visit_nway_fact_row_batches`) never constructs this -- see
/// [`push_region_rows_reference`] vs. [`push_region_rows`].
#[cfg(test)]
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct NwayFactRows {
    pub(crate) regions: Vec<NwayRegionRow>,
    pub(crate) region_analyzers: Vec<NwayRegionAnalyzerRow>,
    pub(crate) feature_diffs: Vec<NwayFeatureDiffRow>,
}

/// Production fact-table batch. `regions`/`region_analyzers` stay
/// `Vec<Row>` (untouched this round -- see the task brief); `feature_diffs`
/// is a direct Arrow-column builder instead of `Vec<NwayFeatureDiffRow>`:
/// `nway_feature_diffs` is the highest-row-volume warehouse table (~23.4B
/// rows), so [`push_region_rows`] appends straight into it, skipping the
/// per-row `Arc::clone` bumps a `Vec<NwayFeatureDiffRow>` would require (6
/// `Arc<str>`/`Option<Arc<str>>` fields per row).
#[derive(Default)]
pub(crate) struct NwayFactBatch {
    pub(crate) regions: Vec<NwayRegionRow>,
    pub(crate) region_analyzers: Vec<NwayRegionAnalyzerRow>,
    pub(crate) feature_diffs: NwayFeatureDiffsColumns,
}

impl NwayFactBatch {
    fn is_empty(&self) -> bool {
        self.regions.is_empty() && self.region_analyzers.is_empty() && self.feature_diffs.is_empty()
    }

    fn clear(&mut self) {
        self.regions.clear();
        self.region_analyzers.clear();
        self.feature_diffs = NwayFeatureDiffsColumns::new();
    }
}

/// Per-document identifiers shared by every n-way fact row, hoisted to
/// Arc<str> once so the multi-billion-row builders clone refcounts instead of
/// allocating Strings per row.
struct NwayRowIds {
    run_id: std::sync::Arc<str>,
    source_id: std::sync::Arc<str>,
    text_id: std::sync::Arc<str>,
    analyzers: Vec<std::sync::Arc<str>>,
}

impl NwayRowIds {
    fn new(run_id: &str, source_id: &str, text_id: &str, analyses: &[Analysis]) -> Self {
        Self {
            run_id: run_id.into(),
            source_id: source_id.into(),
            text_id: text_id.into(),
            analyzers: analyses
                .iter()
                .map(|analysis| std::sync::Arc::from(analysis.analyzer.as_str()))
                .collect(),
        }
    }

    fn analyzer(&self, analyzer_id: &str) -> std::sync::Arc<str> {
        self.analyzers
            .iter()
            .find(|candidate| candidate.as_ref() == analyzer_id)
            .cloned()
            .unwrap_or_else(|| analyzer_id.into())
    }
}

pub(crate) fn source_row(
    run_id: &str,
    source_id: &str,
    aat_path: &str,
    analysis: &Analysis,
) -> SourceRow {
    SourceRow {
        run_id: run_id.to_owned(),
        source_id: source_id.to_owned(),
        text_id: analysis.text_id.clone(),
        aat_path: aat_path.to_owned(),
        source_bytes: analysis.source_text.len() as u64,
        source_chars: analysis.source_text.chars().count() as u64,
    }
}

pub(crate) fn analysis_row(run_id: &str, source_id: &str, analysis: &Analysis) -> AnalysisRow {
    AnalysisRow {
        run_id: run_id.to_owned(),
        source_id: source_id.to_owned(),
        text_id: analysis.text_id.clone(),
        analyzer_id: analysis.analyzer.clone(),
        morpheme_count: analysis.morphemes.len() as u64,
    }
}

#[cfg(test)]
pub(crate) fn morpheme_rows(
    run_id: &str,
    source_id: &str,
    analysis: &Analysis,
) -> Vec<MorphemeRow> {
    morpheme_rows_for_range(run_id, source_id, analysis, 0..analysis.morphemes.len())
}

pub(crate) fn morpheme_rows_for_range(
    run_id: &str,
    source_id: &str,
    analysis: &Analysis,
    range: Range<usize>,
) -> Vec<MorphemeRow> {
    // The four id columns are constant across the whole analysis; share one
    // Arc<str> per column and bump the refcount per row instead of cloning a
    // fresh String per morpheme.
    let run_id = std::sync::Arc::<str>::from(run_id);
    let source_id = std::sync::Arc::<str>::from(source_id);
    let text_id = std::sync::Arc::<str>::from(analysis.text_id.as_ref());
    let analyzer_id = std::sync::Arc::<str>::from(analysis.analyzer.as_ref());
    analysis
        .morphemes
        .iter()
        .enumerate()
        .skip(range.start)
        .take(range.end.saturating_sub(range.start))
        .map(|(index, morpheme)| MorphemeRow {
            run_id: std::sync::Arc::clone(&run_id),
            source_id: std::sync::Arc::clone(&source_id),
            text_id: std::sync::Arc::clone(&text_id),
            analyzer_id: std::sync::Arc::clone(&analyzer_id),
            morpheme_index: index as u64,
            byte_start: morpheme.byte_span.start as u64,
            byte_end: morpheme.byte_span.end as u64,
            char_start: morpheme.char_span.start as u64,
            char_end: morpheme.char_span.end as u64,
            surface: morpheme.surface.clone(),
        })
        .collect()
}

#[cfg(test)]
pub(crate) fn morpheme_feature_rows(
    run_id: &str,
    source_id: &str,
    analysis: &Analysis,
) -> Vec<MorphemeFeatureRow> {
    morpheme_feature_rows_for_range(run_id, source_id, analysis, 0..analysis.morphemes.len())
}

/// Reference `Vec<Row>`-collecting implementation, retained for this
/// module's own unit tests only. Production code (`pipeline.rs`) uses
/// [`push_morpheme_features_for_range`] instead, which appends straight into
/// a [`MorphemeFeaturesColumns`] builder and never materializes a
/// `Vec<MorphemeFeatureRow>`.
#[cfg(test)]
pub(crate) fn morpheme_feature_rows_for_range(
    run_id: &str,
    source_id: &str,
    analysis: &Analysis,
    range: Range<usize>,
) -> Vec<MorphemeFeatureRow> {
    let run_id = std::sync::Arc::<str>::from(run_id);
    let source_id = std::sync::Arc::<str>::from(source_id);
    let text_id = std::sync::Arc::<str>::from(analysis.text_id.as_ref());
    let analyzer_id = std::sync::Arc::<str>::from(analysis.analyzer.as_ref());
    analysis
        .morphemes
        .iter()
        .enumerate()
        .skip(range.start)
        .take(range.end.saturating_sub(range.start))
        .flat_map(|(index, morpheme)| {
            let run_id = std::sync::Arc::clone(&run_id);
            let source_id = std::sync::Arc::clone(&source_id);
            let text_id = std::sync::Arc::clone(&text_id);
            let analyzer_id = std::sync::Arc::clone(&analyzer_id);
            morpheme
                .features
                .iter()
                .map(move |(key, value)| MorphemeFeatureRow {
                    run_id: std::sync::Arc::clone(&run_id),
                    source_id: std::sync::Arc::clone(&source_id),
                    text_id: std::sync::Arc::clone(&text_id),
                    analyzer_id: std::sync::Arc::clone(&analyzer_id),
                    morpheme_index: index as u64,
                    feature_key: std::sync::Arc::clone(key),
                    feature_value: value.as_ref().map(std::sync::Arc::clone),
                })
        })
        .collect()
}

/// Append every morpheme-feature in `range` directly into `columns`' Arrow
/// builders. This is the producer side of the direct-column path: `run_id`,
/// `source_id`, `text_id`, and `analyzer_id` are passed by `&str` and copied
/// straight into the builder's buffers, so -- unlike the retained
/// `Vec<MorphemeFeatureRow>` reference path above -- no `Arc::clone` happens
/// per row here at all.
pub(crate) fn push_morpheme_features_for_range(
    run_id: &str,
    source_id: &str,
    analysis: &Analysis,
    range: Range<usize>,
    columns: &mut MorphemeFeaturesColumns,
) {
    let text_id = analysis.text_id.as_str();
    let analyzer_id = analysis.analyzer.as_str();
    for (index, morpheme) in analysis
        .morphemes
        .iter()
        .enumerate()
        .skip(range.start)
        .take(range.end.saturating_sub(range.start))
    {
        for (key, value) in morpheme.features.iter() {
            columns.push_row(
                run_id,
                source_id,
                text_id,
                analyzer_id,
                index as u64,
                key.as_ref(),
                value.as_deref(),
            );
        }
    }
}

pub(crate) fn projection_span_rows(
    run_id: &str,
    source_id: &str,
    text_id: &str,
    spans: &[ab_plaintext::ProjectionSpan],
) -> Vec<ProjectionSpanRow> {
    let run_id = std::sync::Arc::<str>::from(run_id);
    let source_id = std::sync::Arc::<str>::from(source_id);
    let text_id = std::sync::Arc::<str>::from(text_id);
    spans
        .iter()
        .map(|span| ProjectionSpanRow {
            run_id: std::sync::Arc::clone(&run_id),
            source_id: std::sync::Arc::clone(&source_id),
            text_id: std::sync::Arc::clone(&text_id),
            projected_char_start: span.projected_char_start,
            projected_char_end: span.projected_char_end,
            aat_pointer: span.aat_pointer.clone(),
            inline_kind: span.inline_kind.clone(),
            is_ruby_base: span.is_ruby_base,
            is_gaiji: span.is_gaiji,
            is_note: span.is_note,
        })
        .collect()
}

#[cfg(test)]
pub(crate) fn nway_fact_rows(
    run_id: &str,
    source_id: &str,
    source_text: &str,
    analyses: &[Analysis],
) -> Result<NwayFactRows, MorphDiffError> {
    let text_id = analyses
        .first()
        .map(|analysis| analysis.text_id.clone())
        .unwrap_or_default();
    let ids = NwayRowIds::new(run_id, source_id, &text_id, analyses);
    let mut rows = NwayFactRows::default();
    let char_map = ab_morph_diff::CharByteMap::new(source_text);
    visit_nway_regions_with_source_text(analyses, source_text, &[], |region| {
        push_region_rows_reference(&ids, source_text, &char_map, region, &mut rows);
    })?;
    Ok(rows)
}

/// Batches n-way fact rows across `analyses`, flushing to `on_batch` every
/// `batch_region_limit` regions (production: every 10k, see
/// `append_warehouse_nway_fact_rows` in `lib.rs`). `pattern_counts` is fed
/// directly from each region's `feature_groups` as it is visited (see
/// `push_region_rows`), so the caller no longer needs to separately call
/// `WarehouseFeaturePatternAccumulator::record` against a materialized
/// `Vec<NwayFeatureDiffRow>` after the fact.
pub(crate) fn visit_nway_fact_row_batches<F>(
    run_id: &str,
    source_id: &str,
    source_text: &str,
    analyses: &[Analysis],
    batch_region_limit: usize,
    pattern_counts: &mut WarehouseFeaturePatternAccumulator,
    mut on_batch: F,
) -> AnyhowResult<()>
where
    F: FnMut(&mut NwayFactBatch) -> AnyhowResult<()>,
{
    let text_id = analyses
        .first()
        .map(|analysis| analysis.text_id.clone())
        .unwrap_or_default();
    let ids = NwayRowIds::new(run_id, source_id, &text_id, analyses);
    let batch_region_limit = batch_region_limit.max(1);
    let mut batch = NwayFactBatch::default();
    let mut flush_error = None;
    let char_map = ab_morph_diff::CharByteMap::new(source_text);
    visit_nway_regions_with_source_text(analyses, source_text, &[], |region| {
        if flush_error.is_some() {
            return;
        }
        push_region_rows(
            &ids,
            source_text,
            &char_map,
            region,
            &mut batch,
            pattern_counts,
        );
        if batch.regions.len() >= batch_region_limit {
            if let Err(error) = on_batch(&mut batch) {
                flush_error = Some(error);
            }
            batch.clear();
        }
    })?;
    if let Some(error) = flush_error {
        return Err(error);
    }
    if !batch.is_empty() {
        on_batch(&mut batch)?;
    }
    Ok(())
}

/// Pushes `region`'s `NwayRegionRow` and `NwayRegionAnalyzerRow`s (shared by
/// both the production and reference feature-diff paths) and returns whether
/// its excerpt is nonempty whitespace, for reuse by the feature-diff/pattern
/// bookkeeping that follows.
fn push_region_and_analyzer_rows(
    ids: &NwayRowIds,
    source_text: &str,
    char_map: &ab_morph_diff::CharByteMap,
    region: &NwayRegion,
    regions: &mut Vec<NwayRegionRow>,
    region_analyzers: &mut Vec<NwayRegionAnalyzerRow>,
) -> bool {
    let byte_span = byte_span_from_char_span(char_map, &region.text_span);
    let excerpt = &source_text[byte_span.clone()];
    let is_nonempty_whitespace = !excerpt.is_empty() && excerpt.chars().all(char::is_whitespace);
    regions.push(NwayRegionRow {
        run_id: std::sync::Arc::clone(&ids.run_id),
        source_id: std::sync::Arc::clone(&ids.source_id),
        text_id: std::sync::Arc::clone(&ids.text_id),
        region_index: region.region_index as u64,
        byte_start: byte_span.start as u64,
        byte_end: byte_span.end as u64,
        char_start: region.text_span.start as u64,
        char_end: region.text_span.end as u64,
        is_nonempty_whitespace,
        is_agreement: region.is_agreement(),
        has_coverage_mismatch: region.has_coverage_mismatch(),
        has_segmentation_disagreement: region.has_segmentation_disagreement(),
        has_feature_disagreement: region.has_feature_disagreement(),
    });

    region_analyzers.extend(
        region
            .per_analyzer
            .iter()
            .map(|entry| NwayRegionAnalyzerRow {
                run_id: std::sync::Arc::clone(&ids.run_id),
                source_id: std::sync::Arc::clone(&ids.source_id),
                text_id: std::sync::Arc::clone(&ids.text_id),
                region_index: region.region_index as u64,
                analyzer_id: ids.analyzer(&entry.analyzer),
                covers_exactly: entry.covers_exactly,
                morpheme_start: entry.indices.start as u64,
                morpheme_end: entry.indices.end as u64,
                surfaces: entry.surfaces.clone(),
            }),
    );

    is_nonempty_whitespace
}

/// Production feature-diff path: appends each n-way feature-diff directly
/// into `batch.feature_diffs`' Arrow builder (via plain `&str`/`Option<&str>`,
/// so `StringBuilder::append_value` copies bytes -- no `Arc::clone` at all)
/// and feeds `pattern_counts` straight from `region.feature_groups`, instead
/// of materializing a `Vec<NwayFeatureDiffRow>` first. This is
/// content-identical to the retained reference path
/// ([`push_region_rows_reference`]) because `group.values` is already
/// partitioned and ordered exactly like the `BTreeMap<Option<FeatureValue>,
/// Vec<AnalyzerId>>` the old row-based `WarehouseFeaturePatternAccumulator::
/// record` reconstructs (`ab_morph_diff::nway::value_group` builds `values`
/// from such a `BTreeMap` and sorts each value's analyzers) -- see
/// `feature_pattern_accumulator_region_group_path_matches_reference_row_path`
/// in `lib.rs`.
fn push_region_rows(
    ids: &NwayRowIds,
    source_text: &str,
    char_map: &ab_morph_diff::CharByteMap,
    region: &NwayRegion,
    batch: &mut NwayFactBatch,
    pattern_counts: &mut WarehouseFeaturePatternAccumulator,
) {
    let is_nonempty_whitespace = push_region_and_analyzer_rows(
        ids,
        source_text,
        char_map,
        region,
        &mut batch.regions,
        &mut batch.region_analyzers,
    );

    for group in &region.feature_groups {
        if group.values.len() < 2 {
            continue;
        }
        let (scope_type, scope_position, scope_surface) = feature_scope_parts(&group.scope);
        pattern_counts.record_region_feature_group(
            ids.source_id.as_ref(),
            ids.text_id.as_ref(),
            is_nonempty_whitespace,
            group.key.as_ref(),
            scope_type.as_ref(),
            scope_position,
            scope_surface.as_deref(),
            &group.values,
        );
        for value_group in &group.values {
            batch.feature_diffs.push_row(
                ids.run_id.as_ref(),
                ids.source_id.as_ref(),
                ids.text_id.as_ref(),
                region.region_index as u64,
                group.key.as_ref(),
                scope_type.as_ref(),
                scope_position,
                scope_surface.as_deref(),
                value_group.value.as_deref(),
                &value_group.analyzers, // &[AnalyzerId] = &[String], already sorted
            );
        }
    }
}

/// Reference `Vec<Row>`-collecting implementation, retained for this
/// module's own unit tests and the differential tests in `lib.rs` that
/// characterize [`push_region_rows`] against it. Production code
/// (`visit_nway_fact_row_batches`) uses `push_region_rows` instead, which
/// appends straight into a `NwayFeatureDiffsColumns` builder and never
/// materializes a `Vec<NwayFeatureDiffRow>`.
#[cfg(test)]
fn push_region_rows_reference(
    ids: &NwayRowIds,
    source_text: &str,
    char_map: &ab_morph_diff::CharByteMap,
    region: &NwayRegion,
    rows: &mut NwayFactRows,
) {
    push_region_and_analyzer_rows(
        ids,
        source_text,
        char_map,
        region,
        &mut rows.regions,
        &mut rows.region_analyzers,
    );

    for group in &region.feature_groups {
        if group.values.len() < 2 {
            continue;
        }
        let (scope_type, scope_position, scope_surface) = feature_scope_parts(&group.scope);
        for value_group in &group.values {
            rows.feature_diffs.push(NwayFeatureDiffRow {
                run_id: std::sync::Arc::clone(&ids.run_id),
                source_id: std::sync::Arc::clone(&ids.source_id),
                text_id: std::sync::Arc::clone(&ids.text_id),
                region_index: region.region_index as u64,
                feature_key: std::sync::Arc::clone(&group.key),
                scope_type: std::sync::Arc::clone(&scope_type),
                scope_position,
                scope_surface: scope_surface.clone(),
                feature_value: value_group.value.clone(),
                analyzers: value_group
                    .analyzers
                    .iter()
                    .map(|a| ids.analyzer(a))
                    .collect(),
            });
        }
    }
}

/// Test-only: decodes a finished [`NwayFeatureDiffsColumns`] `RecordBatch`
/// (`nway_feature_diffs_schema()`'s 10-column order) back into
/// `Vec<NwayFeatureDiffRow>`, honoring nulls in `scope_position`/
/// `scope_surface`/`feature_value`. Exists solely so the production
/// direct-to-Arrow path ([`push_region_rows`]) can be asserted equal, row by
/// row, to the retained `Vec<Row>` reference path
/// ([`push_region_rows_reference`]) even though `NwayFeatureDiffsColumns`
/// itself is not `Clone`/`PartialEq` -- see
/// `batched_nway_fact_rows_match_collected_rows` (this module) and
/// `push_region_rows_emits_maximal_contiguous_feature_diff_runs` (`lib.rs`).
#[cfg(test)]
pub(crate) fn decode_feature_diff_rows(batch: &RecordBatch) -> Vec<NwayFeatureDiffRow> {
    fn strings(batch: &RecordBatch, index: usize) -> &StringArray {
        batch
            .column(index)
            .as_any()
            .downcast_ref::<StringArray>()
            .expect("nway_feature_diffs column is a StringArray")
    }
    fn u64s(batch: &RecordBatch, index: usize) -> &UInt64Array {
        batch
            .column(index)
            .as_any()
            .downcast_ref::<UInt64Array>()
            .expect("nway_feature_diffs column is a UInt64Array")
    }

    let run_id = strings(batch, 0);
    let source_id = strings(batch, 1);
    let text_id = strings(batch, 2);
    let region_index = u64s(batch, 3);
    let feature_key = strings(batch, 4);
    let scope_type = strings(batch, 5);
    let scope_position = u64s(batch, 6);
    let scope_surface = strings(batch, 7);
    let feature_value = strings(batch, 8);
    let analyzers = batch
        .column(9)
        .as_any()
        .downcast_ref::<ListArray>()
        .expect("nway_feature_diffs analyzers column is a ListArray");

    (0..batch.num_rows())
        .map(|row| {
            let list = analyzers.value(row);
            let strs = list
                .as_any()
                .downcast_ref::<StringArray>()
                .expect("analyzers list items are Utf8");
            NwayFeatureDiffRow {
                run_id: run_id.value(row).into(),
                source_id: source_id.value(row).into(),
                text_id: text_id.value(row).into(),
                region_index: region_index.value(row),
                feature_key: feature_key.value(row).into(),
                scope_type: scope_type.value(row).into(),
                scope_position: (!scope_position.is_null(row)).then(|| scope_position.value(row)),
                scope_surface: (!scope_surface.is_null(row))
                    .then(|| scope_surface.value(row).into()),
                feature_value: (!feature_value.is_null(row))
                    .then(|| feature_value.value(row).into()),
                analyzers: (0..strs.len()).map(|i| strs.value(i).into()).collect(),
            }
        })
        .collect()
}

fn feature_scope_parts(
    scope: &NwayFeatureScope,
) -> (
    std::sync::Arc<str>,
    Option<u64>,
    Option<std::sync::Arc<str>>,
) {
    use std::sync::{Arc, LazyLock};
    static WHOLE_REGION: LazyLock<Arc<str>> = LazyLock::new(|| "whole_region".into());
    static TOKEN_POSITION: LazyLock<Arc<str>> = LazyLock::new(|| "token_position".into());
    static SURFACE: LazyLock<Arc<str>> = LazyLock::new(|| "surface".into());
    match scope {
        NwayFeatureScope::WholeRegion => (Arc::clone(&WHOLE_REGION), None, None),
        NwayFeatureScope::TokenPosition { position } => {
            (Arc::clone(&TOKEN_POSITION), Some(*position as u64), None)
        }
        NwayFeatureScope::Surface { surface } => {
            (Arc::clone(&SURFACE), None, Some(surface.as_str().into()))
        }
    }
}

fn byte_span_from_char_span(
    char_map: &ab_morph_diff::CharByteMap,
    char_span: &Range<usize>,
) -> Range<usize> {
    // Mirrors the legacy `char_indices().nth(i).unwrap_or(source_text.len())`:
    // a char index past the end clamps to source.len().
    let source_byte_len = char_map.source_byte_len();
    let start = char_map
        .byte_offset_at_char(char_span.start)
        .unwrap_or(source_byte_len);
    let end = char_map
        .byte_offset_at_char(char_span.end)
        .unwrap_or(source_byte_len);
    start..end
}

#[cfg(test)]
mod tests {
    use std::ops::Range;
    use std::sync::Arc;

    use ab_morph_diff::{Analysis, FeatureMap, Morpheme};

    use super::*;

    #[test]
    fn maps_source_analysis_morphemes_and_features() {
        let analysis = Analysis {
            text_id: "work-a".to_owned(),
            analyzer: "vibrato:unidic".to_owned(),
            source_text: Arc::from("今日"),
            morphemes: vec![m(
                "今日",
                0..6,
                0..2,
                [("pos1", Some("名詞")), ("lemma", Some("今日"))],
            )],
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        };

        assert_eq!(
            source_row("run-a", "source-a", "scratch/a.json", &analysis).text_id,
            "work-a"
        );
        assert_eq!(
            analysis_row("run-a", "source-a", &analysis).morpheme_count,
            1
        );

        let morphemes = morpheme_rows("run-a", "source-a", &analysis);
        assert_eq!(morphemes[0].surface, "今日");
        assert_eq!(morphemes[0].byte_start, 0);
        assert_eq!(morphemes[0].byte_end, 6);

        let features = morpheme_feature_rows("run-a", "source-a", &analysis);
        assert_eq!(features.len(), 2);
        assert!(features.iter().any(|row| row.feature_key.as_ref() == "pos1"
            && row.feature_value.as_deref() == Some("名詞")));
    }

    #[test]
    fn morpheme_row_ranges_keep_original_indices() {
        let analysis = analysis(
            "work-a",
            "vibrato",
            "今日は晴れ",
            vec![
                m("今日", 0..6, 0..2, [("pos1", Some("名詞"))]),
                m("は", 6..9, 2..3, [("pos1", Some("助詞"))]),
                m("晴れ", 9..15, 3..5, [("pos1", Some("名詞"))]),
            ],
        );

        let morphemes = morpheme_rows_for_range("run-a", "source-a", &analysis, 1..3);
        let features = morpheme_feature_rows_for_range("run-a", "source-a", &analysis, 1..3);

        assert_eq!(
            morphemes
                .iter()
                .map(|row| (row.morpheme_index, row.surface.as_str()))
                .collect::<Vec<_>>(),
            vec![(1, "は"), (2, "晴れ")]
        );
        assert_eq!(
            features
                .iter()
                .map(|row| (row.morpheme_index, row.feature_key.as_ref()))
                .collect::<Vec<_>>(),
            vec![(1, "pos1"), (2, "pos1")]
        );
    }

    #[test]
    fn maps_nway_regions_to_three_fact_tables() {
        let analyses = vec![
            analysis(
                "work-a",
                "vibrato",
                "今日",
                vec![m("今日", 0..6, 0..2, [("pos1", Some("名詞"))])],
            ),
            analysis(
                "work-a",
                "sudachi-a",
                "今日",
                vec![m("今日", 0..6, 0..2, [("pos1", Some("名詞"))])],
            ),
            analysis(
                "work-a",
                "sudachi-c",
                "今日",
                vec![
                    m("今", 0..3, 0..1, [("pos1", Some("名詞"))]),
                    m("日", 3..6, 1..2, [("pos1", Some("名詞"))]),
                ],
            ),
        ];

        let facts = nway_fact_rows("run-a", "source-a", "今日", &analyses).unwrap();

        assert_eq!(facts.regions.len(), 1);
        assert!(facts.regions[0].has_segmentation_disagreement);
        assert_eq!(facts.region_analyzers.len(), 3);
        assert!(
            facts
                .region_analyzers
                .iter()
                .any(|row| row.analyzer_id.as_ref() == "vibrato" && row.surfaces == vec!["今日"])
        );
        assert!(
            facts
                .region_analyzers
                .iter()
                .any(|row| row.analyzer_id.as_ref() == "sudachi-c"
                    && row.surfaces == vec!["今", "日"])
        );
        assert!(facts.feature_diffs.is_empty());
    }

    #[test]
    fn batched_nway_fact_rows_match_collected_rows() {
        let analyses = vec![
            analysis(
                "work-a",
                "vibrato",
                "今日は晴れ",
                vec![
                    m("今日", 0..6, 0..2, [("pos1", Some("名詞"))]),
                    m("は", 6..9, 2..3, [("pos1", Some("助詞"))]),
                    m("晴れ", 9..15, 3..5, [("pos1", Some("動詞"))]),
                ],
            ),
            analysis(
                "work-a",
                "sudachi-a",
                "今日は晴れ",
                vec![
                    m("今日", 0..6, 0..2, [("pos1", Some("名詞"))]),
                    m("は", 6..9, 2..3, [("pos1", Some("助詞"))]),
                    m("晴れ", 9..15, 3..5, [("pos1", Some("名詞"))]),
                ],
            ),
        ];
        let collected = nway_fact_rows("run-a", "source-a", "今日は晴れ", &analyses).unwrap();
        let mut batched_regions = Vec::new();
        let mut batched_region_analyzers = Vec::new();
        let mut batched_feature_diffs = Vec::new();
        let mut pattern_counts = crate::WarehouseFeaturePatternAccumulator::default();

        // `feature_diffs` is a `NwayFeatureDiffsColumns` arrow builder (not
        // `Clone`/`PartialEq`), so decode each flushed batch's builder back
        // into `Vec<NwayFeatureDiffRow>` via `decode_feature_diff_rows` and
        // compare full row VALUES (not just counts) against the row-based
        // reference collector -- proving the direct-to-Arrow production
        // producer is content-identical, not merely count-identical.
        visit_nway_fact_row_batches(
            "run-a",
            "source-a",
            "今日は晴れ",
            &analyses,
            1,
            &mut pattern_counts,
            |batch| {
                batched_regions.extend(batch.regions.clone());
                batched_region_analyzers.extend(batch.region_analyzers.clone());
                batched_feature_diffs
                    .extend(decode_feature_diff_rows(&batch.feature_diffs.finish()));
                Ok(())
            },
        )
        .unwrap();

        assert_eq!(batched_regions, collected.regions);
        assert_eq!(batched_region_analyzers, collected.region_analyzers);
        assert_eq!(batched_feature_diffs, collected.feature_diffs);
    }

    #[test]
    fn collapsed_rows_unnest_to_per_analyzer_expansion() {
        // Four analyzers agree on segmentation (one morpheme spanning the
        // whole region) but disagree on the `pos1` feature: vibrato and
        // sudachi-c both say 名詞, sudachi-a says 動詞, and mecab has no
        // `pos1` feature at all (the `None` value-group).
        let analyses = vec![
            analysis(
                "work-a",
                "vibrato",
                "今日",
                vec![m("今日", 0..6, 0..2, [("pos1", Some("名詞"))])],
            ),
            analysis(
                "work-a",
                "sudachi-c",
                "今日",
                vec![m("今日", 0..6, 0..2, [("pos1", Some("名詞"))])],
            ),
            analysis(
                "work-a",
                "sudachi-a",
                "今日",
                vec![m("今日", 0..6, 0..2, [("pos1", Some("動詞"))])],
            ),
            analysis("work-a", "mecab", "今日", vec![m("今日", 0..6, 0..2, [])]),
        ];

        let mut batched_feature_diffs = Vec::new();
        let mut pattern_counts = crate::WarehouseFeaturePatternAccumulator::default();
        visit_nway_fact_row_batches(
            "run-a",
            "source-a",
            "今日",
            &analyses,
            1,
            &mut pattern_counts,
            |batch| {
                batched_feature_diffs
                    .extend(decode_feature_diff_rows(&batch.feature_diffs.finish()));
                Ok(())
            },
        )
        .unwrap();
        let collapsed = batched_feature_diffs;

        // Expected per-analyzer expansion (UNNEST of collapsed), restricted to
        // the `whole_region` scope group: a single-token region also emits an
        // equivalent `surface` scope group for the same feature (the n-way
        // diff engine's normal behavior, unrelated to the collapse), so
        // filter to one scope to keep the expected set unambiguous.
        let mut expanded: Vec<(String, String)> = Vec::new(); // (feature_value_or_∅, analyzer)
        for row in collapsed
            .iter()
            .filter(|row| row.scope_type.as_ref() == "whole_region")
        {
            for a in &row.analyzers {
                expanded.push((
                    row.feature_value.as_deref().unwrap_or("∅").to_owned(),
                    a.to_string(),
                ));
            }
        }
        expanded.sort();
        let mut want = vec![
            ("名詞".to_owned(), "sudachi-c".to_owned()),
            ("名詞".to_owned(), "vibrato".to_owned()),
            ("動詞".to_owned(), "sudachi-a".to_owned()),
            ("∅".to_owned(), "mecab".to_owned()),
        ];
        want.sort();
        assert_eq!(expanded, want);
        // And each collapsed row's analyzers are ascending & non-empty.
        for row in &collapsed {
            assert!(!row.analyzers.is_empty());
            let mut sorted = row.analyzers.clone();
            sorted.sort();
            assert_eq!(row.analyzers, sorted, "analyzers must be ascending");
        }
    }

    #[test]
    fn maps_projection_spans_to_rows_with_shared_ids() {
        let spans = vec![ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 2,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "ruby".to_owned(),
            is_ruby_base: true,
            is_gaiji: false,
            is_note: false,
        }];
        let rows = projection_span_rows("run-a", "source-a", "work-a", &spans);
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].run_id.as_ref(), "run-a");
        assert_eq!(rows[0].text_id.as_ref(), "work-a");
        assert_eq!(rows[0].projected_char_end, 2);
        assert!(rows[0].is_ruby_base);
    }

    fn m(
        surface: &str,
        byte_span: Range<usize>,
        char_span: Range<usize>,
        features: impl IntoIterator<Item = (&'static str, Option<&'static str>)>,
    ) -> Morpheme {
        let mut map = FeatureMap::new();
        for (key, value) in features {
            let _ = map.insert(key.into(), value.map(Into::into));
        }
        Morpheme {
            surface: surface.to_owned(),
            byte_span,
            char_span,
            features: map,
        }
    }

    fn analysis(
        text_id: &str,
        analyzer: &str,
        source_text: &str,
        morphemes: Vec<Morpheme>,
    ) -> Analysis {
        Analysis {
            text_id: text_id.to_owned(),
            analyzer: analyzer.to_owned(),
            source_text: Arc::from(source_text),
            morphemes,
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        }
    }
}
