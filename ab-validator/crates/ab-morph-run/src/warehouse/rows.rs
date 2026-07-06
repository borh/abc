use std::ops::Range;

#[cfg(test)]
use ab_morph_diff::MorphDiffError;
use ab_morph_diff::{Analysis, NwayFeatureScope, NwayRegion, visit_nway_regions_with_source_text};
use ab_warehouse::schema::{
    AnalysisRow, MorphemeFeatureRow, MorphemeRow, NwayFeatureDiffRow, NwayRegionAnalyzerRow,
    NwayRegionRow, ProjectionSpanRow, SourceRow,
};
use anyhow::Result as AnyhowResult;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct NwayFactRows {
    pub(crate) regions: Vec<NwayRegionRow>,
    pub(crate) region_analyzers: Vec<NwayRegionAnalyzerRow>,
    pub(crate) feature_diffs: Vec<NwayFeatureDiffRow>,
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
                    feature_key: key.to_string(),
                    feature_value: value.as_ref().map(ToString::to_string),
                })
        })
        .collect()
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
    let mut rows = NwayFactRows::default();
    let char_map = ab_morph_diff::CharByteMap::new(source_text);
    visit_nway_regions_with_source_text(analyses, source_text, &[], |region| {
        push_region_rows(
            run_id,
            source_id,
            &text_id,
            source_text,
            &char_map,
            region,
            &mut rows,
        );
    })?;
    Ok(rows)
}

pub(crate) fn visit_nway_fact_row_batches<F>(
    run_id: &str,
    source_id: &str,
    source_text: &str,
    analyses: &[Analysis],
    batch_region_limit: usize,
    mut on_batch: F,
) -> AnyhowResult<()>
where
    F: FnMut(&NwayFactRows) -> AnyhowResult<()>,
{
    let text_id = analyses
        .first()
        .map(|analysis| analysis.text_id.clone())
        .unwrap_or_default();
    let batch_region_limit = batch_region_limit.max(1);
    let mut rows = NwayFactRows::default();
    let mut flush_error = None;
    let char_map = ab_morph_diff::CharByteMap::new(source_text);
    visit_nway_regions_with_source_text(analyses, source_text, &[], |region| {
        if flush_error.is_some() {
            return;
        }
        push_region_rows(
            run_id,
            source_id,
            &text_id,
            source_text,
            &char_map,
            region,
            &mut rows,
        );
        if rows.regions.len() >= batch_region_limit {
            if let Err(error) = on_batch(&rows) {
                flush_error = Some(error);
            }
            rows.clear();
        }
    })?;
    if let Some(error) = flush_error {
        return Err(error);
    }
    if !rows.is_empty() {
        on_batch(&rows)?;
    }
    Ok(())
}

impl NwayFactRows {
    fn is_empty(&self) -> bool {
        self.regions.is_empty() && self.region_analyzers.is_empty() && self.feature_diffs.is_empty()
    }

    fn clear(&mut self) {
        self.regions.clear();
        self.region_analyzers.clear();
        self.feature_diffs.clear();
    }
}

fn push_region_rows(
    run_id: &str,
    source_id: &str,
    text_id: &str,
    source_text: &str,
    char_map: &ab_morph_diff::CharByteMap,
    region: &NwayRegion,
    rows: &mut NwayFactRows,
) {
    let byte_span = byte_span_from_char_span(char_map, &region.text_span);
    let excerpt = &source_text[byte_span.clone()];
    rows.regions.push(NwayRegionRow {
        run_id: run_id.to_owned(),
        source_id: source_id.to_owned(),
        text_id: text_id.to_owned(),
        region_index: region.region_index as u64,
        byte_start: byte_span.start as u64,
        byte_end: byte_span.end as u64,
        char_start: region.text_span.start as u64,
        char_end: region.text_span.end as u64,
        is_nonempty_whitespace: !excerpt.is_empty() && excerpt.chars().all(char::is_whitespace),
        is_agreement: region.is_agreement(),
        has_coverage_mismatch: region.has_coverage_mismatch(),
        has_segmentation_disagreement: region.has_segmentation_disagreement(),
        has_feature_disagreement: region.has_feature_disagreement(),
    });

    rows.region_analyzers.extend(
        region
            .per_analyzer
            .iter()
            .map(|entry| NwayRegionAnalyzerRow {
                run_id: run_id.to_owned(),
                source_id: source_id.to_owned(),
                text_id: text_id.to_owned(),
                region_index: region.region_index as u64,
                analyzer_id: entry.analyzer.clone(),
                covers_exactly: entry.covers_exactly,
                morpheme_start: entry.indices.start as u64,
                morpheme_end: entry.indices.end as u64,
                surfaces: entry.surfaces.clone(),
            }),
    );

    for group in &region.feature_groups {
        if group.values.len() < 2 {
            continue;
        }
        let (scope_type, scope_position, scope_surface) = feature_scope_parts(&group.scope);
        for value_group in &group.values {
            for analyzer_id in &value_group.analyzers {
                rows.feature_diffs.push(NwayFeatureDiffRow {
                    run_id: run_id.to_owned(),
                    source_id: source_id.to_owned(),
                    text_id: text_id.to_owned(),
                    region_index: region.region_index as u64,
                    feature_key: group.key.to_string(),
                    scope_type: scope_type.clone(),
                    scope_position,
                    scope_surface: scope_surface.clone(),
                    feature_value: value_group.value.as_ref().map(ToString::to_string),
                    analyzer_id: analyzer_id.clone(),
                });
            }
        }
    }
}

fn feature_scope_parts(scope: &NwayFeatureScope) -> (String, Option<u64>, Option<String>) {
    match scope {
        NwayFeatureScope::WholeRegion => ("whole_region".to_owned(), None, None),
        NwayFeatureScope::TokenPosition { position } => {
            ("token_position".to_owned(), Some(*position as u64), None)
        }
        NwayFeatureScope::Surface { surface } => {
            ("surface".to_owned(), None, Some(surface.clone()))
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
        assert!(
            features.iter().any(
                |row| row.feature_key == "pos1" && row.feature_value.as_deref() == Some("名詞")
            )
        );
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
                .map(|row| (row.morpheme_index, row.feature_key.as_str()))
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
                .any(|row| row.analyzer_id == "vibrato" && row.surfaces == vec!["今日"])
        );
        assert!(
            facts
                .region_analyzers
                .iter()
                .any(|row| row.analyzer_id == "sudachi-c" && row.surfaces == vec!["今", "日"])
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
        let mut batched = NwayFactRows::default();

        visit_nway_fact_row_batches(
            "run-a",
            "source-a",
            "今日は晴れ",
            &analyses,
            1,
            |batch| {
                batched.regions.extend(batch.regions.clone());
                batched
                    .region_analyzers
                    .extend(batch.region_analyzers.clone());
                batched.feature_diffs.extend(batch.feature_diffs.clone());
                Ok(())
            },
        )
        .unwrap();

        assert_eq!(batched, collected);
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
