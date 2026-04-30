use std::collections::{BTreeMap, BTreeSet};
use std::ops::Range;

use crate::{
    Analysis, AnalyzerId, FeatureKey, MorphDiffError, NwayAnalyzerRegion, NwayComparison,
    NwayFeatureGroup, NwayFeatureScope, NwayFeatureValueGroup, NwayRegion, NwaySegmentationGroup,
    NwayStats, validate_analysis_against_source,
};

pub(crate) fn compare_nway_with_source_text(
    analyses: &[Analysis],
    source_text: &str,
    compare_keys: &[FeatureKey],
    _context_keys: &[FeatureKey],
) -> Result<NwayComparison, MorphDiffError> {
    if analyses.len() < 2 {
        return Err(MorphDiffError::InvalidInput {
            message: "N-way comparison requires at least two analyses".to_owned(),
        });
    }
    let text_id = analyses[0].text_id.clone();
    for analysis in analyses {
        if analysis.text_id != text_id {
            return Err(MorphDiffError::TextIdMismatch {
                from: text_id,
                to: analysis.text_id.clone(),
            });
        }
        validate_analysis_against_source(analysis, source_text)?;
    }

    let analysis_refs = analyses.iter().collect::<Vec<_>>();
    let mut regions =
        shared_regions_with_source_len(&analysis_refs, source_text.chars().count(), compare_keys)?;
    for (index, region) in regions.iter_mut().enumerate() {
        region.region_index = index;
    }
    let stats = derive_nway_stats(&analysis_refs, source_text, &regions);

    Ok(NwayComparison {
        text_id: analyses[0].text_id.clone(),
        analyzers: analyses
            .iter()
            .map(|analysis| analysis.analyzer.clone())
            .collect(),
        regions,
        stats,
    })
}

pub(crate) fn shared_regions_with_source_len(
    analyses: &[&Analysis],
    source_len: usize,
    compare_keys: &[FeatureKey],
) -> Result<Vec<NwayRegion>, MorphDiffError> {
    if analyses.len() < 2 {
        return Err(MorphDiffError::InvalidInput {
            message: "shared region alignment requires at least two analyses".to_owned(),
        });
    }

    let mut cursors = vec![0usize; analyses.len()];
    let mut regions = Vec::new();
    while cursors
        .iter()
        .enumerate()
        .any(|(index, cursor)| *cursor < analyses[index].morphemes.len())
    {
        let region_start = next_start(analyses, &cursors, source_len);
        let mut region_end = next_end(analyses, &cursors, source_len);
        let starts = cursors.clone();

        loop {
            let old = cursors.clone();
            for (analysis_index, analysis) in analyses.iter().enumerate() {
                while cursors[analysis_index] < analysis.morphemes.len()
                    && analysis.morphemes[cursors[analysis_index]].char_span.start < region_end
                {
                    region_end =
                        region_end.max(analysis.morphemes[cursors[analysis_index]].char_span.end);
                    cursors[analysis_index] += 1;
                }
            }
            if old == cursors {
                break;
            }
        }

        let per_analyzer = analyses
            .iter()
            .enumerate()
            .map(|(analysis_index, analysis)| {
                let indices = starts[analysis_index]..cursors[analysis_index];
                NwayAnalyzerRegion {
                    analyzer: analysis.analyzer.clone(),
                    indices: indices.clone(),
                    surfaces: analysis.morphemes[indices.clone()]
                        .iter()
                        .map(|morpheme| morpheme.surface.clone())
                        .collect(),
                    covers_exactly: covers_exactly(analysis, indices, region_start..region_end),
                }
            })
            .collect::<Vec<_>>();
        let segmentation_groups = segmentation_groups(&per_analyzer);
        let feature_groups = feature_groups(analyses, &per_analyzer, compare_keys);
        regions.push(NwayRegion {
            region_index: regions.len(),
            text_span: region_start..region_end,
            per_analyzer,
            segmentation_groups,
            feature_groups,
        });
    }
    Ok(regions)
}

fn next_start(analyses: &[&Analysis], cursors: &[usize], source_len: usize) -> usize {
    analyses
        .iter()
        .enumerate()
        .filter_map(|(index, analysis)| analysis.morphemes.get(cursors[index]))
        .map(|morpheme| morpheme.char_span.start)
        .min()
        .unwrap_or(source_len)
}

fn next_end(analyses: &[&Analysis], cursors: &[usize], source_len: usize) -> usize {
    let start = next_start(analyses, cursors, source_len);
    analyses
        .iter()
        .enumerate()
        .filter_map(|(index, analysis)| analysis.morphemes.get(cursors[index]))
        .map(|morpheme| morpheme.char_span.end)
        .min()
        .unwrap_or(source_len)
        .max(start + usize::from(start < source_len))
}

pub(crate) fn covers_exactly(
    analysis: &Analysis,
    indices: Range<usize>,
    span: Range<usize>,
) -> bool {
    if indices.is_empty() {
        return false;
    }
    let first = &analysis.morphemes[indices.start];
    let last = &analysis.morphemes[indices.end - 1];
    first.char_span.start == span.start
        && last.char_span.end == span.end
        && analysis.morphemes[indices]
            .windows(2)
            .all(|pair| pair[0].char_span.end == pair[1].char_span.start)
}

fn segmentation_groups(per_analyzer: &[NwayAnalyzerRegion]) -> Vec<NwaySegmentationGroup> {
    let mut groups = BTreeMap::<Vec<String>, Vec<AnalyzerId>>::new();
    for entry in per_analyzer {
        groups
            .entry(entry.surfaces.clone())
            .or_default()
            .push(entry.analyzer.clone());
    }
    let mut groups = groups
        .into_iter()
        .map(|(surfaces, mut analyzers)| {
            analyzers.sort();
            NwaySegmentationGroup {
                surfaces,
                analyzers,
            }
        })
        .collect::<Vec<_>>();
    groups.sort();
    groups
}

fn feature_groups(
    analyses: &[&Analysis],
    per_analyzer: &[NwayAnalyzerRegion],
    compare_keys: &[FeatureKey],
) -> Vec<NwayFeatureGroup> {
    let keys = feature_keys(analyses, per_analyzer, compare_keys);
    let mut groups = Vec::new();
    for key in keys {
        if per_analyzer
            .iter()
            .all(|entry| entry.covers_exactly && entry.indices.len() == 1)
        {
            groups.push(feature_group_for_whole_region(
                analyses,
                per_analyzer,
                key.clone(),
            ));
        }
        groups.extend(feature_groups_by_token_position(
            analyses,
            per_analyzer,
            key.clone(),
        ));
        groups.extend(feature_groups_by_surface(analyses, per_analyzer, key));
    }
    groups.sort();
    groups.dedup();
    groups
}

fn feature_keys(
    analyses: &[&Analysis],
    per_analyzer: &[NwayAnalyzerRegion],
    compare_keys: &[FeatureKey],
) -> BTreeSet<FeatureKey> {
    if !compare_keys.is_empty() {
        return compare_keys.iter().cloned().collect();
    }
    let mut keys = BTreeSet::new();
    for (analysis_index, entry) in per_analyzer.iter().enumerate() {
        for morpheme in &analyses[analysis_index].morphemes[entry.indices.clone()] {
            keys.extend(morpheme.features.keys().cloned());
        }
    }
    keys
}

fn feature_group_for_whole_region(
    analyses: &[&Analysis],
    per_analyzer: &[NwayAnalyzerRegion],
    key: FeatureKey,
) -> NwayFeatureGroup {
    let mut values = BTreeMap::<Option<String>, Vec<AnalyzerId>>::new();
    for (analysis_index, entry) in per_analyzer.iter().enumerate() {
        let morpheme = &analyses[analysis_index].morphemes[entry.indices.start];
        values
            .entry(morpheme.features.get(&key).cloned().flatten())
            .or_default()
            .push(entry.analyzer.clone());
    }
    value_group(key, NwayFeatureScope::WholeRegion, values)
}

fn feature_groups_by_token_position(
    analyses: &[&Analysis],
    per_analyzer: &[NwayAnalyzerRegion],
    key: FeatureKey,
) -> Vec<NwayFeatureGroup> {
    let Some(count) = per_analyzer
        .first()
        .filter(|entry| entry.covers_exactly)
        .map(|entry| entry.indices.len())
    else {
        return Vec::new();
    };
    if count <= 1
        || !per_analyzer
            .iter()
            .all(|entry| entry.covers_exactly && entry.indices.len() == count)
    {
        return Vec::new();
    }

    let mut groups = Vec::new();
    for position in 0..count {
        let surface = &analyses[0].morphemes[per_analyzer[0].indices.start + position].surface;
        if !per_analyzer
            .iter()
            .enumerate()
            .all(|(analysis_index, entry)| {
                analyses[analysis_index].morphemes[entry.indices.start + position].surface
                    == *surface
            })
        {
            continue;
        }
        let mut values = BTreeMap::<Option<String>, Vec<AnalyzerId>>::new();
        for (analysis_index, entry) in per_analyzer.iter().enumerate() {
            let morpheme = &analyses[analysis_index].morphemes[entry.indices.start + position];
            values
                .entry(morpheme.features.get(&key).cloned().flatten())
                .or_default()
                .push(entry.analyzer.clone());
        }
        groups.push(value_group(
            key.clone(),
            NwayFeatureScope::TokenPosition { position },
            values,
        ));
    }
    groups
}

fn feature_groups_by_surface(
    analyses: &[&Analysis],
    per_analyzer: &[NwayAnalyzerRegion],
    key: FeatureKey,
) -> Vec<NwayFeatureGroup> {
    let mut hits = BTreeMap::<String, Vec<(usize, usize)>>::new();
    let mut per_analyzer_counts = BTreeMap::<(usize, String), usize>::new();
    for (analysis_index, entry) in per_analyzer.iter().enumerate() {
        if !entry.covers_exactly {
            continue;
        }
        for morpheme_index in entry.indices.clone() {
            let surface = analyses[analysis_index].morphemes[morpheme_index]
                .surface
                .clone();
            *per_analyzer_counts
                .entry((analysis_index, surface.clone()))
                .or_default() += 1;
            hits.entry(surface)
                .or_default()
                .push((analysis_index, morpheme_index));
        }
    }

    hits.into_iter()
        .filter(|(surface, values)| {
            values.len() >= 2
                && values.iter().all(|(analysis_index, _)| {
                    per_analyzer_counts
                        .get(&(*analysis_index, surface.clone()))
                        .copied()
                        .unwrap_or(0)
                        == 1
                })
        })
        .map(|(surface, values)| {
            let mut grouped = BTreeMap::<Option<String>, Vec<AnalyzerId>>::new();
            for (analysis_index, morpheme_index) in values {
                grouped
                    .entry(
                        analyses[analysis_index].morphemes[morpheme_index]
                            .features
                            .get(&key)
                            .cloned()
                            .flatten(),
                    )
                    .or_default()
                    .push(per_analyzer[analysis_index].analyzer.clone());
            }
            value_group(key.clone(), NwayFeatureScope::Surface { surface }, grouped)
        })
        .collect()
}

fn value_group(
    key: FeatureKey,
    scope: NwayFeatureScope,
    values: BTreeMap<Option<String>, Vec<AnalyzerId>>,
) -> NwayFeatureGroup {
    NwayFeatureGroup {
        key,
        scope,
        values: values
            .into_iter()
            .map(|(value, mut analyzers)| {
                analyzers.sort();
                NwayFeatureValueGroup { value, analyzers }
            })
            .collect(),
    }
}

fn derive_nway_stats(
    analyses: &[&Analysis],
    source_text: &str,
    regions: &[NwayRegion],
) -> NwayStats {
    let source_len = source_text.chars().count();
    let mut all_boundaries = BTreeSet::new();
    let boundary_sets = analyses
        .iter()
        .map(|analysis| {
            analysis
                .morphemes
                .iter()
                .flat_map(|morpheme| [morpheme.char_span.start, morpheme.char_span.end])
                .filter(|offset| *offset != 0 && *offset != source_len)
                .collect::<BTreeSet<_>>()
        })
        .collect::<Vec<_>>();
    for set in &boundary_sets {
        all_boundaries.extend(set.iter().copied());
    }
    let unanimous_boundary_count = all_boundaries
        .iter()
        .filter(|boundary| boundary_sets.iter().all(|set| set.contains(boundary)))
        .count();
    let variable_boundary_count = all_boundaries.len() - unanimous_boundary_count;

    NwayStats {
        analyzers: analyses.len(),
        regions: regions.len(),
        agreement_regions: regions
            .iter()
            .filter(|region| region.is_agreement())
            .count(),
        regions_with_feature_disagreement: regions
            .iter()
            .filter(|region| region.has_feature_disagreement())
            .count(),
        regions_with_segmentation_disagreement: regions
            .iter()
            .filter(|region| region.has_segmentation_disagreement())
            .count(),
        regions_with_coverage_mismatch: regions
            .iter()
            .filter(|region| region.has_coverage_mismatch())
            .count(),
        whitespace_regions: regions
            .iter()
            .filter(|region| {
                crate::stats::char_span_is_whitespace_only(source_text, &region.text_span)
            })
            .count(),
        lexical_regions: regions
            .iter()
            .filter(|region| {
                !crate::stats::char_span_is_whitespace_only(source_text, &region.text_span)
            })
            .count(),
        unanimous_boundary_count,
        variable_boundary_count,
    }
}
