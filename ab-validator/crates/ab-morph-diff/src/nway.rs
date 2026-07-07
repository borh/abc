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
) -> Result<NwayComparison, MorphDiffError> {
    let mut regions = Vec::new();
    let stats =
        visit_nway_regions_with_source_text(analyses, source_text, compare_keys, |region| {
            regions.push(region.clone());
        })?;

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

pub(crate) fn visit_nway_regions_with_source_text(
    analyses: &[Analysis],
    source_text: &str,
    compare_keys: &[FeatureKey],
    mut visit: impl FnMut(&NwayRegion),
) -> Result<NwayStats, MorphDiffError> {
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
    let mut stats = NwayStatsAccumulator::new(&analysis_refs, source_text);
    visit_shared_regions_with_source_len(
        &analysis_refs,
        source_text.chars().count(),
        compare_keys,
        true,
        |mut region| {
            region.region_index = stats.regions;
            stats.record(&region);
            visit(&region);
        },
    )?;

    Ok(stats.finish())
}

pub(crate) fn shared_regions_without_features_with_source_len(
    analyses: &[&Analysis],
    source_len: usize,
) -> Result<Vec<NwayRegion>, MorphDiffError> {
    let mut regions = Vec::new();
    visit_shared_regions_with_source_len(analyses, source_len, &[], false, |region| {
        regions.push(region);
    })?;
    Ok(regions)
}

fn visit_shared_regions_with_source_len(
    analyses: &[&Analysis],
    source_len: usize,
    compare_keys: &[FeatureKey],
    build_feature_groups: bool,
    mut visit: impl FnMut(NwayRegion),
) -> Result<(), MorphDiffError> {
    if analyses.len() < 2 {
        return Err(MorphDiffError::InvalidInput {
            message: "shared region alignment requires at least two analyses".to_owned(),
        });
    }

    let mut cursors = vec![0usize; analyses.len()];
    let mut region_index = 0usize;
    while cursors
        .iter()
        .enumerate()
        .any(|(index, cursor)| *cursor < analyses[index].morphemes.len())
    {
        let region_start = next_start(analyses, &cursors, source_len);
        let mut region_end = next_end(analyses, &cursors, source_len);
        let starts = cursors.clone();

        loop {
            let mut changed = false;
            for (analysis_index, analysis) in analyses.iter().enumerate() {
                while cursors[analysis_index] < analysis.morphemes.len()
                    && analysis.morphemes[cursors[analysis_index]].char_span.start < region_end
                {
                    region_end =
                        region_end.max(analysis.morphemes[cursors[analysis_index]].char_span.end);
                    cursors[analysis_index] += 1;
                    changed = true;
                }
            }
            if !changed {
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
        let feature_groups = if build_feature_groups {
            feature_groups(analyses, &per_analyzer, compare_keys)
        } else {
            Vec::new()
        };
        visit(NwayRegion {
            region_index,
            text_span: region_start..region_end,
            per_analyzer,
            segmentation_groups,
            feature_groups,
        });
        region_index += 1;
    }
    Ok(())
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
    // Group by borrowed surface lists first so the (common) agreement case
    // does not deep-clone every analyzer's surface Vec just to key the map.
    let mut groups = BTreeMap::<&[String], Vec<AnalyzerId>>::new();
    for entry in per_analyzer {
        groups
            .entry(entry.surfaces.as_slice())
            .or_default()
            .push(entry.analyzer.clone());
    }
    let mut groups = groups
        .into_iter()
        .map(|(surfaces, mut analyzers)| {
            analyzers.sort();
            NwaySegmentationGroup {
                surfaces: surfaces.to_vec(),
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
    if keys.is_empty() {
        return Vec::new();
    }

    // The region "shape" (scope eligibility and surface alignment) is the
    // same for every feature key; compute it once instead of per key.
    let whole_region_eligible = per_analyzer
        .iter()
        .all(|entry| entry.covers_exactly && entry.indices.len() == 1);
    let aligned_positions = aligned_token_positions(analyses, per_analyzer);
    let surface_hits = surface_hit_groups(analyses, per_analyzer);

    let mut groups = Vec::new();
    for key in keys {
        if whole_region_eligible {
            groups.push(feature_group_for_whole_region(
                analyses,
                per_analyzer,
                key.clone(),
            ));
        }
        for &position in &aligned_positions {
            let mut values = BTreeMap::<Option<crate::FeatureValue>, Vec<AnalyzerId>>::new();
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
        for (surface, hit_indices) in &surface_hits {
            let mut grouped = BTreeMap::<Option<crate::FeatureValue>, Vec<AnalyzerId>>::new();
            for &(analysis_index, morpheme_index) in hit_indices {
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
            groups.push(value_group(
                key.clone(),
                NwayFeatureScope::Surface {
                    surface: (*surface).to_owned(),
                },
                grouped,
            ));
        }
    }
    groups.sort();
    groups.dedup();
    groups
}

/// Token positions comparable across analyzers: every analyzer covers the
/// region exactly with the same morpheme count (> 1) and the surfaces at the
/// position agree. Key-independent, so computed once per region.
fn aligned_token_positions(
    analyses: &[&Analysis],
    per_analyzer: &[NwayAnalyzerRegion],
) -> Vec<usize> {
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
    (0..count)
        .filter(|&position| {
            let surface = &analyses[0].morphemes[per_analyzer[0].indices.start + position].surface;
            per_analyzer
                .iter()
                .enumerate()
                .all(|(analysis_index, entry)| {
                    analyses[analysis_index].morphemes[entry.indices.start + position].surface
                        == *surface
                })
        })
        .collect()
}

/// Surfaces occurring exactly once per covering analyzer in at least two
/// analyzers, with their (analysis, morpheme) hits. Key-independent, so
/// computed once per region.
fn surface_hit_groups<'a>(
    analyses: &[&'a Analysis],
    per_analyzer: &[NwayAnalyzerRegion],
) -> Vec<(&'a str, Vec<(usize, usize)>)> {
    let mut hits = BTreeMap::<&str, Vec<(usize, usize)>>::new();
    let mut counts = BTreeMap::<(usize, &str), usize>::new();
    for (analysis_index, entry) in per_analyzer.iter().enumerate() {
        if !entry.covers_exactly {
            continue;
        }
        for morpheme_index in entry.indices.clone() {
            let surface: &str = &analyses[analysis_index].morphemes[morpheme_index].surface;
            *counts.entry((analysis_index, surface)).or_default() += 1;
            hits.entry(surface)
                .or_default()
                .push((analysis_index, morpheme_index));
        }
    }
    hits.into_iter()
        .filter(|(surface, hit_indices)| {
            hit_indices.len() >= 2
                && hit_indices.iter().all(|(analysis_index, _)| {
                    counts
                        .get(&(*analysis_index, *surface))
                        .copied()
                        .unwrap_or(0)
                        == 1
                })
        })
        .collect()
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
    let mut values = BTreeMap::<Option<crate::FeatureValue>, Vec<AnalyzerId>>::new();
    for (analysis_index, entry) in per_analyzer.iter().enumerate() {
        let morpheme = &analyses[analysis_index].morphemes[entry.indices.start];
        values
            .entry(morpheme.features.get(&key).cloned().flatten())
            .or_default()
            .push(entry.analyzer.clone());
    }
    value_group(key, NwayFeatureScope::WholeRegion, values)
}

fn value_group(
    key: FeatureKey,
    scope: NwayFeatureScope,
    values: BTreeMap<Option<crate::FeatureValue>, Vec<AnalyzerId>>,
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

struct NwayStatsAccumulator<'a> {
    analyzers: usize,
    regions: usize,
    agreement_regions: usize,
    regions_with_feature_disagreement: usize,
    regions_with_segmentation_disagreement: usize,
    regions_with_coverage_mismatch: usize,
    whitespace_regions: usize,
    lexical_regions: usize,
    unanimous_boundary_count: usize,
    variable_boundary_count: usize,
    whitespace_checker: crate::stats::WhitespaceSpanChecker<'a>,
}

impl<'a> NwayStatsAccumulator<'a> {
    fn new(analyses: &[&Analysis], source_text: &'a str) -> Self {
        let (unanimous_boundary_count, variable_boundary_count) =
            boundary_counts(analyses, source_text.chars().count());
        Self {
            analyzers: analyses.len(),
            regions: 0,
            agreement_regions: 0,
            regions_with_feature_disagreement: 0,
            regions_with_segmentation_disagreement: 0,
            regions_with_coverage_mismatch: 0,
            whitespace_regions: 0,
            lexical_regions: 0,
            unanimous_boundary_count,
            variable_boundary_count,
            whitespace_checker: crate::stats::WhitespaceSpanChecker::new(source_text),
        }
    }

    fn record(&mut self, region: &NwayRegion) {
        self.regions += 1;
        self.agreement_regions += usize::from(region.is_agreement());
        self.regions_with_feature_disagreement += usize::from(region.has_feature_disagreement());
        self.regions_with_segmentation_disagreement +=
            usize::from(region.has_segmentation_disagreement());
        self.regions_with_coverage_mismatch += usize::from(region.has_coverage_mismatch());
        if self
            .whitespace_checker
            .is_whitespace_only(&region.text_span)
        {
            self.whitespace_regions += 1;
        } else {
            self.lexical_regions += 1;
        }
    }

    fn finish(self) -> NwayStats {
        NwayStats {
            analyzers: self.analyzers,
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

fn boundary_counts(analyses: &[&Analysis], source_len: usize) -> (usize, usize) {
    // Sorted deduped Vecs instead of BTreeSets: morpheme boundaries number in
    // the millions on large documents, and flat storage avoids the per-node
    // BTree overhead that showed up in warehouse-run RSS profiles.
    let boundary_sets = analyses
        .iter()
        .map(|analysis| {
            let mut offsets = analysis
                .morphemes
                .iter()
                .flat_map(|morpheme| [morpheme.char_span.start, morpheme.char_span.end])
                .filter(|offset| *offset != 0 && *offset != source_len)
                .collect::<Vec<_>>();
            offsets.sort_unstable();
            offsets.dedup();
            offsets
        })
        .collect::<Vec<_>>();
    let mut all_boundaries = boundary_sets.iter().flatten().copied().collect::<Vec<_>>();
    all_boundaries.sort_unstable();
    all_boundaries.dedup();
    let unanimous_boundary_count = all_boundaries
        .iter()
        .filter(|boundary| {
            boundary_sets
                .iter()
                .all(|set| set.binary_search(boundary).is_ok())
        })
        .count();
    let variable_boundary_count = all_boundaries.len() - unanimous_boundary_count;
    (unanimous_boundary_count, variable_boundary_count)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{Analysis, FeatureMap, Morpheme};

    use super::boundary_counts;

    fn features(values: &[(&str, Option<&str>)]) -> FeatureMap {
        values
            .iter()
            .map(|(key, value)| ((*key).into(), value.map(Into::into)))
            .collect()
    }

    fn m(source: &str, surface: &str, start: usize, end: usize, features: FeatureMap) -> Morpheme {
        let byte_start = source
            .char_indices()
            .nth(start)
            .map(|(idx, _)| idx)
            .unwrap_or(source.len());
        let byte_end = source
            .char_indices()
            .nth(end)
            .map(|(idx, _)| idx)
            .unwrap_or(source.len());
        Morpheme {
            surface: surface.to_owned(),
            byte_span: byte_start..byte_end,
            char_span: start..end,
            features,
        }
    }

    fn analysis(analyzer: &str, text_id: &str, source: &str, morphemes: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: text_id.to_owned(),
            source_text: Arc::from(source),
            morphemes,
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        }
    }

    #[test]
    fn boundary_counts_split_unanimous_and_variable() {
        // "今日は" (3 chars): analyzer A splits 今日|は (boundary {2});
        // analyzer B splits 今|日|は (boundaries {1, 2}).
        // Unanimous: {2}; variable: {1}.
        let source = "今日は";
        let a = analysis(
            "w",
            "a",
            source,
            vec![
                m(source, "今日", 0, 2, features(&[])),
                m(source, "は", 2, 3, features(&[])),
            ],
        );
        let b = analysis(
            "w",
            "b",
            source,
            vec![
                m(source, "今", 0, 1, features(&[])),
                m(source, "日", 1, 2, features(&[])),
                m(source, "は", 2, 3, features(&[])),
            ],
        );
        let (unanimous, variable) = boundary_counts(&[&a, &b], 3);
        assert_eq!((unanimous, variable), (1, 1));
    }
}
