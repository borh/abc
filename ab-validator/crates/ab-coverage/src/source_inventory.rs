use std::collections::BTreeMap;

use ab_source_syntax::{SourceMarker, SourceMarkerKind, aozora_body_range, source_markers};
use regex::Regex;
use serde::Serialize;

use crate::matrix::Row;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceInventoryPattern {
    pub row_id: String,
    pub source_patterns: Vec<String>,
}

#[derive(Debug, Clone, Default, Serialize, PartialEq, Eq)]
pub struct SourceInventorySummary {
    pub work_id: String,
    /// All lexical source occurrences, including editorial examples.
    pub markers_total: u64,
    pub row_counts: BTreeMap<String, MarkerClassSummary>,
    pub unknown_examples: Vec<SourceMarkerOccurrence>,
    pub source_region_events: SourceRegionEventSummary,
    /// Full source occurrences assigned to packaging or explicit boundaries,
    /// excluded from body-family and unreviewed-marker counts.
    pub classified_region_markers: Vec<ClassifiedRegionMarker>,
}

#[derive(Debug, Clone, Default, Serialize, PartialEq, Eq)]
pub struct MarkerClassSummary {
    pub occurrences: u64,
}

#[derive(Debug, Clone, Default, Serialize, PartialEq, Eq)]
pub struct SourceRegionEventSummary {
    pub terminal_provenance_occurrences: u64,
    pub front_matter_occurrences: u64,
    pub body_end_boundary_occurrences: u64,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum SourceMarkerRegion {
    FrontMatter,
    BodyEndBoundary,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct ClassifiedRegionMarker {
    pub region: SourceMarkerRegion,
    pub occurrence: SourceMarkerOccurrence,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct SourceMarkerOccurrence {
    pub work_id: String,
    pub line: usize,
    pub kind: String,
    pub raw: String,
    pub body: String,
}

#[must_use]
pub fn patterns_from_rows(rows: &[Row]) -> Vec<SourceInventoryPattern> {
    rows.iter()
        .map(|row| SourceInventoryPattern {
            row_id: row.id.clone(),
            source_patterns: row.source_patterns.clone(),
        })
        .collect()
}

#[must_use]
pub fn inventory_document(
    work_id: &str,
    text: &str,
    patterns: &[SourceInventoryPattern],
) -> SourceInventorySummary {
    inventory_document_observed(work_id, text, patterns, |_, _, _| {})
}

pub(crate) fn inventory_document_observed(
    work_id: &str,
    text: &str,
    patterns: &[SourceInventoryPattern],
    mut observe: impl FnMut(&SourceMarker<'_>, Option<&SourceMarkerRegion>, &[String]),
) -> SourceInventorySummary {
    let compiled_patterns = compile_patterns(patterns);
    let markers = source_markers(text);
    let (body, _) = aozora_body_range(text);
    let mut summary = SourceInventorySummary {
        work_id: work_id.to_owned(),
        ..SourceInventorySummary::default()
    };

    for (index, marker) in markers.iter().enumerate() {
        summary.markers_total += 1;
        if marker.kind == SourceMarkerKind::SegmentBoundaryTerminalProvenance {
            summary.source_region_events.terminal_provenance_occurrences += 1;
        }
        // Source packaging examples remain occurrences but do not measure body syntax.
        let region = if marker.span.end <= body.start {
            summary.source_region_events.front_matter_occurrences += 1;
            Some(SourceMarkerRegion::FrontMatter)
        } else if marker.raw == "［＃本文終わり］"
            && text[..marker.span.start]
                .rsplit('\n')
                .next()
                .unwrap_or_default()
                .trim()
                .is_empty()
            && text[marker.span.end..]
                .split('\n')
                .next()
                .unwrap_or_default()
                .trim()
                .is_empty()
        {
            summary.source_region_events.body_end_boundary_occurrences += 1;
            Some(SourceMarkerRegion::BodyEndBoundary)
        } else {
            None
        };
        if let Some(region) = region {
            observe(marker, Some(&region), &[]);
            summary
                .classified_region_markers
                .push(ClassifiedRegionMarker {
                    region,
                    occurrence: SourceMarkerOccurrence {
                        work_id: work_id.to_owned(),
                        line: marker.span.line,
                        kind: format!("{:?}", marker.kind),
                        raw: marker.raw.to_owned(),
                        body: marker.body.to_owned(),
                    },
                });
            continue;
        }
        let mut matched = matching_rows(marker.raw, &compiled_patterns);
        if let Some(next_marker) = markers.get(index + 1)
            && marker.span.end == next_marker.span.start
        {
            append_composite_matching_rows(
                &mut matched,
                &text[marker.span.start..next_marker.span.end],
                marker.raw,
                next_marker.raw,
                &compiled_patterns,
            );
        }
        if matched.is_empty() {
            match marker.kind {
                SourceMarkerKind::RubyExplicit | SourceMarkerKind::RubyImplicit => {
                    add_default_match(&mut matched, patterns, "ruby.basic");
                }
                SourceMarkerKind::GaijiFullwidth | SourceMarkerKind::GaijiAscii => {
                    add_default_match(&mut matched, patterns, "gaiji.marker");
                }
                SourceMarkerKind::EditorialNoteBottomTextCorrection => {
                    add_default_match(&mut matched, patterns, "annotation.chuuki");
                }
                _ => {}
            }
        }

        observe(marker, None, &matched);
        if matched.is_empty() {
            summary.unknown_examples.push(SourceMarkerOccurrence {
                work_id: work_id.to_owned(),
                line: marker.span.line,
                kind: format!("{:?}", marker.kind),
                raw: marker.raw.to_owned(),
                body: marker.body.to_owned(),
            });
        } else {
            for row_id in matched {
                summary.row_counts.entry(row_id).or_default().occurrences += 1;
            }
        }
    }

    summary
}

pub(crate) struct CompiledSourceInventoryPattern {
    row_id: String,
    source_patterns: Vec<Regex>,
}

pub(crate) fn compile_patterns(
    patterns: &[SourceInventoryPattern],
) -> Vec<CompiledSourceInventoryPattern> {
    patterns
        .iter()
        // This row documents a research bucket; using it as a classifier hides
        // otherwise-unreviewed Aozora commands from the source-authority gate.
        .filter(|pattern| pattern.row_id != "editor_note.unmapped")
        .map(|pattern| CompiledSourceInventoryPattern {
            row_id: pattern.row_id.clone(),
            source_patterns: pattern
                .source_patterns
                .iter()
                .map(|source_pattern| {
                    Regex::new(source_pattern).unwrap_or_else(|err| {
                        panic!("invalid source inventory regex {source_pattern:?}: {err}")
                    })
                })
                .collect(),
        })
        .collect()
}

pub(crate) fn matching_rows(raw: &str, patterns: &[CompiledSourceInventoryPattern]) -> Vec<String> {
    let mut rows = Vec::new();
    append_matching_rows(&mut rows, raw, patterns);
    rows
}

fn append_matching_rows(
    rows: &mut Vec<String>,
    raw: &str,
    patterns: &[CompiledSourceInventoryPattern],
) {
    for pattern in patterns {
        if pattern
            .source_patterns
            .iter()
            .any(|source_pattern| source_pattern.is_match(raw))
            && !rows.contains(&pattern.row_id)
        {
            rows.push(pattern.row_id.clone());
        }
    }
}

fn append_composite_matching_rows(
    rows: &mut Vec<String>,
    raw: &str,
    left_raw: &str,
    right_raw: &str,
    patterns: &[CompiledSourceInventoryPattern],
) {
    for pattern in patterns {
        let matches_composite = pattern
            .source_patterns
            .iter()
            .any(|source_pattern| source_pattern.is_match(raw));
        let matches_part = pattern.source_patterns.iter().any(|source_pattern| {
            source_pattern.is_match(left_raw) || source_pattern.is_match(right_raw)
        });
        if matches_composite && !matches_part && !rows.contains(&pattern.row_id) {
            rows.push(pattern.row_id.clone());
        }
    }
}

fn add_default_match(rows: &mut Vec<String>, patterns: &[SourceInventoryPattern], row_id: &str) {
    if patterns.iter().any(|pattern| pattern.row_id == row_id)
        && !rows.iter().any(|row| row == row_id)
    {
        rows.push(row_id.to_owned());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_known_rows_from_source_patterns() {
        let patterns = vec![
            pattern("layout.yokogumi", vec![r"［＃ここから横組み］"]),
            pattern("gaiji.marker", vec![r"※［＃[^］]+］"]),
        ];
        let summary = inventory_document(
            "w1",
            "※［＃「口＋世」、U+546D］\n［＃ここから横組み］",
            &patterns,
        );

        assert_eq!(summary.markers_total, 2);
        assert_eq!(summary.row_counts["gaiji.marker"].occurrences, 1);
        assert_eq!(summary.row_counts["layout.yokogumi"].occurrences, 1);
        assert!(summary.unknown_examples.is_empty());
    }

    #[test]
    fn unknown_command_is_reported_with_raw_body_and_span() {
        let patterns = vec![pattern("layout.yokogumi", vec![r"［＃ここから横組み］"])];
        let summary = inventory_document("w1", "［＃謎の注記］", &patterns);

        assert_eq!(summary.markers_total, 1);
        assert_eq!(summary.unknown_examples.len(), 1);
        assert_eq!(summary.unknown_examples[0].work_id, "w1");
        assert_eq!(summary.unknown_examples[0].raw, "［＃謎の注記］");
        assert_eq!(summary.unknown_examples[0].body, "謎の注記");
    }

    #[test]
    fn catch_all_editor_note_row_does_not_hide_unmapped_commands() {
        let patterns = vec![
            pattern("editor_note.unmapped", vec![r"［＃[^］]+］"]),
            pattern("layout.yokogumi", vec![r"［＃ここから横組み］"]),
        ];
        let summary = inventory_document("w1", "［＃謎の注記］", &patterns);

        assert_eq!(summary.markers_total, 1);
        assert_eq!(summary.unknown_examples.len(), 1);
        assert_eq!(summary.unknown_examples[0].raw, "［＃謎の注記］");
        assert!(!summary.row_counts.contains_key("editor_note.unmapped"));
    }

    #[test]
    fn adjacent_markers_can_classify_composite_source_patterns() {
        let patterns = vec![
            pattern("gaiji.marker", vec![r"※［＃[^］]+］"]),
            pattern("ruby.basic", vec![r"《[^》]+》"]),
            pattern("gaiji_ruby.inline_base", vec![r"※［＃[^］]+］《[^》]+》"]),
        ];
        let summary = inventory_document("w1", "※［＃「口＋世」、U+546D］《おくび》", &patterns);

        assert_eq!(summary.markers_total, 2);
        assert_eq!(summary.row_counts["gaiji.marker"].occurrences, 1);
        assert_eq!(summary.row_counts["ruby.basic"].occurrences, 1);
        assert_eq!(summary.row_counts["gaiji_ruby.inline_base"].occurrences, 1);
        assert!(summary.unknown_examples.is_empty());
    }

    #[test]
    fn editorial_and_segment_markers_are_inventory_visible() {
        let summary = inventory_document(
            "w1",
            "［ルビの「おもて」は底本では「うら」］\n［＃地付き］（fixture）\n底本：fixture",
            &[],
        );

        assert_eq!(summary.markers_total, 2);
        assert_eq!(summary.unknown_examples.len(), 2);
        assert!(summary.unknown_examples.iter().any(|example| {
            example.kind == "EditorialNoteRubyCorrection"
                && example.raw == "［ルビの「おもて」は底本では「うら」］"
        }));
        assert!(summary.unknown_examples.iter().any(|example| {
            example.kind == "SegmentBoundaryTerminalProvenance"
                && example.raw == "［＃地付き］（fixture）"
        }));
    }

    #[test]
    fn bottom_text_correction_notes_default_to_annotation_row() {
        let patterns = vec![
            pattern("annotation.chuuki", vec![]),
            pattern("ruby.basic", vec![]),
        ];
        let summary = inventory_document(
            "w1",
            "豌豆《ゑんどう》「豌豆」は底本では「跣豆」］の大さ",
            &patterns,
        );

        assert_eq!(summary.markers_total, 2);
        assert_eq!(summary.unknown_examples, []);
        assert_eq!(
            summary
                .row_counts
                .get("annotation.chuuki")
                .map(|count| count.occurrences),
            Some(1)
        );
    }

    #[test]
    fn region_classification_preserves_examples_without_body_coverage() {
        let text = "題名\n著者\n----------\n【テキスト中に現れる記号について】\n［＃］：入力者注\n（例）［＃本文終わり］\n（例）漢字《かんじ》\n----------\n［＃］\n漢字《かんじ》\n［＃本文終わり］\n翻訳の底本：例\n";
        let summary = inventory_document("w", text, &[pattern("ruby.basic", vec![])]);
        assert_eq!(summary.markers_total, 6);
        assert_eq!(summary.source_region_events.front_matter_occurrences, 3);
        assert_eq!(
            summary.source_region_events.body_end_boundary_occurrences,
            1
        );
        assert_eq!(summary.classified_region_markers.len(), 4);
        assert_eq!(summary.row_counts["ruby.basic"].occurrences, 1);
        assert_eq!(summary.unknown_examples.len(), 1);
        assert_eq!(summary.unknown_examples[0].line, 9);
    }

    #[test]
    fn unfenced_legend_and_inline_boundary_remain_unreviewed() {
        let summary = inventory_document(
            "w",
            "【テキスト中に現れる記号について】\n［＃］\n引用「［＃本文終わり］」\n",
            &[],
        );
        assert_eq!(summary.unknown_examples.len(), 2);
        assert!(summary.classified_region_markers.is_empty());
    }

    #[test]
    fn variant_editorial_header_is_source_apparatus() {
        let summary = inventory_document(
            "w",
            "題名\n［表記について］\n●［＃］は、入力者注を示す。\n----------\n本文\n",
            &[],
        );
        assert_eq!(summary.source_region_events.front_matter_occurrences, 1);
        assert!(summary.unknown_examples.is_empty());
    }

    fn pattern(row_id: &str, source_patterns: Vec<&str>) -> SourceInventoryPattern {
        SourceInventoryPattern {
            row_id: row_id.to_owned(),
            source_patterns: source_patterns.into_iter().map(str::to_owned).collect(),
        }
    }
}
