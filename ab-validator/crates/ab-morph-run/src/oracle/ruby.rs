//! Ruby-base extraction and per-analyzer reading selection for the ruby oracle.

use super::ruby_contract::{
    AnalyzerRubyReadingEvidence, RubyOracleClassification, RubyReadingAlignment,
    RubyReadingEvidenceDetail,
};
use ab_morph_diff::{Analysis, Morpheme};
use ab_plaintext::ProjectionSpan;
use ab_warehouse::schema::NwayRegionOracleEvidenceRow;
use serde_json::Value;

pub struct RubyBase {
    pub char_start: u64,
    pub char_end: u64,
    pub base: String,
    pub reading: String,
}

/// Extract ruby bases (with editor reading) from the projected ruby spans.
/// The reading is resolved from the retained AAT via the span's JSON pointer.
/// Bases without a non-empty reading are skipped (no oracle judgment possible).
pub(crate) fn ruby_bases(aat: &Value, spans: &[ProjectionSpan]) -> Vec<RubyBase> {
    spans
        .iter()
        .filter(|span| span.is_ruby_base)
        .filter_map(|span| {
            let node = aat.pointer(&span.aat_pointer)?;
            let reading = node.get("reading").and_then(Value::as_str)?;
            if reading.is_empty() {
                return None;
            }
            let base = node
                .get("base")
                .and_then(Value::as_str)
                .unwrap_or("")
                .to_owned();
            Some(RubyBase {
                char_start: span.projected_char_start,
                char_end: span.projected_char_end,
                base,
                reading: reading.to_owned(),
            })
        })
        .collect()
}

/// Select the reading feature for a morpheme by analyzer family (spec R7).
pub(crate) fn morpheme_reading(analyzer_id: &str, morpheme: &Morpheme) -> Option<String> {
    let feat = |key: &str| {
        morpheme
            .features
            .get(key)
            .and_then(|value| value.as_ref())
            .map(|value| value.to_string())
            // A present-but-empty or "*" feature is absent, not a reading — else a
            // kana="*" would normalize to "" and become a false non-match instead
            // of falling back to pron. (Analyzers already map "*"→None at parse
            // time, but the guard makes the contract robust to any caller.)
            .filter(|value| !value.is_empty() && value != "*")
    };
    if analyzer_id.starts_with("vibrato") || analyzer_id.starts_with("vaporetto") {
        feat("kana").or_else(|| feat("pron"))
    } else if analyzer_id.starts_with("sudachi") {
        feat("reading_form")
    } else {
        None // test analyzers emit no reading
    }
}

pub struct RegionSpan {
    pub region_index: u64,
    pub char_start: u64,
    pub char_end: u64,
    pub is_disagreement: bool,
}

/// region_index for a base: first disagreement region overlapping the base,
/// else the region containing base_start, else 0.
///
/// Bounded via binary search: regions are sorted ascending by `char_start` and
/// tile contiguously (non-overlapping), so the set of regions overlapping
/// `[base_start, base_end)` is a single contiguous window. `partition_point`
/// finds its left edge in O(log R); the window is then scanned linearly for
/// the first disagreement (same order as the original `.find`), and — since
/// regions are non-overlapping and sorted — only the window's first entry can
/// possibly contain `base_start` (any later entry's start is >= the first
/// entry's end, which is already > base_start), so checking it there
/// reproduces the original "region containing base_start" fallback exactly.
fn region_index_for(regions: &[RegionSpan], base: &RubyBase) -> u64 {
    let bs = base.char_start;
    let be = base.char_end;
    let start_idx = regions.partition_point(|r| r.char_end <= bs);
    let mut containing: Option<&RegionSpan> = None;
    for r in &regions[start_idx..] {
        if r.char_start >= be {
            break;
        }
        if r.is_disagreement {
            return r.region_index;
        }
        if containing.is_none() && r.char_start <= bs {
            containing = Some(r);
        }
    }
    containing.map(|r| r.region_index).unwrap_or(0)
}

/// One analyzer's reading over a ruby base: the raw concatenated reading, its
/// normalized form, and the alignment outcome. `align` ∈ `exact`,
/// `boundary-misalign` (covered morphemes do not exactly tile the base),
/// `no-reading` (a covered morpheme has no reading feature). raw/norm are None
/// unless `align == "exact"`.
struct Reading {
    norm: Option<String>,
    raw: Option<String>,
    align: &'static str,
}

fn reading_alignment(value: &str) -> RubyReadingAlignment {
    match value {
        "exact" => RubyReadingAlignment::Exact,
        "boundary-misalign" => RubyReadingAlignment::BoundaryMisalign,
        "no-reading" => RubyReadingAlignment::NoReading,
        other => panic!("unknown ruby reading alignment {other}"),
    }
}

fn analyzer_reading(analysis: &Analysis, base: &RubyBase) -> Reading {
    let bs = base.char_start as usize;
    let be = base.char_end as usize;
    // Bounded window via binary search: `analysis.morphemes` is sorted ascending
    // by `char_span.start` and non-overlapping, so `char_span.end` is also
    // non-decreasing. `partition_point` finds the first morpheme whose end is
    // past `bs` (the left edge of the covered window) in O(log M); every
    // morpheme from there on has `end` >= that first `end` > bs, so the
    // `bs < m.char_span.end` half of the original filter holds automatically
    // for the whole forward scan, and stopping once `start >= be` reproduces
    // the `m.char_span.start < be` half exactly. Net effect: the identical
    // covered set as the old O(M) filter, in O(log M + window).
    let start_idx = analysis
        .morphemes
        .partition_point(|m| m.char_span.end <= bs);
    let mut covered: Vec<&Morpheme> = Vec::new();
    for m in &analysis.morphemes[start_idx..] {
        if m.char_span.start >= be {
            break;
        }
        covered.push(m);
    }
    // exact tiling: non-empty, contiguous, first.start == bs, last.end == be.
    let tiles = covered.first().is_some_and(|m| m.char_span.start == bs)
        && covered.last().is_some_and(|m| m.char_span.end == be)
        && covered
            .windows(2)
            .all(|w| w[0].char_span.end == w[1].char_span.start);
    if !tiles {
        return Reading {
            norm: None,
            raw: None,
            align: "boundary-misalign",
        };
    }
    let mut concat = String::new();
    for m in &covered {
        match morpheme_reading(&analysis.analyzer, m) {
            Some(r) => concat.push_str(&r),
            None => {
                return Reading {
                    norm: None,
                    raw: None,
                    align: "no-reading",
                };
            }
        }
    }
    Reading {
        norm: Some(super::reading_norm::normalize(&concat)),
        raw: Some(concat),
        align: "exact",
    }
}

/// One analyzer's outcome for a base, retained (without name clones or
/// evidence-map entries) until the emit gate decides the base is worth
/// materializing.
struct AnalyzerOutcome<'a> {
    analyzer: &'a str,
    reading: Reading,
    is_match: bool,
}

pub fn adjudicate(
    run_id: &str,
    source_id: &str,
    text_id: &str,
    ruby_bases: &[RubyBase],
    analyses: &[Analysis],
    regions: &[RegionSpan],
) -> Vec<NwayRegionOracleEvidenceRow> {
    // ≥2-analyzer precondition (spec §Precondition): a single-analyzer run has no
    // cross-analyzer disagreement to adjudicate, and region_index would not map to
    // a real n-way region. Encode it in the pure contract, not just the caller.
    if analyses.len() < 2 {
        return Vec::new();
    }
    let mut rows = Vec::new();
    // Reused across bases: the ~34%-fully-matching common case never grows
    // this beyond `analyses.len()`, so it settles into a single allocation.
    let mut outcomes: Vec<AnalyzerOutcome> = Vec::with_capacity(analyses.len());
    for base in ruby_bases {
        let ruby_norm = super::reading_norm::normalize(&base.reading);
        if ruby_norm.is_empty() {
            // An all-non-kana/interpunct-only ruby reading normalizes to "" — skip
            // it, else an all-non-kana analyzer reading would falsely "match" on
            // "" == "" instead of being correctly judged unadjudicable.
            continue;
        }
        outcomes.clear();
        let mut any_comparable = false;
        let mut any_loser = false;
        for analysis in analyses {
            let reading = analyzer_reading(analysis, base);
            if reading.align == "exact" {
                any_comparable = true;
            }
            let is_match = reading.norm.as_deref() == Some(ruby_norm.as_str());
            if !is_match {
                any_loser = true;
            }
            // Deferred: no name clone, no evidence-map entry yet — built only
            // if the emit gate below decides this base is worth a row.
            outcomes.push(AnalyzerOutcome {
                analyzer: analysis.analyzer.as_str(),
                reading,
                is_match,
            });
        }
        // Emit iff ≥1 analyzer failed to match.
        if !any_loser {
            continue;
        }
        // Rebuild winners/losers/detail now, in the same per-analyzer order as
        // the loop above, deferred from it purely to skip this allocation work
        // on the fully-matching (no-emit) path above.
        let mut winners = Vec::new();
        let mut losers = Vec::new();
        let mut detail = std::collections::BTreeMap::new();
        for outcome in outcomes.drain(..) {
            if outcome.is_match {
                winners.push(outcome.analyzer.to_owned());
            } else {
                losers.push(outcome.analyzer.to_owned());
            }
            detail.insert(
                outcome.analyzer.to_owned(),
                AnalyzerRubyReadingEvidence {
                    reading: outcome.reading.raw,
                    norm: outcome.reading.norm,
                    matches: outcome.is_match,
                    align: reading_alignment(outcome.reading.align),
                },
            );
        }
        // resolved: ≥1 winner. nonstandard_ruby: no winner but ≥1 analyzer produced a
        // comparable (exact) reading — a genuine reading the dictionaries lack.
        // no_comparable_reading: no analyzer produced a comparable reading (all
        // boundary-misalign or no-reading) — not a dictionary signal.
        let classification = if !winners.is_empty() {
            "resolved"
        } else if any_comparable {
            "nonstandard_ruby"
        } else {
            "no_comparable_reading"
        };
        let winning_analyzer = if winners.len() == 1 {
            Some(winners[0].clone())
        } else {
            None
        };
        let classification_enum = match classification {
            "resolved" => RubyOracleClassification::Resolved,
            "nonstandard_ruby" => RubyOracleClassification::NonstandardRuby,
            "no_comparable_reading" => RubyOracleClassification::NoComparableReading,
            other => panic!("unknown ruby oracle classification {other}"),
        };
        let evidence_detail = serde_json::to_string(&RubyReadingEvidenceDetail {
            ruby_base: base.base.clone(),
            ruby_reading: base.reading.clone(),
            ruby_reading_norm: ruby_norm.clone(),
            classification: classification_enum,
            per_analyzer: detail,
        })
        .expect("ruby reading evidence detail serializes");
        rows.push(NwayRegionOracleEvidenceRow {
            run_id: run_id.to_owned(),
            source_id: source_id.to_owned(),
            text_id: text_id.to_owned(),
            region_index: region_index_for(regions, base),
            projected_char_start: base.char_start,
            projected_char_end: base.char_end,
            oracle_source: "ruby".to_owned(),
            classification: classification.to_owned(),
            winning_analyzer,
            losing_analyzers: losers,
            evidence_detail,
            // Resolved bases have ≥1 exact-tiling analyzer that matched the editor
            // ruby, so the normalized editor reading is the authoritative adjudicated
            // reading. Unresolved bases (nonstandard_ruby / no_comparable_reading)
            // have no confirmed match and carry no adjudicated reading.
            adjudicated_reading: if classification == "resolved" {
                Some(ruby_norm.clone())
            } else {
                None
            },
        });
    }
    rows
}

/// Verbatim copy of the pre-refactor `adjudicate` body (builds the evidence
/// `detail` map and clones winner/loser names inside the per-analyzer loop,
/// before the emit gate). Retained as the differential oracle for
/// `adjudicate_matches_reference_*` tests below — never called from
/// production code.
#[cfg(test)]
fn adjudicate_reference(
    run_id: &str,
    source_id: &str,
    text_id: &str,
    ruby_bases: &[RubyBase],
    analyses: &[Analysis],
    regions: &[RegionSpan],
) -> Vec<NwayRegionOracleEvidenceRow> {
    if analyses.len() < 2 {
        return Vec::new();
    }
    let mut rows = Vec::new();
    for base in ruby_bases {
        let ruby_norm = super::reading_norm::normalize(&base.reading);
        if ruby_norm.is_empty() {
            continue;
        }
        let mut winners = Vec::new();
        let mut losers = Vec::new();
        let mut detail = std::collections::BTreeMap::new();
        let mut any_comparable = false;
        for analysis in analyses {
            let reading = analyzer_reading(analysis, base);
            if reading.align == "exact" {
                any_comparable = true;
            }
            let is_match = reading.norm.as_deref() == Some(ruby_norm.as_str());
            if is_match {
                winners.push(analysis.analyzer.clone());
            } else {
                losers.push(analysis.analyzer.clone());
            }
            detail.insert(
                analysis.analyzer.clone(),
                AnalyzerRubyReadingEvidence {
                    reading: reading.raw,
                    norm: reading.norm,
                    matches: is_match,
                    align: reading_alignment(reading.align),
                },
            );
        }
        if losers.is_empty() {
            continue;
        }
        let classification = if !winners.is_empty() {
            "resolved"
        } else if any_comparable {
            "nonstandard_ruby"
        } else {
            "no_comparable_reading"
        };
        let winning_analyzer = if winners.len() == 1 {
            Some(winners[0].clone())
        } else {
            None
        };
        let classification_enum = match classification {
            "resolved" => RubyOracleClassification::Resolved,
            "nonstandard_ruby" => RubyOracleClassification::NonstandardRuby,
            "no_comparable_reading" => RubyOracleClassification::NoComparableReading,
            other => panic!("unknown ruby oracle classification {other}"),
        };
        let evidence_detail = serde_json::to_string(&RubyReadingEvidenceDetail {
            ruby_base: base.base.clone(),
            ruby_reading: base.reading.clone(),
            ruby_reading_norm: ruby_norm.clone(),
            classification: classification_enum,
            per_analyzer: detail,
        })
        .expect("ruby reading evidence detail serializes");
        rows.push(NwayRegionOracleEvidenceRow {
            run_id: run_id.to_owned(),
            source_id: source_id.to_owned(),
            text_id: text_id.to_owned(),
            region_index: region_index_for(regions, base),
            projected_char_start: base.char_start,
            projected_char_end: base.char_end,
            oracle_source: "ruby".to_owned(),
            classification: classification.to_owned(),
            winning_analyzer,
            losing_analyzers: losers,
            evidence_detail,
            // Resolved bases have ≥1 exact-tiling analyzer that matched the editor
            // ruby, so the normalized editor reading is the authoritative adjudicated
            // reading. Unresolved bases (nonstandard_ruby / no_comparable_reading)
            // have no confirmed match and carry no adjudicated reading.
            adjudicated_reading: if classification == "resolved" {
                Some(ruby_norm.clone())
            } else {
                None
            },
        });
    }
    rows
}

#[cfg(test)]
mod tests {
    use super::*;
    use ab_morph_diff::{Analysis, FeatureMap, Morpheme};
    use serde_json::json;

    fn morph(surface: &str, chars: std::ops::Range<usize>, feats: &[(&str, &str)]) -> Morpheme {
        let mut features = FeatureMap::default();
        for (k, v) in feats {
            let _ = features.insert((*k).into(), Some((*v).into()));
        }
        Morpheme {
            surface: surface.to_owned(),
            byte_span: 0..0,
            char_span: chars,
            features,
        }
    }

    #[test]
    fn extracts_ruby_bases_with_reading_from_pointer() {
        let aat = json!({
            "blocks": [ { "content": [ { "kind": "ruby", "base": "東京", "reading": "とうきょう" } ] } ]
        });
        let spans = vec![ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 2,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "ruby".to_owned(),
            is_ruby_base: true,
            is_gaiji: false,
            is_note: false,
        }];
        let bases = ruby_bases(&aat, &spans);
        assert_eq!(bases.len(), 1);
        assert_eq!(bases[0].reading, "とうきょう");
        assert_eq!(bases[0].char_start, 0);
        assert_eq!(bases[0].char_end, 2);
    }

    #[test]
    fn skips_ruby_without_reading() {
        let aat = json!({ "blocks": [ { "content": [ { "kind": "ruby", "base": "x" } ] } ] });
        let spans = vec![ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 1,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "ruby".to_owned(),
            is_ruby_base: true,
            is_gaiji: false,
            is_note: false,
        }];
        assert!(ruby_bases(&aat, &spans).is_empty());
    }

    #[test]
    fn reading_by_family() {
        let vib = morph(
            "東京",
            0..2,
            &[("kana", "トウキョウ"), ("pron", "トーキョー")],
        );
        assert_eq!(
            morpheme_reading("vibrato:unidic-novel-202512", &vib).as_deref(),
            Some("トウキョウ")
        );
        let vib_pron_only = morph("x", 0..1, &[("pron", "トーキョー")]);
        assert_eq!(
            morpheme_reading("vibrato", &vib_pron_only).as_deref(),
            Some("トーキョー")
        );
        // kana present but "*" → falls back to pron, not a false empty reading.
        let vib_star = morph("x", 0..1, &[("kana", "*"), ("pron", "トーキョー")]);
        assert_eq!(
            morpheme_reading("vibrato", &vib_star).as_deref(),
            Some("トーキョー")
        );
        let sud = morph("東京", 0..2, &[("reading_form", "トウキョウ")]);
        assert_eq!(
            morpheme_reading("sudachi-c", &sud).as_deref(),
            Some("トウキョウ")
        );
        let t = morph("x", 0..1, &[]);
        assert_eq!(morpheme_reading("test:single", &t), None);
    }

    fn analysis(id: &str, morphs: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: id.to_owned(),
            text_id: "work-a".to_owned(),
            source_text: std::sync::Arc::from(""),
            morphemes: morphs,
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        }
    }
    fn base(cs: u64, ce: u64, reading: &str) -> RubyBase {
        RubyBase {
            char_start: cs,
            char_end: ce,
            base: "".to_owned(),
            reading: reading.to_owned(),
        }
    }
    fn regions() -> Vec<RegionSpan> {
        vec![RegionSpan {
            region_index: 0,
            char_start: 0,
            char_end: 10,
            is_disagreement: true,
        }]
    }

    #[test]
    fn adjudicate_matches_reference_across_match_single_and_multi_loser_bases() {
        // Differential oracle test (Lever 3, task 1): `adjudicate` defers the
        // evidence-map build and name clones to after the emit gate;
        // `adjudicate_reference` is the verbatim pre-refactor implementation.
        // The two must return byte-for-byte identical rows for every base
        // shape the refactor touches: fully-matching (no row), single-loser,
        // and multi-loser (order-sensitive `losing_analyzers`/`detail`).
        let vibrato = analysis(
            "vibrato",
            vec![
                morph("東京", 0..2, &[("kana", "トウキョウ")]), // base A: matches
                morph("東京", 2..4, &[("kana", "トウキョウ")]), // base B: matches
                morph("東京", 4..6, &[("kana", "トウケイ")]),   // base C: loses
            ],
        );
        let sudachi = analysis(
            "sudachi-c",
            vec![
                morph("東京", 0..2, &[("reading_form", "トウキョウ")]), // A: matches
                morph("東京", 2..4, &[("reading_form", "トウキョウ")]), // B: matches
                morph("東京", 4..6, &[("reading_form", "トウキョウ")]), // C: matches
            ],
        );
        let vaporetto = analysis(
            "vaporetto",
            vec![
                morph("東京", 0..2, &[("kana", "トウキョウ")]), // A: matches
                morph("東京", 2..4, &[("kana", "トウケイ")]),   // B: loses
                morph("東京", 4..6, &[("kana", "トウケイ")]),   // C: loses
            ],
        );
        let bases = vec![
            base(0, 2, "とうきょう"), // A: fully matching -> no row
            base(2, 4, "とうきょう"), // B: single loser (vaporetto)
            base(4, 6, "とうきょう"), // C: multiple losers (vibrato, vaporetto)
        ];
        let analyses = vec![vibrato, sudachi, vaporetto];
        let regions = regions();

        let current = adjudicate("r", "s", "t", &bases, &analyses, &regions);
        let reference = adjudicate_reference("r", "s", "t", &bases, &analyses, &regions);

        assert_eq!(
            current, reference,
            "refactored adjudicate must byte-for-byte match the pre-refactor reference"
        );

        // Sanity on the fixture itself, so a broken fixture can't make the
        // differential comparison above vacuously trivial.
        assert_eq!(reference.len(), 2, "base A fully matches and emits nothing");
        assert_eq!(reference[0].losing_analyzers, vec!["vaporetto".to_owned()]);
        assert_eq!(
            reference[1].losing_analyzers,
            vec!["vibrato".to_owned(), "vaporetto".to_owned()],
            "losing_analyzers preserves per-analyzer (analyses-slice) order"
        );
        assert!(reference[1].evidence_detail.contains("vibrato"));
        assert!(reference[1].evidence_detail.contains("vaporetto"));
    }

    #[test]
    fn all_match_emits_nothing() {
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウキョウ")])],
        );
        let b = analysis(
            "sudachi-c",
            vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])],
        );
        let rows = adjudicate(
            "r",
            "s",
            "t",
            &[base(0, 2, "とうきょう")],
            &[a, b],
            &regions(),
        );
        assert!(rows.is_empty());
    }

    #[test]
    fn unique_winner_when_one_matches() {
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウケイ")])],
        ); // wrong
        let b = analysis(
            "sudachi-c",
            vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])],
        );
        let rows = adjudicate(
            "r",
            "s",
            "t",
            &[base(0, 2, "とうきょう")],
            &[a, b],
            &regions(),
        );
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].winning_analyzer.as_deref(), Some("sudachi-c"));
        assert_eq!(rows[0].losing_analyzers, vec!["vibrato".to_owned()]);
        assert_eq!(rows[0].oracle_source, "ruby");
        assert_eq!(rows[0].region_index, 0);
    }

    #[test]
    fn zero_match_is_nonstandard_ruby() {
        let a = analysis("vibrato", vec![morph("本気", 0..2, &[("kana", "ホンキ")])]);
        let b = analysis(
            "sudachi-c",
            vec![morph("本気", 0..2, &[("reading_form", "ホンキ")])],
        );
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "マジ")], &[a, b], &regions());
        assert_eq!(rows.len(), 1);
        assert!(rows[0].winning_analyzer.is_none());
        assert_eq!(rows[0].losing_analyzers.len(), 2);
        assert!(rows[0].evidence_detail.contains("nonstandard_ruby"));
        // Not resolved → no adjudicated reading (the ruby stays evidence only).
        assert!(rows[0].adjudicated_reading.is_none());
    }

    #[test]
    fn nonstandard_ruby_when_exact_but_no_match() {
        // both tile exactly & disagree with the editor → genuine reading gap.
        let a = analysis("vibrato", vec![morph("本気", 0..2, &[("kana", "ホンキ")])]);
        let b = analysis(
            "sudachi-c",
            vec![morph("本気", 0..2, &[("reading_form", "ホンキ")])],
        );
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "マジ")], &[a, b], &regions());
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].classification, "nonstandard_ruby");
    }

    #[test]
    fn ruby_reading_does_not_change_base_span_alignment() {
        // The editor reading is longer than the base. Token alignment still uses
        // the projected base span, so exact base-token tiling remains comparable.
        let a = analysis("vibrato", vec![morph("名前", 0..2, &[("kana", "ナマエ")])]);
        let b = analysis(
            "sudachi-c",
            vec![morph("名前", 0..2, &[("reading_form", "ナマエ")])],
        );
        let rows = adjudicate(
            "r",
            "s",
            "t",
            &[RubyBase {
                char_start: 0,
                char_end: 2,
                base: "名前".to_owned(),
                reading: "めいしょう".to_owned(),
            }],
            &[a, b],
            &regions(),
        );

        assert_eq!(rows.len(), 1);
        let detail: crate::oracle::ruby_contract::RubyReadingEvidenceDetail =
            serde_json::from_str(&rows[0].evidence_detail).unwrap();
        assert_eq!(detail.ruby_base, "名前");
        assert_eq!(detail.ruby_reading, "めいしょう");
        assert_eq!(detail.ruby_reading_norm, "めいしょう");
        assert_eq!(rows[0].projected_char_start, 0);
        assert_eq!(rows[0].projected_char_end, 2);
    }

    #[test]
    fn all_boundary_misalign_is_no_comparable_reading() {
        // every analyzer straddles the base end → boundary-misalign, zero winners.
        let a = analysis(
            "vibrato",
            vec![morph("本気說", 0..3, &[("kana", "ホンキセツ")])],
        );
        let b = analysis(
            "sudachi-c",
            vec![morph("本気說", 0..3, &[("reading_form", "ホンキセツ")])],
        );
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "ほんき")], &[a, b], &regions());
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].classification, "no_comparable_reading");
        // No exact-tiling comparison → misalignment stays evidence, no adjudicated
        // reading is emitted.
        assert!(rows[0].adjudicated_reading.is_none());
    }

    #[test]
    fn all_no_reading_is_no_comparable_reading() {
        // exact tiling but every covered morpheme lacks a reading feature → no-reading.
        let a = analysis("vibrato", vec![morph("本気", 0..2, &[])]);
        let b = analysis("sudachi-c", vec![morph("本気", 0..2, &[])]);
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "ほんき")], &[a, b], &regions());
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].classification, "no_comparable_reading");
    }

    #[test]
    fn resolved_row_classification() {
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウケイ")])],
        );
        let b = analysis(
            "sudachi-c",
            vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])],
        );
        let rows = adjudicate(
            "r",
            "s",
            "t",
            &[base(0, 2, "とうきょう")],
            &[a, b],
            &regions(),
        );
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].classification, "resolved");
        // Resolved: the normalized editor reading is surfaced as the adjudicated
        // authoritative reading (never altering tokenization).
        assert_eq!(rows[0].adjudicated_reading.as_deref(), Some("とうきょう"));
    }

    #[test]
    fn boundary_misalign_counts_as_nonmatch() {
        // one morpheme straddles the base end → unalignable → non-match
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウキョウ")])],
        );
        let b = analysis(
            "sudachi-c",
            vec![morph("東京市", 0..3, &[("reading_form", "トウキョウシ")])],
        );
        let rows = adjudicate(
            "r",
            "s",
            "t",
            &[base(0, 2, "とうきょう")],
            &[a, b],
            &regions(),
        );
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].winning_analyzer.as_deref(), Some("vibrato"));
        assert!(rows[0].evidence_detail.contains("boundary-misalign"));
    }

    #[test]
    fn single_analyzer_emits_nothing() {
        // ≥2-analyzer precondition: even a mismatching lone analyzer yields no rows.
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウケイ")])],
        );
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "とうきょう")], &[a], &regions());
        assert!(rows.is_empty());
    }

    #[test]
    fn evidence_detail_keeps_raw_reading() {
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウケイ")])],
        );
        let b = analysis(
            "sudachi-c",
            vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])],
        );
        let rows = adjudicate(
            "r",
            "s",
            "t",
            &[base(0, 2, "とうきょう")],
            &[a, b],
            &regions(),
        );
        // raw (pre-normalization) reading is retained for manual review of norm bugs.
        assert!(rows[0].evidence_detail.contains("トウケイ"));
    }

    #[test]
    fn prop_winner_and_loser_partition() {
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウキョウ")])],
        );
        let b = analysis(
            "sudachi-c",
            vec![morph("東京", 0..2, &[("reading_form", "トウケイ")])],
        );
        let n = 2;
        let rows = adjudicate(
            "r",
            "s",
            "t",
            &[base(0, 2, "とうきょう")],
            &[a, b],
            &regions(),
        );
        for row in &rows {
            let winners = row.winning_analyzer.iter().count();
            assert_eq!(
                winners + row.losing_analyzers.len(),
                n,
                "winner+losers partition all analyzers when unique"
            );
            if row.winning_analyzer.is_none() {
                // ambiguous or nonstandard: losers must be non-empty (emit rule)
                assert!(!row.losing_analyzers.is_empty());
            }
        }
    }

    #[test]
    fn empty_normalized_ruby_reading_is_skipped() {
        // "・" (interpunct only) normalizes to "" — must not be adjudicated, else
        // an all-non-kana analyzer reading would falsely "match" on "" == "".
        let a = analysis("vibrato", vec![morph("x", 0..1, &[("kana", "*")])]);
        let b = analysis(
            "sudachi-c",
            vec![morph("x", 0..1, &[("reading_form", "*")])],
        );
        let rows = adjudicate("r", "s", "t", &[base(0, 1, "・")], &[a, b], &regions());
        assert!(rows.is_empty());

        // Same for a latin-string ruby reading.
        let a = analysis("vibrato", vec![morph("x", 0..1, &[("kana", "*")])]);
        let b = analysis(
            "sudachi-c",
            vec![morph("x", 0..1, &[("reading_form", "*")])],
        );
        let rows = adjudicate("r", "s", "t", &[base(0, 1, "ABC")], &[a, b], &regions());
        assert!(rows.is_empty());
    }

    #[test]
    fn ambiguous_multiple_winners_yields_null_winner() {
        // Two analyzers match the ruby reading, one does not: ambiguous (≥2
        // winners) is distinct from nonstandard_ruby (0 winners) and must resolve
        // to a null winning_analyzer with classification "resolved".
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウキョウ")])],
        );
        let b = analysis(
            "sudachi-c",
            vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])],
        );
        let c = analysis(
            "vaporetto",
            vec![morph("東京", 0..2, &[("kana", "トウケイ")])],
        ); // wrong
        let rows = adjudicate(
            "r",
            "s",
            "t",
            &[base(0, 2, "とうきょう")],
            &[a, b, c],
            &regions(),
        );
        assert_eq!(rows.len(), 1);
        assert!(rows[0].winning_analyzer.is_none());
        assert_eq!(rows[0].losing_analyzers.len(), 1);
        assert!(rows[0].evidence_detail.contains("\"resolved\""));
        assert!(!rows[0].evidence_detail.contains("nonstandard_ruby"));
    }

    #[test]
    fn oracle_pipeline_from_aat_to_parquet() {
        use ab_plaintext::visible_text_projection_with_spans;
        use ab_warehouse::schema::{WarehousePaths, WarehouseTable};
        use ab_warehouse::writer::{WarehouseWriter, parquet_table_row_count};
        use serde_json::json;

        let aat = json!({
            "blocks": [ { "content": [
                { "kind": "ruby", "base": "東京", "reading": "とうきょう" },
                { "kind": "text", "value": "は" }
            ] } ]
        });
        let (_text, spans) = visible_text_projection_with_spans(&aat);
        let bases = ruby_bases(&aat, &spans);
        assert_eq!(
            bases.len(),
            1,
            "one ruby base extracted from real projection spans"
        );
        assert_eq!(bases[0].reading, "とうきょう");

        // sudachi matches the editor ruby; vibrato does not.
        let a = analysis(
            "vibrato",
            vec![morph("東京", 0..2, &[("kana", "トウケイ")])],
        );
        let b = analysis(
            "sudachi-c",
            vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])],
        );
        let regions = vec![RegionSpan {
            region_index: 0,
            char_start: 0,
            char_end: 3,
            is_disagreement: true,
        }];
        let rows = adjudicate("run-a", "src-a", "work-a", &bases, &[a, b], &regions);
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].winning_analyzer.as_deref(), Some("sudachi-c"));

        let root = std::env::temp_dir().join(format!("oracle-e2e-{}", std::process::id()));
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_nway_region_oracle_evidence(&rows).unwrap();
        writer.finalize().unwrap();
        assert_eq!(
            parquet_table_row_count(&paths.final_dir, WarehouseTable::NwayRegionOracleEvidence)
                .unwrap(),
            1
        );
        let _ = std::fs::remove_dir_all(root);
    }
}
