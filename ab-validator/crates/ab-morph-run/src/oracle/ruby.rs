//! Ruby-base extraction and per-analyzer reading selection for the ruby oracle.

use ab_morph_diff::{Analysis, Morpheme};
use ab_plaintext::ProjectionSpan;
use ab_warehouse::schema::NwayRegionOracleEvidenceRow;
use serde_json::{Value, json};

pub(crate) struct RubyBase {
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

pub(crate) struct RegionSpan {
    pub region_index: u64,
    pub char_start: u64,
    pub char_end: u64,
    pub is_disagreement: bool,
}

/// region_index for a base: first disagreement region overlapping the base,
/// else the region containing base_start, else 0.
fn region_index_for(regions: &[RegionSpan], base: &RubyBase) -> u64 {
    let overlaps = |r: &RegionSpan| r.char_start < base.char_end && base.char_start < r.char_end;
    regions
        .iter()
        .find(|r| overlaps(r) && r.is_disagreement)
        .or_else(|| {
            regions
                .iter()
                .find(|r| r.char_start <= base.char_start && base.char_start < r.char_end)
        })
        .map(|r| r.region_index)
        .unwrap_or(0)
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

fn analyzer_reading(analysis: &Analysis, base: &RubyBase) -> Reading {
    let bs = base.char_start as usize;
    let be = base.char_end as usize;
    let covered: Vec<&Morpheme> = analysis
        .morphemes
        .iter()
        .filter(|m| m.char_span.start < be && bs < m.char_span.end)
        .collect();
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

pub(crate) fn adjudicate(
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
    for base in ruby_bases {
        let ruby_norm = super::reading_norm::normalize(&base.reading);
        let mut winners = Vec::new();
        let mut losers = Vec::new();
        let mut detail = serde_json::Map::new();
        for analysis in analyses {
            let reading = analyzer_reading(analysis, base);
            let is_match = reading.norm.as_deref() == Some(ruby_norm.as_str());
            if is_match {
                winners.push(analysis.analyzer.clone());
            } else {
                losers.push(analysis.analyzer.clone());
            }
            detail.insert(
                analysis.analyzer.clone(),
                json!({
                    "reading": reading.raw,
                    "norm": reading.norm,
                    "match": is_match,
                    "align": reading.align,
                }),
            );
        }
        // Emit iff ≥1 analyzer failed to match.
        if losers.is_empty() {
            continue;
        }
        let classification = if winners.is_empty() {
            "nonstandard_ruby"
        } else {
            "resolved"
        };
        let winning_analyzer = if winners.len() == 1 {
            Some(winners[0].clone())
        } else {
            None
        };
        let evidence_detail = json!({
            "ruby_base": base.base,
            "ruby_reading": base.reading,
            "ruby_reading_norm": ruby_norm,
            "classification": classification,
            "per_analyzer": detail,
        })
        .to_string();
        rows.push(NwayRegionOracleEvidenceRow {
            run_id: run_id.to_owned(),
            source_id: source_id.to_owned(),
            text_id: text_id.to_owned(),
            region_index: region_index_for(regions, base),
            projected_char_start: base.char_start,
            projected_char_end: base.char_end,
            oracle_source: "ruby".to_owned(),
            winning_analyzer,
            losing_analyzers: losers,
            evidence_detail,
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
}
