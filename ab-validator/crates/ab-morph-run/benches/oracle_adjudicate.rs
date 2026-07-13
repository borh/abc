//! Micro-bench for `oracle::ruby::adjudicate`  confirms the
//! deferred evidence-map-build refactor does not regress the emit-heavy path,
//! and quantifies the win on the fully-matching (no-emit) path.

use std::hint::black_box;
use std::ops::Range;
use std::sync::Arc;

use ab_morph_diff::{Analysis, FeatureMap, Morpheme};
use ab_morph_run::{RegionSpan, RubyBase, adjudicate};
use criterion::{Criterion, criterion_group, criterion_main};

/// Mirrors corpus shape: ~34% of bases have all analyzers agree (no emitted
/// row), the rest have exactly one dissenting analyzer (emitted row).
const BASE_COUNT: usize = 1_000;

fn morph(chars: Range<usize>, feats: &[(&str, &str)]) -> Morpheme {
    let mut features = FeatureMap::default();
    for (k, v) in feats {
        let _ = features.insert((*k).into(), Some((*v).into()));
    }
    Morpheme {
        surface: "東京".to_owned(),
        byte_span: 0..0,
        char_span: chars,
        features,
    }
}

fn analysis(id: &str, morphemes: Vec<Morpheme>) -> Analysis {
    Analysis {
        analyzer: id.to_owned(),
        text_id: "bench".to_owned(),
        source_text: Arc::from(""),
        morphemes,
        warnings: Vec::new(),
        ortho_annotations: None,
        ortho_offset_map: None,
    }
}

fn fixture() -> (Vec<RubyBase>, Vec<Analysis>, Vec<RegionSpan>) {
    let mut bases = Vec::with_capacity(BASE_COUNT);
    let mut vibrato_morphs = Vec::with_capacity(BASE_COUNT);
    let mut sudachi_morphs = Vec::with_capacity(BASE_COUNT);
    for i in 0..BASE_COUNT {
        let cs = (i * 2) as u64;
        let ce = cs + 2;
        bases.push(RubyBase {
            char_start: cs,
            char_end: ce,
            base: "東京".to_owned(),
            reading: "とうきょう".to_owned(),
        });
        // ~34% of bases: every analyzer agrees -> no emitted row.
        let full_match = i % 3 == 0;
        let vibrato_kana = if full_match {
            "トウキョウ"
        } else {
            "トウケイ"
        };
        vibrato_morphs.push(morph(cs as usize..ce as usize, &[("kana", vibrato_kana)]));
        sudachi_morphs.push(morph(
            cs as usize..ce as usize,
            &[("reading_form", "トウキョウ")],
        ));
    }
    let analyses = vec![
        analysis("vibrato", vibrato_morphs),
        analysis("sudachi-c", sudachi_morphs),
    ];
    let regions = vec![RegionSpan {
        region_index: 0,
        char_start: 0,
        char_end: (BASE_COUNT * 2) as u64,
        is_disagreement: true,
    }];
    (bases, analyses, regions)
}

fn bench_adjudicate(c: &mut Criterion) {
    let (bases, analyses, regions) = fixture();
    c.bench_function("ruby_adjudicate_match_heavy_1000_bases", |b| {
        b.iter(|| {
            adjudicate(
                black_box("run"),
                black_box("source"),
                black_box("text"),
                black_box(&bases),
                black_box(&analyses),
                black_box(&regions),
            )
        })
    });
}

criterion_group!(benches, bench_adjudicate);
criterion_main!(benches);
