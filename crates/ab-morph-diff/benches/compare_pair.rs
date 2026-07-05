use std::hint::black_box;

use ab_morph_diff::{Analysis, FeatureMap, Morpheme, compare_pair_compact_with_source_text};
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_compare_pair_compact(c: &mut Criterion) {
    let source = "吾輩は猫である。今日は晴れである。\n".repeat(2_000);
    let from = analysis("vibrato", &source, false);
    let to = analysis("sudachi", &source, true);

    c.bench_function("compare_pair_compact_representative_work", |b| {
        b.iter(|| {
            compare_pair_compact_with_source_text(
                black_box(&from),
                black_box(&to),
                black_box(&source),
                black_box(&[]),
                black_box(10),
            )
            .unwrap()
        })
    });
}

fn analysis(analyzer: &str, source: &str, split_today: bool) -> Analysis {
    let mut morphemes = Vec::new();
    let chars = source.chars().collect::<Vec<_>>();
    let mut char_index = 0usize;
    while char_index < chars.len() {
        if !split_today
            && chars[char_index] == '今'
            && chars.get(char_index + 1).copied() == Some('日')
        {
            morphemes.push(morpheme(source, "今日", char_index, char_index + 2));
            char_index += 2;
        } else {
            let surface = chars[char_index].to_string();
            morphemes.push(morpheme(source, &surface, char_index, char_index + 1));
            char_index += 1;
        }
    }
    Analysis {
        analyzer: analyzer.to_owned(),
        text_id: "bench".to_owned(),
        source_text: source.to_owned(),
        morphemes,
        warnings: Vec::new(),
        ortho_annotations: None,
        ortho_offset_map: None,
    }
}

fn morpheme(source: &str, surface: &str, char_start: usize, char_end: usize) -> Morpheme {
    Morpheme {
        surface: surface.to_owned(),
        byte_span: byte_span(source, char_start)..byte_span(source, char_end),
        char_span: char_start..char_end,
        features: features(&[("pos1", Some("名詞")), ("reading", Some(surface))]),
    }
}

fn byte_span(source: &str, char_index: usize) -> usize {
    source
        .char_indices()
        .nth(char_index)
        .map(|(index, _)| index)
        .unwrap_or(source.len())
}

fn features(values: &[(&str, Option<&str>)]) -> FeatureMap {
    values
        .iter()
        .map(|(key, value)| ((*key).into(), value.map(Into::into)))
        .collect()
}

criterion_group!(benches, bench_compare_pair_compact);
criterion_main!(benches);
