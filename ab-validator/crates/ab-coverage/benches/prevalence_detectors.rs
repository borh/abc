use std::{hint::black_box, path::PathBuf};

use ab_coverage::{
    detectors::{DetectorContext, DetectorRegistry},
    matrix::CoverageMatrix,
};
use criterion::{Criterion, criterion_group, criterion_main};
use serde_json::{Value, json};

fn bench_prevalence_detectors(c: &mut Criterion) {
    let matrix_path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../data/aozora-syntax-coverage.toml");
    let matrix = CoverageMatrix::from_toml(&matrix_path).expect("load coverage matrix");
    let registry = DetectorRegistry::from_matrix(matrix.rows());
    let aat = synthetic_aat(10_000);
    let ctx = DetectorContext {
        aat: &aat,
        source: "｜山《やま》※［＃U+4E00］［＃傍点］［＃改ページ］",
    };

    let mut group = c.benchmark_group("prevalence_detectors");
    group.bench_function("repeated_detect", |b| {
        b.iter(|| {
            registry
                .rows()
                .map(|row_id| registry.detect(row_id, black_box(&ctx)))
                .sum::<u64>()
        });
    });
    group.bench_function("detect_all", |b| {
        b.iter(|| registry.detect_all(black_box(&ctx)));
    });
    group.finish();
}

fn synthetic_aat(node_count: usize) -> Value {
    let kinds = [
        json!({"kind": "ruby", "base": "山", "reading": "やま", "direction": "left"}),
        json!({"kind": "gaiji", "description": "U+4E00、第1水準", "jis_code": "1-16-01"}),
        json!({"kind": "style", "style_type": "boten", "content": []}),
        json!({"kind": "heading", "content": []}),
        json!({"kind": "figure", "caption": "図"}),
        json!({"kind": "caption"}),
        json!({"kind": "tcy"}),
        json!({"kind": "warichu"}),
    ];
    let content = (0..node_count)
        .map(|index| kinds[index % kinds.len()].clone())
        .collect::<Vec<_>>();
    json!({"version": 1, "work_id": "bench", "blocks": content, "meta": {}})
}

criterion_group!(benches, bench_prevalence_detectors);
criterion_main!(benches);
