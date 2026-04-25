use criterion::{Criterion, criterion_group, criterion_main};

fn bench_adapter_baseline(c: &mut Criterion) {
    let input = "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。";
    c.bench_function("adapter_input_scan_baseline", |b| {
        b.iter(|| {
            assert!(input.contains('《'));
            assert!(input.contains("※［＃"));
            input.len()
        })
    });
}

criterion_group!(benches, bench_adapter_baseline);
criterion_main!(benches);
