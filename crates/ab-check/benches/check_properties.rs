use criterion::{Criterion, criterion_group, criterion_main};
use serde_json::Value;

fn bench_check_properties(c: &mut Criterion) {
    let schema = ab_check::check::schema_validator().unwrap();
    let value: Value =
        serde_json::from_str(include_str!("../../../data/fixtures/aat-valid-nested.json")).unwrap();
    let txt = "吾輩《わがはい》は※［＃「口＋世」、U+546D］［＃未知］\n第一章";

    c.bench_function("check_properties", |b| {
        b.iter(|| ab_check::check::check_value(txt, &value, &schema))
    });
}

criterion_group!(benches, bench_check_properties);
criterion_main!(benches);
