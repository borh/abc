use criterion::{Criterion, criterion_group, criterion_main};
use serde_json::Value;

fn bench_aat_json_io(c: &mut Criterion) {
    let schema: Value =
        serde_json::from_str(include_str!("../../../data/aat-schema.json")).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    let fixture = include_str!("../../../data/fixtures/aat-valid-nested.json");

    c.bench_function("aat_json_parse_validate_serialize", |b| {
        b.iter(|| {
            let value: Value = serde_json::from_str(fixture).unwrap();
            validator.validate(&value).unwrap();
            serde_json::to_string(&value).unwrap()
        })
    });
}

criterion_group!(benches, bench_aat_json_io);
criterion_main!(benches);
