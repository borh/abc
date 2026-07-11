use criterion::{Criterion, criterion_group, criterion_main};
use serde_json::{Value, json};

fn bench_aat_json_io(c: &mut Criterion) {
    // The fixture below is a `"version": 1` document; validate it against
    // the frozen v1 schema (see `data/aat-schema-v1.json`), not whichever
    // schema `data/aat-schema.json` currently points at.
    let schema: Value =
        serde_json::from_str(include_str!("../../../data/aat-schema-v1.json")).unwrap();
    let validator = jsonschema::validator_for(&schema).unwrap();
    let fixture = large_aat_fixture();
    let fixture_json = serde_json::to_string(&fixture).unwrap();

    c.bench_function("aat_json_parse_validate_serialize_large", |b| {
        b.iter(|| {
            let value: Value = serde_json::from_str(&fixture_json).unwrap();
            validator.validate(&value).unwrap();
            serde_json::to_string(&value).unwrap()
        })
    });
}

fn large_aat_fixture() -> Value {
    let mut content = Vec::new();
    for idx in 0..10_000 {
        content.push(json!({
            "kind": "text",
            "value": format!("吾輩は猫である。{idx}\n")
        }));
        if idx % 16 == 0 {
            content.push(json!({
                "kind": "ruby",
                "base": "吾輩",
                "reading": "わがはい"
            }));
        }
        if idx % 97 == 0 {
            content.push(json!({
                "kind": "gaiji",
                "description": "「口＋世」、U+546D",
                "resolved": "",
                "jis_code": null,
                "unresolved_reason": null
            }));
        }
    }
    json!({
        "version": 1,
        "work_id": "bench_large",
        "blocks": [
            {
                "kind": "paragraph",
                "content": content
            }
        ],
        "meta": {
            "adapter": "bench",
            "adapter_version": "bench",
            "source_encoding": "utf-8",
            "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": []
        }
    })
}

criterion_group!(benches, bench_aat_json_io);
criterion_main!(benches);
