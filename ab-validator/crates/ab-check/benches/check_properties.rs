use criterion::{Criterion, criterion_group, criterion_main};
use serde_json::{Value, json};

fn bench_check_properties(c: &mut Criterion) {
    let schema = ab_check::check::schema_validator().unwrap();
    let txt = large_aozora_text();
    let value = large_matching_aat(&txt);

    c.bench_function("check_properties_large", |b| {
        b.iter(|| ab_check::check::check_value(&txt, &value, schema))
    });
}

fn large_aozora_text() -> String {
    let mut text =
        String::from("タイトル\n著者\n--------------------\n凡例\n--------------------\n");
    for idx in 0..20_000 {
        text.push_str(&format!(
            "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。第{idx}行。\n"
        ));
    }
    text
}

fn large_matching_aat(txt: &str) -> Value {
    json!({
        "version": 1,
        "work_id": "bench_large",
        "blocks": [
            {
                "kind": "paragraph",
                "content": [
                    {
                        "kind": "text",
                        "value": ab_check::source_projection::comparison_lossy_body(ab_check::properties::body_text(txt))
                    }
                ]
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

criterion_group!(benches, bench_check_properties);
criterion_main!(benches);
