use aozora2_adapter::aat_json_from_bytes;
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_adapter_aat(c: &mut Criterion) {
    let input = large_aozora_text();
    c.bench_function("adapter_aat_json_large", |b| {
        b.iter(|| aat_json_from_bytes(input.as_bytes()).unwrap())
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
    text.push_str("底本：ベンチ\n");
    text
}

criterion_group!(benches, bench_adapter_aat);
criterion_main!(benches);
