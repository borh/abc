use std::{
    hint::black_box,
    path::{Path, PathBuf},
};

use ab_aat_to_parser_ir::{ConversionOptions, MappingDocument, PreparedConverter, SchemaSet};
use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use serde_json::{Value, json};

fn bench_convert_aat(c: &mut Criterion) {
    let repo = repo_root();
    let abc = std::env::var_os("AB_ABC_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| repo.join("data/abc-schemas"));
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    let converter = PreparedConverter::new(mapping, schemas).unwrap();

    let mut group = c.benchmark_group("convert_aat");
    for (label, paragraphs) in [("small", 50usize), ("large", 2_000usize)] {
        let aat = aat_fixture(paragraphs);
        group.bench_function(label, |b| {
            b.iter_batched(
                || aat.clone(),
                |aat| {
                    converter
                        .convert(black_box(aat), ConversionOptions::default())
                        .unwrap()
                },
                BatchSize::LargeInput,
            )
        });
    }
    group.finish();
}

fn aat_fixture(paragraphs: usize) -> Value {
    let mut blocks = Vec::with_capacity(paragraphs);
    for idx in 0..paragraphs {
        let mut content = vec![json!({
            "kind": "text",
            "value": format!("吾輩は猫である{idx}。名前はまだ無い。どこで生れたかとんと見当がつかぬ。")
        })];
        if idx % 4 == 0 {
            content.push(json!({
                "kind": "ruby",
                "base": "吾輩",
                "reading": "わがはい"
            }));
            content.push(json!({
                "kind": "text",
                "value": "何でも薄暗いじめじめした所でニャーニャー泣いていた事だけは記憶している。"
            }));
        }
        if idx % 31 == 0 {
            content.push(json!({
                "kind": "gaiji",
                "description": "「口＋世」、U+546D",
                "resolved": "",
                "jis_code": null,
                "unresolved_reason": null
            }));
        }
        blocks.push(json!({ "kind": "paragraph", "content": content }));
    }
    json!({
        "version": 1,
        "work_id": "000000",
        "meta": {
            "adapter": "bench",
            "adapter_version": "bench 0.1.0",
            "source_encoding": "utf-8",
            "source_hash":
                "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": []
        },
        "blocks": blocks
    })
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

criterion_group!(benches, bench_convert_aat);
criterion_main!(benches);
