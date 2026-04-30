use std::{fs, hint::black_box, path::Path};

use ab_morph_run::{OutputProfile, run_analyze_aat};
use criterion::{Criterion, criterion_group, criterion_main};
use serde_json::json;
use tempfile::TempDir;

fn bench_analyze_aat(c: &mut Criterion) {
    let Some(analyzers) = analyzer_specs() else {
        eprintln!(
            "skipping analyze_aat bench; set AB_MORPH_RUN_BENCH_ANALYZERS, AB_VIBRATO_DICT, or AB_SUDACHI_DICT"
        );
        return;
    };
    let fixture = fixture_aat_dir();
    let output = TempDir::new().unwrap();

    let mut group = c.benchmark_group("analyze_aat");
    for jobs in [1usize, 2usize] {
        group.bench_function(format!("jobs_{jobs}"), |b| {
            b.iter(|| {
                run_analyze_aat(
                    None,
                    Some(black_box(fixture.path())),
                    black_box(&analyzers),
                    &output.path().join(format!("analyses-{jobs}.jsonl")),
                    Some(&output.path().join(format!("comparisons-{jobs}.jsonl"))),
                    Some(&output.path().join(format!("errors-{jobs}.jsonl"))),
                    false,
                    jobs,
                    OutputProfile::Compact,
                    Some(&output.path().join(format!("examples-{jobs}.jsonl"))),
                    10,
                    None,
                )
                .unwrap()
            })
        });
    }
    group.finish();
}

fn analyzer_specs() -> Option<Vec<String>> {
    if let Ok(value) = std::env::var("AB_MORPH_RUN_BENCH_ANALYZERS") {
        let analyzers = value
            .split(',')
            .filter(|value| !value.is_empty())
            .map(str::to_owned)
            .collect::<Vec<_>>();
        return (!analyzers.is_empty()).then_some(analyzers);
    }
    if std::env::var_os("AB_VIBRATO_DICT").is_some() {
        return Some(vec!["vibrato".to_owned()]);
    }
    if std::env::var_os("AB_SUDACHI_DICT").is_some() {
        return Some(vec!["sudachi-c".to_owned()]);
    }
    None
}

fn fixture_aat_dir() -> TempDir {
    let dir = TempDir::new().unwrap();
    for idx in 0..8 {
        write_aat(dir.path(), idx);
    }
    dir
}

fn write_aat(root: &Path, idx: usize) {
    let text = "吾輩は猫である。今日は晴れである。\n".repeat(200);
    let value = json!({
        "version": 1,
        "work_id": format!("bench-{idx:06}"),
        "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": text}]}],
        "meta": {
            "adapter": "bench",
            "adapter_version": "bench",
            "source_encoding": "utf-8",
            "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": []
        }
    });
    fs::write(
        root.join(format!("{idx:06}.json")),
        serde_json::to_vec(&value).unwrap(),
    )
    .unwrap();
}

criterion_group!(benches, bench_analyze_aat);
criterion_main!(benches);
