use std::{
    fs,
    path::{Path, PathBuf},
    time::Duration,
};

use ab_index::{features::FeatureDetector, index::build_index};
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_index_build(c: &mut Criterion) {
    let root = std::env::var_os("AB_CORPUS")
        .map(PathBuf::from)
        .unwrap_or_else(fixture_corpus);
    let detector =
        FeatureDetector::from_toml(&repo_root().join("data/feature-patterns.toml")).unwrap();
    let mut group = c.benchmark_group("index_build");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(20));
    group.bench_function("build_index", |b| {
        b.iter(|| {
            let index = build_index(&root, &detector).unwrap();
            assert!(index.works_count > 0);
            index
        })
    });
    group.finish();
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .unwrap()
        .to_path_buf()
}

fn fixture_corpus() -> PathBuf {
    let root =
        std::env::temp_dir().join(format!("ab-validator-index-bench-{}", std::process::id()));
    let files = root.join("cards/000001/files/1_ruby_1");
    fs::create_dir_all(&files).unwrap();
    let body = "タイトル\n著者\n--------------------\n凡例\n--------------------\n吾輩《わがはい》は※［＃「口＋世」、U+546D］である。\n".repeat(1000);
    for idx in 0..100 {
        fs::write(files.join(format!("{idx}_ruby.txt")), &body).unwrap();
    }
    root
}

criterion_group!(benches, bench_index_build);
criterion_main!(benches);
