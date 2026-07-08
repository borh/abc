//! Micro-bench for `MorphemeFeaturesColumns` (Lever 2a, task 3): the direct
//! Arrow-builder producer path for `morpheme_features`, which replaces the
//! `Vec<MorphemeFeatureRow>` intermediate and its per-row `Arc::clone` bumps
//! (previously ~5 per row: 4 shared id columns + the feature key). Measures
//! build (`push_row` x N) + append (parquet write) together, since that is
//! the actual hot path this refactor targets.

use std::hint::black_box;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use ab_warehouse::schema::{WarehousePaths, WarehouseTable};
use ab_warehouse::writer::{MorphemeFeaturesColumns, WarehouseWriter};
use criterion::{Criterion, criterion_group, criterion_main};

/// Roughly the shape of a real `morpheme_features` shard: ~500k rows,
/// several feature keys per morpheme, and an occasional `None` value (e.g.
/// missing `lemma`).
const ROW_COUNT: usize = 500_000;
const FEATURE_KEYS: [&str; 4] = ["pos1", "pos2", "lemma", "reading"];

fn bench_direct_build_and_append(c: &mut Criterion) {
    c.bench_function("morpheme_features_direct_columns_500k_rows", |b| {
        b.iter(|| {
            let root = temp_dir("warehouse-columns-bench");
            let paths = WarehousePaths::new(&root, "bench-run");
            let mut writer = WarehouseWriter::create_for_tables(
                paths.clone(),
                &[WarehouseTable::MorphemeFeatures],
                3,
            )
            .unwrap();

            let mut columns = MorphemeFeaturesColumns::new();
            for index in 0..ROW_COUNT {
                let morpheme_index = (index / FEATURE_KEYS.len()) as u64;
                let feature_key = FEATURE_KEYS[index % FEATURE_KEYS.len()];
                // ~1 in 17 features is unset, mirroring optional features
                // like `lemma` on some analyzers.
                let feature_value = if index % 17 == 0 { None } else { Some("名詞") };
                columns.push_row(
                    "run-bench",
                    "source-bench",
                    "text-bench",
                    "vibrato:unidic-novel-202512",
                    morpheme_index,
                    feature_key,
                    feature_value,
                );
            }

            writer
                .append_morpheme_feature_columns(black_box(columns))
                .unwrap();
            writer.finalize().unwrap();
            let _ = std::fs::remove_dir_all(&root);
        });
    });
}

fn temp_dir(label: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    std::env::temp_dir().join(format!(
        "ab-warehouse-{label}-{}-{unique}",
        std::process::id()
    ))
}

criterion_group!(benches, bench_direct_build_and_append);
criterion_main!(benches);
