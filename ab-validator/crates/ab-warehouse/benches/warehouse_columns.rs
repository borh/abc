//! Micro-benches for the direct Arrow-builder producer paths added in Lever
//! 2a/2b (tasks 3-4): `MorphemeFeaturesColumns` (`morpheme_features`) and
//! `NwayFeatureDiffsColumns` (`nway_feature_diffs`), both of which replace a
//! `Vec<Row>` intermediate and its per-row `Arc::clone` bumps. Measures build
//! (`push_row` x N) + append (parquet write) together, since that is the
//! actual hot path these refactors target.

use std::hint::black_box;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use ab_warehouse::schema::{WarehousePaths, WarehouseTable};
use ab_warehouse::writer::{MorphemeFeaturesColumns, NwayFeatureDiffsColumns, WarehouseWriter};
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
                let feature_value = if index % 17 == 0 {
                    None
                } else {
                    Some("名詞")
                };
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

/// Roughly the shape of a real `nway_feature_diffs` shard: ~500k rows across
/// the three scope types, an occasional `None` feature_value (an analyzer
/// that didn't emit that feature), and 2-3 analyzers sharing each value
/// group (the `analyzer_id` scalar-row fan-out this table is named for).
const NWAY_ROW_COUNT: usize = 500_000;
const NWAY_FEATURE_KEYS: [&str; 4] = ["pos1", "pos2", "pos3", "pos4"];
const NWAY_ANALYZERS: [&str; 3] = ["vibrato:unidic-novel-202512", "sudachi-a", "sudachi-c"];

fn bench_direct_build_and_append_nway_feature_diffs(c: &mut Criterion) {
    c.bench_function("nway_feature_diffs_direct_columns_500k_rows", |b| {
        b.iter(|| {
            let root = temp_dir("warehouse-columns-nway-bench");
            let paths = WarehousePaths::new(&root, "bench-run");
            let mut writer = WarehouseWriter::create_for_tables(
                paths.clone(),
                &[WarehouseTable::NwayFeatureDiffs],
                3,
            )
            .unwrap();

            let mut columns = NwayFeatureDiffsColumns::new();
            for index in 0..NWAY_ROW_COUNT {
                let region_index =
                    (index / (NWAY_FEATURE_KEYS.len() * NWAY_ANALYZERS.len())) as u64;
                let feature_key = NWAY_FEATURE_KEYS[index % NWAY_FEATURE_KEYS.len()];
                let analyzer_id = NWAY_ANALYZERS[index % NWAY_ANALYZERS.len()];
                // Cycle whole_region / token_position / surface scopes.
                let (scope_type, scope_position, scope_surface) = match index % 3 {
                    0 => ("whole_region", None, None),
                    1 => ("token_position", Some((index % 7) as u64), None),
                    _ => ("surface", None, Some("東京")),
                };
                // ~1 in 17 feature values is unset, mirroring an analyzer
                // that didn't emit that feature for this scope.
                let feature_value = if index % 17 == 0 {
                    None
                } else {
                    Some("名詞")
                };
                columns.push_row(
                    "run-bench",
                    "source-bench",
                    "text-bench",
                    region_index,
                    feature_key,
                    scope_type,
                    scope_position,
                    scope_surface,
                    feature_value,
                    &[analyzer_id],
                );
            }

            writer
                .append_nway_feature_diff_columns(black_box(columns))
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

criterion_group!(
    benches,
    bench_direct_build_and_append,
    bench_direct_build_and_append_nway_feature_diffs
);
criterion_main!(benches);
