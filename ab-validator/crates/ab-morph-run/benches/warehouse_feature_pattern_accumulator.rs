//! Micro-bench for `WarehouseFeaturePatternAccumulator::record` /
//! `into_rows`  confirms the `BTreeMap` -> linear-scan
//! group-detection refactor does not regress throughput, on a synthetic
//! multi-region `feature_diffs` slice shaped like the real producer output
//! (contiguous `WarehouseFeatureGroupKey` runs per region).

use std::hint::black_box;
use std::sync::Arc;

use ab_morph_run::WarehouseFeaturePatternAccumulator;
use ab_warehouse::schema::{NwayFeatureDiffRow, NwayRegionRow};
use criterion::{Criterion, criterion_group, criterion_main};

const REGION_COUNT: u64 = 20_000;

fn diff_row(
    region_index: u64,
    feature_key: &str,
    analyzer_id: &str,
    feature_value: &str,
) -> NwayFeatureDiffRow {
    NwayFeatureDiffRow {
        run_id: Arc::from("run-bench"),
        source_id: Arc::from("source-bench"),
        text_id: Arc::from("text-bench"),
        region_index,
        feature_key: Arc::from(feature_key),
        scope_type: Arc::from("whole_region"),
        scope_position: None,
        scope_surface: None,
        feature_value: Some(Arc::from(feature_value)),
        analyzers: vec![Arc::from(analyzer_id)],
    }
}

/// Two contiguous group-key runs per region (`pos1`, `pos2`), each with two
/// disagreeing analyzers -- mirrors how `push_region_rows` emits one
/// contiguous block per `feature_groups` entry.
fn fixture() -> (Vec<NwayRegionRow>, Vec<NwayFeatureDiffRow>) {
    let mut regions = Vec::with_capacity(REGION_COUNT as usize);
    let mut feature_diffs = Vec::with_capacity(REGION_COUNT as usize * 4);
    for region_index in 0..REGION_COUNT {
        regions.push(NwayRegionRow {
            run_id: Arc::from("run-bench"),
            source_id: Arc::from("source-bench"),
            text_id: Arc::from("text-bench"),
            region_index,
            byte_start: 0,
            byte_end: 0,
            char_start: 0,
            char_end: 0,
            is_nonempty_whitespace: false,
            is_agreement: false,
            has_coverage_mismatch: false,
            has_segmentation_disagreement: false,
            has_feature_disagreement: true,
        });
        feature_diffs.push(diff_row(region_index, "pos1", "vibrato", "名詞"));
        feature_diffs.push(diff_row(region_index, "pos1", "sudachi-c", "動詞"));
        feature_diffs.push(diff_row(region_index, "pos2", "vibrato", "A"));
        feature_diffs.push(diff_row(region_index, "pos2", "sudachi-c", "B"));
    }
    (regions, feature_diffs)
}

fn bench_record_and_into_rows(c: &mut Criterion) {
    let (regions, feature_diffs) = fixture();
    c.bench_function("warehouse_feature_pattern_accumulator_20k_regions", |b| {
        b.iter(|| {
            let mut accumulator = WarehouseFeaturePatternAccumulator::default();
            accumulator
                .record(black_box(&regions), black_box(&feature_diffs))
                .unwrap();
            black_box(accumulator.into_rows())
        })
    });
}

criterion_group!(benches, bench_record_and_into_rows);
criterion_main!(benches);
