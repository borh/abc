use std::{fs, hint::black_box, path::Path};

use ab_morph_run::{OrthoDetectMode, WarehouseProfile, run_analyze_aat_warehouse};
use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use serde_json::json;
use tempfile::TempDir;

fn bench_warehouse_detector_lifetime(c: &mut Criterion) {
    let Some(analyzers) = analyzer_specs() else {
        eprintln!(
            "skipping warehouse detector benchmark; set AB_MORPH_RUN_BENCH_ANALYZERS and a detector resource"
        );
        return;
    };
    let Some((mode, ml_model)) = detector_mode() else {
        eprintln!(
            "skipping warehouse detector benchmark; set AB_ORTHO_ML_MODEL or AB_VIBRATO_DICT"
        );
        return;
    };
    let fixture = fixture_aat_dir();

    c.bench_function("warehouse_detector_lifetime/65_inputs_jobs_2", |b| {
        b.iter_batched(
            || TempDir::new().unwrap(),
            |output| {
                run_analyze_aat_warehouse(
                    None,
                    Some(black_box(fixture.path())),
                    black_box(&analyzers),
                    output.path(),
                    "bench",
                    2,
                    WarehouseProfile::Triage,
                    1,
                    mode,
                    ml_model.clone(),
                    None,
                )
                .unwrap();
            },
            BatchSize::LargeInput,
        );
    });
}

fn analyzer_specs() -> Option<Vec<String>> {
    std::env::var("AB_MORPH_RUN_BENCH_ANALYZERS")
        .ok()
        .map(|value| {
            value
                .split(',')
                .filter(|value| !value.is_empty())
                .map(str::to_owned)
                .collect::<Vec<_>>()
        })
        .filter(|values| !values.is_empty())
        .or_else(|| {
            (std::env::var_os("AB_VIBRATO_DICT").is_some()
                || std::env::var_os("AB_VIBRATO_DICT_DIR").is_some())
            .then(|| vec!["vibrato".to_owned()])
        })
}

fn detector_mode() -> Option<(OrthoDetectMode, Option<std::path::PathBuf>)> {
    if let Some(path) = std::env::var_os("AB_ORTHO_ML_MODEL") {
        return Some((OrthoDetectMode::Ml, Some(path.into())));
    }
    (std::env::var_os("AB_VIBRATO_DICT").is_some()
        || std::env::var_os("AB_VIBRATO_DICT_DIR").is_some())
    .then_some((OrthoDetectMode::Heuristic, None))
}

fn fixture_aat_dir() -> TempDir {
    let dir = TempDir::new().unwrap();
    for index in 0..65 {
        write_aat(dir.path(), index);
    }
    dir
}

fn write_aat(root: &Path, index: usize) {
    let value = json!({
        "version": 1,
        "work_id": format!("bench-{index:06}"),
        "blocks": [{
            "kind": "paragraph",
            "content": [{"kind": "text", "value": "吾輩は猫である。今日は晴れである。"}]
        }],
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
        root.join(format!("{index:06}.json")),
        serde_json::to_vec(&value).unwrap(),
    )
    .unwrap();
}

criterion_group!(benches, bench_warehouse_detector_lifetime);
criterion_main!(benches);
