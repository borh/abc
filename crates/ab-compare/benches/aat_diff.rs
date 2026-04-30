use std::{fs, hint::black_box, path::Path};

use ab_compare::aat_diff::compare_aat_dirs_with_limit;
use criterion::{Criterion, criterion_group, criterion_main};
use serde_json::json;
use tempfile::TempDir;

fn bench_aat_diff(c: &mut Criterion) {
    let fixture = fixture_dirs();
    c.bench_function("aat_diff_representative_tree", |b| {
        b.iter(|| {
            compare_aat_dirs_with_limit(
                black_box(fixture.a.path()),
                black_box(fixture.b.path()),
                black_box(Some(20)),
            )
            .unwrap()
        })
    });
}

struct FixtureDirs {
    a: TempDir,
    b: TempDir,
}

fn fixture_dirs() -> FixtureDirs {
    let a = TempDir::new().unwrap();
    let b = TempDir::new().unwrap();
    for idx in 0..200 {
        write_aat(a.path(), idx, false);
        write_aat(b.path(), idx, idx % 7 == 0);
    }
    FixtureDirs { a, b }
}

fn write_aat(root: &Path, idx: usize, variant: bool) {
    let path = root.join(format!("{idx:06}.json"));
    let body = (0..200)
        .map(|line| {
            if variant && line % 19 == 0 {
                json!({"kind": "ruby", "base": format!("猫{line}"), "reading": "ねこ"})
            } else {
                json!({"kind": "text", "value": format!("吾輩は猫である。{idx}-{line}\n")})
            }
        })
        .collect::<Vec<_>>();
    let value = json!({
        "version": 1,
        "work_id": format!("bench-{idx:06}"),
        "blocks": [{"kind": "paragraph", "content": body}],
        "meta": {
            "adapter": "bench",
            "adapter_version": "bench",
            "source_encoding": "utf-8",
            "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": []
        }
    });
    fs::write(path, serde_json::to_vec(&value).unwrap()).unwrap();
}

criterion_group!(benches, bench_aat_diff);
criterion_main!(benches);
