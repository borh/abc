# Morph Warehouse Smoke Report - 2026-05-01

## Command

Selected AAT: `scratch/morph-full-corpus/aats/aozora-rs-adapter/000183_52736-4806bcb5510c.json`

`AB_SUDACHI_DICT`: `/nix/store/9x4lgx2bi4yzw9inflrp869693br6rp9-sudachi-dictionary-20260116-full/share/sudachi/system.dic`

```bash
rm -rf scratch/morph-warehouse-smoke
mkdir -p scratch/morph-warehouse-smoke/aats
ln -s "$(realpath \"scratch/morph-full-corpus/aats/aozora-rs-adapter/000183_52736-4806bcb5510c.json\")" "scratch/morph-warehouse-smoke/aats/$(basename \"scratch/morph-full-corpus/aats/aozora-rs-adapter/000183_52736-4806bcb5510c.json\")"
AB_SUDACHI_DICT="/nix/store/9x4lgx2bi4yzw9inflrp869693br6rp9-sudachi-dictionary-20260116-full/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-warehouse-smoke/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --warehouse-dir scratch/morph-warehouse-smoke/warehouse \
    --run-id smoke-2026-05-01 \
    --jobs 1
```

## Artifact

- Warehouse path: `scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01`
- Canonical files:

```text
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/analyses.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/errors.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/morpheme_features.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/morphemes.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/nway_feature_diffs.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/nway_region_analyzers.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/nway_regions.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/run_analyzers.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/runs.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/sources.parquet
scratch/morph-warehouse-smoke/warehouse/runs/smoke-2026-05-01/views.sql
```

## DuckDB checks

- `warehouse_nway_regions` row count: 40.
- `top_segmentation_patterns` row count: 3.

## Notes

The smoke verifies that warehouse mode writes comprehensive Parquet facts directly. `top_segmentation_patterns` is filtered to regions with segmentation disagreement. No JSONL files are produced by the warehouse run.
