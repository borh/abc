# Morph Warehouse SQL Report

This directory contains DuckDB SQL templates for reproducible Aozora morph warehouse reports.

Build a report from a warehouse run:

```bash
reports/morph-warehouse/build-report.sh \
  /db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12 \
  /db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12 \
  50
```

The runner writes all generated data to the output directory. Use `/db` for real corpus reports so DuckDB temporary files and TSV outputs do not consume project filesystem space.

The current query set is triage-safe. It reads only:

- `runs.parquet`
- `sources.parquet`
- `analyses.parquet`
- `morphemes.parquet`
- `nway_regions.parquet`
- `nway_region_analyzers.parquet`
- `feature_pattern_counts.parquet`
- `errors.parquet`

Raw feature drill-down queries should be added separately after a full-profile warehouse run exists.

## Interactive marimo notebook

The colocated marimo notebook is `reports/morph-warehouse/warehouse_explorer.py`.
It uses inline `uv`/PEP 723 dependencies, so it can be launched without adding a Python project environment.
Use the wrapper on this Nix-based workstation; it adds the GCC runtime library path needed by the DuckDB Python wheel and then runs the current pinned marimo tool version through `uvx`:

```bash
reports/morph-warehouse/open-warehouse-explorer.sh
```

By default the wrapper binds to:

```text
farspark.hyakutake-barbel.ts.net:27188
```

Override the host or port with `AB_MORPH_MARIMO_HOST` and `AB_MORPH_MARIMO_PORT`.

By default it opens the current triage warehouse:

```text
/db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12
```

Override the run directory or DuckDB temporary spill directory with:

```bash
AB_MORPH_WAREHOUSE_RUN_DIR=/db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12 \
AB_MORPH_DUCKDB_TEMP_DIR=/db/ab-validator/tmp/marimo-morph-warehouse \
reports/morph-warehouse/open-warehouse-explorer.sh
```

The notebook uses contextual marimo UI controls for report view, row limit, feature key, term probes, source/text substring filters, and comma- or whitespace-separated exact `source_id` / `text_id` lists. It is triage-safe: it only reads the same warehouse tables as the static report, plus the selected source's AAT JSON when the `AAT structure` view is active. AAT JSON is rendered with marimo's native JSON tree viewer.
