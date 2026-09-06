# Benchmark Baselines

These benchmarks measure index construction, AAT JSON I/O, and property checks.

## Commands

```bash
cargo bench -p ab-index --bench index_build
cargo bench -p ab-check --bench aat_json_io
cargo bench -p ab-check --bench check_properties
```

Set `AB_CORPUS=/path/to/aozorabunko` when running `index_build` to benchmark
the real corpus instead of the deterministic synthetic fixture.

Current local baselines are stored in `benchmarks/baselines/`. Treat them as
same-machine comparison points, not portable performance promises.

## Sample Files

Sample files are JSON objects:

```json
{
  "corpus_hash": "sha256:...",
  "generated_from_index": "index.json",
  "work_ids": ["000148_799"]
}
```

The checked-in files are synthetic samples. Generate a sample from the
selected corpus index for corpus measurements. Benchmark runs against full samples must verify that the current
index `corpus_hash` matches the sample `corpus_hash` before reporting numbers.
