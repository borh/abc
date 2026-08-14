# Benchmark Baselines

Benchmarks are used to decide whether JSON remains acceptable as the canonical
wire format and whether check concurrency needs changes.

## Commands

```bash
cargo bench -p ab-index --bench index_build
cargo bench -p ab-check --bench aat_json_io
cargo bench -p ab-check --bench check_properties
```

Set `AB_CORPUS=references/aozorabunko` when running `index_build` to benchmark
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

The checked-in files are tiny placeholders until a real corpus index is
generated. Benchmark runs against full samples must verify that the current
index `corpus_hash` matches the sample `corpus_hash` before reporting numbers.

JSON remains the canonical adapter and report format unless parse, serialize,
and schema validation exceed 15% of end-to-end batch runtime on `mixed-100`.
