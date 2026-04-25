# Benchmark Baselines

Benchmarks are used to decide whether JSON remains acceptable as the canonical
wire format and whether adapter/check concurrency needs changes.

## Commands

```bash
cargo bench -p ab-index --bench index_build
cargo bench -p ab-check --bench aat_json_io
cargo bench -p ab-check --bench check_properties

cd adapters/aozora2
cargo bench --bench adapter_bench
```

Set `AB_CORPUS=references/aozorabunko` when running `index_build` to benchmark
the real corpus instead of the deterministic synthetic fixture.

## Full Corpus Benchmark

Use the full runner after changes that can affect corpus loading, adapter
output, validation throughput, JSON size, or report size:

```bash
AB_CORPUS=references/aozorabunko \
AB_BENCH_JOBS=16 \
benchmarks/run-full-corpus.sh
```

The runner writes `summary.json`, `index.json`, stderr logs, and validation
reports under `/tmp/ab-validator-bench-*` by default. Override the output
directory with `AB_BENCH_OUT=/path/to/results`.

The summary records:

- indexed work count and corpus hash
- index build seconds and works/second
- validation seconds and works/second
- index/report artifact bytes
- failing property count

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
