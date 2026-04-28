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

cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --bench adapter_bench
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

## Adapter Smoke Validation

Before comparing parser adapters, build the adapter and run a corpus-backed
sample through `ab-check`:

```bash
cargo build --release --manifest-path adapters/aozora-rs/Cargo.toml
cargo run --release -p ab-index -- \
  --corpus references/aozorabunko \
  --output /tmp/ab-validator-aozora-rs-sample/index.json
jq '[.works[] | select((.features | index("ruby")) or (.features | index("gaiji")) or (.features | index("jisage_line"))) | .id][0:100]' \
  /tmp/ab-validator-aozora-rs-sample/index.json \
  > /tmp/ab-validator-aozora-rs-sample/work-ids.json
cargo run --release -p ab-check -- \
  --index /tmp/ab-validator-aozora-rs-sample/index.json \
  --corpus references/aozorabunko \
  --work-ids /tmp/ab-validator-aozora-rs-sample/work-ids.json \
  --adapter adapters/aozora-rs/target/release/aozora-rs-adapter \
  --output /tmp/ab-validator-aozora-rs-sample/reports \
  --jobs 16 \
  --per-work-timeout 30s
```

The current aozora-rs smoke sample produced 101 reports from the first 100
selected work IDs because the corpus index contains duplicate IDs for alternate
source files. All properties passed.

## Three-Way AAT Parity

`run-aat-parity.sh` runs all three adapters (`aozora2`, `aozora-rs`,
`aozora2html`) over the same work-id list and compares each pair via
`ab-compare`. It replaces the prior `ab-validator-official-html-parity.sh`,
which compared `pandoc`-of-text on both sides because no Rust adapter
rendered HTML — that script reported 0/179 matches not because the parsers
disagreed, but because the comparison was structurally broken. The new
script compares structured AAT plus parser-emitted semantic summaries,
which is what the harness was designed for.

```bash
benchmarks/run-aat-parity.sh \
  --index /tmp/ab-index.json \
  --work-ids /tmp/ab-work-ids.json \
  --sample 25 \
  --jobs 4
```

Output directory contains per-adapter `reports/` and `aats/`, per-pair
`diff-*.json` and `cmp-*.json`, and a top-level `summary.json` with:

- `total_works.<adapter>`
- `schema_valid_failures.<adapter>`, `parse_complete_failures.<adapter>`
- `aat_block_count_match.<pair>`, `aat_block_count_mismatch.<pair>`
- `semantic_summary_hash_mismatch.<pair>.<syntax_id>` (per matrix row)

## Parser Comparison

After both adapters build and pass sample validation, run:

```bash
AB_CORPUS=references/aozorabunko \
AB_BENCH_JOBS=16 \
AB_BENCH_TIMEOUT=600s \
AB_BENCH_OUT=/tmp/ab-validator-compare-current \
benchmarks/run-parser-comparison.sh
```

The output directory contains the shared index, per-parser reports, persisted
AAT artifacts, `comparison.json`, `aat-structure-comparison.json`,
`aozora-rs-metrics-summary.json`, and `triage.json` from `ab-compare`.
`triage.json` is the first artifact to inspect after a run: it buckets result
differences by validation property and corpus feature, records semantic-summary
diff counts, and lists fallback/source-supplement hotspots to guide parser work.
