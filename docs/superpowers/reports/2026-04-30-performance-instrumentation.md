# Performance Instrumentation Report

Date: 2026-04-30

## Scope

This pass implements measurement scaffolding and low-risk inspection-level wins. It does not claim corpus runtime improvement yet; Phase 2 and Phase 3 optimizations still require flamegraph/dhat evidence on the canonical workload.

## Build profile

Workspace release and bench profiles now use:

- Thin LTO.
- One codegen unit.
- Line-table-only debug info for profiler symbolization.

## Benchmark pinning policy

This machine is a Ryzen 7950X3D-class topology with two L3 groups:

| CPU set | L3 id | L3 size | Use |
| --- | --- | ---: | --- |
| `0-7,16-23` | `0` | `98304K` | large-L3 CCD |
| `8-15,24-31` | `1` | `32768K` | smaller-L3 CCD |

For stable numbers, benchmark commands should be pinned with `taskset` and the chosen CPU set must be recorded with the result.

Recommended defaults:

```bash
# Single-threaded Criterion smoke benchmark on one large-L3 physical core.
taskset -c 0 cargo bench -p ab-morph-diff --bench compare_pair

# Parallel corpus run with eight physical cores on the large-L3 CCD.
taskset -c 0-7 /run/current-system/sw/bin/time -v \
  target/release/ab-morph-run analyze-aat ... --jobs 8

# Cross-CCD sensitivity check, useful before publishing final numbers.
taskset -c 8 cargo bench -p ab-morph-diff --bench compare_pair
```

Avoid mixing pinned and unpinned measurements in the same comparison table.

## Added benchmarks

- `crates/ab-morph-diff/benches/compare_pair.rs`: representative compact pair comparison.
- `crates/ab-morph-run/benches/analyze_aat.rs`: compact AAT runner serial vs. parallel, gated on analyzer dictionary environment.
- `crates/ab-compare/benches/aat_diff.rs`: representative AAT tree diff.

`ab-morph-run` benchmark requirements:

- Set `AB_MORPH_RUN_BENCH_ANALYZERS` to a comma-separated analyzer list, or
- Set `AB_VIBRATO_DICT` for `vibrato`, or
- Set `AB_SUDACHI_DICT` for `sudachi-c`.

## Low-risk changes implemented

- `ab-check` now compiles the AAT JSON schema once through `LazyLock` instead of rebuilding validators at each call site.
- `ab-diff-utils::first_difference` no longer materializes full `Vec<char>` values before locating the first mismatch. It only walks to the first difference and materializes bounded snippets.
- `ab-compare::aat_diff` now uses `rustc_hash::FxHashMap` for internal count accumulators during AAT tree walks and converts back to `BTreeMap` at ordered output boundaries.

## Release-build baseline

Command:

```bash
/run/current-system/sw/bin/time -v cargo build --release
```

Result:

- Wall time: `0:39.90`.
- User time: `401.16s`.
- System time: `42.48s`.
- CPU: `1111%`.
- Peak RSS: `1116556 KB`.

Note: `/usr/bin/time` is not available in this environment; `/run/current-system/sw/bin/time` was used.

## Deferred measurements

These still need to be run before profile-led optimization over the real corpus:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0-7 cargo bench -p ab-morph-run --bench analyze_aat
```

For full-corpus CLI numbers, run the canonical `ab-morph-run analyze-aat` command pinned to `0-7` and capture `/run/current-system/sw/bin/time -v` output.

## Pinned benchmark smoke results

Commands:

```bash
taskset -c 0 cargo bench -p ab-morph-diff --bench compare_pair -- --sample-size 10 --measurement-time 2
taskset -c 0 cargo bench -p ab-compare --bench aat_diff -- --sample-size 10 --measurement-time 2
```

Results:

| Benchmark | CPU set | Mean range |
| --- | --- | ---: |
| `compare_pair_compact_representative_work` | `0` | `260.58 ms..262.50 ms` |
| `aat_diff_representative_tree` | `0` | `36.649 ms..36.873 ms` |

These are smoke baselines for the new benchmark targets, not final corpus-performance claims.

## Pinned `ab-morph-run` benchmark smoke

Command:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0-7 cargo bench -p ab-morph-run --bench analyze_aat -- --sample-size 10 --measurement-time 2
```

Results:

| Benchmark | CPU set | Mean range |
| --- | --- | ---: |
| `analyze_aat/jobs_1` | `0-7` | `98.480 ms..99.207 ms` |
| `analyze_aat/jobs_2` | `0-7` | `74.282 ms..75.171 ms` |

This smoke used the benchmark's synthetic AAT fixture and the reproducible Sudachi full dictionary from the flake. It validates the benchmark path and pinning policy; it is not a full-corpus throughput number.

## Pinned full-corpus baseline attempt

Command:

```bash
mkdir -p scratch/perf-baseline
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0-7 /run/current-system/sw/bin/time -v \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/perf-baseline/analyses.jsonl.zst \
    --comparisons-output scratch/perf-baseline/comparisons.jsonl.zst \
    --examples-output scratch/perf-baseline/examples.jsonl.zst \
    --nway-output scratch/perf-baseline/nway.jsonl.zst \
    --errors-output scratch/perf-baseline/errors.jsonl.zst \
    --manifest-output scratch/perf-baseline/manifest.json \
    --jobs 8 \
    --progress-interval-seconds 30
```

Result:

- Inputs discovered: `17894`.
- Termination: signal 15 after `30:44.16`.
- Peak RSS: `72419252 KB` (~69.1 GiB).
- Peak observed progress RSS before termination: `71888828 KB` at `1839s`.
- User time: `14146.74s`.
- System time: `491.35s`.
- CPU: `793%`.
- Durable output artifacts: none. The run was terminated before output writers finished/flushed.

Conclusion: the canonical `--jobs 8` three-analyzer full-corpus run is memory-bound and not currently a viable baseline. The next optimization should reduce resident analyzer/dictionary memory or reduce concurrent analyzer workers before chasing smaller map/string optimizations.

## Shared Sudachi dictionary smoke

Change: `sudachi-a` and `sudachi-c` now share one loaded `JapaneseDictionary` when constructed by `ab-morph-run` from the same `AB_SUDACHI_DICT` path.

Pinned benchmark command:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0-7 cargo bench -p ab-morph-run --bench analyze_aat -- --sample-size 10 --measurement-time 2
```

Results after sharing:

| Benchmark | CPU set | Mean range | Criterion comparison |
| --- | --- | ---: | --- |
| `analyze_aat/jobs_1` | `0-7` | `98.860 ms..99.113 ms` | no change detected |
| `analyze_aat/jobs_2` | `0-7` | `74.345 ms..75.506 ms` | no change detected |

This change targets resident memory for multi-Sudachi-mode runs. It is not expected to improve per-source throughput.

## Streaming compact N-way smoke

Change: compact N-way output now visits N-way regions and accumulates stats/examples directly instead of building a full `NwayComparison { regions: Vec<NwayRegion> }` per source before reducing it to a compact row.

Pinned benchmark command:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0-7 cargo bench -p ab-morph-run --bench analyze_aat -- --sample-size 10 --measurement-time 2
```

Results after streaming compact N-way:

| Benchmark | CPU set | Mean range | Criterion comparison |
| --- | --- | ---: | --- |
| `analyze_aat/jobs_1` | `0-7` | `98.920 ms..99.839 ms` | no change detected |
| `analyze_aat/jobs_2` | `0-7` | `74.764 ms..76.014 ms` | no change detected |

This targets peak memory on large individual documents. It should not materially change small-fixture throughput.

## Large8 bounded memory baseline

This baseline uses the eight largest checked AAT JSON files by byte size. It preserves the stressful shape of the full corpus run: large documents, three analyzers, compact pairwise output, compact examples, compact N-way output, and `--jobs 8` pinned to the large-L3 CCD.

The run overlapped with other local work, so wall time and CPU utilization are not authoritative. Process RSS/PSS progress and `/run/current-system/sw/bin/time -v` peak RSS are still useful memory measurements.

Command:

```bash
rm -rf scratch/perf-large8
mkdir -p scratch/perf-large8/aats/aozora-rs-adapter scratch/perf-large8/out
find scratch/morph-full-corpus/aats -type f -name '*.json' -printf '%s %p\n' \
  | sort -nr \
  | head -8 \
  | awk '{print $2}' \
  | while read -r path; do
      ln -s "$(realpath "$path")" "scratch/perf-large8/aats/aozora-rs-adapter/$(basename "$path")"
    done

AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0-7 /run/current-system/sw/bin/time -v \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/perf-large8/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/perf-large8/out/analyses.jsonl.zst \
    --comparisons-output scratch/perf-large8/out/comparisons.jsonl.zst \
    --examples-output scratch/perf-large8/out/examples.jsonl.zst \
    --nway-output scratch/perf-large8/out/nway.jsonl.zst \
    --errors-output scratch/perf-large8/out/errors.jsonl.zst \
    --manifest-output scratch/perf-large8/out/manifest.json \
    --jobs 8 \
    --progress-interval-seconds 30
```

Result:

- Inputs: `8` largest AAT files.
- Exit status: `0`.
- Peak RSS: `54380356 KB` (~51.9 GiB).
- Peak observed progress RSS: `54382368 KB` at `242s`.
- Output rows: `24` analyses, `24` pairwise comparisons, `240` examples, `8` N-way rows, `0` errors.
- Artifact sizes: `4K` analyses, `8K` comparisons, `12K` examples, `28K` N-way, `4K` errors.

Conclusion: streaming compact N-way prevents the earlier full-corpus `70+ GB` failure pattern, but large concurrent inputs still create ~52 GiB process RSS. The next memory target is per-document analysis retention and scheduling: avoid running the largest AAT files concurrently, or write a memory-aware scheduler that limits concurrent large documents while preserving `--jobs 8` for normal-sized documents.

## Single largest file memory baseline

This isolates per-document memory from scheduling/concurrency effects.

Command:

```bash
rm -rf scratch/perf-one-largest
mkdir -p scratch/perf-one-largest/aats/aozora-rs-adapter scratch/perf-one-largest/out
largest=$(find scratch/morph-full-corpus/aats -type f -name '*.json' -printf '%s %p\n' | sort -nr | head -1 | awk '{print $2}')
ln -s "$(realpath "$largest")" "scratch/perf-one-largest/aats/aozora-rs-adapter/$(basename "$largest")"

AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0 /run/current-system/sw/bin/time -v \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/perf-one-largest/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/perf-one-largest/out/analyses.jsonl.zst \
    --comparisons-output scratch/perf-one-largest/out/comparisons.jsonl.zst \
    --examples-output scratch/perf-one-largest/out/examples.jsonl.zst \
    --nway-output scratch/perf-one-largest/out/nway.jsonl.zst \
    --errors-output scratch/perf-one-largest/out/errors.jsonl.zst \
    --manifest-output scratch/perf-one-largest/out/manifest.json \
    --jobs 1 \
    --progress-interval-seconds 15
```

Result:

- Input: `001529_50685-dd3b2fe4e5bf.json`, `41956998` bytes.
- Exit status: `0`.
- Peak RSS: `18804424 KB` (~17.9 GiB).
- Peak observed progress RSS: `18804564 KB` at `257s`.
- Rows: `3` analyses, `3` comparisons, `30` examples, `1` N-way row, `0` errors.

Conclusion: there is a real per-document memory issue independent of scheduling. The largest single file holds roughly 18 GiB while reducing the three analyses. Scheduling remains useful for corpus stability, but the first correctness target is reducing per-document retained analysis/comparison memory.
