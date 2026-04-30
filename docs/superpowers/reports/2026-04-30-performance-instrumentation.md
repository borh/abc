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

## Planned compact feature-string thinning

The single-file run shows the peak is per-document retained analysis payload. The first targeted thinning step is to store feature keys and feature values as compact inline strings in `ab-morph-diff`'s model. These strings are short and repeated across every morpheme (`pos1`, `reading_form`, `名詞`, `一般`, etc.), so they are a high-leverage target without changing JSON output shape.

## Compact feature-string thinning result

Change:

- `ab-morph-diff::FeatureKey` and `FeatureValue` now use `compact_str::CompactString`.
- Feature maps still serialize with the same JSON shape.
- Runner summary/example rows convert feature keys and values back to owned `String` only at the output boundary.

Verification:

- `cargo test -p ab-morph-diff -p ab-morph-analyzers -p ab-morph-run`: pass.
- `cargo fmt --all -- --check`: pass.
- `cargo clippy -p ab-morph-diff -p ab-morph-analyzers -p ab-morph-run --all-targets -- -D warnings`: pass.
- `cargo check --workspace --benches`: pass.
- `cargo build --release`: pass.

Single-largest-file rerun:

```bash
rm -rf scratch/perf-one-largest-compactstr
mkdir -p scratch/perf-one-largest-compactstr/aats/aozora-rs-adapter scratch/perf-one-largest-compactstr/out
largest=$(find scratch/morph-full-corpus/aats -type f -name '*.json' -printf '%s %p\n' | sort -nr | head -1 | awk '{print $2}')
ln -s "$(realpath "$largest")" "scratch/perf-one-largest-compactstr/aats/aozora-rs-adapter/$(basename "$largest")"

AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0 /run/current-system/sw/bin/time -v \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/perf-one-largest-compactstr/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/perf-one-largest-compactstr/out/analyses.jsonl.zst \
    --comparisons-output scratch/perf-one-largest-compactstr/out/comparisons.jsonl.zst \
    --examples-output scratch/perf-one-largest-compactstr/out/examples.jsonl.zst \
    --nway-output scratch/perf-one-largest-compactstr/out/nway.jsonl.zst \
    --errors-output scratch/perf-one-largest-compactstr/out/errors.jsonl.zst \
    --manifest-output scratch/perf-one-largest-compactstr/out/manifest.json \
    --jobs 1 \
    --progress-interval-seconds 15
```

Result:

- Input: `001529_50685-dd3b2fe4e5bf.json`, `41956998` bytes.
- Exit status: `0`.
- Wall time: `6:13.76`.
- Peak RSS: `16478584 KB` (~15.7 GiB).
- Previous single-file peak RSS: `18804424 KB` (~17.9 GiB).
- Reduction: `2325840 KB` (~2.2 GiB), about `12.4%`.
- Rows: `3` analyses, `3` comparisons, `30` examples, `1` N-way row, `0` errors.

Conclusion: compact feature strings are a real but partial improvement. They reduce retained analysis payload by about 2.2 GiB on the largest single file, but the remaining ~15.7 GiB peak still confirms a larger per-document memory bug. The next target should be changing the analysis representation or reduction strategy so compact runs do not retain full morpheme feature maps for all analyzers at once.

## Root-cause memory fix: skip N-way feature grouping in pairwise alignment

Investigation after compact feature strings showed that the largest single-file run still peaked near 16 GiB. Reducing Sudachi chunk size and streaming analyzer token construction did not remove the spike; the spike occurred after analyzer output was available, during comparison.

Root cause:

- Pairwise alignment delegated to the generic N-way shared-region builder.
- That builder eagerly computed full N-way feature groups for every aligned region.
- Pairwise projection never used those feature groups; it only needed spans, indices, surfaces, and coverage flags.
- On a large Aozora work with roughly 1.34M morphemes across three analyzers, this caused allocator-heavy temporary `BTreeMap`/group construction for every pairwise region.

Fix:

- Pairwise alignment now calls `shared_regions_without_features_with_source_len`, which uses the same N-way span alignment but skips feature-group construction.
- Pairwise feature comparison now merges sorted flat feature maps directly instead of allocating a per-morpheme `BTreeSet` of keys.
- Sudachi analysis no longer materializes an intermediate `Vec<RawToken>` for each chunk, and reuses one tokenizer/list per document.
- Sudachi chunking prefers sentence/paragraph boundaries, keeps adjacent sentence punctuation together (`！？`, `！！！`), does not split decimal points (`5.4`, `５．４`), and only logs an aggregate warning if a large delimiterless span requires a hard split.

Single-largest-file rerun:

```bash
rm -rf scratch/perf-one-largest-no-pair-feature-groups
mkdir -p scratch/perf-one-largest-no-pair-feature-groups/aats/aozora-rs-adapter scratch/perf-one-largest-no-pair-feature-groups/out
largest=$(find scratch/morph-full-corpus/aats -type f -name '*.json' -printf '%s %p\n' | sort -nr | head -1 | awk '{print $2}')
ln -s "$(realpath "$largest")" "scratch/perf-one-largest-no-pair-feature-groups/aats/aozora-rs-adapter/$(basename "$largest")"

AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  taskset -c 0 /run/current-system/sw/bin/time -v \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/perf-one-largest-no-pair-feature-groups/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/perf-one-largest-no-pair-feature-groups/out/analyses.jsonl.zst \
    --comparisons-output scratch/perf-one-largest-no-pair-feature-groups/out/comparisons.jsonl.zst \
    --examples-output scratch/perf-one-largest-no-pair-feature-groups/out/examples.jsonl.zst \
    --nway-output scratch/perf-one-largest-no-pair-feature-groups/out/nway.jsonl.zst \
    --errors-output scratch/perf-one-largest-no-pair-feature-groups/out/errors.jsonl.zst \
    --manifest-output scratch/perf-one-largest-no-pair-feature-groups/out/manifest.json \
    --jobs 1 \
    --progress-interval-seconds 15
```

Result:

- Input: `001529_50685-dd3b2fe4e5bf.json`, `41956998` bytes.
- Exit status: `0`.
- Wall time: `7:28.96`.
- Peak RSS: `3168948 KB` (~3.0 GiB).
- Previous compact-string peak RSS: `16478584 KB` (~15.7 GiB).
- Original single-file peak RSS: `18804424 KB` (~17.9 GiB).
- Reduction versus compact-string baseline: about `80.8%`.
- Reduction versus original single-file baseline: about `83.1%`.
- Rows: `3` analyses, `3` comparisons, `30` examples, `1` N-way row, `0` errors.
- No Sudachi hard-split warnings were emitted for this input with the restored large fallback threshold.

Conclusion: the dominant per-document memory bug was unnecessary N-way feature grouping in the pairwise alignment path, not retained source text or Sudachi dictionary sharing. The process now stays near 3 GiB on the prior worst single-file case.

## String interning validation: largest-file analyzer output

Purpose: validate whether a string interner or symbol table is worth prototyping after the pairwise memory fix reduced peak RSS to roughly 3 GiB. This run measures duplication in analyzer output strings only: analyzer IDs, morpheme surfaces, feature keys, and feature values.

Command shape:

```bash
rm -rf scratch/perf-string-stats-largest
mkdir -p scratch/perf-string-stats-largest/aats/aozora-rs-adapter scratch/perf-string-stats-largest/out
largest=$(find scratch/morph-full-corpus/aats -type f -name '*.json' -printf '%s %p\n' | sort -nr | awk 'NR==1 {print $2}')
ln -s "$(realpath "$largest")" "scratch/perf-string-stats-largest/aats/aozora-rs-adapter/$(basename "$largest")"
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
time cargo run --release -p ab-morph-run -- \
    analyze-aat \
    --aat-dir scratch/perf-string-stats-largest/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --jobs 1 \
    --analyses-output scratch/perf-string-stats-largest/out/analyses.jsonl.zst \
    --errors-output scratch/perf-string-stats-largest/out/errors.jsonl.zst \
    --string-stats-output scratch/perf-string-stats-largest/out/string-stats.json
```

Warmed run:

- Wall time: `0:41.91`.
- User CPU: `31.56s`.
- System CPU: `10.43s`.
- Peak RSS: `2,866,304 KB`.
- No-stats warmed reference for the same analysis-only shape: `0:44.61`, peak RSS `2,866,576 KB`.

Report summary from `scratch/perf-string-stats-largest/out/string-stats.json`:

| category | occurrences | unique values | duplicate occurrence % | total bytes | unique bytes | duplicate byte % |
|---|---:|---:|---:|---:|---:|---:|
| analyzer IDs | 3 | 3 | 0.00% | 43 | 43 | 0.00% |
| surfaces | 1,337,885 | 20,643 | 98.46% | 5,606,886 | 131,556 | 97.65% |
| feature keys | 21,025,163 | 37 | 100.00% | 140,657,466 | 252 | 100.00% |
| feature values | 10,591,649 | 71,710 | 99.32% | 69,665,821 | 720,678 | 98.97% |

Interpretation:

- Feature keys are the clearest interning/symbol candidate: only `37` distinct strings over `21M` occurrences.
- Feature values also have strong duplication: `71,710` distinct values over `10.6M` occurrences.
- Surface strings are highly duplicated by occurrence, but the total raw byte payload is small relative to the feature payload and surfaces are more user-visible, so they are a lower-priority interning target.
- The then-current `CompactString` representation already removed many heap allocations for short keys/values, so the next prototype should focus on symbol IDs for feature keys and values, not a trie and not analyzer IDs.
- Because memory is now acceptable and overall speed is the primary concern, the next step should be a measured symbol-ID prototype behind the morph model boundary. Success criteria: no JSON schema change, lower or neutral wall time on `compare_pair` and largest-file analysis/comparison runs, and no RSS regression.

## Interned feature strings prototype

Change: `ab-morph-diff::FeatureKey` and `FeatureValue` now use an interned string wrapper backed by `Arc<str>`. Serialization remains unchanged: feature keys and values still serialize as JSON strings. The interner is thread-local to avoid cross-worker lock contention; this is a prototype for symbol-like storage, not a trie.

Regression coverage:

- Repeated `FeatureKey` and `FeatureValue` construction shares storage within the thread.
- Serialized keys and values remain plain JSON strings.

Largest-file analysis-only warmed comparison:

| representation | wall time | peak RSS |
|---|---:|---:|
| compact strings, no string stats | `0:44.61` | `2,866,576 KB` |
| interned feature strings, no string stats | `0:42.22` | `2,475,740 KB` |

Largest-file full compact comparison shape with pairwise summaries, examples, and N-way output:

| representation | wall time | peak RSS |
|---|---:|---:|
| no pairwise N-way feature groups, compact strings | `7:28.96` | `3,168,948 KB` |
| interned feature strings | `5:02.08` | `2,655,968 KB` |

Interpretation:

- The interner is useful on both target dimensions: speed and memory.
- The win is large enough to keep the prototype rather than back it out.
- The remaining risk is corpus-long interner growth, because thread-local interners retain unique feature strings for the worker thread lifetime. This should be checked on an 8-file and then full-corpus run before considering deeper symbol-table work.
- A trie is still not indicated: exact string interning captures the measured duplication without introducing tokenizer-adjacent dictionary complexity.

## Interned feature strings: 8-largest corpus-shaped run

Purpose: validate that thread-local feature string interning does not regress memory when a worker processes multiple large AAT files, and compare against the prior 8-largest compact baseline.

Command shape:

```bash
rm -rf scratch/perf-interned-8-largest
mkdir -p scratch/perf-interned-8-largest/aats/aozora-rs-adapter scratch/perf-interned-8-largest/out
find scratch/morph-full-corpus/aats -type f -name '*.json' -printf '%s %p\n' \
  | sort -nr \
  | awk 'NR<=8 {print $2}' \
  | while read -r input; do
      ln -s "$(realpath "$input")" "scratch/perf-interned-8-largest/aats/aozora-rs-adapter/$(basename "$input")"
    done
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
time cargo run --release -p ab-morph-run -- \
    analyze-aat \
    --aat-dir scratch/perf-interned-8-largest/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --jobs 8 \
    --analyses-output scratch/perf-interned-8-largest/out/analyses.jsonl.zst \
    --comparisons-output scratch/perf-interned-8-largest/out/comparisons.jsonl.zst \
    --examples-output scratch/perf-interned-8-largest/out/examples.jsonl.zst \
    --nway-output scratch/perf-interned-8-largest/out/nway.jsonl.zst \
    --errors-output scratch/perf-interned-8-largest/out/errors.jsonl.zst \
    --manifest-output scratch/perf-interned-8-largest/out/manifest.json
```

Inputs:

- `000077_1323-b51132c1dd72.json`
- `000091_522-f7514a80c216.json`
- `000118_1745-a30a16b68711.json`
- `000148_56923-8b813e03a8b8.json`
- `001034_4823-ee91d5f3886b.json`
- `001529_50685-dd3b2fe4e5bf.json`
- `001562_56145-c9fe64a731a3.json`
- `001562_56146-66c41ed10b8f.json`

Result:

- Wall time: `6:14.96`.
- User CPU: `1068.57s`.
- System CPU: `61.97s`.
- Peak RSS: `7,927,232 KB`.
- Errors: `0` rows.
- Analyses: `24` rows.
- Pairwise compact comparisons: `24` rows.
- Examples: `240` rows.
- N-way rows: `8` rows.

Comparison:

| run | wall time | peak RSS |
|---|---:|---:|
| prior 8-largest compact baseline before root/interner fixes | `11:07.84` | `51,931,200 KB` |
| interned feature strings + pairwise feature-group fix | `6:14.96` | `7,927,232 KB` |

Interpretation:

- The thread-local interner did not create runaway memory growth on the 8-largest multi-file shape.
- Peak RSS dropped by roughly `84.7%` versus the prior 8-largest compact baseline.
- Wall time also improved materially, though this workstation was not isolated for speed benchmarking.
- The next validation target is a whole-corpus compact run. At this point the memory profile is stable enough to try it without expecting the previous 70+ GiB/OOM failure mode.

## Interned feature strings: full corpus compact run

Purpose: validate the new memory profile on the complete checked AAT corpus, with compact pairwise summaries, bounded examples, and N-way rows enabled.

Command shape:

```bash
rm -rf scratch/perf-interned-full-corpus
mkdir -p scratch/perf-interned-full-corpus/out
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
time cargo run --release -p ab-morph-run -- \
    analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --jobs 8 \
    --analyses-output scratch/perf-interned-full-corpus/out/analyses.jsonl.zst \
    --comparisons-output scratch/perf-interned-full-corpus/out/comparisons.jsonl.zst \
    --examples-output scratch/perf-interned-full-corpus/out/examples.jsonl.zst \
    --nway-output scratch/perf-interned-full-corpus/out/nway.jsonl.zst \
    --errors-output scratch/perf-interned-full-corpus/out/errors.jsonl.zst \
    --manifest-output scratch/perf-interned-full-corpus/out/manifest.json
```

Result:

- Input files: `17,894`.
- Wall time: `58:56.59`.
- User CPU: `24,928.41s`.
- System CPU: `768.01s`.
- CPU utilization: `726%`.
- Peak RSS: `13,032,272 KB`.
- Errors: `0` rows.
- Analyses: `53,682` rows.
- Pairwise compact comparisons: `53,682` rows.
- Examples: `536,718` rows.
- N-way rows: `17,894` rows.

Artifact sizes:

| artifact | compressed size |
|---|---:|
| analyses | `812K` |
| pairwise comparisons | `4.2M` |
| examples | `15M` |
| nway | `26M` |
| errors | `4.0K` |
| manifest | `4.0K` |

Interpretation:

- The previous whole-corpus OOM failure mode is resolved for compact artifacts.
- The complete corpus now fits in about `13.0 GB` peak RSS with `--jobs 8`.
- Storage is small enough for comprehensive comparisons to be routine: all compact artifacts together are under `50 MB` compressed.
- There were no analyzer, projection, pairwise comparison, or N-way comparison errors on the checked corpus.
- The next practical step is to treat `scratch/perf-interned-full-corpus/out` as the current complete comparison artifact set and use the summary commands for top pairwise, feature, and N-way difference reports.
