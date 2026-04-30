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
