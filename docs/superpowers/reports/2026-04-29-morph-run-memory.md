# Morph runner memory report

Date: 2026-04-29

## Question

`ab-morph-run analyze-aat --jobs 8` was observed using roughly 21 GB of memory during full-corpus runs. The goal was to reduce memory without reducing throughput.

## Root cause

Before the fix, the parallel runner implemented `--jobs` by creating shard directories and recursively invoking the serial runner once per shard. Each shard runner loaded its own analyzer set.

For the common full-corpus command:

```bash
--jobs 8 --analyzer vibrato --analyzer sudachi-c
```

that meant eight Vibrato analyzer instances and eight Sudachi analyzer instances were resident in one process. The dictionaries dominate memory.

## Measurement method

Two 60-second full-corpus compact runs were started against:

```text
scratch/morph-full-corpus/aats/aozora-rs-adapter
```

Both used:

```bash
--jobs 8 --analyzer vibrato --analyzer sudachi-c --output-profile compact
```

At 60 seconds, `/proc/<pid>/smaps_rollup` was sampled, then the measurement process was terminated.

## Results

| implementation | RSS | PSS | Private dirty | Shared clean |
| --- | ---: | ---: | ---: | ---: |
| per-shard analyzer loading | 15,870,884 kB | 9,641,128 kB | 8,573,292 kB | 7,228,852 kB |
| shared loaded analyzers | 6,390,588 kB | 6,388,652 kB | 5,321,980 kB | 1,956 kB |

Observed reduction at the 60-second sample:

| metric | reduction |
| --- | ---: |
| RSS | 9,480,296 kB |
| PSS | 3,252,476 kB |
| Private dirty | 3,251,312 kB |

## Fix

`ab-morph-run` now loads analyzers once in `run_analyze_aat`, wraps each loaded analyzer in `Arc`, and passes those shared analyzers into worker threads. The parallel runner still partitions input and writes deterministic shard outputs, but it no longer recursively calls `run_analyze_aat` and no longer reloads dictionaries per shard.

The analyzer APIs already use immutable `&self` analysis. Sudachi creates per-analysis `StatefulTokenizer` values over the shared dictionary. Vibrato creates per-analysis workers from the shared tokenizer.

## Throughput check

A complete full-corpus compact run after the fix completed in 1,045 seconds with `--jobs 8` and produced the same row counts and artifact sizes as the previous compact full-corpus run.

The previous compact run's scratch directory birth time and manifest mtime also span 1,045 seconds, so no wall-time regression was observed in this environment.

## Remaining notes

- This reduces duplicate dictionary residency, but one Sudachi dictionary and one Vibrato dictionary are still large and expected to remain resident.
- Peak memory can still exceed the 60-second sample during unusually large documents because each comparison temporarily holds both analyses and comparison regions for the current input.
- If memory needs to be reduced further, the next target is compact-mode streaming: write compact summaries/examples without retaining full `source_text` clones longer than necessary.
