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

Compact runs also clear per-analysis `source_text` clones after writing compact analysis summaries. Comparisons then use the document text as shared source context through `compare_pair_with_source_text`, preserving validation while avoiding duplicate retained source strings inside the stored analysis vector.

## Throughput check

A complete full-corpus compact run after the shared-analyzer fix completed in 1,045 seconds with `--jobs 8` and produced the same row counts and artifact sizes as the previous compact full-corpus run.

After recursive AAT discovery, progress telemetry, and compact source-text trimming, the canonical full-corpus compact run completed in 1,037 seconds with `--jobs 8 --progress`. It reported `inputs=17894`, `rss_kb=1553272`, and `pss_kb=1551273` at process end. No wall-time regression was observed in this environment.

## Remaining notes

- This reduces duplicate dictionary residency, but one Sudachi dictionary and one Vibrato dictionary are still large and expected to remain resident.
- Peak memory can still exceed the 60-second sample during unusually large documents because each comparison temporarily holds both analyses and comparison regions for the current input.
- If memory needs to be reduced further, the next target is streaming compact comparison summaries/examples without materializing every region for very large documents.

## Streaming compact comparison update

Compact output now uses `compare_pair_compact_with_source_text`, which accumulates `ComparisonStats` and bounded examples without storing a full `Vec<Region>` or `Vec<FeatureDiff>` for each comparison. Full output still uses `compare_pair` and remains behavior-compatible.

The expected memory impact is limited to per-document peaks for unusually large works. Typical full-corpus RSS/PSS is expected to look similar to the previous shared-analyzer run because dictionary residency and retained morpheme vectors dominate ordinary corpus runs.

Verification on 2026-04-29:

```bash
cargo fmt --all -- --check
cargo test -p ab-morph-diff
cargo test -p ab-morph-run
cargo build --release -p ab-morph-run
```

All four commands completed successfully. The optional cleaned-workspace smoke command did not produce fresh artifacts because the runtime inputs were unavailable in `scratch/`; `ab-morph-run` returned `No such file or directory` before writing output.

## Regenerated full-corpus compact run after streaming comparison

AAT inputs were regenerated from `scratch/ab-index.json` with `aozora-rs-adapter`:

```bash
target/release/ab-check \
  --index scratch/ab-index.json \
  --corpus references/aozorabunko \
  --adapter adapters/aozora-rs/target/release/aozora-rs-adapter \
  --output scratch/morph-full-corpus/reports \
  --aat-output scratch/morph-full-corpus/aats \
  --jobs 16
```

AAT generation produced 17,894 AAT JSON files and 17,894 reports in 14m48s.

The full compact morph command was rerun with `--jobs 8 --progress` and zstd outputs. It completed successfully:

| metric | value |
| --- | ---: |
| inputs | 17,894 |
| wall time | 1,015 seconds |
| error rows | 0 |
| final RSS | 764,664 kB |
| final PSS | 762,664 kB |
| sampled peak RSS | 17,161,224 kB |
| sampled peak PSS | 17,159,286 kB |
| sampled peak private dirty | 15,944,968 kB |

The high peak was short-lived: 7 of 200 samples were above 16 GB RSS, and 29 of 200 samples were above 12 GB RSS. End-of-run memory stayed low, so the remaining problem is large-document concurrent peak memory, not steady-state dictionary residency.

Largest regenerated AAT/plaintext inputs included:

| AAT file | AAT bytes | projected chars |
| --- | ---: | ---: |
| `001529_50685-dd3b2fe4e5bf.json` | 41,956,998 | 632,346 |
| `000118_1745-a30a16b68711.json` | 30,413,476 | 236,577 |
| `001562_56145-c9fe64a731a3.json` | 19,711,121 | 536,295 |
| `001562_56146-66c41ed10b8f.json` | 14,120,757 | 720,828 |
| `001562_33224-e48e57f82f86.json` | 9,739,544 | 592,649 |

Interpretation: streaming compact comparisons removed retained `Comparison` region/feature payloads, but peak memory is still dominated by concurrently analyzing very large works across multiple worker lanes. The next optimization should be size-aware scheduling for compact parallel runs: isolate oversized AAT inputs so only one huge document is processed at a time while normal-sized inputs continue across the remaining workers.
