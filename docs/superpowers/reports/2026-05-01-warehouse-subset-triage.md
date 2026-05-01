# Warehouse subset triage, 2026-05-01

## Workload

Input: first 12 checked AAT JSON files from `scratch/morph-full-corpus/aats/aozora-rs-adapter`, staged as symlinks under `scratch/morph-warehouse-subset-2026-05-01/aats`.

Analyzers: `vibrato`, `sudachi-a`, `sudachi-c`.

Run command:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  nix shell nixpkgs#time -c time -v \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-warehouse-subset-2026-05-01/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --warehouse-dir scratch/morph-warehouse-subset-2026-05-01/warehouse \
    --run-id subset-12-timed-2026-05-01 \
    --jobs 1
```

## Runtime and memory

- Wall time: 54.72s.
- User time: 49.20s.
- System time: 5.28s.
- Max RSS: 6,191,480 KB, about 5.9 GiB.
- Errors: 0.

Interpretation: warehouse output is not the memory bottleneck at this scale. RSS is still dominated by analyzer dictionaries and tokenizer state. Parallel warehouse mode should not be added naively, because multiple independent analyzer sets would multiply this resident memory.

## Storage

Total sealed run size: 36M.

Largest tables:

| Table | Size |
| --- | ---: |
| `nway_feature_diffs.parquet` | 14M |
| `morpheme_features.parquet` | 8.1M |
| `morphemes.parquet` | 7.2M |
| `nway_region_analyzers.parquet` | 4.0M |
| `nway_regions.parquet` | 2.6M |

Interpretation: storage shape is acceptable for comprehensive runs. Feature-diff facts are the largest table, but they are exactly the high-value analytical surface.

## Top N-way source rows

`000006_1869-62320f0f4474` dominates this subset:

- Regions: 78,068.
- Feature-disagreement regions: 77,879.
- Segmentation-disagreement regions: 3,425.
- Coverage mismatch regions: 0.
- Variable boundary count: 4,512.

The next largest rows are `000006_3310-49cd8c085df4` and `000005_53194-ebb0cbaf64b3`.

## Top lexical pairwise segmentation rows

The worst pairwise lexical segmentation rows are:

| Source | Pair | Regions | Segmentation | Feature | Variable boundaries |
| --- | --- | ---: | ---: | ---: | ---: |
| `000006_1869-62320f0f4474` | `sudachi-a` vs `vibrato:unidic-cwj-202512` | 75,722 | 3,079 | 72,609 | 4,087 |
| `000006_1869-62320f0f4474` | `sudachi-c` vs `vibrato:unidic-cwj-202512` | 75,722 | 2,973 | 72,746 | 4,031 |
| `000006_3310-49cd8c085df4` | `sudachi-c` vs `vibrato:unidic-cwj-202512` | 56,190 | 1,179 | 55,033 | 1,377 |
| `000006_3310-49cd8c085df4` | `sudachi-a` vs `vibrato:unidic-cwj-202512` | 56,190 | 1,094 | 55,091 | 1,253 |
| `000005_53194-ebb0cbaf64b3` | `sudachi-a` vs `sudachi-c` | 46,336 | 1,010 | 45,110 | 1,042 |

This confirms that the warehouse pairwise report is useful as the replacement for `summarize-compact` first-pass triage.

## Top lexical segmentation patterns

Top recurring segmentation patterns:

1. `sudachi-a+sudachi-c:[お|勢] ; vibrato:[お勢]` — 265 examples.
2. `vibrato:[文|三] ; sudachi-a+sudachi-c:[文三]` — 149 examples.
3. `vibrato:[々|々] ; sudachi-a+sudachi-c:[々々]` — 60 examples across 6 sources.
4. `vibrato:[お|政] ; sudachi-a+sudachi-c:[お政]` — 55 examples.
5. `sudachi-a+vibrato:[何|と|なく] ; sudachi-c:[何となく]` — 54 examples across 5 sources.

This is the right level of abstraction for recurring segmentation differences. Concrete regions are useful only after selecting one of these patterns or a source row.

## Top lexical `pos1` feature patterns

Top `pos1` patterns are dominated by `sudachi-c` reporting `空白` while `sudachi-a` and Vibrato report ordinary lexical categories:

1. `助詞=>sudachi-a+vibrato ; 空白=>sudachi-c` — 57,787 examples.
2. `名詞=>sudachi-a+vibrato ; 空白=>sudachi-c` — 35,488 examples.
3. `空白=>sudachi-c ; 補助記号=>sudachi-a+vibrato` — 27,114 examples.
4. `動詞=>sudachi-a+vibrato ; 空白=>sudachi-c` — 23,409 examples.
5. `助動詞=>sudachi-a+vibrato ; 空白=>sudachi-c` — 17,636 examples.

This likely points to a Sudachi-C feature extraction or feature-alignment issue, not a real linguistic difference. It should be investigated before interpreting feature-pattern frequencies as analyzer-quality evidence.

## Error report

`errors.parquet` is empty for this subset. `summarize-warehouse-errors` prints only the header.

## Reporting UX conclusion

Warehouse reporting now covers the useful first-pass triage surfaces:

- Source-level N-way ranking: `summarize-warehouse-nway`.
- Analyzer-pair ranking: `summarize-warehouse-pairwise --filter lexical-only`.
- Exact recurring segmentation and feature patterns: `summarize-warehouse-patterns --filter lexical-only`.
- Concrete drill-down evidence: `summarize-warehouse-regions --filter lexical-only`.
- Failure aggregation: `summarize-warehouse-errors`.

The raw region table is intentionally verbose and should be treated as drill-down, not a top-level report.

## Follow-up

The initial subset exposed a real Sudachi adapter bug: `sudachi-c` appeared as `pos1=空白` for many lexical regions. This was traced to using `InfoSubset::empty()` while reading POS/form fields. See `docs/superpowers/reports/2026-05-01-sudachi-feature-subset-fix.md` for the fix and corrected subset results.

Remaining next steps:

1. Keep warehouse mode serial until analyzer dictionary sharing or a bounded-memory worker model exists; naive parallel mode would multiply the 5.9 GiB RSS.
2. Move workflow docs toward warehouse-first triage and mark JSONL compact reporting as legacy/debug.
3. Consider a focused drill-down command that takes a pattern row and returns bounded concrete regions, rather than printing full feature payloads for arbitrary regions.
