# Tokenizer comparison follow-ups: qkana reclaim, work-level rollup, 字下げ leakage (2026-07-11)

Closes the three follow-ups from
`docs/handoffs/2026-07-11-tokenizer-full-corpus-comparison.md`, with
re-verification runs on hinoki (branch
`feat/tokenizer-comparison-followups`).

## 1. qkana surface-mismatch works reclaimed (all 326)

Root cause was **not** dictionary surface normalization. Vibrato's
emitted surfaces are raw slices of the input; the failures came from our
own sequential re-matcher (`find_sequential_span`), whose whitespace-skip
heuristic consumes source whitespace before matching — so any token whose
surface *begins* with whitespace and contains non-whitespace (e.g.
`　　～～…`, `　　　　――――` dash/space runs) could never match. All 326
errors were this one class.

Fix (`ab-morph-analyzers`): the vibrato wrapper now emits
`byte_span` from the tokenizer's own `range_byte()` (shifted by the
chunk offset) and no `emitted_surface`, exactly like the Sudachi and
Vaporetto wrappers. Spans go through `validate_reported_span`; the
sequential matcher remains only for engines that report no offsets. For
works that previously tokenized, morphemes are identical (verified: the
novel and kindai-bungo full-run aggregates are byte-identical to
2026-07-10, see §4).

On the abc side, join-stats now consumes the `char_start`/`char_end`
that `tokenize-plaintext` writes instead of re-deriving spans by walking
surfaces over the text (`reconstruct-token-spans` deleted — the Clojure
re-walk would have re-skipped the reclaimed works, and span
reconstruction belongs in Rust). abc validates only the annotation-join
precondition (ascending, non-overlapping, in-bounds) and skips works
fail-closed on invalid rows.

## 2. Work-level aggregation over multi-file works

`stats/work-level.jsonl` (new, one row per bare work id) rolls the
per-file-stem rows up: `file_count`, `file_stems`, summed
`text_scalar_count`/`annotation_counts`/`classifications`.
`aggregate.json` gains `distinct_work_count` next to the file-entry
`work_count`; the workflow step declares the new `work-level` output.
Corpus check: 17,886 file entries → 17,605 distinct works, exactly the
236 multi-file works (max 3 fragments per work) found earlier.

## 3. 字下げ layout-note leakage: fixed in the plaintext projection

Investigation (over the 2026-07-10 warehouse run) confirmed the leak and
located it precisely:

- The aozora adapter *parses* layout notes correctly and emits them as
  AAT `raw` nodes (`x-source-marker-kind`: indent, pageBreak, kaeriten,
  containerOpen/Close, …), but `ab-plaintext`'s `walk_inline` projected
  their `source` verbatim — 144,493 raw projection spans across 6,894
  sources (41% of the corpus). The 22,849 字下げ disagreement regions
  were just the subset where the three dictionaries also split the
  marker text differently. Only 5 of 24,867 字下げ-containing regions
  were legitimate prose.
- The parser-IR path (annotation-join-stats) never had the bug: the
  converter drops raw nodes entirely, so the join-stats plaintexts were
  always clean.

Fix (`ab-plaintext`): `walk_inline` now skips `raw` nodes, mirroring the
converter's sibling projections; `ab-check`'s visible-fragments walker
matches (its source-*order* comparison walker keeps raw on purpose,
since the markup text does exist in the raw source).

**Still open (adapter bug, needs dump regeneration):** inline style
markers survive *inside* AAT text-node values — e.g.
`{"kind":"style","style_type":"bold","content":[{"kind":"text","value":"［＃太字］ピアノ［＃太字終わり］"}]}`
(work 001670_55342 has 4,284 such marker strings). No projection can fix
that; it needs an adapter fix and a repin-dump regeneration.

## 4. Re-verification runs (hinoki, 2026-07-11)

Join-stats: `/db/soranoha/annotation-join-stats/aozora-repin-1a4f864-full-{unidic-novel,qkana,kindai-bungo}-2026-07-11/`
(2026-07-10 baselines kept). All workflows `passed`, **0 tokenize errors
for every profile** — qkana now covers all 17,886 file entries.

- unidic-novel and kindai-bungo: `annotation_counts` and
  `classification_rates` **byte-identical** to 2026-07-10 — the span
  seam change provably does not alter results for previously-successful
  works.
- qkana (now full-corpus, 3,556,636 rubies — same denominator as the
  other profiles for the first time):

| class | unidic-novel | qkana | kindai-bungo |
|---|---|---|---|
| aligned-single | 57.45% | 57.37% | 57.18% |
| aligned-multi | 10.50% | 10.50% | 10.54% |
| stem-prefix | 30.06% | 30.11% | 30.22% |
| conflict | 1.99% | 2.02% | 2.06% |

  Maximum spread is 0.27pp (aligned-single); conflict spans
  1.99–2.06%. The corpus-wide conclusion stands on the full
  denominator: join classification is not a profile discriminator.

Morph-warehouse: run/report
`dict-cmp-profiles-repin-full-2026-07-11` (fresh compute — the run
identity includes the engine binary hash, so the rebuild recomputed
automatically). Versus 2026-07-10:

- error rows 1,160 → 892; **all remaining errors are source-ingest
  classes** (unclosed_bracket 487, unmatched_close 378, …, analyzer_id
  NULL) — zero analyzer errors.
- nway regions 133.1M → 130.4M (~2% of former "disagreements" were
  layout-marker noise); 字下げ absent from every report table.
- Findings unchanged in structure: kindai-bungo remains the token-level
  outlier (kindai↔novel 1.47M, kindai↔qkana 1.36M vs novel↔qkana 0.49M
  pairwise regions), led by lexicalized classical forms (彼の 47,257;
  いまし 25,959; だって/なん/います/いない/かな 15–19k).

## Engine/ops notes

- Correction: the join-stats *step* costs ≈7.3 h single-core per profile
  at stride 1 (the annotation×token join dominates; logs are silent for
  the whole step). The earlier ≈15 min figure was the stride-44 number.
- Warehouse skip-recompute identity = AAT content hash + engine binary
  sha256 + dictionaries + analyzers + profile + schema; force-flag
  exists but is not needed after engine rebuilds.

## Follow-ups

- Adapter: strip markers inside style-node text values (太字/斜体
  class), then regenerate the repin dump — after which the remaining 892
  source-ingest error rows (bracket parity classes) are also worth an
  adapter pass.
- The join-stats step is an O(annotations × tokens) `filterv` per work
  (`abc.tools.annotation-join/overlapping`); an interval-index or sorted
  merge would cut the 7 h step to minutes if re-runs become frequent.
- ADR 0028 evidence set (per-work conflict-delta tails) can now be
  recomputed over the full qkana corpus and at work level via
  `work-level.jsonl`.
