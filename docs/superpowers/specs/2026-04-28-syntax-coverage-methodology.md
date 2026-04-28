# Aozora Syntax Coverage — Methodology

This document records how `data/aozora-syntax-coverage.toml` is built and
maintained. The matrix is the durable artefact; this doc explains the
auditing process so a future contributor can reproduce or update it.

Companion plan: `docs/superpowers/plans/2026-04-28-syntax-coverage-report.md`.
JSON Schema: `data/aozora-syntax-coverage.schema.json`.

## Taxonomy seeding

The 45 rows in the matrix were seeded from three sources, in order:

1. **`references/parsers/aozora2html/lib/aozora2html/tag/*.rb`** — 37
   Ruby tag classes drove the operational enumeration. Each class maps
   to one or more matrix rows (sometimes collapsed: `multiline_jisage`,
   `jisage`, and `oneline_jisage` fold into one indentation row;
   sometimes split: `gaiji.rb` covers both `unicode_codepoint` and
   `jis_code` sub-cases, which are separate matrix rows because they
   differ in lossiness).
2. **`references/parsers/aozora2html/lib/aozora2html.rb` chuuki tables**
   — `KAERITEN_TABLE`, `KUNTEN_KAESI`, `ACCENT_TABLE`, etc. supply
   features that have no dedicated tag class. These produced the
   `kunten.kaeriten`, `kunten.okurigana`, and `accent.diacritic` rows.
3. **`references/aozorabunko/rules/{kijyunn,chuuki_ichiran}.html`** —
   spec items neither parser handles. These rows record
   `recognition = "unrecognised"` for all three parsers — itself a
   deliberate finding (the spec is broader than any implementation).

Cross-references between rows and seed sources are recorded inline in
each row's `evidence` field.

## Performance calibration (pilot, 2026-04-28)

Pilot scope: 100 ruby-bearing works (`scratch/coverage-pilot-ids.json`,
selected from `scratch/ab-index.json` with
`jq '[.works[] | select(.features | index("ruby")) | .id][0:100]'`).
Hardware: 16-core local workstation (`--jobs 16`).

| run        | wall-clock | per-(parser×work) |
| ---        | ---        | ---               |
| cold cache | 11.0 s     | ~37 ms            |
| warm cache | 0.3 s      | ~1 ms             |

Both runs cover three parsers (aozora2, aozora-rs, aozora2html). Logs:
`scratch/coverage-pilot/{cold,warm}.log`. Summaries:
`scratch/coverage-pilot/{cold,warm}-summary.json`.

### Full-corpus projection

Linear scaling from the pilot: 11 s × (17 887 / 100) ≈ 33 min cold pass at
`--jobs 16`. The 90-minute aozora2html budget set in the plan therefore
holds with substantial headroom, so the chosen strategy is:

- `corpus_prevalence.coverage_basis = "full_corpus"` for every row.
- No stratified-sample fallback.

The warm-rerun budget of "well under a minute" is met (0.3 s on the pilot;
expected full-corpus warm rerun under 30 s).

### Full-corpus run (2026-04-28)

| run        | works  | wall-clock | notes                                       |
| ---        | ---    | ---        | ---                                         |
| cold cache | 17 613 | 1 674 s    | 28 min; 281 unreadable zips skipped         |
| warm cache | 17 613 | 9 s        | well under the 60 s budget                  |

281 corpus zips are unreadable (`No CDFH found` / `EOCD missing`); two
aozora2html files (`JISTABLE`, `jinmeiyou_kyoyou_list`) crash the
adapter with exit 1. Both classes of failure are recorded in
`scratch/ab-coverage-full/cold.log` and excluded from the merged
matrix counts. Cross-check vs `ab-index` ruby-tagged works: matrix
14 190 vs index 14 320 (0.9 % delta, under the 1 % bug threshold;
explained by the unreadable-zip skip).

## Cache layout

```
target/parser-cache/<parser_id>/<adapter_sha>/<input_sha>.json
```

- `parser_id` — `aozora2` | `aozora-rs` | `aozora2html`.
- `adapter_sha` — sha256-of-(sha256(rel_path) || sha256(content))
  pairs over the parser's adapter source tree plus the upstream parser
  source tree. Implementation: `crates/ab-coverage/src/cache.rs`
  (`AdapterFingerprintInputs::for_parser`,
  `compute_adapter_sha`). Globs include `adapters/<id>/{Cargo.toml,
  Cargo.lock, src/**/*.rs}` and the upstream parser equivalents under
  `references/parsers/<id>/`. Build artefacts (`target/`,
  `__pycache__/`, `node_modules/`, `.git/`) are excluded.
- `input_sha` — sha256 of the raw work source bytes as ingested.

A code change to either the adapter or the upstream parser produces a
new `adapter_sha` directory, so stale cache hits are impossible by
construction. Old directories linger until manual cleanup; the runner
will eventually print pruning instructions.

## Detector model

Each matrix row carries `aat_nodes` (kinds of AAT inline/block nodes the
row maps to) and `source_patterns` (raw-source regexes). The detector
registry (`crates/ab-coverage/src/detectors.rs`) builds one detector per
row:

- For `aat_nodes`: count every AAT node whose `kind` is in the list.
  Generic structural kinds (`text`, `paragraph`) are dropped — they
  would match the backbone of every work and dwarf real signal.
- For `source_patterns`: count regex matches against the decoded
  source. UTF-8 input passes through; Shift-JIS input is decoded via
  `encoding_rs` first (matches the ingestion path).

A row's detector totals are the sum across both rule kinds; a row with
neither AAT-kind nor source-pattern declarations contributes zero.
Detector tuning per row (e.g., the current `annotation.bouki` /
`annotation.chuuki` / `kunten.okurigana` rows share an `aat_nodes` set
and therefore report identical counts) is a follow-up.

### Sample-works selection

Per-row counter keeps a top-5 heap of `(occurrence_count, work_id)`
tuples. Sort key: `count` descending; ties broken by lexicographic
`work_id` ascending. Implementation: `RowCounter::observe` in
`crates/ab-coverage/src/prevalence.rs`.

## Recognition + fidelity classification

Recognition is read off the parser source; fidelity is read off the
adapter source plus a fixture-driven AAT inspection. Per-parser passes
landed as separate commits to avoid three-way contention on the same
TOML rows:

| parser      | classifier source                                                                       |
| ---         | ---                                                                                     |
| aozora2     | `references/parsers/aozora2/crates/aozora-core/src/parser/`, `node/`                    |
| aozora-rs   | `references/parsers/aozora-rs/aozora-rs/aozora-rs-core/src/{tokenizer,scopenizer,retokenizer}/` |
| aozora2html | `references/parsers/aozora2html/lib/aozora2html/tag/*.rb`, `lib/aozora2html.rb`         |

| adapter     | classifier source                                                                       |
| ---         | ---                                                                                     |
| aozora2     | `adapters/aozora2/src/lib.rs`                                                            |
| aozora-rs   | `adapters/aozora-rs/src/{aat,projection,source}.rs`                                     |
| aozora2html | `adapters/aozora2html/adapter.py`                                                       |

Findings JSON is in `scratch/findings-<id>.json` per pass; the merger
(`crates/ab-coverage/src/bin/merge.rs`) applies findings to the matrix
while preserving comments and ordering via `toml_edit`.

## Forbidden combinations

Encoded in `crates/ab-coverage/src/schema.rs`, asserted by
`forbidden_combinations_rejected` in
`crates/ab-coverage/tests/schema_matrix.rs`:

- `recognition = "aborts"` ⇒ only `aat_fidelity = "not_applicable"`.
  If the parser doesn't return, there is nothing to adapt.
- `recognition = "parsed"` ⇒ never `aat_fidelity = "synthesised"`.
  Synthesis means the adapter recovered a node the parser didn't emit;
  if the parser did emit it, the right value is `preserved` / `lossy`
  / `dropped`.
- `recognition = "unrecognised"` ⇒ only `aat_fidelity = "synthesised"`
  or `"not_applicable"`. There is nothing to drop or lose if the
  parser never recognised the syntax.

## Cross-parser findings

The single biggest split is between aozora-rs and the other two: a
large class of features that aozora2html and aozora2 both `parsed` are
`unrecognised` by aozora-rs because its grammar is built around the
`は` / `に` backref forms, and the corpus uses the `の` form. Examples
(works-with-feature in parentheses):

- `kunten.okurigana` (14 105), `annotation.bouki` (14 106),
  `annotation.chuuki` (14 105), `ruby.placement_directional`
  (14 106): aozora2/aozora2html `parsed`; aozora-rs `unrecognised`.
- `gaiji.*` (5 500–6 700 works each): aozora2 `parsed`, aozora2html
  `normalised` (when `--use-unicode` resolves the marker), aozora-rs
  `unrecognised` (no gaiji handler — adapter `synthesises` from raw
  source events instead).
- `decoration.keigakomi`, `decoration.direction_override`,
  `indentation.jisage_oneline`, `indentation.jizume`,
  `layout.yokogumi`, `layout.tcy`, `warigaki.parenthetical`,
  `kunten.kaeriten` (12 354 works each): same pattern — aozora2/
  aozora2html `parsed`; aozora-rs `unrecognised`.
- `ruby.double` (14 105): aozora2 / aozora-rs `normalised` — both
  flatten the two-line ruby into separate single-ruby decorations;
  only aozora2html keeps the double structure.
- `iteration.kunoji` (7 443) is `unrecognised` everywhere except
  aozora2html: a corpus-wide gap in the Rust parsers for the kunoji
  vertical iteration mark.

For a downstream consumer choosing a parser by coverage breadth on
this corpus, aozora2html is the broadest, aozora2 a close second, and
aozora-rs a deliberate minimum-viable subset that leans on the
adapter for synthesis.

## Deferred work

- Reconciliation between `data/feature-patterns.toml` (regex-driven
  `ab-index` flags) and the matrix. The matrix is currently the
  authoritative source for parser/adapter coverage; ab-index keeps
  using `feature-patterns.toml` for feature flags. A follow-up plan
  will fold the two.
- Per-row detector functions (`detector_id` referencing a small
  hand-written fn) where the generic AAT-kind detector over-matches.
  The rows currently sharing identical `aat_nodes` sets are the first
  candidates.
- Adding `AozoraEpub3-JDK21`, `aozora-parser.js`, and
  `aozorabunko-extractor` as additional parser/adapter columns. The
  open-keyed schema and the per-parser cache layout already accept
  them; it is a pure append per row.
