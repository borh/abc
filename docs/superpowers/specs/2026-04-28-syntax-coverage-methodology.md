# Aozora Syntax Coverage — Methodology

This document records how `data/aozora-syntax-coverage.toml` is built and
maintained. The matrix is the durable artefact; this doc explains the
auditing process so a future contributor can reproduce or update it.

Companion plan: `docs/superpowers/plans/2026-04-28-syntax-coverage-report.md`.
JSON Schema: `data/aozora-syntax-coverage.schema.json`.

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
