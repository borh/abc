---
plan_id: "2026-04-28-syntax-coverage-report"
status: not_started
started:
next_update: 2026-05-05
owner: unassigned
target_prerequisites: []
---

# Whole-Corpus Aozora Syntax Coverage Report

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [x]`) syntax for tracking.

**Goal:** Produce a comprehensive coverage report for every Aozora Bunko syntax feature against each parser and each adapter. The current `data/aozora-syntax-coverage.toml` only enumerates 9 priority-1 rows with one global `status` field — that is too coarse to answer "what does each parser actually recognise, and what does each adapter preserve?" The report must distinguish parser-level recognition from adapter-level fidelity, and must be backed by whole-corpus prevalence numbers (not sampling) for the parsers we already have wired.

**Scope of parsers covered now:** `aozora2`, `aozora-rs`, `aozora2html`. The other three sources under `references/parsers/` — `AozoraEpub3-JDK21` (Java), `aozora-parser.js` (JavaScript), and `aozorabunko-extractor` (utility/converter) — have no adapter in this workspace yet and are deliberately out of scope for this plan. The schema and cache layout below are designed so that adding them later is purely additive: a new parser ID becomes a new sub-table key per row and a new sub-directory in the cache; no shape changes to existing rows, no migration of prevalence numbers.

**Architecture:** Three deliverables.

1. **Feature taxonomy** — extend `data/aozora-syntax-coverage.toml` from 9 rows to a complete enumeration. Seed from the Ruby parser's `references/parsers/aozora2html/lib/aozora2html/tag/*.rb` (37 tag classes — the operational enumeration), augment with `references/parsers/aozora2html/lib/aozora2html.rb` chuuki tables (KAERITEN, KUNTEN, ACCENT, etc.) for items that don't have a dedicated tag class, then cross-reference `references/aozorabunko/rules/kijyunn.html` and `chuuki_ichiran.html` for items neither implementation handles (those land as `recognition = "unrecognised"` for all three parsers — itself a finding worth recording).
2. **Per-row schema extension** — each `[[syntax]]` row gains:
   - Open-keyed `parsers.<parser_id>.recognition` ∈ `parsed | normalised | unrecognised | aborts`
   - Open-keyed `adapters.<adapter_id>.aat_fidelity` ∈ `preserved | lossy | dropped | synthesised | not_applicable`
   - `corpus_prevalence.{works_with_feature, total_occurrences, detector_id, sample_works}` populated from a whole-corpus run
3. **Companion methodology doc** at `docs/superpowers/specs/2026-04-28-syntax-coverage-methodology.md` — explains how each judgment was reached (which file/line of which parser was inspected; which detector ran the prevalence count) so the report is auditable.

**Tech stack:** Rust (`crates/ab-coverage`, new workspace crate, owns matrix loader, schema validator, fixture-driven adapter classifier, rayon-driven prevalence pipeline, content-addressed cache), Python 3 with `lxml` for re-walking cached aozora2html XHTML when needed. Existing `ab-index` continues to drive feature-flag indexing; this plan does not migrate it.

---

## Schema additions to `data/aozora-syntax-coverage.toml`

Each existing and new `[[syntax]]` row gains:

```toml
[syntax.parsers.aozora2]
recognition = "parsed"  # parsed | normalised | unrecognised | aborts
evidence = "references/parsers/aozora2/crates/aozora-core/src/parser/ruby_parser.rs:42"
notes = ""

[syntax.parsers.aozora-rs]
recognition = "parsed"
evidence = "references/parsers/aozora-rs/aozora-rs/aozora-rs-core/src/retokenizer/definitions.rs:88"
notes = ""

[syntax.parsers.aozora2html]
recognition = "parsed"
evidence = "references/parsers/aozora2html/lib/aozora2html/tag/ruby.rb:1"
notes = ""

[syntax.adapters.aozora2]
aat_fidelity = "preserved"  # preserved | lossy | dropped | synthesised | not_applicable
evidence = "adapters/aozora2/src/lib.rs:120"
notes = ""

[syntax.adapters.aozora-rs]
aat_fidelity = "preserved"
evidence = "adapters/aozora-rs/src/aat.rs:312"
notes = ""

[syntax.adapters.aozora2html]
aat_fidelity = "preserved"
evidence = "adapters/aozora2html/adapter.py:280"
notes = ""

[syntax.corpus_prevalence]
works_with_feature = 0
total_occurrences = 0
detector_id = "ruby_basic"
coverage_basis = "full_corpus"  # full_corpus | stratified_sample
sample_works = []  # up to 5 work IDs with the highest occurrence counts; ties broken by lexicographic work_id
```

### Definitions

- **parser recognition** (axis 1):
  - `parsed` — parser has explicit grammar/handler that accepts the syntax and emits a structured token/node.
  - `normalised` — parser consumes the syntax but discards or rewrites it into untyped output (e.g., aozora2html `--use-unicode` resolves `※［＃...］` to a plain Unicode character, losing the marker).
  - `unrecognised` — parser passes the source through as raw text; no grammar branch matches.
  - `aborts` — parser raises an error or terminates rather than producing output.

- **adapter AAT fidelity** (axis 2, classifying the AAT mapping layer relative to the parser's output for this row):
  - `preserved` — adapter emits an AAT node that round-trips the parser's structural decision faithfully.
  - `lossy` — adapter emits a node but discards information the parser did capture (e.g., drops a sub-attribute, normalises the kind).
  - `dropped` — adapter produces no AAT node despite the parser recognising the feature.
  - `synthesised` — adapter emits an AAT node for a feature the parser did not emit a structured token for, either by inspecting parser-normalised output or by re-parsing raw source. (Example: an adapter detects that a parser silently rewrote a gaiji marker to a Unicode character and reconstructs the marker by diffing against raw source.)
  - `not_applicable` — used only when the parser `aborts` on this feature; there is no parser output to adapt.

### Forbidden combinations (enforced by the schema validator)

The two axes describe different concerns, but not every combination is sensible. The schema rejects:

- `recognition = "aborts"` paired with any `aat_fidelity` other than `not_applicable`.
- `recognition = "parsed"` paired with `aat_fidelity = "synthesised"` (synthesis means the adapter recovered something the parser didn't emit — if the parser did emit it, the right value is `preserved`/`lossy`/`dropped`).
- `recognition = "unrecognised"` paired with `aat_fidelity = "lossy"` or `"dropped"` — there's nothing to lose if the parser never recognised it; the only valid options are `synthesised` (adapter regex-matches raw source) or `not_applicable`.

These rules are encoded in `crates/ab-coverage/src/schema.rs` and asserted by a unit test that runs every row of the populated matrix through the validator.

### Forward-extensibility

The TOML uses open-keyed sub-tables (`[syntax.parsers.<id>]`, `[syntax.adapters.<id>]`), so adding `AozoraEpub3`, `aozora-parser.js`, or `aozorabunko-extractor` later is a row-by-row append, not a schema migration. The cache layout (Task 5) follows the same pattern: a new top-level directory per parser ID, no changes to existing entries.

---

## Files

- Modify: `data/aozora-syntax-coverage.toml` — extend to full taxonomy; add per-parser/per-adapter sub-tables; populate `corpus_prevalence`.
- Create: `data/aozora-syntax-coverage.schema.json` — JSON Schema for the new TOML shape.
- Create: `docs/superpowers/specs/2026-04-28-syntax-coverage-methodology.md` — companion methodology doc.
- Create: `crates/ab-coverage/` — new workspace crate (matrix loader, schema validator, content-addressed parser-output cache, per-row detectors, rayon corpus pass, TOML merge step).
- Modify: `crates/ab-index/src/syntax_coverage.rs` — `SyntaxRow` currently uses default serde behaviour, so unknown fields are ignored and adding new sub-tables does not break ab-index. The plan still touches this file to (a) confirm the relaxed-deserialize behaviour with a regression test and (b) add a comment pointing readers to `crates/ab-coverage` as the canonical home of the extended schema. ab-index does not need to read recognition/fidelity values; it stays narrow.
- Modify: `Cargo.toml` (workspace) — add `crates/ab-coverage` member.
- Create: `benchmarks/run-coverage.sh` — single-command driver: builds the parsers, runs the coverage tool, writes the populated matrix and a `summary.json`.
- Modify: `benchmarks/README.md` — document the coverage runner.
- **Not modified:** `data/feature-patterns.toml`. It is a parallel (regex-based) classification used by `ab-index` for routing. The prior design doc proposed reconciling it once the matrix can drive routing; that reconciliation is explicitly deferred. During this plan, `feature-patterns.toml` remains the authoritative source for `ab-index` feature flags. The new matrix is the authoritative source for parser/adapter coverage and prevalence. A follow-up plan will reconcile.

No changes to `data/aat-schema.json`. No changes to existing adapter binaries.

---

## Task 0: Confirm Prerequisites

- [ ] **Step 1: Verify the corpus is present and indexable.**

```bash
ls references/aozorabunko/cards | wc -l
find references/aozorabunko/cards -name "*.zip" | wc -l   # ~17 887 zips
find references/aozorabunko/cards -name "*_ruby_*.zip" | wc -l   # ~14 049
```

- [ ] **Step 2: Verify each parser's source is reachable at the documented paths.**

```bash
ls references/parsers/aozora2html/lib/aozora2html/tag/ | wc -l   # 37
ls references/parsers/aozora2/crates/aozora-core/src/parser/     # 7 modules + mod.rs
ls references/parsers/aozora-rs/aozora-rs/aozora-rs-core/src/    # tokenizer, scopenizer, retokenizer dirs
```

- [ ] **Step 3: Verify the cache directory is writable.**

Default cache root: `target/parser-cache/` (gitignored, per-checkout, wiped on `cargo clean`).

```bash
mkdir -p target/parser-cache && touch target/parser-cache/.probe && rm target/parser-cache/.probe
```

- [ ] **Step 4: Audit existing `ab-index` feature detectors.**

Read `crates/ab-index/src/features.rs` (or wherever the feature flags live) and `data/feature-patterns.toml`. Record the existing flag names; Task 5's `detector_id` values will reference matrix row IDs (e.g., `ruby_basic`), not these feature flags, but rows whose detector is "count nodes of AAT kind X" can reuse the AAT post-parse without going through `ab-index` at all.

---

## Task 1: Extract Authoritative Feature Taxonomy and Draft Schema

**Files:** `data/aozora-syntax-coverage.toml` (taxonomy rows), `data/aozora-syntax-coverage.schema.json` (initial draft co-evolving with the rows).

The schema and the taxonomy are drafted together so the schema reflects the actual row shape. Final enforcement happens in Task 2 once the row list stabilises.

- [x] **Step 1: Enumerate Ruby parser tag classes.**

Walk `references/parsers/aozora2html/lib/aozora2html/tag/*.rb` (37 files). For each: capture class name, source markers it triggers on (`re_*` constants and the `register_tag` call), the chuuki text patterns, the HTML it produces. Some collapse into one matrix row (`multiline_jisage`, `jisage`, `oneline_jisage` → one indentation row); some split (`gaiji.rb` covers both `U+...` and `第N水準` sub-cases).

- [x] **Step 2: Augment with chuuki-table entries that have no dedicated tag class.**

Read `references/parsers/aozora2html/lib/aozora2html.rb` for table constants (`KAERITEN_TABLE`, `KUNTEN_KAESI`, `ACCENT_TABLE`, etc.). Add the missing ones as their own matrix rows.

- [x] **Step 3: Cross-reference the official spec.**

Read `references/aozorabunko/rules/kijyunn.html` and `chuuki_ichiran.html`. Add rows for spec-defined items neither parser handles (those rows record `recognition = "unrecognised"` for all three parsers — a deliberate finding).

- [x] **Step 4: Reconcile with existing matrix rows.**

The 9 priority-1 rows keep their `id` values. New rows take next-available IDs in the same dotted-namespace style.

- [x] **Step 5: Draft the JSON Schema.**

Capture the new TOML shape, the four-state and five-state enums, and the prevalence sub-table. Forbidden combinations are encoded as `oneOf`/`if-then` clauses or, more practically, asserted by a Rust validator (Task 2).

- [ ] **Step 6: Commit.**

Expected: `feat(coverage): extend syntax matrix to full taxonomy` (rows + draft schema; `parsers.*` and `adapters.*` left as placeholder strings `"unknown"` to be filled by Task 3).

---

## Task 2: Finalise and Enforce Schema

**Files:** `data/aozora-syntax-coverage.schema.json`, `crates/ab-coverage/src/schema.rs`.

- [x] **Step 1: Lock the JSON Schema after the taxonomy stabilises.**

Resolve any shape changes from Task 1 (e.g., a row needed a sub-field unforeseen by the initial draft).

- [x] **Step 2: Implement the Rust validator.**

`SchemaValidator::validate(matrix) -> Result<(), Vec<RowError>>` checks every row against the schema, then runs the forbidden-combination rules (e.g., `aborts` requires `not_applicable`).

- [x] **Step 3: Add a workspace test that runs on every `cargo test`.**

The test loads `data/aozora-syntax-coverage.toml`, runs `SchemaValidator::validate`, and reports row IDs of any failures. Initially it tolerates `"unknown"` placeholders; after Task 3 lands, those become forbidden.

- [ ] **Step 4: Commit.**

Expected: `feat(ab-coverage): schema validator + cargo-test integration`.

---

## Task 3: Classify Parser Recognition + Adapter Fidelity (Single Pass per Parser)

**Files:** `crates/ab-coverage/src/classifier.rs`, `crates/ab-coverage/tests/classification.rs`, `data/aozora-syntax-coverage.toml`.

Recognition (read parser source) and fidelity (read adapter source + run fixtures) are both classified in this task, parser-by-parser, with one commit per parser. This avoids three-way write contention on the same TOML rows that the prior version of this plan caused.

- [ ] **Step 1: Build a fixture set keyed by matrix row.**

Each row needs a minimal `.txt` source that exercises only that feature. Reuse `adapters/aozora2html/tests/fixtures/` where they overlap; add new fixtures under `crates/ab-coverage/tests/fixtures/<row_id>/source.txt`. Also keep an expected-AAT-shape file per row that the fidelity classifier checks against.

- [ ] **Step 2: Classify `aozora2`.**

  - **Recognition** — read the upstream parser at `references/parsers/aozora2/crates/aozora-core/src/parser/` (7 modules: `ruby_parser.rs`, `block_parser.rs`, `command_parser.rs`, `content_parser.rs`, `reference_parser.rs`, `reference_resolver.rs`, `utils.rs`) and the node model at `references/parsers/aozora2/crates/aozora-core/src/node/`. The earlier review made clear this parser handles substantially more than ruby/gaiji — block start/end commands, headings, font-size, style, kaeriten, okurigana, references all have dedicated modules. Cite the file:line where each row's grammar handler lives.
  - **Fidelity** — read `adapters/aozora2/src/lib.rs` (the adapter wrapper, 243 lines, calls into the upstream and projects results to AAT). Run the per-row fixtures through the built adapter binary and inspect the emitted AAT for the typed nodes the row expects. Cite the wrapper line that emits (or fails to emit) the node.
  - Commit: `feat(coverage): classify aozora2 recognition + fidelity`.

- [ ] **Step 3: Classify `aozora-rs`.**

  - **Recognition** — read the upstream at `references/parsers/aozora-rs/aozora-rs/aozora-rs-core/src/`:
    - `tokenizer/definition.rs` — token type enumeration
    - `scopenizer/parser.rs` — scope parsing
    - `retokenizer/definitions.rs` — retokenized node types (Kunten, Figure, DecoBegin, DecoEnd, Break, etc.)
    - Cross-reference with `adapters/aozora-rs/src/parser.rs` for the adapter's body-selection wrapper, but recognition is decided by the upstream.
  - **Fidelity** — read `adapters/aozora-rs/src/aat.rs` and `adapters/aozora-rs/src/projection.rs`. Run fixtures and inspect AAT.
  - Commit: `feat(coverage): classify aozora-rs recognition + fidelity`.

- [ ] **Step 4: Classify `aozora2html`.**

  - **Recognition** — each `tag/*.rb` class maps directly to one or more rows; chuuki tables in `lib/aozora2html.rb` cover the rest. `--use-unicode` mode forces gaiji rows to `recognition = "normalised"`, not `"parsed"`.
  - **Fidelity** — read `adapters/aozora2html/adapter.py`. Run fixtures through the wrapper script + adapter and inspect AAT. Forbidden combinations (e.g., `recognition = "normalised"` + `aat_fidelity = "synthesised"`) are valid and expected for the gaiji-marker case where the adapter would recover the discarded marker by diffing the raw source — note whether the current adapter actually does this or instead emits `aat_fidelity = "dropped"`.
  - Commit: `feat(coverage): classify aozora2html recognition + fidelity`.

- [ ] **Step 5: Re-run schema validation.**

After each commit and at the end, `cargo test -p ab-coverage` must pass with no `"unknown"` placeholders remaining in the rows that have been classified.

---

## Task 4: Whole-Corpus Prevalence Pipeline

**Files:** `crates/ab-coverage/src/{prevalence,cache,detectors}.rs`, `benchmarks/run-coverage.sh`.

- [ ] **Step 1: Implement the parser-output cache with version-keyed paths.**

Cache layout:

```
target/parser-cache/<parser_id>/<adapter_sha256>/<input_sha256>.json
```

- `parser_id` — `aozora2`, `aozora-rs`, `aozora2html`.
- `adapter_sha256` — deterministic content hash of adapter+parser sources. Specifically: collect every regular file under `adapters/<id>/**` (excluding `target/`, `__pycache__/`, `.cargo-lock`, generated files) and the parser sources under `references/parsers/<id>/**` (excluding `target/`, `node_modules/`, `.git/`, build artefacts), sort the resulting paths lexicographically, then compute `sha256(sha256(path_1_relative) || sha256(content_1) || sha256(path_2_relative) || sha256(content_2) || ...)`. Path is included so a rename invalidates; content is included so an in-place edit invalidates; sort order is fixed so the hash is reproducible. The exact include/exclude globs are defined in `crates/ab-coverage/src/cache.rs` and reflected in the methodology doc. Computed once at startup; embedded in the cache path.
- `input_sha256` — sha256 of the raw work source bytes as ingested.

Stale cache from a parser code change is impossible by construction: a code change moves entries into a new `<adapter_sha256>` directory. Old entries linger on disk (small cost) until manual cleanup; the runner prints how to prune (`rm -rf target/parser-cache/<parser_id>/<old_sha>` after confirming).

Cache hit returns the AAT JSON; cache miss runs the adapter and writes the result.

- [ ] **Step 2: Implement per-row detectors.**

Detectors live as small fns in `crates/ab-coverage/src/detectors/<row_id>.rs`. The matrix's `corpus_prevalence.detector_id` records the row ID (e.g., `ruby_basic`), matching the file name. Each detector takes a parsed AAT and returns an occurrence count plus the list of work IDs the feature appeared in.

For rows whose detector is "count nodes of AAT kind X," the implementation reuses the cached AAT directly. For rows that need raw-source evidence (e.g., `gaiji.marker` for adapters that drop the marker), the detector falls back to scanning the raw source bytes.

`detector_id` naming is row-name-only (no `ab-index:` prefix), and the methodology doc lists the implementation file for each.

- [ ] **Step 3: `sample_works` selection rule.**

For each row, the detector keeps a max-heap of (occurrence_count, work_id) pairs and emits the top 5 at the end of the corpus pass. Ties on count are broken by lexicographic `work_id` ordering. Recorded in the methodology doc.

- [ ] **Step 4: Drive the corpus pass with rayon.**

```rust
corpus.par_iter().for_each(|work| {
    for parser in active_parsers() {
        let aat = cache.get_or_run(parser, work)?;
        for row in matrix.rows() {
            counters[parser][row].add(detector(row, &aat, work));
        }
    }
});
```

- [ ] **Step 5: TOML merge step.**

Conservative merge: only writes `corpus_prevalence.{works_with_feature, total_occurrences, sample_works}` per row. Does not touch `parsers.*` or `adapters.*`. Preserves all existing comments and field ordering by parsing into a structure-preserving AST (use `toml_edit`, not the lossy `toml` deserializer).

- [ ] **Step 6: Commit the tool (no matrix data yet).**

Expected: `feat(ab-coverage): whole-corpus prevalence pipeline with versioned parser cache`.

---

## Task 5: Calibrate Performance Budget on a Pilot Sample

**Files:** `docs/superpowers/specs/2026-04-28-syntax-coverage-methodology.md` (early stub).

This task replaces the previous plan's uncalibrated `<120 s` claim. Per the review: the prevalence workload is *parse-each-work-through-each-adapter*, not the regex-based ab-index scan that hit sub-minute totals before. Per-work cost is 50–200× higher.

- [ ] **Step 1: Pilot run on 100 ruby-bearing works.**

```bash
# build/refresh the index if absent
mkdir -p scratch
test -f scratch/ab-index.json || cargo run --release -p ab-index -- \
  --corpus references/aozorabunko --output scratch/ab-index.json

# select 100 ruby-bearing work IDs
jq '[.works[] | select(.features | index("ruby")) | .id][0:100]' \
  scratch/ab-index.json > scratch/coverage-pilot-ids.json

# pilot, no cache, all three adapters
benchmarks/run-coverage.sh --corpus references/aozorabunko \
  --work-ids scratch/coverage-pilot-ids.json \
  --out-dir scratch/coverage-pilot \
  --jobs 16 --no-cache
```

Capture per-adapter median and p95 latency, total wall-clock.

- [ ] **Step 2: Project full-corpus runtime.**

Multiply per-adapter median by 14 049 (ruby-bearing works) for each Rust adapter; by ~14 049 for aozora2html (which is the bottleneck). Project wall-clock with rayon at `--jobs 16`.

- [ ] **Step 3: Decide cold-pass strategy.**

  - If projected aozora2html cold pass is under ~90 minutes wall-clock: proceed with full corpus.
  - If projected aozora2html cold pass exceeds that budget: fall back to a stratified sample (e.g., for each row, pick up to 200 works with that feature flag from `ab-index`, dedupe; cold-pass aozora2html only those works; record `corpus_prevalence.coverage_basis = "stratified_sample"` instead of `"full_corpus"` per row).
  - Rust adapters are cheap enough to always run full corpus.

The decision is recorded in the methodology doc (Task 7) and reflected in the TOML by setting `corpus_prevalence.coverage_basis` per row.

- [ ] **Step 4: Commit pilot artefacts (the methodology doc gains a "Performance calibration" section with measured numbers).**

Expected: `docs(coverage): pilot timing measurement and corpus strategy decision`.

---

## Task 6: Run on Whole Corpus and Materialise Numbers

- [ ] **Step 1: Cold-cache run.**

```bash
benchmarks/run-coverage.sh --corpus references/aozorabunko --jobs 16
```

The strategy from Task 5 Step 3 is in effect (full corpus or stratified). Stderr → `scratch/ab-coverage-<timestamp>/cold.log`. Cache populated; matrix updated; `summary.json` written.

- [ ] **Step 2: Warm rerun to confirm caching works.**

```bash
benchmarks/run-coverage.sh --corpus references/aozorabunko --jobs 16
```

Expect well under a minute. If not, the cache is broken — fix before declaring the task done.

- [ ] **Step 3: Cross-check prevalence against `ab-index`.**

For rows whose feature maps onto an existing `ab-index` flag (`feature_keys` field), compare `works_with_feature` to the index's count. Discrepancies > 1 % flag detector bugs.

- [ ] **Step 4: Commit the populated matrix.**

Expected: `data(coverage): populate whole-corpus prevalence`.

---

## Task 7: Companion Methodology Document

**Files:** `docs/superpowers/specs/2026-04-28-syntax-coverage-methodology.md`.

- [ ] **Step 1: Document the taxonomy seeding process.** Cite which Ruby tag class / chuuki table / kijyunn section gave each row.
- [ ] **Step 2: Document the recognition classifier.** State the file:line evidence rule used for each parser, with corrected paths (`references/parsers/aozora2/crates/aozora-core/src/parser/*`, `references/parsers/aozora-rs/aozora-rs/aozora-rs-core/src/{tokenizer,scopenizer,retokenizer}/`, `references/parsers/aozora2html/lib/aozora2html/tag/*`).
- [ ] **Step 3: Document the fidelity classifier.** Adapter source rules; highlight the gaiji-marker `normalised` + `synthesised` case.
- [ ] **Step 4: Document the corpus pass.** Cache layout (parser_id / adapter_sha / input_sha), detector list, rayon configuration, measured timings from Task 5, the chosen `coverage_basis` per row.
- [ ] **Step 5: Document forbidden-combination rules and why.**
- [ ] **Step 6: Document deferred work:** `feature-patterns.toml` reconciliation; adding `AozoraEpub3`, `aozora-parser.js`, `aozorabunko-extractor` as additional parser/adapter columns (the schema and cache already accept them).
- [ ] **Step 7: Cross-reference findings.** Three or four largest mismatches between parsers and what they imply for downstream consumers.
- [ ] **Step 8: Commit.** `docs(coverage): methodology document`.

---

## Task 8: Plan Queue Maintenance

- [ ] **Step 1: Refresh `docs/superpowers/PLAN-EXECUTION-ORDER.md`.**

Move this plan to "Archived plans" after Task 9 verification passes. Mention the populated matrix and methodology doc as the durable artefacts.

- [ ] **Step 2: Commit and archive.**

---

## Task 9: Final Verification

- [ ] **Step 1: Workspace and adapter checks.**

```bash
cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
```

- [ ] **Step 2: Schema and combination-rule validation.**

```bash
cargo test -p ab-coverage matrix_schema_valid
cargo test -p ab-coverage forbidden_combinations_rejected
```

- [ ] **Step 3: Warm-cache run completes within budget.**

`benchmarks/run-coverage.sh --corpus references/aozorabunko --jobs 16` finishes in under a minute on warm cache.

- [ ] **Step 4: Reporting completeness.**

Every `[[syntax]]` row has every `parsers.<id>.recognition` and `adapters.<id>.aat_fidelity` set to a non-`"unknown"` enum value, every row has `corpus_prevalence.coverage_basis` set, and no forbidden combinations remain.

- [ ] **Step 5: Mark plan front-matter `status: done`, archive, refresh queue.**

---

## Success criteria

A reviewer asking "what does aozora-rs actually recognise that aozora2 does not?" can answer it in under a minute by reading `data/aozora-syntax-coverage.toml`, with every claim backed by a `file:line` citation in `evidence`. A reviewer asking "how often does the `decoration.kunten` syntax appear in the corpus?" can answer it from the same file. The methodology doc reproduces the classification process so a future contributor can re-run it after a parser update — and the cache invalidates automatically when adapter or parser source changes, because the cache key includes a content hash of those sources.

When the team is ready to add `AozoraEpub3`, `aozora-parser.js`, or `aozorabunko-extractor` as additional parsers/adapters, the follow-up plan only has to (a) add an adapter binary in `adapters/<id>/`, (b) implement classifiers for the new ID's recognition and fidelity, and (c) extend the corpus pass to include the new parser ID. The schema and cache layout already accept them; no migration of existing rows is required.
