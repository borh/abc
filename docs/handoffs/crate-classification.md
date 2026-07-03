# Crate classification & navigability report

Scope: read-only deepening-review of the 13 workspace crates in
`/home/bor/Projects/ab-validator/crates/`. No crate is refactored. The proposed
`crates/README.md` body appears in §6, not written to disk.

Method: for each crate read `Cargo.toml` + `src/lib.rs` + real source modules;
whole-crate LOC counted over `src/**/*.rs` excluding `target/`. Consumer graph
taken from `use ab_*::` occurrences (workspace + adapters, excluding build
artifacts) and from path/workspace dep edges in `Cargo.toml` files.

Caveat: whole-crate LOC includes tests and benches in some files; the relative
ordering is still meaningful. Notable correction to the task's stated sizes:
`ab-morph-run` is ~14.1k LOC (not 2k), `ab-coverage` ~2.1k (not the thin leaf
implied), `ab-morph-diff` ~3.0k, `ab-morph-analyzers` ~1.7k.

## 1. Per-crate classification

Legend for the "Class" column (deepening-review types):

- **Deep** — cohesive concern, substantive code, stable-ish interface, defensible boundary.
- **Seam (keep)** — one dominant impl/consumer but a credible present or imminent variation (multiple impls, or a trait with a real second implementation).
- **Fake seam (inline candidate)** — one impl, one consumer, no present or imminent variation; the boundary hides no protocol/trust/state risk.
- **Leaf CLI** — crate exists primarily as a binary entry point; its lib is the CLI's private implementation, not a shared API.

### 1a. Classification table

| Crate | Whole-crate LOC | Class | Consumers (file:line evidence) | Interface exposed (one line) | Judgment |
|---|---|---|---|---|---|
| `ab-source-syntax` | 993 | Deep | `ab-plaintext/src/aozora.rs:2`; `ab-check` dep (Cargo.toml:14); `adapters/aozora-rs/src/lib.rs:3`, `adapters/aozora-rs/src/source.rs:6`; `adapters/aozora2/src/lib.rs:2` | Lowest-level Aozora source tokenizer (`SourceEvent`, `SourceSpan`, `comparison_lossy_body`, `SourceAnnotations`); zero deps (`Crates/lib.rs:1` shows no `[dependencies]` for core). | Genuine foundation: borrowed-lifetime event model shared by two adapters and two workspace crates. Boundary pays rent. |
| `ab-ir` | 2085 | Deep (but see §3) | adapter-facing: `adapters/aozora-rs/src/aat.rs:3` (uses `ab_ir::visible_projection`, `block_content_mut`, `provenance_counts`); workspace: `ab-oracle/src/evaluate.rs:1` (`AatDocument`); declared in root `Cargo.toml:40`. | Rust IR tree (`Block`, `Inline`, ~60 builder methods on `Inline`/`Block`, `blocks_to_aat_json`, `blocks_to_aat_projection`, `visible_projection`, `provenance_counts`) plus `aat_view::{AatDocument, select, SelectorError}` implementing the AAT selector protocol. | Cohesive and substantial. **Risk:** it is a *Rust-level* adapter contract (see §3), not just an internal helper, and it is not version-stable. |
| `ab-plaintext` | 432 | Deep (small) | `ab-morph-analyzers/src/lib.rs:14`, `ab-morph-run/src/lib.rs:15`, `ab-check` dep (Cargo.toml:13); `ab-ir` dep (Cargo.toml:4). | `PlainTextDocument{text_id,source_format,text}`, `from_aat_value`, `visible_text_projection`, `from_aozora_honbun_bytes`. | The shared "what is the text to tokenize?" abstraction for the morphology + check stacks. Two input formats (AAT, raw honbun) justify the boundary despite small LOC. |
| `ab-diff-utils` | 317 | Fake seam (inline candidate) | exactly one: `ab-compare` (Cargo.toml:13) at `ab-compare/src/aat_diff.rs:7` and `ab-compare/src/triage.rs:3`. Root declares it (Cargo.toml:34) but no other crate consumes it. | Three free functions + one generic struct: `first_difference`, `FrequencyTable<K,E>`, `hash_bytes`/`hash_json`/`hash_string_sequence`. | tiny utility bag. The only consumer is `ab-compare`. `first_difference` and the hash helpers are general enough that the seam *could* pay rent if morphology adopted them, but today they do not (morphology has its own diff in `ab-morph-diff`). Severe fake-seam smell; extraction plan exists (§2). |
| `ab-morph-diff` | 3007 | Deep | `ab-morph-analyzers` (5 files, e.g. `lib.rs:15`), `ab-morph-run` (`compact.rs:467`, `nway.rs:5,6`, `warehouse/rows.rs:4,5,7`, `lib.rs:17,20,23`); proptest + criterion in tree. | `Analysis`, `Morpheme`, `compare_pair`, `validate_analysis`, n-way regions, `CompactComparison`, streaming variants. | Cohesive domain (morpheme alignment + diff model). Real independent test surface (proptest). Boundary justified. |
| `ab-morph-analyzers` | 1701 | Seam (keep) | exactly one consumer crate: `ab-morph-run/src/lib.rs:14`. | `trait MorphAnalyzer { analyzer_id, analyze }` with three impls: `VibratoAnalyzer` (`vibrato.rs:212`), `SudachiAnalyzer` (`sudachi.rs:109`), `VaporettoAnalyzer` (`vaporetto.rs:90`). | Trait has **three present implementations** (`MorphAnalyzer` trait, `lib.rs:17`). This is a real variation seam even with a single runner consumer — adding an analyzer does not touch the runner's call sites. Keep; document the trait as the contract. |
| `ab-morph-run` | 14119 | Deep (large leaf) | no internal Rust consumers; only its own benches (`benches/analyze_aat.rs:3`). It is the top of the morphology dependency cone. | `options::{OutputProfile,WarehouseProfile}`, `run_*`, `summarize_*`, nway/compact/warehouse summary rows, `resolve_source_id_aat_paths`. | Huge orchestration crate (largest in repo). The boundary is less about a shared API and more about being the runner binary's body. Acceptable as a crate; flag as a *braided-concern* candidate (orchestration + warehouse I/O reuse + summary generation all in one lib) but out of scope here. |
| `ab-warehouse` | 1675 | Seam (borderline) | exactly one consumer: `ab-morph-run` (`warehouse/rows.rs`, `warehouse/mod.rs:3,4`). | Parquet warehouse schema (`WarehouseTable`, ~14 row structs in `schema.rs`), `WarehouseWriter`, staging/final path layout, `sql` module. | Substantial and cohesive, but **single consumer**. The schema (`schema.rs`) is a real wire contract with Parquet files on disk → not a fake seam (bytes persist, downstream DuckDB reads them). Keep, but the Rust API surface is private to one caller; only the *Parquet schema* is a true contract. |
| `ab-check` | 1469 | Leaf CLI (small lib) | consumed by its own `main.rs:3` and tests; declared in root `Cargo.toml:41` (workspace dep offered to others) but **no other crate `use`s it**. The task's "ab-oracle -> ab-check" edge is a Cargo dep only; `ab-oracle/src/*` has no `use ab_check`. | `check::{check_single, run_batch, schema_validator}`, `aat`, `encoding`, `properties`, `source_projection`. | Lib is the CLI's private impl; the workspace dep is aspirational (oracle was meant to call it but currently imports only `ab-ir`). The crate is a deep *feature* (validation harness) but the lib-as-shared-API framing is weak. |
| `ab-compare` | 1457 | Leaf CLI | no internal consumers; only its own `main.rs:3`. | `compare_report_dirs`, `CompareSummary`, `aat_diff`, `metrics`, `triage`. | CLI comparing two `ab-check` report directories. Lib exists only to feed the bin. No second consumer. |
| `ab-coverage` | 2084 | Leaf CLI (+ 1 extra bin) | no internal consumers; bins at `src/bin/coverage.rs:22` and `src/bin/merge.rs:7`. | `matrix`, `schema`, `merge`, `prevalence`, `detectors`, `adapter`, `cache`. | Two binaries (`ab-coverage`, `ab-coverage-merge`) over a shared lib. Substantive code, but lib is the bins' shared backend, not a cross-crate API. |
| `ab-index` | 889 | Leaf CLI | no internal consumers; only its own `main.rs:7`, benches, tests. | `features`, `index` (`build_index`/`query_*`/`sample`/`write_index`), `encoding`, `syntax_coverage`. | Standalone feature-index builder/query CLI. No `ab-source-syntax` dep despite the task hint — `grep "use ab_source_syntax"` in `ab-index/src/` returns nothing. Independent leaf. |
| `ab-oracle` | 1815 | Leaf CLI | no internal consumers; own `main.rs:3`. The Cargo dep on `ab-check` (Cargo.toml:9) is **currently unused in source** (`grep use ab_check crates/ab-oracle/src/` → none; only `ab-ir` is imported, `evaluate.rs:1`). | `evaluate`, `audit`, `report`, `data`, `adapter_run`, `oracle_quality`. | Top of the evaluation cone (oracle correctness axis). Substantive, but a leaf binary with a private lib. The stale `ab-check` dep is dead weight. |

### 1b. Summary of fake seams and weak seams

- **`ab-diff-utils`** — the only unambiguous fake seam: one consumer (`ab-compare`), three unrelated micro-helpers (first-diff, frequency table, hashing), no present variation, no protocol on disk. Inline candidate pending a confirmed second consumer.
  - **2026-07-02 graph refutation (`docs/handoffs/graph-query-verification.md`):** `chiasmus_graph dead-code` on the crate shows the public API (`FirstDifference`, `FrequencyTable`, `hash_*`) is **not** dead — all dead-code hits are `#[test]` fns — and grep confirms two production call sites in `ab-compare` (`aat_diff.rs:7`, `triage.rs:3`). Under the strict `architecture-triage` definition ("abstraction has one implementation or no present variation"), this is **not** a fake seam: it has a present production consumer. Inline remains a *taste* call (one consumer), not a structural defect. Re-classify: **small shared-utilities crate, optional inline — not fake-seam.**
- **`ab-warehouse`** — single Rust consumer (`ab-morph-run`), but the Parquet schema is a persistent wire contract read by external DuckDB tooling. Not deletable; the Rust API seam is thin but the on-disk schema seam is real.
- **`ab-morph-analyzers`** — single consumer but a *real* trait with three impls. This is the canonical "seam paying rent via variation." Keep.
- **`ab-check`, `ab-compare`, `ab-coverage`, `ab-index`, `ab-oracle`** — all leaf CLIs whose libs are private to their own bins. Not fake seams (they are entry points, not abstraction boundaries), but the "lib + bin" split gives a false impression of a reusable API. `ab-check` and `ab-oracle` additionally carry dead/aspirational workspace deps.

## 2. The thin crates — is the seam paying rent today?

- **`ab-diff-utils` (317 LOC):** No. Exactly one consumer (`ab-compare`). The three exported helpers (`first_difference`, `FrequencyTable`, `hash_*`) are independent concerns bundled in one crate. There is a plan doc, `docs/superpowers/plans/2026-04-28-diff-utils-extraction.md`, recording the *extraction* decision — meaning the seam was created deliberately, not as a natural boundary. No second consumer has materialized. The hashing helpers are general enough that morphology *could* use them, but `ab-morph-diff` does its own thing. **Verdict: inline candidate** unless a second consumer is imminent. If kept, split into three crates or move into `ab-compare` and stop advertising it as a workspace dep.
- **`ab-coverage/merge` (the `merge` module, not the "merge" bin):** Reviewed as part of `ab-coverage`. `merge.rs` exposes `apply_classifier_findings` / `apply_prevalence_findings` and several `Findings` structs (`ab-coverage/src/merge.rs:20-70`); it is consumed only by `ab-coverage/src/bin/merge.rs:7`. The module exists to share logic between two bins in the same crate — that is a *legitimate intra-crate seam*, not a fake cross-crate seam. Paying rent (two bins). No action.
- **`ab-plaintext` (432 LOC):** Yes. Two distinct input representations (AAT visible-text projection vs. raw Aozora `honbun` bytes) both funneled into one `PlainTextDocument` consumed by three crates (`ab-ir`, `ab-check`, `ab-morph-analyzers`/`ab-morph-run`). The two-format variation is real and in production. Boundary justified despite small size.
- **`ab-morph-analyzers` (1701 LOC, 60 LOC lib.rs):** Yes, via variation not via consumer count. `MorphAnalyzer` trait has three live implementations (`Vibrato`, `Sudachi`, `Vaporetto`) behind dictionary/pinned git deps; the runner `ab-morph-run` consumes the trait, not each struct. Adding a fourth tokenizer does not touch the runner. The crate *looks* thin from `lib.rs` (60 LOC) because the real mass is in `vibrato.rs`/`sudachi.rs`/`vaporetto.rs`. **Do not** treat the small `lib.rs` as evidence of shallowness — this is a deep module with a small interface, which is the *ideal* shape.

## 3. Adapter ↔ workspace boundary

**Current state — the boundary is documented in intent but enforced in Rust types, and is not version-stable.**

- `docs/superpowers/specs/2026-04-26-parser-neutral-ir-design.md:16-23` states the design intent: "`ab-ir` owns document/block/inline types and AAT JSON projection" and "`aozora-rs-adapter` maps `aozora-rs-core::Retokenized` into `ab-ir`." Step 4 of that plan ("Port `aozora-rs-adapter` to use `ab-ir` values") was executed.
- The AAT *JSON* contract is normative in `data/aat-schema.json` and `docs/aat-contract.md` (versioned `version = 1`, backward-compat rules for optional fields, AAT v2 break semantics in `aat-contract.md` "Versioning").
- **But adapters do not consume the JSON contract — they consume Rust types.** Evidence: `adapters/aozora-rs/src/aat.rs:3` does `use ab_ir::{...}` and calls `ab_ir::visible_projection`, `ab_ir::block_content_mut`, `ab_ir::provenance_counts` (call sites at `adapters/aozora-rs/src/aat.rs:119,124,137,464,642,753,2006,2199,2240`). The adapter builds `Vec<Block>` and then serializes via `blocks_to_aat_json`. The `aozora2` adapter goes further and depends only on `ab-source-syntax` (not `ab-ir`), emitting AAT JSON directly via `serde_json`. `aozora2html` depends on **no** `ab-*` crate at all — it emits pure JSON.
- So three adapters demonstrate three different boundary strategies:
  1. `aozora-rs`: depends on `ab-ir` + `ab-source-syntax` Rust crates.
  2. `aozora2`: depends on `ab-source-syntax` only; emits AAT JSON by hand.
  3. `aozora2html`: depends on no `ab-*` crate; emits AAT JSON by hand.
- **This affects whether `ab-ir` needs to be version-stable.** Today the adapters are vendored checkouts pinned to this exact workspace (path deps `../../crates/ab-ir`), so a breaking `ab-ir` change only breaks adapters in this repo, which are rebuilt in lockstep. The moment an adapter is published against a crate version (or built outside this workspace), `ab-ir`'s Rust API becomes a de-facto external contract with no semver discipline — `version.workspace = true = "0.1.0"` and no `[lib]` visibility gating.

**Recommendation (reporting only, not implementing):** the contractual boundary for adapters should be the **AAT JSON schema** (`data/aat-schema.json` + `docs/aat-contract.md`), not `ab-ir` Rust types. `aozora2` and `aozora2html` already prove adapters can emit compliant JSON without `ab-ir`. `ab-ir` should be re-framed as an *optional convenience library* for adapters that want typed builders, not as the contract. If `ab-ir`'s Rust API must remain the boundary, it needs explicit semver + a compatibility note in its crate docs. This is a **Protocol Design** candidate per deepening-review (wire/schema contract lacking versioning rules for the Rust-typed path) and should be routed to `hammock-driven-design` or `rich-hickey-review` before any refactor.

## 4. Proposed grouping for `crates/README.md`

Original suggestion: ADAPTER-FACING CONTRACT (ab-ir, ab-source-syntax), EVALUATION HARNESS (ab-check, ab-compare, ab-oracle, ab-coverage), MORPHOLOGY (ab-morph-diff, ab-morph-analyzers, ab-morph-run, ab-warehouse, ab-plaintext), CLI BINS (ab-index).

Validation against evidence:

- **ADAPTER-FACING CONTRACT** — keep, but nuance the membership. `ab-ir` is Rust-typed and consumed by exactly one adapter (`aozora-rs`); `ab-source-syntax` is Rust-typed and consumed by two adapters + two crates. The *actual* adapter-facing contract is the JSON schema (`data/aat-schema.json`), documented in §3. Both `ab-ir` and `ab-source-syntax` are "Rust helpers for adapters," not "the contract." I will label the bucket ADAPTER-FACING HELPERS and link the JSON contract separately. **`ab-plaintext` is borderline** — adapters do not consume it today, but it bridges AAT JSON → text and is morally part of the AAT contract surface; I place it in MORPHOLOGY (its real consumers) with a cross-reference note.
- **MORPHOLOGY** — keep all five (`ab-morph-diff`, `ab-morph-analyzers`, `ab-morph-run`, `ab-warehouse`, `ab-plaintext`). Consumer graph confirms a clean cone: `ab-plaintext`/`ab-morph-diff` → `ab-morph-analyzers` → `ab-morph-run` → (`ab-warehouse`). `ab-plaintext` sits at the bottom and is shared with `ab-check`, so it spans buckets; flagged.
- **EVALUATION HARNESS** — `ab-check`, `ab-compare`, `ab-oracle`, `ab-coverage`. Confirmed: these are the three result axes from `aat-contract.md` (schema validity = `ab-check`, adapter comparison = `ab-compare`, oracle correctness = `ab-oracle`) plus coverage matrix tooling (`ab-coverage`). `ab-oracle`'s `ab-check` dep is currently dead (§1), which slightly weakens the "harness is cohesive" story but the bucketing still holds. Note: `ab-coverage` is arguably its own bucket (corpus coverage tooling) but groups naturally here.
- **CLI BINS** — the task's "CLI BINS (ab-index)" bucket is misleading: **all of `ab-check`, `ab-compare`, `ab-index`, `ab-coverage`, `ab-oracle` produce binaries** (each has a `src/main.rs` or `src/bin/`). Rather than a separate CLI bin bucket, I mark each crate with a `(CLI)` tag and a Bin name. `ab-index` becomes a standalone feature-indexing CLI; I keep it in its own one-crate bucket (INDEXING & FEATURES) because it shares no morph/eval deps and has no internal consumers.

Crates spanning buckets: `ab-plaintext` (MORPHOLOGY consumer graph + ADAPTER/AAT contract surface). `ab-source-syntax` (ADAPTER-FACING + foundation used by `ab-plaintext`/`ab-check`).

## 5. Hickey hazard check (per the deepening-review acceptance list)

- **Braided concerns:** `ab-morph-run` (orchestration + warehouse I/O + summary generation in one 14k-LOC lib) is the clearest braided-concern candidate. Out of scope to fix here; flagged for a future `rich-hickey-review` → `codebase-simplification` pass. No other crate braids unrelated concerns — each has one cohesive domain.
- **State / time / identity:** `ab-warehouse` persists Parquet to disk with a `run_id` identity (`schema.rs:179-209`); state/identity is encoded in file layout. Real concern, handled by `WarehousePaths`. `ab-oracle` review state is a succession (`aat-contract.md` "Oracle Correctness Provenance": "review state is a succession of `[[case.review]]` values"), correctly modeled as append-only. No hidden state risk found.
- **Protocol seam:** the biggest one is §3 — the Rust-typed `ab-ir` adapter contract has no versioning rules while the JSON sibling does. **Protocol Design** candidate.
- **Trust seam:** adapters are trusted producers of AAT (faithfulness axis is *separate* from oracle correctness per `aat-contract.md`); `ab-oracle` correctly keeps these axes separate (`oracle_status` independent of `oracle_evidence_strength`). No hidden trust conflation.
- **Shared state / behavior preservation:** not applicable (read-only investigation; no refactor proposed).

**Deepening acceptance check for the "Deep" verdicts:** all four deep crates (`ab-source-syntax`, `ab-ir`, `ab-morph-diff`, `ab-plaintext`) pass — cohesive single concern, honest interfaces, and either present variation (analyzers) or multiple real consumers. `ab-ir` fails only the "protocol/lifecycle explicit enough for the risk" item because its version-stability as an adapter contract is undefined (§3).

## 6. Proposed `crates/README.md` content

Below is the candidate file body. It is **not** written to `crates/README.md`; it is in this report for review. Whole-crate LOC and consumer counts are as measured above.

```markdown
# ab-validator workspace crates

This directory holds the 13 crates that make up the ab-validator workspace
(see the root `Cargo.toml`). Parser adapter checkouts under `adapters/` are
separate, vendored crates and are **excluded** from the workspace.

This file groups crates by role and flags which surfaces are stable contracts
versus private implementations.

## Adapter-facing contract

The **normative** adapter contract is the AAT JSON schema, not a Rust crate:

- Schema: `data/aat-schema.json` (AAT v1).
- Semantics: `docs/aat-contract.md` (result axes, selector protocol, span
  coordinates, versioning).
- Adapter fidelity matrix: `docs/adapter-fidelity.md`.

Adapters *may* emit compliant AAT JSON without depending on any crate below
(the `aozora2html` adapter does this). The Rust crates in this section are
**optional typed helpers** for adapters that want them — they are not the
contract and are not yet version-stable.

| Crate | Role | Adapter consumers |
| --- | --- | --- |
| `ab-source-syntax` (993 LOC) | Lowest-level Aozora source tokenizer: borrowed-lifetime `SourceEvent`/`SourceSpan`, `comparison_lossy_body`, `SourceAnnotations`. Zero runtime deps. | `aozora-rs`, `aozora2` |
| `ab-ir` (2085 LOC) | Parser-neutral document IR (`Block`, `Inline`) plus AAT projection (`blocks_to_aat_json`, `visible_projection`, `provenance_counts`) and the AAT selector protocol (`aat_view::AatDocument::select`). ⚠️ Consumed as **Rust types** by `aozora-rs`; **not** version-stable. Prefer emitting AAT JSON directly. | `aozora-rs` (Rust types), `ab-oracle` (selector) |

## Morphology pipeline

A clean dependency cone: text extraction at the bottom, diff model above it,
pluggable analyzers, then the runner + warehouse.

| Crate | LOC | Role | Consumed by |
| --- | --- | --- | --- |
| `ab-plaintext` | 432 | `PlainTextDocument` + two inputs: `from_aat_value` (AAT JSON) and `from_aozora_honbun_bytes` (raw source). Shared with `ab-check`. | `ab-ir`, `ab-check`, `ab-morph-analyzers`, `ab-morph-run` |
| `ab-morph-diff` | 3007 | Morpheme alignment + diff model: `Analysis`, `compare_pair`, n-way regions, `validate_analysis`, streaming/compact variants. Has proptest + criterion. | `ab-morph-analyzers`, `ab-morph-run` |
| `ab-morph-analyzers` | 1701 | `trait MorphAnalyzer` with three impls: `VibratoAnalyzer`, `SudachiAnalyzer`, `VaporettoAnalyzer`. The seam is real (add a tokenizer without touching the runner). | `ab-morph-run` |
| `ab-warehouse` | 1675 | Parquet warehouse schema + writer + SQL. The **on-disk Parquet schema** is the real contract (read by external DuckDB); the Rust API is private to one consumer. | `ab-morph-run` |
| `ab-morph-run` | 14119 | Top-of-cone runner + summary tooling (`options`, `run_*`, `summarize_*`, nway/compact/warehouse summaries, `resolve_source_id_aat_paths`). Largest crate. | (its own `ab-morph` binary + benches) |

## Evaluation harness

Maps to the three result axes in `docs/aat-contract.md` (schema validity,
adapter comparison, oracle correctness) plus coverage tooling. Each crate
ships a CLI; its `lib` is the CLI's private backend unless noted.

| Crate | LOC | Bin(s) | Role |
| --- | --- | --- | --- |
| `ab-check` | 1469 | `ab-check` (CLI) | Schema validity + parser invariant checks (`check_single`, `run_batch`, `schema_validator`). ⚠️ Declares a workspace dep offered to others but currently only its own bin consumes it. |
| `ab-compare` | 1457 | `ab-compare` (CLI) | Compares two `ab-check` report directories (`compare_report_dirs`). Only consumer of `ab-diff-utils`. |
| `ab-oracle` | 1815 | `ab-oracle` (CLI) | Oracle correctness axis: `evaluate`, `audit`, `report`, `data`. Keeps oracle status independent of evidence strength. ⚠️ Has a stale, unused `ab-check` dep. |
| `ab-coverage` | 2084 | `ab-coverage`, `ab-coverage-merge` | Coverage matrix + schema validation for `data/aozora-syntax-coverage.toml`. Two bins share the lib. |

## Indexing & features

| Crate | LOC | Bin(s) | Role |
| --- | --- | --- | --- |
| `ab-index` | 889 | `ab-index` (CLI) | Standalone feature-index builder/query tool. No internal consumers; independent of the morph/eval stacks. |

## Shared utilities

| Crate | LOC | Role | Status |
| --- | --- | --- | --- |
| `ab-diff-utils` | 317 | `first_difference`, `FrequencyTable`, `hash_{bytes,json,string_sequence}`. | ⚠️ **Fake seam candidate.** Single consumer (`ab-compare`); three unrelated helpers in one crate. Inline-or-split candidate unless a second consumer is imminent. |

## Cross-cutting notes

- **Boundary honesty:** only `ab-source-syntax`, `ab-ir`, and the AAT JSON
  schema are adapter-facing; everything else is internal. `ab-ir` is consumed
  as Rust types by one adapter — see `docs/handoffs/crate-classification.md` §3.
- **Leaf CLI crates** (`ab-check`, `ab-compare`, `ab-index`, `ab-coverage`,
  `ab-oracle`) expose a `lib` that is effectively the CLI's private
  implementation; do not treat them as stable cross-crate APIs.
- **Pending cleanups** surfaced by this review (not executed here): the unused
  `ab-check` dep in `ab-oracle`; the fake-seam status of `ab-diff-utils`; the
  protocol/versioning gap on `ab-ir` as an adapter contract.
```

## 7. Next-route recommendations (deepening-review)

- **ab-diff-utils → `codebase-simplification`** (Delete/Inline): one impl, one consumer, no variation. Decide inline-vs-split after confirming no imminent second consumer.
- **ab-ir adapter contract → `hammock-driven-design` / `rich-hickey-review`** (Protocol Design): define whether the adapter boundary is JSON-only or Rust-typed, and add versioning rules for whichever path is chosen.
- **ab-morph-run → `rich-hickey-review` then `codebase-simplification`** (potential Decomplect): the 14k-LOC lib braids orchestration, warehouse I/O reuse, and summary generation.
- All other deep/seam crates: characterize-and-leave; no action.
