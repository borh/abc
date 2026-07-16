# Parser RQ P4A2 Ledger-Authoritative R1 Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Derive R1 semantic source-recognition coverage and supporting byte
accountability from authenticated P4A1 ledgers without reinterpreting P1 records.

**Architecture:** New source-recognition protocols coexist with immutable P1
v1. Rust authenticates each per-work capture generation and derives two interval
projections. A closed corpus index separately authenticates the mapping from
work membership to distinct captures and recognition records. Clojure gates R1
only on semantic integer equality; node-span coverage receives a new supporting
key.

**Tech Stack:** Rust 2024, serde/serde_json, Hegel; Clojure, Malli, Charred;
Draft 2020-12 JSON Schema; Nix and Kaocha.

## Global Constraints

- P4A1 must be complete with an accepted policy generation.
- Do not migrate or rewrite P1 v1 artifacts.
- R1 gates `recognized_bytes == eligible_bytes`; accounted cannot pass R1.
- `recognized ⊆ accounted ⊆ eligible`; both complements conserve bytes.
- Corpus membership remains the authenticated P1 index set.
- `capture_generation_ref` identifies one P4A1 work capture;
  `corpus_generation_ref` identifies the closed P4A2 index. Never substitute
  one for the other.
- Invalid ledgers are unavailable; valid semantic gaps are available failures.
- Node-span evidence remains visible as `:parser_ir_node_span_coverage`.

---

### Task 1: Define immutable source-recognition protocols

**Files:**
- Create: `abc/schemas/parser-rq-source-recognition-work.schema.json`
- Create: `abc/schemas/parser-rq-source-recognition-index.schema.json`
- Create: `abc/schemas/parser-rq-source-recognition-aggregate.schema.json`
- Create: `abc/test/fixtures/parser-rq/source-recognition/*.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:** Closed protocols with recognized/accounted intervals, semantic
gaps, unaccounted intervals, exact completeness, per-work
`capture_generation_ref`, and index/aggregate `corpus_generation_ref`.

- [ ] Write RED tests rejecting unknown fields, `recognized_bytes >
  accounted_bytes`, either projection beyond eligible, missing work membership,
  unavailable records containing trusted totals, and missing/duplicate/swapped
  work-to-capture mappings. Exercise distinct captures in a multi-work corpus
  plus single- and zero-work corpora.
- [ ] Run focused Kaocha. Expected: missing-schema failure.
- [ ] Implement schema IDs
  `abc/parser-rq-source-recognition-{work,index,aggregate}/v1`; do not edit P1
  v1 schemas.
- [ ] Run focused Kaocha and schema drift; commit as
  `feat(parser-rq): define source-recognition protocols`.

### Task 2: Add a pure ledger analyzer beside legacy analysis

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/recognition.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs`
- Test: `ab-validator/crates/ab-parser-rq-source-accountability/tests/recognition.rs`
- Test: `ab-validator/crates/ab-parser-rq-source-accountability/tests/recognition_properties.rs`

**Interfaces:**

```rust
pub fn analyze_recognition(input: RecognitionInput) -> RecognitionAnalysis;
```

`RecognitionInput` contains authenticated decoded bytes, ledger bytes, policy
bytes, generation manifest, qualification identity, and work ID.

- [ ] Write RED tests for full typed recognition, opaque-only accountability,
  recovered semantic gaps, invalid target identity, unknown policy row, and
  generation mismatch.
- [ ] Validate all facts before interval arithmetic. Semantic dispositions feed
  recognized; `PreservedOpaque` is added only to accounted.
- [ ] Add Hegel subset and dual-conservation properties.
- [ ] Run new and legacy P1 suites; commit as
  `feat(parser-rq): derive ledger source recognition`.

### Task 3: Produce deterministic corpus records and aggregates

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/recognition_corpus.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/recognition_aggregate.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/main.rs`
- Test: `ab-validator/crates/ab-parser-rq-source-accountability/tests/recognition_corpus.rs`

**Interfaces:** CLI commands `analyze-recognition-corpus` and
`aggregate-recognition` consume explicit index/store roots and publish their
summary last.

The producer writes `capture_generation_ref` into each work record and its
corresponding index entry. After all recognition-record blobs exist, it computes
`corpus_generation_ref = sha256(JCS(index - corpus_generation_ref))`; the
aggregate copies that exact reference. This excludes only the self-reference
and includes membership, ordered entries, each capture reference, and each
record blob identity.

- [ ] Write RED trust tests for exact membership, authenticated locators,
  distinct multi-work capture references, swapped/missing/duplicate mappings,
  single/zero work, unavailable propagation, checked totals, and byte-identical
  repeated runs.
- [ ] Reuse the shared authenticated blob-store API; do not copy CAS code.
- [ ] Aggregate per-work intervals without cross-work merging. Run focused tests
  and CLI help; commit as `feat(parser-rq): aggregate source recognition`.

### Task 4: Migrate the R1 envelope without hiding legacy evidence

**Files:**
- Modify: `abc/src/abc/tools/parser_rq_source_accountability.clj`
- Modify: `abc/src/abc/tools/parser_release_qualification.clj`
- Modify: `abc/test/abc/tools/parser_rq_source_accountability_test.clj`
- Modify: `abc/test/abc/tools/parser_release_qualification_test.clj`

**Interfaces:** `derive-source-recognition-envelope` returns the existing
`:source_span_coverage`; legacy node evidence is exposed under
`:parser_ir_node_span_coverage`.

- [ ] Write RED tests where accounted is 100% but recognized is 90%; R1 must be
  0.9 and fail. Add full-recognition pass and unavailable-ledger cases.
- [ ] Authenticate the new aggregate through P0 manifest bindings and derive the
  ratio from exact integers. Historical manifests remain instrument-missing.
- [ ] Run focused Kaocha; commit as
  `feat(parser-rq): derive R1 from semantic recognition`.

### Task 5: Drift-check the migration fixture

**Files:**
- Create: `abc/test/fixtures/parser-rq/source-recognition-capture/*`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/recognition_fixture.rs`
- Modify: `abc/test/abc/tools/parser_rq_source_accountability_test.clj`

- [ ] Generate clean, opaque-unknown, and recovered-malformed works through
  P4A1/P4A2 production paths. Assert recognized/accounted divergence.
- [ ] Mutate each identity edge and prove unavailable through the real manifest
  path. Regenerate twice byte-identically.
- [ ] Run crate tests, focused Clojure, schema drift, comment hygiene, and
  `just validate-migration`.
- [ ] Request whole-range review; commit as
  `test(parser-rq): drift-check ledger R1 migration`.
