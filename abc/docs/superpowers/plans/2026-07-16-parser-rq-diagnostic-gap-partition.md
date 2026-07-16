# Parser RQ P4A3 Diagnostic Gap Partition Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Partition authenticated P4A2 semantic gaps into exact authorized
recovery and silent intervals without changing R1 recognition.

**Architecture:** A disposable characterization repairs the executable repro set
and joins every diagnostic span to ledger-derived semantic gaps. ABC freezes an
independent closed policy. A pure Rust authorizer validates raw diagnostics;
existing interval algebra performs the partition and Clojure derives R2.

**Tech Stack:** Rust 2024, serde/serde_json, Hegel; Clojure, Malli, Charred;
Draft 2020-12 JSON Schema; Nix and Kaocha.

## Global Constraints

- P4A2 must be complete with authenticated semantic gaps.
- Diagnostics never increase R1 recognized or accounted bytes.
- Unknown/internal/malformed/policy-drift diagnostics make R2 unavailable only.
- Authorized spans are exact decoded UTF-8 intervals; no expansion.
- Empty authenticated diagnostics are available and vacuous.
- `silent_drop_count` counts per-work maximal intervals, never constructs.

---

### Task 1: Characterize all diagnostics against semantic gaps

**Files:**
- Create temporarily: `ab-validator/crates/ab-parser-rq-source-accountability/tests/diagnostic_gap_characterization.rs`
- Create: `ab-validator/docs/superpowers/reports/2026-07-16-diagnostic-gap-partition.json`
- Create: `ab-validator/docs/superpowers/reports/2026-07-16-diagnostic-gap-partition.md`
- Delete before commit: temporary test above

- [ ] Enumerate all 21 live codes. Override only the three non-emitting
  executable repros; retain their documented repro text. Run all source cases
  through P4A1 capture and P4A2 recognition.
- [ ] Record exact diagnostic span, semantic-gap intersection, recovery output,
  severity/source, and proposed disposition. Internal codes are
  `reject_internal`.
- [ ] Run twice and compare bytes. Stop if authorization needs expansion, lacks
  an emitted diagnostic, or would alter R1.
- [ ] Delete the probe and commit reports as
  `docs(parser-rq): characterize diagnostic gap partition`.

### Task 2: Freeze ABC diagnostic policy and protocols

**Files:**
- Create: `abc/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-gap-policy.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-gap-result.schema.json`
- Create: `abc/data/parser-rq-ab-aozora-diagnostic-gap-v1.json`
- Create: `abc/test/fixtures/parser-rq/diagnostic-gap/*.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

- [ ] Write RED tests for the complete 21-code table, unknown fields/codes,
  kind/code mismatch, conditional PUA codepoint, internal source, and
  unavailable values containing intervals.
- [ ] Copy Task 1's accepted disposition table verbatim. Runtime code may not
  generate or infer it. Pin raw-schema and policy JCS hashes.
- [ ] Run focused Kaocha and schema drift; commit as
  `feat(parser-rq): define diagnostic gap policy`.

### Task 3: Implement the pure authorizer

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/Cargo.toml`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/model.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/authorize.rs`
- Test: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/authorize.rs`
- Test: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/properties.rs`
- Modify: `ab-validator/Cargo.toml`
- Modify: `ab-validator/Cargo.lock`

**Interfaces:**

```rust
pub fn authorize(
    capture: &ValidatedDiagnosticCapture,
    policy: &ValidatedGapPolicy,
    context: WorkContext<'_>,
) -> AuthorizationAnalysis;
```

- [ ] Write RED examples for every policy row and failure condition. Add Hegel
  order, duplicate, and overlap properties.
- [ ] Parse JSON only at validation. The pure function accepts closed values and
  returns no interval claims when unavailable.
- [ ] Validate UTF-8 endpoints against authenticated decoded source; normalize
  only after all entries pass. Pin live vocabulary drift independently.
- [ ] Run test, clippy, and fmt; commit as
  `feat(parser-rq): authorize diagnostic gap intervals`.

### Task 4: Aggregate and derive R2 without touching R1

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/aggregate.rs`
- Test: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/aggregate.rs`
- Modify: `abc/src/abc/tools/parser_rq_source_accountability.clj`
- Modify: `abc/test/abc/tools/parser_rq_source_accountability_test.clj`

- [ ] Write RED tests for exact work sets, generation identity, partition
  conservation, empty vacuity, one-byte residual silence, and no cross-work
  merging.
- [ ] Use P1 `reconcile(semantic_gap, authorized)`; do not duplicate interval
  algebra. Count per-work maximal silent intervals.
- [ ] Prove R1 values are byte-identical before/after diagnostic evidence.
- [ ] Run Rust and focused Kaocha; commit as
  `feat(parser-rq): derive diagnostic gap partition`.

### Task 5: Complete Capture -> Partition -> Drift verification

**Files:**
- Create: `abc/test/fixtures/parser-rq/diagnostic-gap-capture/*`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/fixture_capture.rs`
- Modify: `abc/test/abc/tools/parser_rq_source_accountability_test.clj`

- [ ] Generate clean/vacuous, authorized-recovery, observe-only, opaque-unknown,
  and silent-gap fixtures through production capture paths.
- [ ] Mutate raw diagnostics, policy, source, generation, semantic gaps, and
  decision ledger separately; every mutation becomes unavailable through the
  manifest boundary.
- [ ] Regenerate twice byte-identically. Run crate tests, clippy, fmt, focused
  Clojure, schema drift, comment hygiene, and `just validate-migration`.
- [ ] Request review for raw-diagnostic bypass, circular authority, R1 mutation,
  and cross-work merging; commit as
  `test(parser-rq): drift-check diagnostic gap partition`.
