# P4B Parser Predicate Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement bounded, fail-closed instruments for diagnostic-envelope completeness and Parser-IR schema conformance, then compose their identity-bound observations without performing P5's authoritative capture, admission, or ADR promotion.

**Architecture:** Deepen `abc.tools.parser-rq-capture` as the single authentication and closed-membership authority. Predicate 4 is a pure Clojure projection over authenticated raw diagnostic bytes. Predicate 5 exposes the existing Rust converter's compiled schema-validator outcome through a qualification-only path while preserving the production API's hard abort. Independent analyzers derive independent envelopes; a small pure composer installs both after checking construction-time coherence, while the release gate remains the authoritative nine-observation backstop.

**Tech Stack:** Clojure/Malli/Kaocha/test.check, Rust/serde/jsonschema/Hegel, JSON Schema 2020-12, Python semantic-closure generator, shell smoke fixtures, Nix flakes.

## Global Constraints

- Follow `abc/docs/superpowers/specs/2026-07-17-parser-rq-predicate-hardening-design.md` exactly.
- Do not modify `abc/data/parser-release-qualification-predicates.edn`; P4B does not rotate predicate-set identity.
- Authenticated candidate defects are available failures; missing or unauthenticated evidence is unavailable.
- The shared Clojure protocol is the authoritative consumer-side implementation of blob authentication, exact membership, generation identity, envelope construction, and wire-status mapping.
- Rust may reject an incomplete producer workset defensively, but it must not become a second evidence-authentication authority.
- Predicate 4 never consumes P4A authorization or gap-disposition verdicts.
- Predicate 5 uses one generated value and one invocation of the existing compiled validator. Do not add a validator or re-run conversion.
- `PreparedConverter::convert` keeps its current fail-fast contract and error prefix; only the qualification API retains an invalid generated value.
- Full validation ledgers are immutable logical blobs; checked-in fixtures retain only bounded ledgers and witnesses.
- Use Hegel for Rust outcome and aggregation properties and `clojure.test.check` for the shared Clojure protocol properties.
- Stop before corpus-scale capture, final candidate pinning, registry admission, or ADR 0039 status change.
- Run comment hygiene and all language checks required by `/home/bor/Projects/soranoha/AGENTS.md`.

## File Structure

- `abc/src/abc/tools/parser_rq_capture.clj` owns shared transport/authentication invariants.
- `abc/src/abc/tools/parser_rq_diagnostic_completeness.clj` owns predicate-4 validation, arithmetic, and observation derivation.
- `abc/src/abc/tools/parser_rq_parser_ir_conformance.clj` owns predicate-5 record authentication, folding, and observation derivation.
- `abc/src/abc/tools/parser_rq_predicate_hardening.clj` owns pure two-envelope installation only.
- `abc/schemas/parser-rq-diagnostic-completeness-{policy,work,index,aggregate}.schema.json` own predicate-4 closed contracts.
- `abc/schemas/parser-rq-parser-ir-conformance-{policy,work,index,aggregate}.schema.json` own predicate-5 closed contracts.
- `ab-validator/crates/ab-aat-to-parser-ir/src/schema.rs` exposes structured errors from the existing compiled validator.
- `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs` owns the single-pass production/qualification outcome split.
- `ab-validator/crates/ab-aat-to-parser-ir/src/qualification.rs` owns predicate-5 record production and pure aggregation.
- `ab-validator/reports/parser-ir/predicate-hardening-identity.py` derives reviewed semantic-closure manifests and policy hashes.
- `abc/test/fixtures/parser-rq/predicate-hardening-capture/` contains the bounded Capture -> Derive -> Drift fixture.

---

### Task 1: Deepen the Shared Capture Protocol

**Files:**
- Modify: `abc/src/abc/tools/parser_rq_capture.clj`
- Modify: `abc/test/abc/tools/parser_rq_capture_test.clj`

**Interfaces:**

```clojure
(closed-membership-errors expected-work-ids records) ; => vector of strings
(capture-generation-ref generation-value)            ; => "sha256:..."
(observation-envelope identity-ref value details)     ; => closed envelope
(map-wire-status status-map json-status)              ; => value or unavailable result
```

`records` use `:work_id`; order is irrelevant. `status-map` is supplied by an authenticated instrument policy and must be a total one-to-one map over that policy's allowed statuses. The common module must not name either predicate's statuses.

- [ ] **Step 1: Add failing exact-membership tests**

Add `closed-membership-is-order-independent-test`, `closed-membership-rejects-omission-extra-and-duplicate-test`, and a test.check property over generated unique expected IDs, permutations, one omission, one extra, and one duplicate. Pin that only a permutation returns no errors.

```clojure
(is (empty? (capture/closed-membership-errors ["a" "b"]
                                              [{:work_id "b"} {:work_id "a"}])))
(is (seq (capture/closed-membership-errors ["a" "b"]
                                           [{:work_id "a"} {:work_id "a"}])))
```

- [ ] **Step 2: Add failing generation, envelope, and mapping tests**

Assert map-key order cannot change `capture-generation-ref`; changing one logical blob hash does. Assert `observation-envelope` includes `:value`, `:identity_ref`, and closed `:details`. Assert a known JSON status maps exactly and unknown, missing, duplicate-target, or non-total mappings yield `{:status :unavailable :reason :status-mapping-invalid}`.

- [ ] **Step 3: Run the focused tests and confirm red**

Run:

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-capture-test
```

Expected: FAIL with unresolved vars for the four interfaces.

- [ ] **Step 4: Implement the minimal generic operations**

Use sets only after detecting duplicate records. Hash generation values with `hash/sha256-json-jcs`. Extend the envelope contract with optional closed `:details`, preserving existing two-field envelopes. Return typed values; do not throw for hostile capture data. Keep instrument-specific enums and arithmetic out of this namespace.

- [ ] **Step 5: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-capture-test
git add abc/src/abc/tools/parser_rq_capture.clj \
  abc/test/abc/tools/parser_rq_capture_test.clj
git commit -m "feat(parser-rq): deepen shared capture protocol"
```

### Task 2: Freeze Independent Predicate Record Contracts

**Files:**
- Create: `abc/schemas/parser-rq-diagnostic-completeness-policy.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-completeness-work.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-completeness-index.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-completeness-aggregate.schema.json`
- Create: `abc/schemas/parser-rq-parser-ir-conformance-policy.schema.json`
- Create: `abc/schemas/parser-rq-parser-ir-conformance-work.schema.json`
- Create: `abc/schemas/parser-rq-parser-ir-conformance-index.schema.json`
- Create: `abc/schemas/parser-rq-parser-ir-conformance-aggregate.schema.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Closed unions:**

- Predicate 4 work status: `complete`, `invalid_diagnostic_envelope`, or `unavailable`.
- Predicate 4 aggregate status: `measured`, `invalid_diagnostic_envelope`, or `unavailable`.
- Predicate 5 work status: `schema_valid`, `schema_invalid`, `no_output`, or `unavailable`.
- Predicate 5 aggregate status: `measured`, `no_parser_ir_output`, or `unavailable`.

- [ ] **Step 1: Add failing schema-union tests**

Register all eight paths in `validate-design-bundle-test`. Add minimal positive fixtures plus negative cases: a complete diagnostic record without counts; invalid diagnostics without a ledger ref; vacuous aggregate with nonzero count; schema-invalid Parser-IR without output identity; no-output carrying output identity; unavailable records carrying candidate measurements; and indexes with duplicate record IDs.

- [ ] **Step 2: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.validate-design-bundle-test
```

Expected: FAIL because the eight schemas do not exist.

- [ ] **Step 3: Implement the eight closed schemas**

Every object uses `additionalProperties: false`. Both indexes require explicit `expected_work_ids` and records carrying `work_id` plus logical blob reference. Policy schemas bind algorithm version, exact expected membership hash, schema ID/hash, validator semantic-closure hash, and a closed `status_mapping`. Predicate 4 additionally binds vacuity semantics; predicate 5 binds generated-output and no-output semantics. Do not create concrete policies yet: their semantic closures do not exist until Tasks 3-6 land. Schema shape does not replace analyzer equality checks.

- [ ] **Step 4: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.validate-design-bundle-test
git add abc/schemas/parser-rq-diagnostic-completeness-*.schema.json \
  abc/schemas/parser-rq-parser-ir-conformance-*.schema.json \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-rq): define predicate hardening record contracts"
```

### Task 3: Expose One Structured Parser-IR Validation Outcome

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/schema.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/divergence.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**

```rust
pub enum ParserIrValidation {
    Valid,
    Invalid { errors: Vec<String> },
}

pub enum QualificationConversion {
    Valid(ConversionOutput),
    Invalid {
        parser_ir: Value,
        errors: Vec<String>,
    },
}

impl PreparedConverter {
    pub fn convert_for_qualification(
        &self,
        aat: Value,
        options: ConversionOptions,
    ) -> Result<QualificationConversion>;
}
```

- [ ] **Step 1: Characterize current production failure behavior**

Add `production-convert-still-hard-aborts-on-invalid-parser-ir`. Construct a test-only Parser-IR schema that rejects a known generated field, invoke `PreparedConverter::convert`, and pin that it returns an error beginning `parser-IR validation failed at` and exposes no `ConversionOutput`. In a module unit test compiled with the library's `cfg(test)`, add a bundle-invocation counter at `DivergenceRecorder::bundle`; assert the invalid production path does not invoke it. This pins the current validation boundary, not merely its error text.

- [ ] **Step 2: Add failing qualification-path tests**

Using the same rejecting schema, assert `convert_for_qualification` returns `QualificationConversion::Invalid` with the generated value and a nonempty sorted/deduplicated validation-error vector, but no divergence bundle. Add a validator-call test seam under `cfg(test)` and assert exactly one Parser-IR validation call for both the valid and invalid qualification cases. Assert valid qualification returns `QualificationConversion::Valid`, production and qualification valid outputs are byte-identical, and divergence bundling runs exactly once only for valid conversion.

- [ ] **Step 3: Confirm red**

```bash
nix develop ./ab-validator#checks --command cargo test \
  -p ab-aat-to-parser-ir --test integration qualification_
```

Expected: FAIL because `ParserIrValidation`, `QualificationConversion`, and `convert_for_qualification` are absent.

- [ ] **Step 4: Refactor validation without changing conversion timing**

Add `validation_errors(&jsonschema::Validator, &Value, &str) -> Vec<String>` in `schema.rs`; have `validate_compiled` adapt its first structured error back to the current `anyhow` prefix. Validate at the existing boundary: after the Parser-IR value is constructed but before `aat_meta`, `recorder.bundle`, or divergence-schema validation. On invalid, return `QualificationConversion::Invalid` immediately; the production adapter immediately restores the old error. On valid, construct the divergence bundle once and return `QualificationConversion::Valid`. Do not set `validate_output_parser_ir: false`, validate later, or execute new invalid-path work.

- [ ] **Step 5: Run all converter tests and commit**

```bash
nix develop ./ab-validator#checks --command cargo test -p ab-aat-to-parser-ir
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
git add ab-validator/crates/ab-aat-to-parser-ir/src/{schema.rs,convert.rs,divergence.rs,lib.rs} \
  ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat(parser-ir): expose qualification validation outcome"
```

### Task 4: Produce Predicate-5 Work Records and Aggregate

**Files:**
- Create: `ab-validator/crates/ab-aat-to-parser-ir/src/qualification.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/main.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/tests/qualification_properties.rs`

**Interfaces:**

```rust
pub fn qualify_work(request: QualificationRequest) -> Result<QualificationWorkRecord>;
pub fn aggregate_work_records(
    expected_work_ids: &[String],
    records: &[QualificationWorkRecord],
) -> Result<QualificationAggregate>;
```

CLI: `ab-aat-to-parser-ir qualify --aat ... --mapping ... --work-id ... --qualification-identity-ref ... --policy ... --parser-ir-out ... --ledger-out ... --record-out ...`.

- [ ] **Step 1: Add failing example tests**

Pin `schema_valid`, `schema_invalid`, and `no_output` work records. `schema_invalid` must retain Parser-IR logical identity, bounded witnesses, and full-ledger logical reference. `no_output` is used only when conversion fails before a Parser-IR value exists. A missing policy/schema/mapping/input is an execution error for the producer and later becomes unavailable at the capture boundary; it is not `no_output`.

- [ ] **Step 2: Add Hegel properties before implementation**

Add `hegeltest = "0.28"` as a dev dependency. Generate unique work IDs and statuses. Assert permutation invariance, exact rejection of omissions/extras/duplicates, arithmetic equality with a simple model, monotonic nondecrease of invalid count when valid becomes invalid, and `generated_outputs == valid + invalid`. Assert an all-`no_output` set yields JSON status `no_parser_ir_output`, never a numeric ratio.

- [ ] **Step 3: Confirm red**

```bash
nix develop ./ab-validator#checks --command cargo test \
  -p ab-aat-to-parser-ir --test qualification_properties
```

Expected: FAIL because the qualification module and dependency are absent.

- [ ] **Step 4: Implement record production and pure aggregation**

Stream-hash emitted Parser-IR and ledger bytes into `{sha256, bytes, media_type}` values. Cap inline witnesses at 20 stable sorted errors; keep all errors in the ledger blob. Producer aggregation uses the explicit expected list and returns an error on non-exact membership. It is a producer guard only; Task 6's Clojure analyzer independently authenticates bytes and membership.

- [ ] **Step 5: Add and test the CLI without changing `convert`**

The new subcommand calls `convert_for_qualification`; the existing `Convert` arm remains byte-for-byte behaviorally equivalent. Add an integration test invoking both commands on a valid fixture and comparing Parser-IR bytes, plus an invalid-schema fixture proving only `qualify` writes an invalid record.

- [ ] **Step 6: Verify and commit**

```bash
nix develop ./ab-validator#checks --command cargo test -p ab-aat-to-parser-ir
git add ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml \
  ab-validator/crates/ab-aat-to-parser-ir/src/{qualification.rs,lib.rs,main.rs} \
  ab-validator/crates/ab-aat-to-parser-ir/tests/qualification_properties.rs
git commit -m "feat(parser-rq): capture parser ir conformance records"
```

### Task 5: Implement Diagnostic Completeness Derivation

**Files:**
- Create: `abc/src/abc/tools/parser_rq_diagnostic_completeness.clj`
- Create: `abc/test/abc/tools/parser_rq_diagnostic_completeness_test.clj`

**Interfaces:**

```clojure
(derive-work policy qualification-identity-ref authenticated-diagnostic-bytes)
;; => {:record <work value> :ledger_blob {:ref <logical ref> :bytes <exact bytes>}}
(aggregate policy expected-work-ids work-records)
(derive-observation policy qualification-identity aggregate)
```

- [ ] **Step 1: Add failing valid and vacuous tests**

Assert a schema-valid diagnostic envelope counts all entries complete. Assert an authenticated empty `{ "schemaVersion": 3, "data": [] }` corpus produces the Clojure double value `1.0` and details `{:diagnostic_count 0 :works_with_diagnostics 0 :vacuous true}`. Assert adding or changing a P4A authorization value cannot change this result for unchanged raw bytes and policy.

- [ ] **Step 2: Add the disposable malformed-byte probe**

Supply authenticated `{ "schemaVersion": 3, "data": [` bytes. Assert `derive-work` returns `invalid_diagnostic_envelope` with bounded witnesses and a ledger ref; aggregate JSON status is the same string; observation value is `:invalid-diagnostic-envelope`; and the evaluator marks it available/fail.

- [ ] **Step 3: Add unavailable and membership tests**

Assert missing/authentication-failed bytes, policy/schema/validator/identity mismatch, and malformed index membership yield unavailable. Any unavailable expected work makes only predicate 4 unavailable. Assert no invalid artifact is mislabeled unavailable.

- [ ] **Step 4: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-diagnostic-completeness-test
```

Expected: FAIL because the namespace does not exist.

- [ ] **Step 5: Implement the pure projection**

Parse only the exact authenticated byte array returned by `authenticated-read`. Validate with the pinned raw diagnostic schema. For a valid envelope, `emitted == complete == count(data)`, so emit the double `1.0` directly; the inherited ratio unit is degenerate and no fractional value is reachable. For an empty valid set, emit the same `1.0` with explicit vacuity counts—never evaluate `0/0`. For malformed or schema-invalid authenticated bytes, return canonical complete-ledger bytes alongside the work record and their computed logical reference; the capture caller writes those exact bytes before publishing its index. Emit the failure sentinel rather than inventing a denominator. Use `capture/map-wire-status` for the JSON-to-Clojure mapping.

- [ ] **Step 6: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-diagnostic-completeness-test
git add abc/src/abc/tools/parser_rq_diagnostic_completeness.clj \
  abc/test/abc/tools/parser_rq_diagnostic_completeness_test.clj
git commit -m "feat(parser-rq): derive diagnostic completeness"
```

### Task 6: Authenticate and Derive Parser-IR Conformance

**Files:**
- Create: `abc/src/abc/tools/parser_rq_parser_ir_conformance.clj`
- Create: `abc/test/abc/tools/parser_rq_parser_ir_conformance_test.clj`

**Interfaces:**

```clojure
(authenticate-record store policy qualification-identity-ref index-entry)
(aggregate policy expected-work-ids authenticated-records)
(derive-observation policy qualification-identity aggregate)
```

- [ ] **Step 1: Add failing record-authentication tests**

Assert record, Parser-IR, and validation-ledger logical references are re-hashed. Assert `schema_invalid` is available only when its generated value and complete ledger authenticate. Assert `no_output` has neither output nor schema ledger. Assert a producer's claimed valid status with mismatched policy/schema/validator identity becomes unavailable, not failure.

- [ ] **Step 2: Add failing denominator and sentinel tests**

For `[valid, invalid, no_output]`, assert ratio `1/2` and exact counts `expected=3, generated=2, valid=1, invalid=1, no_output=1`. For all-valid records, assert the observation value is specifically the Clojure double `1.0`—not `1M`, an integer, Ratio, or string. For all `no_output`, assert value `:no-parser-ir-output` and available/fail. Assert removing a no-output work from the index is unavailable rather than a smaller passing denominator.

- [ ] **Step 3: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-parser-ir-conformance-test
```

Expected: FAIL because the namespace does not exist.

- [ ] **Step 4: Implement pure authentication and folding**

Authenticate the closed index, each work record, every generated Parser-IR blob, and required ledger before folding. Recompute all counts from authenticated work records; never trust aggregate metadata. Use exact integer numerator/denominator arithmetic internally, then emit a Clojure double; an all-valid result must be exactly the double `1.0` required by the live `:=` comparator. Counts remain the lossless authority disclosed alongside the value. Use `capture/map-wire-status` for `no_parser_ir_output`.

- [ ] **Step 5: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-parser-ir-conformance-test
git add abc/src/abc/tools/parser_rq_parser_ir_conformance.clj \
  abc/test/abc/tools/parser_rq_parser_ir_conformance_test.clj
git commit -m "feat(parser-rq): derive parser ir schema conformance"
```

### Task 7: Freeze Semantic Closures and Concrete Policies

**Files:**
- Create: `ab-validator/reports/parser-ir/predicate-hardening-identity.py`
- Create: `ab-validator/reports/parser-ir/tests/test_predicate_hardening_identity.py`
- Create generated: `ab-validator/data/parser-rq-diagnostic-completeness-validator-v1.json`
- Create generated: `ab-validator/data/parser-rq-parser-ir-conformance-validator-v1.json`
- Create: `abc/data/parser-rq-diagnostic-completeness-policy-v1.json`
- Create: `abc/data/parser-rq-parser-ir-conformance-policy-v1.json`
- Modify: `abc/test/abc/tools/parser_rq_diagnostic_completeness_test.clj`
- Modify: `abc/test/abc/tools/parser_rq_parser_ir_conformance_test.clj`

- [ ] **Step 1: Add failing semantic-closure tests**

Model the existing `publication-validator-identity.py` generator, but keep this generator predicate-neutral. Assert AST-derived transitive repository-local import closure equals the reviewed manifest, helper-byte mutation changes the semantic hash, an added/removed helper fails set equality, schema-byte mutation changes policy identity, and both policies have different IDs and hashes.

- [ ] **Step 2: Confirm red**

```bash
nix develop ./ab-validator#checks --command pytest -q \
  ab-validator/reports/parser-ir/tests/test_predicate_hardening_identity.py
```

Expected: FAIL because the generator and manifests are absent.

- [ ] **Step 3: Implement closure discovery and policy generation**

The diagnostic closure contains its now-existing Clojure analyzer, shared protocol, JSON-schema validation helper, and raw diagnostic schema. The Parser-IR closure contains `schema.rs`, `convert.rs`, `qualification.rs`, the relevant Cargo lock/toolchain coordinates, and Parser-IR schema. Derive actual owned-code imports and assert set equality with the reviewed path list; never merely hash a hand-maintained list. Runtime/library dependencies that cannot be represented as repository paths are explicit closed identity fields.

- [ ] **Step 4: Generate twice, compare, and install**

```bash
nix develop ./ab-validator#checks --command python \
  ab-validator/reports/parser-ir/predicate-hardening-identity.py \
  --repo-root . --instrument diagnostic-completeness --out /tmp/diag-1.json
nix develop ./ab-validator#checks --command python \
  ab-validator/reports/parser-ir/predicate-hardening-identity.py \
  --repo-root . --instrument diagnostic-completeness --out /tmp/diag-2.json
cmp /tmp/diag-1.json /tmp/diag-2.json
```

Repeat for `parser-ir-conformance`, then install the generated manifests and create the two policies from their exact identity fields. The policies use the bounded fixture workset identity for P4B; P5 creates final policies only after pinning the candidate and authoritative corpus. Compute JCS policy hashes with the generator, never by hand.

- [ ] **Step 5: Replace in-memory test policies and prove drift**

Load the committed policies in both analyzer suites. Assert each policy validates against its Task-2 schema and any semantic-closure, schema, expected-workset, or status-map mutation causes identity failure before derivation.

- [ ] **Step 6: Verify and commit**

```bash
nix develop ./ab-validator#checks --command pytest -q \
  ab-validator/reports/parser-ir/tests/test_predicate_hardening_identity.py
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-diagnostic-completeness-test \
  --focus abc.tools.parser-rq-parser-ir-conformance-test
git add ab-validator/reports/parser-ir/predicate-hardening-identity.py \
  ab-validator/reports/parser-ir/tests/test_predicate_hardening_identity.py \
  ab-validator/data/parser-rq-*-validator-v1.json \
  abc/data/parser-rq-*-policy-v1.json \
  abc/test/abc/tools/parser_rq_diagnostic_completeness_test.clj \
  abc/test/abc/tools/parser_rq_parser_ir_conformance_test.clj
git commit -m "feat(parser-rq): bind predicate hardening semantics"
```

### Task 8: Compose Without Replacing Gate Coherence

**Files:**
- Create: `abc/src/abc/tools/parser_rq_predicate_hardening.clj`
- Create: `abc/test/abc/tools/parser_rq_predicate_hardening_test.clj`
- Modify: `abc/test/abc/tools/parser_release_qualification_test.clj`

**Interface:**

```clojure
(install-observations measurements qualification-identity
                      diagnostic-result parser-ir-result)
```

- [ ] **Step 1: Add failing construction-boundary tests**

Assert installation succeeds only for two closed results with the exact full target identity, exact corpus membership, distinct expected policy identities, and absent target keys. Reject scalar inputs, overwrite, mixed candidate/corpus revisions, swapped policies, one unavailable result, and unknown fields.

- [ ] **Step 2: Pin available typed failures explicitly in the evaluator**

Add `typed-failure-sentinels-are-available-failures-test` to `parser_release_qualification_test.clj`. For predicates 4 and 5, assert `:invalid-diagnostic-envelope` and `:no-parser-ir-output` yield `:fail`, not `:unavailable`. Add `parser-ir-all-valid-double-passes-exact-comparator-test`: install predicate 5's derived all-valid envelope, assert `(double? (:value envelope))`, and run the live predicate through `evaluate-predicate` to obtain `:pass`. These tests must pin both sides of the evaluator contract rather than rely on incidental coercion or the current unavailable allowlist.

- [ ] **Step 3: Pin the defense-in-depth boundary**

Construct an incoherent measurement map without the composer and assert `coherent-observations?` and `build-report` reject it. Separately assert the composer rejects the same mixture earlier. Name the composer a construction guard and the gate the release authority in docstrings.

- [ ] **Step 4: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-predicate-hardening-test \
  --focus abc.tools.parser-release-qualification-test
```

Expected: FAIL because the composer and explicit sentinel contract are absent.

- [ ] **Step 5: Implement the minimal immutable installer**

Return a new measurements map. Do not execute commands, open files, calculate ratios, inspect the registry, or call admission. Reuse shared envelope validation and the full `qualification-identity-ref`; do not duplicate gate admission projection logic.

- [ ] **Step 6: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-predicate-hardening-test \
  --focus abc.tools.parser-release-qualification-test
git add abc/src/abc/tools/parser_rq_predicate_hardening.clj \
  abc/test/abc/tools/parser_rq_predicate_hardening_test.clj \
  abc/test/abc/tools/parser_release_qualification_test.clj
git commit -m "feat(parser-rq): compose predicate hardening observations"
```

### Task 9: Prove Capture, Derive, Drift and Close P4B

**Files:**
- Create: `abc/test/fixtures/parser-rq/predicate-hardening-capture/manifest.json`
- Create: bounded files under `abc/test/fixtures/parser-rq/predicate-hardening-capture/store/`
- Create: `ab-validator/tests/parser-rq-predicate-hardening-capture-smoke.sh`
- Modify: `ab-validator/flake.nix`
- Modify: `abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md`
- Modify: `abc/docs/superpowers/specs/2026-07-17-parser-rq-predicate-hardening-design.md`

- [ ] **Step 1: Add a failing three-work capture smoke**

Use an explicit three-work fixture workset: one clean empty-diagnostic/valid-IR work, one nonempty-diagnostic/valid-IR work, and one fault-injected invalid-IR characterization work. The script invokes the real diagnostic producer and `ab-aat-to-parser-ir qualify`, writes immutable blobs, closes independent indexes, and derives both aggregates. It must not discover membership from the output directory.

- [ ] **Step 2: Add byte-identical drift tests**

Regenerate the bounded capture into a temporary directory and compare canonical manifest, indexes, records, ledgers, aggregates, and composed measurements byte-for-byte with the committed fixture. Normalize no timestamps or machine paths because those fields must not enter evidence identity.

- [ ] **Step 3: Register the pure Nix check**

Add `parser-rq-predicate-hardening-capture-smoke` to `ab-validator/flake.nix`. Its sandbox inputs include only repository fixtures and required binaries. It performs no Hinoki access and no registry mutation.

- [ ] **Step 4: Run focused cross-language verification**

```bash
nix develop ./ab-validator#checks --command cargo test -p ab-aat-to-parser-ir
nix develop ./ab-validator#checks --command pytest -q \
  ab-validator/reports/parser-ir/tests/test_predicate_hardening_identity.py
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-capture-test \
  --focus abc.tools.parser-rq-diagnostic-completeness-test \
  --focus abc.tools.parser-rq-parser-ir-conformance-test \
  --focus abc.tools.parser-rq-predicate-hardening-test \
  --focus abc.tools.parser-release-qualification-test
nix build ./ab-validator#checks.x86_64-linux.parser-rq-predicate-hardening-capture-smoke
```

Expected: all exit 0; drift comparison is silent.

- [ ] **Step 5: Update the design evidence ledger and roadmap honestly**

Mark each architecture-review item `implemented and verified` only beside its actual green test name. Record P4B implemented/drift-checked in the campaign roadmap. State explicitly that predicate set, authoritative measurements, registry, and ADR 0039 remain untouched and P5 is still required.

- [ ] **Step 6: Run repository verification**

```bash
scripts/comment-hygiene-check.sh
just python-quality
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
just validate-migration
```

Expected: every command exits 0. If a pre-existing unrelated failure occurs, record it with the exact command and output; do not weaken P4B checks.

- [ ] **Step 7: Prove non-goals and commit**

```bash
git diff --exit-code HEAD -- \
  abc/data/parser-release-qualification-predicates.edn \
  abc/data/aat-parser-ir-compatibility.edn \
  abc/docs/adr/0039-custom-parser-release-qualification.md \
  abc/docs/reports/parser-release-qualification-measurements.edn
git add abc/test/fixtures/parser-rq/predicate-hardening-capture \
  ab-validator/tests/parser-rq-predicate-hardening-capture-smoke.sh \
  ab-validator/flake.nix \
  abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md \
  abc/docs/superpowers/specs/2026-07-17-parser-rq-predicate-hardening-design.md
git commit -m "test(parser-rq): prove predicate hardening drift"
```

## Self-Review Checklist

- Every acceptance criterion in the approved design maps to a named test or verification step above.
- No step asks an implementer to choose an enum, ownership boundary, denominator, identity primitive, or command interface.
- The Parser-IR refactor has a disconfirming condition: a second conversion, second validator pass, or changed production abort stops execution and reopens design.
- The shared protocol remains generic; instrument semantics stay in their owning namespaces.
- Rust producer membership checks do not replace consumer-side Clojure authentication.
- Both typed sentinels are explicitly pinned as available failures at the live evaluator.
- P4A, P2, registry admission, final capture, and ADR promotion are absent from the trust path and the diff.
- Generated paths are identified as generated, corpus-scale artifacts stay out of Git, and fixture membership is closed and bounded.
