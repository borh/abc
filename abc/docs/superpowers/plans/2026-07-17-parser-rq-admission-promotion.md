# P5 Parser Admission and Promotion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Bind the remaining release instruments, freeze one reproducible custom-parser candidate, publish its sole authorized nine-predicate capture, resolve exact registry admission, and conditionally promote ADR 0039 from committed evidence.

**Architecture:** Add a closed Capture -> Derive instrument for predicates 1, 7, and 9; then deepen the existing qualification gate as the sole owner of admission membership and full-evidence conflict precedence. A pure campaign module authenticates candidate, capture, evaluation, evidence integrity, and canonical-projection values without executing parsers or deciding verdicts. Implementation and bounded fixtures finish before candidate freeze; hinoki then performs one pre-authorized capture, an independent full-corpus audit, registry evaluation, and governance transition.

**Tech Stack:** Clojure/Malli/Kaocha/test.check, Python/pytest, JSON Schema 2020-12, Nix flakes, `ab-check`, GNU `time(1)`, Git detached worktrees, JCS SHA-256, ADR evidence governance.

## Global Constraints

- Follow `abc/docs/superpowers/specs/2026-07-17-parser-rq-admission-promotion-design.md` exactly.
- Work on `main`; make reviewable commits. Push the instrument-binding ADR before candidate freeze and push the capture authorization before its window opens.
- The candidate is the last implementation commit, not later evidence or governance HEAD.
- Finish all code, schema, policy, corpus, predicate, and instrument changes before freeze. Any such change afterward requires a new candidate.
- Accept the instrument-binding ADR before freeze. ADR 0040 stays Proposed until the fresh capture satisfies its evidence criterion.
- Authorize exactly one volatile capture with ordinal `1`; do not retry or select a favorable generation.
- Run exactly three serial core repetitions and reduce fatal failures, wall time, and timeouts by maximum.
- Hold the exclusive hinoki campaign lock across all volatile lanes; core and resource executions are serial.
- Require two clean Nix rebuilds with equal output/NAR identities and executable bytes before authorization.
- Require every release-relevant value to be committed, pinned, or manifest-addressed below the configured evidence store. No result may depend on an undeclared external place or service.
- A full-entry conflict overrides nine-field `compatible?` membership. No caller supplies an admission boolean.
- Do not hand-key an observation, rewrite a registry row, weaken a predicate, or promote ADR 0039 from a non-qualified report.
- Corpus-scale execution runs only on `hinoki.hyakutake-barbel.ts.net`; bounded synthetic fixtures remain local.
- Honest `fail`, `conflict`, and `unavailable` outcomes are successful campaign records and leave ADR 0039 Proposed.
- Run comment hygiene and every language check required by `/home/bor/Projects/soranoha/AGENTS.md`.
- Invoke `just validate-migration` without a caller-local `NIX_CONFIG`; the root
  recipe owns the release gate's explicit uncached Nix-evaluation policy.

## File Structure

- `abc/schemas/parser-rq-core-attempt-{policy,work,index,aggregate}.schema.json` own the core attempt protocol.
- `ab-validator/reports/parser-ir/parser-rq-core-attempt-capture.py` owns execution and raw record production only.
- `abc/src/abc/tools/parser_rq_core_attempt.clj` authenticates core records and derives predicates 1, 7, and 9.
- `abc/schemas/parser-rq-{candidate,executable-provenance,capture-authorization,capture-index,evaluation-index,evidence-integrity-receipt}.schema.json` own campaign values.
- `abc/src/abc/tools/parser_rq_campaign.clj` owns pure campaign composition, generation selection, canonical projections, and promotion eligibility.
- `abc/src/abc/tools/parser_release_qualification.clj` remains the sole gate, admission, and verdict authority.
- `ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py` verifies builds, executable identities, and stored evidence bytes.
- `abc/data/parser-rq-core-attempt-policy-v1.json` binds repetition, command, timing, lock, and semantic identity.
- `abc/test/fixtures/parser-rq/admission-promotion/` is the bounded Capture -> Derive -> Drift fixture.
- `abc/docs/adr/0041-parser-release-instrument-bindings.md` governs the final instrument descriptions and predicate-set rotation.
- `abc/docs/reports/parser-rq/runs/` receives only the authoritative post-freeze campaign.

---

### Task 1: Freeze Closed Core and Campaign Contracts

**Files:**
- Create: `abc/schemas/parser-rq-core-attempt-policy.schema.json`
- Create: `abc/schemas/parser-rq-core-attempt-work.schema.json`
- Create: `abc/schemas/parser-rq-core-attempt-index.schema.json`
- Create: `abc/schemas/parser-rq-core-attempt-aggregate.schema.json`
- Create: `abc/schemas/parser-rq-candidate.schema.json`
- Create: `abc/schemas/parser-rq-executable-provenance.schema.json`
- Create: `abc/schemas/parser-rq-capture-authorization.schema.json`
- Create: `abc/schemas/parser-rq-capture-index.schema.json`
- Create: `abc/schemas/parser-rq-evaluation-index.schema.json`
- Create: `abc/schemas/parser-rq-evidence-integrity-receipt.schema.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Closed contracts:**

```clojure
{:authorization_ordinal 1
 :candidate_ref "sha256:..."
 :not_before_utc "...Z"
 :not_after_utc "...Z"
 :repetitions 3
 :reduction :maximum}

{:capture_generation_ref "sha256:..."
 :authorization_ref "sha256:..."
 :candidate_ref "sha256:..."
 :members {:core_attempt "sha256:..." :source_recognition "sha256:..." ...}}
```

- [ ] **Step 1: Add failing schema tests**

Register all ten schemas. Add positive minimal values and negative cases for unknown fields, wrong ordinal, a repetition count other than three, a reduction other than maximum, non-closed capture/evaluation membership, duplicate logical blobs, mismatched candidate references, and unavailable records carrying trusted totals.

- [ ] **Step 2: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.validate-design-bundle-test
```

Expected: FAIL because the ten schemas are absent.

- [ ] **Step 3: Implement closed schemas**

Use `additionalProperties: false` for every object. Use tagged unions for `measured`, `protocol_error`, and `unavailable`. Require logical blob identity as SHA-256, bytes, media type, and relative locator. The capture index has exactly seven named aggregate members plus `measurements`; the evaluation index has exactly the capture, registry hash, admission candidate/report, and qualification report.

- [ ] **Step 4: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.validate-design-bundle-test
git add abc/schemas/parser-rq-*.schema.json \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-rq): define admission campaign contracts"
```

### Task 2: Capture Three Closed Core Repetitions

**Files:**
- Create: `ab-validator/reports/parser-ir/parser-rq-core-attempt-capture.py`
- Create: `ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py`
- Modify: `abc/flake.nix`

**Interfaces:**

```python
def parse_elapsed_record(raw: bytes) -> float: ...
def classify_work(report: dict[str, object]) -> str: ...
def validate_execution_window(now: datetime, authorization: dict[str, object]) -> None: ...
def capture_repetitions(config: CaptureConfig) -> dict[str, object]: ...
```

- [ ] **Step 1: Add failing unit tests**

Pin `parsed`, `fatal_error`, `adapter_timeout`, and `protocol_error`. Reject empty, multiple-line, negative, non-finite, exponent, comma-decimal, and trailing-junk timing bytes. Require exactly three complete repetitions, identical closed work membership, serial invocation, a retained lock, and execution inside the committed window.

- [ ] **Step 2: Confirm red**

```bash
nix develop ./abc --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py
```

Expected: FAIL because the capture module is absent.

- [ ] **Step 3: Implement execution-only capture**

Invoke the provided argv without a shell. Wrap each `ab-check` command with the Nix-resolved GNU time executable using `-f %e`; pass `-o` the repetition record's staging path. Acquire a nonblocking file lock supplied by runtime configuration, enumerate competing parser-related user units before and after each repetition, and record load and memory pressure. Never derive a predicate verdict in Python.

- [ ] **Step 4: Add the Nix check and verify**

Add `parser-rq-core-attempt-python-tests` to `abc/flake.nix` and include the module in `just python-quality`'s tracked Python set if that set is explicit.

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-core-attempt-python-tests
just python-quality
git add ab-validator/reports/parser-ir/parser-rq-core-attempt-capture.py \
  ab-validator/reports/parser-ir/test_parser_rq_core_attempt_capture.py abc/flake.nix
git commit -m "feat(parser-rq): capture closed core attempts"
```

### Task 3: Derive Predicates 1, 7, and 9

**Files:**
- Create: `abc/src/abc/tools/parser_rq_core_attempt.clj`
- Create: `abc/test/abc/tools/parser_rq_core_attempt_test.clj`
- Create: `abc/data/parser-rq-core-attempt-policy-v1.json`
- Modify: `abc/flake.nix`

**Interfaces:**

```clojure
(authenticate-index policy candidate index blob-reader) ; => value or unavailable
(derive-aggregate authenticated)                         ; => aggregate
(observation-envelopes candidate aggregate)             ; => exactly three envelopes
```

- [ ] **Step 1: Add failing authentication and reduction tests**

Test missing, extra, duplicate, cross-candidate, bad blob hash, incomplete repetition, unknown disposition, malformed elapsed value, and command failure. Test record-order independence and maximum reduction:

```clojure
(is (= {:fatal_failures 2.0 :wall_time_seconds 12.5 :timeouts 1.0}
       (select-keys (core/derive-aggregate authenticated)
                    [:fatal_failures :wall_time_seconds :timeouts])))
```

Assert all emitted values are Clojure doubles and pass/fail correctly through the live `evaluate-predicate` comparators.

- [ ] **Step 2: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-core-attempt-test
```

- [ ] **Step 3: Implement pure authentication and derivation**

Reuse `abc.tools.parser-rq-capture` for streaming blob verification, closed membership, generation hashing, and envelopes. A protocol error or incomplete repetition makes all three projections unavailable. Fatal errors and timeouts remain available numeric values.

- [ ] **Step 4: Generate the policy from reviewed values**

The policy binds repetition count `3`, reduction `maximum`, timeout, job count, lock semantics, GNU-time identity, argv template, allowed dispositions, schemas, and the Python/Clojure semantic-closure hashes. Generate it deterministically; do not type a digest.

- [ ] **Step 5: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-core-attempt-test
nix build ./abc#checks.x86_64-linux.clj-kondo
git add abc/src/abc/tools/parser_rq_core_attempt.clj \
  abc/test/abc/tools/parser_rq_core_attempt_test.clj \
  abc/data/parser-rq-core-attempt-policy-v1.json abc/flake.nix
git commit -m "feat(parser-rq): derive core release observations"
```

### Task 4: Make Full-Evidence Conflict a Gate Precondition

**Files:**
- Modify: `abc/src/abc/tools/parser_release_qualification.clj`
- Modify: `abc/test/abc/tools/parser_release_qualification_test.clj`
- Modify: `abc/src/abc/tools/aat_parser_ir_compat.clj`
- Modify: `abc/test/abc/tools/aat_parser_ir_compat_test.clj`

**Interface:**

```clojure
(admission-resolution registry identity admission-candidate)
;; => {:status :admitted|:unadmitted|:conflict|:invalid
;;     :query {...} :report {...}}
```

- [ ] **Step 1: Add failing relation tests**

Pin that `compatible?` uses only the nine `match-keys`, while `admission-report` requires byte-equal complete entries. Add cases for admitted, missing, conflict, invalid candidate, and invalid registry.

- [ ] **Step 2: Add the gate regression**

Construct nine passing coherent observations and a registry row whose nine fields match but whose evidence differs. Call `build-report` with the authenticated admission candidate and assert:

```clojure
(is (= :conflict (get-in report [:admission :status])))
(is (= :not-qualified (:gate_status report)))
(is (= "Proposed" (:adr_0039_status report)))
```

Also prove no candidate, malformed candidate, and missing row are not qualified. Preserve the existing unadmitted all-pass regression.

- [ ] **Step 3: Confirm red**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-release-qualification-test
```

Expected: conflict currently collapses to admitted because `build-report` only calls `admitted?`.

- [ ] **Step 4: Implement one gate-owned resolution**

Extend `build-report` and `report-from-bundle` to require `:admission_candidate`. Compute membership and strict report inside the gate. Expand the report schema admission enum to `:admitted`, `:unadmitted`, `:conflict`, and `:invalid`. Only membership true plus strict `:admitted` satisfies the precondition. Do not accept a precomputed status or boolean.

Add `append-missing` to `abc.tools.aat-parser-ir-compat`: it returns a new registry only when the strict report contains missing entries and no conflict/error, appends the exact candidates in deterministic order, and preserves every existing entry byte-for-value. Extend its CLI with `--append-out`; it never edits the input path and refuses admitted, conflict, or invalid input.

- [ ] **Step 5: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-release-qualification-test
git add abc/src/abc/tools/parser_release_qualification.clj \
  abc/test/abc/tools/parser_release_qualification_test.clj \
  abc/src/abc/tools/aat_parser_ir_compat.clj \
  abc/test/abc/tools/aat_parser_ir_compat_test.clj
git commit -m "fix(parser-rq): give admission conflicts gate precedence"
```

### Task 5: Implement Pure Campaign Composition and Generation Rules

**Files:**
- Create: `abc/src/abc/tools/parser_rq_campaign.clj`
- Create: `abc/test/abc/tools/parser_rq_campaign_test.clj`
- Modify: `abc/deps.edn`

**Interfaces:**

```clojure
(candidate-ref candidate)                          ; => sha256 value
(authorization-ref authorization)                  ; => sha256 value
(capture-generation-ref capture-index)             ; => sha256 value
(evaluation-generation-ref evaluation-index)       ; => sha256 value
(compose-measurements candidate capture members)   ; => exact nine envelopes
(verify-authorization candidate authorization now) ; => errors
(resolve-capture candidate-dir)                     ; => unique authorized capture
(resolve-current-evaluation candidate-dir registry); => unique current-registry evaluation
(promotion-errors inputs)                          ; => closed error vector
```

CLI subcommands: `candidate`, `candidate-ref`, `qualification-identity-ref`,
`authorize`, `authorization-ref`, `verify-authorization-record`,
`verify-authorization`, `runtime-inputs`, `compose`, `capture-ref`,
`verify-capture`, `evaluate`, `publish-evaluation`, `project`,
`verify-registry-closure`, and `verify-promotion`. Each reads explicit paths and
writes deterministic EDN/JSON; none executes a parser or edits governance.

- [ ] **Step 1: Add failing candidate and authorization tests**

Recompute `qualification_identity_ref`; reject missing/extra keys, a ref disagreement, ordinal other than one, wrong candidate, bad interval, execution outside the interval, and more than one authorization. Prove evidence HEAD may differ from `parser_git_rev`.

- [ ] **Step 2: Add failing composition tests**

Install the exact observed keys from the live predicate contract. Reject a scalar bypass, missing/extra/duplicate key, cross-candidate envelope, unavailable aggregate carrying trusted totals, policy/schema/corpus mismatch, and overwrite. Assert composition does not read the registry or evaluate predicates.

- [ ] **Step 3: Add failing generation-selection tests**

Enumerate directories instead of accepting a selected child path. Require exactly one capture bearing the sole authorization and exactly one evaluation whose `registry_ref` equals the current registry's logical hash. Reject sibling captures and zero/multiple current evaluations. Verify canonical measurement/report bytes equal the resolved immutable member bytes.

- [ ] **Step 4: Add failing promotion tests**

Reject stale registry, failed/unavailable predicate, incoherence, any non-admitted status, missing/extra predicate, non-reproducible provenance, absent or invalid evidence-integrity receipt, sibling capture, non-Accepted ADR 0040 or 0041, and canonical drift. A positive fixture has exactly nine passes, admitted coherence, and Accepted projections.

- [ ] **Step 5: Confirm red and implement**

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-campaign-test
```

Hash with JCS after removing the self-reference field. Authenticate every referenced member before composition. Keep filesystem enumeration, canonical copying, and CLI parsing at the shell; keep identity, selection, and promotion decisions pure.

- [ ] **Step 6: Register CLI and verify**

Add `:abc/parser-rq-campaign {:main-opts ["-m" "abc.tools.parser-rq-campaign"]}`.

```bash
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-campaign-test
nix build ./abc#checks.x86_64-linux.clj-kondo
git add abc/src/abc/tools/parser_rq_campaign.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj abc/deps.edn
git commit -m "feat(parser-rq): compose immutable campaign generations"
```

### Task 6: Verify Reproducible Executables and Stored Evidence

**Files:**
- Create: `ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py`
- Create: `ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py`
- Modify: `abc/src/abc/tools/parser_rq_campaign.clj`
- Modify: `abc/test/abc/tools/parser_rq_campaign_test.clj`
- Modify: `abc/flake.nix`

**Interfaces:**

```python
def compare_builds(first: dict[str, object], second: dict[str, object]) -> dict[str, object]: ...
def executable_record(path: Path, nix_output: dict[str, object], argv: list[str]) -> dict[str, object]: ...
def verify_evidence(blobs: list[LogicalBlob], root: Path) -> dict[str, object]: ...
```

- [ ] **Step 1: Add failing provenance tests**

Reject differing derivations, output/NAR hashes, executable lengths/digests, baked Git revision, adapter coordinates, or argv. Pin streaming reads; mutation after metadata collection must fail re-hash.

- [ ] **Step 2: Add failing evidence-integrity tests**

Use one temporary content store. Reject an offline root, missing or wrong bytes, unexpected media type, locator escape, duplicate membership, and a receipt whose listed identities differ from the manifests. Re-hash bytes by streaming rather than trusting metadata.

- [ ] **Step 3: Implement and integrate**

Python emits evidence values only. Clojure authenticates their closed schemas and requires reproducible provenance plus a verified integrity receipt in `promotion-errors`; it does not trust status strings without recomputing member comparisons.

- [ ] **Step 4: Add Nix checks and commit**

```bash
nix develop ./abc --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py
nix build ./abc#checks.x86_64-linux.parser-rq-campaign-provenance-python-tests
just python-quality
nix develop ./abc --command abc/bin/kaocha --focus abc.tools.parser-rq-campaign-test
git add ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py \
  ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py \
  abc/src/abc/tools/parser_rq_campaign.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj abc/flake.nix
git commit -m "feat(parser-rq): verify reproducible evidence"
```

### Task 7: Prove the Bounded End-to-End Transaction

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/main.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/main.rs`
- Modify: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/Cargo.toml`
- Create: `ab-validator/reports/parser-ir/parser-rq-predicate-hardening-capture.py`
- Create: `ab-validator/reports/parser-ir/test_parser_rq_predicate_hardening_capture.py`
- Create: `abc/test/fixtures/parser-rq/admission-promotion/`
- Create: `abc/test/abc/tools/parser_rq_admission_promotion_drift_test.clj`
- Create: `abc/bin/parser-rq-campaign-capture.sh`
- Create: `abc/bin/parser-rq-admission-promotion-smoke.sh`
- Modify: `abc/flake.nix`

- [ ] **Step 1: Build a synthetic nine-predicate fixture**

Use bounded blobs and a fixture identity. Include three different core repetitions whose maxima are not all in the same repetition, all other instrument aggregates, one closed evidence store, an admission candidate, missing and admitted registries, immutable capture/evaluation indexes, canonical projections, and a positive report.

- [ ] **Step 2: Expose the deferred production corpus entrypoints**

Add `capture-corpus` CLIs to the two existing Rust crates; they accept an explicit corpus index, candidate identity ref, policies, evidence store, and output directory. Source accountability invokes the existing fused classified-source capture and R1 fold once per indexed source. Diagnostic authorization consumes those authenticated R1 records plus the predicate-hardening producer's shared raw diagnostic blobs and invokes the existing R2 partition. Neither CLI discovers files from an output directory or reimplements library arithmetic.

Add the Python predicate-hardening producer by extracting the bounded smoke's production path: for every explicit corpus row, invoke the candidate `ab-aozora --mode diagnostics` and `ab-aat-to-parser-ir qualify` once, store diagnostic/Parser-IR/ledger bytes by logical identity, and emit the two closed indexes. Unknown status or missing output is recorded through the implemented status algebra.

Test all three adapters on the bounded corpus and add mutation tests for omitted, extra, reordered, and cross-candidate members. Verify the Rust CLIs with:

```bash
nix develop ./ab-validator --command cargo test \
  --manifest-path ab-validator/Cargo.toml \
  -p ab-parser-rq-source-accountability \
  -p ab-parser-rq-diagnostic-authorization
nix develop ./abc --command pytest -q \
  ab-validator/reports/parser-ir/test_parser_rq_predicate_hardening_capture.py
```

- [ ] **Step 3: Add mutation and drift tests**

Mutate every identity/ref, remove each capture member, add a sibling capture, change the registry without reevaluation, and create nine-field/full-entry conflict. Regenerate all pure outputs into a temporary directory and compare bytes with the fixture.

- [ ] **Step 4: Add the explicit production orchestration map and smoke transaction**

Add a thin production orchestrator that accepts all runtime paths explicitly, verifies authorization before starting, and implements this fixed serial map:

1. `parser-rq-core-attempt-capture.py` produces the three core repetitions;
2. `ab-parser-rq-source-accountability capture-corpus` produces source capture, ledger, recognition, and R1 records;
3. `parser-rq-predicate-hardening-capture.py` captures one raw diagnostic stream per work and produces predicate-4/5 records;
4. `ab-parser-rq-diagnostic-authorization capture-corpus` consumes step 2 plus those same authenticated diagnostic blobs and produces R2 records;
5. `abc.tools.materialize-publication` materializes each explicitly indexed successful work from step 3's Parser-IR, then `publication-rq-capture.py` captures the resulting publication and preservation artifacts;
6. `parser-rq-resource-capture.py` runs last, one work at a time, under the transient-service policy; and
7. the existing pure Clojure analyzers derive their seven aggregates before the campaign composer installs exactly nine envelopes.

The orchestrator retains one lock across the map. It may move staged artifacts by content identity but may not discover membership, decide admission, edit the registry, or retry. Assert each producer receives the candidate-built executable paths from authenticated provenance, never a `PATH` lookup. The shell smoke runs `candidate -> authorize -> capture -> compose -> evaluate -> project -> verify-promotion` entirely in temporary directories. It must exercise the missing-registry evaluation followed by an admitted evaluation without changing capture bytes.

- [ ] **Step 5: Verify and commit**

```bash
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-rq-admission-promotion-drift-test
bash abc/bin/parser-rq-admission-promotion-smoke.sh
nix build ./abc#checks.x86_64-linux.parser-rq-admission-promotion-smoke
git add abc/test/fixtures/parser-rq/admission-promotion \
  abc/test/abc/tools/parser_rq_admission_promotion_drift_test.clj \
  ab-validator/crates/ab-parser-rq-source-accountability/src/main.rs \
  ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml \
  ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/main.rs \
  ab-validator/crates/ab-parser-rq-diagnostic-authorization/Cargo.toml \
  ab-validator/reports/parser-ir/parser-rq-predicate-hardening-capture.py \
  ab-validator/reports/parser-ir/test_parser_rq_predicate_hardening_capture.py \
  abc/bin/parser-rq-campaign-capture.sh \
  abc/bin/parser-rq-admission-promotion-smoke.sh abc/flake.nix
git commit -m "test(parser-rq): prove admission promotion transaction"
```

### Task 8: Govern the Final Instrument Bindings

**Files:**
- Create: `abc/docs/adr/0041-parser-release-instrument-bindings.md`
- Modify: `abc/data/parser-release-qualification-predicates.edn`
- Modify: `abc/test/abc/tools/parser_release_qualification_test.clj`
- Modify: `abc/docs/adr/adr-evidence.edn`
- Create: focused ADR-0041 evidence under the paths generated by `abc.tools.adr-evidence-capture`

- [ ] **Step 1: Add a predicate-rotation regression**

Copy the current predicate value in the test. Change only instruments for predicates 1, 2, 3, 7, and 9 to their implemented versioned producers. Assert all nine predicate IDs, dimensions, observed keys, units, comparators, and thresholds are byte-equal before/after projection, while `predicate_set_hash` changes and recomputes correctly. Update shared test fixtures to the generated hash; do not type it twice.

- [ ] **Step 2: Write ADR 0041**

Record the exact old/new instrument strings, the three-repetition maximum policy for the core attempt, the bounded schema/analyzer/smoke evidence, and that old observations remain historical. Acceptance must precede candidate freeze. State that any semantic predicate change aborts P5.

- [ ] **Step 3: Apply the bindings and generate the hash**

Use the repository hash function from `abc.tools.parser-release-qualification` to write `:predicate_set_hash`; do not manually calculate or paste it into code. Assert only the intended fields changed:

```bash
git diff --word-diff=plain -- abc/data/parser-release-qualification-predicates.edn
nix develop ./abc --command abc/bin/kaocha \
  --focus abc.tools.parser-release-qualification-test \
  --focus abc.tools.parser-rq-core-attempt-test
```

- [ ] **Step 4: Capture and register bounded evidence**

Run the real core schema, analyzer, and smoke checks through the existing ADR evidence capture/register workflow. Regenerate the ADR catalogs and require governance validation:

```bash
just check-no-build
just phase5-checkpoint
just monorepo-adr-governance
just validate-migration
```

- [ ] **Step 5: Accept ADR 0041 and commit**

Only after its registered evidence passes, set its status/date and recapture the governance snapshot. Verify ADR 0040 and ADR 0039 are still Proposed.

```bash
rg -n '^Status:' abc/docs/adr/0039-custom-parser-release-qualification.md \
  abc/docs/adr/0040-process-tree-memory-qualification.md \
  abc/docs/adr/0041-parser-release-instrument-bindings.md
git add abc/docs/adr/0041-parser-release-instrument-bindings.md \
  abc/data/parser-release-qualification-predicates.edn \
  abc/test/abc/tools/parser_release_qualification_test.clj \
  abc/docs/adr/adr-evidence.edn
# Add only the exact catalog, descriptor, and run paths printed by the capture/register commands.
git commit -m "docs(adr): accept parser release instrument bindings"
git push origin main
```

### Task 9: Establish the Pre-Freeze Verification Boundary

**Files:**
- Modify only files required by failures found before freeze.

- [ ] **Step 1: Run all focused tests from clean inputs**

```bash
nix build ./ab-validator#checks.x86_64-linux.parser-rq-core-attempt-python-tests
nix build ./ab-validator#checks.x86_64-linux.parser-rq-campaign-provenance-python-tests
nix build ./abc#checks.x86_64-linux.parser-rq-admission-promotion-smoke
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
just python-quality
just nix-format-check
scripts/comment-hygiene-check.sh
just validate-migration
```

- [ ] **Step 2: Review the complete implementation diff**

Confirm no active code contains `/db/`, untracked `references/`, a caller-supplied admission result, a hand-keyed observation, or an authoritative retry path. Confirm every capture producer is named in `instrument_versions` and the live predicate hash is generated from the committed value.

```bash
rg -n '/db/|references/' \
  abc/src/abc/tools/parser_rq*.clj \
  abc/src/abc/tools/parser_release_qualification.clj \
  ab-validator/crates/ab-parser-rq-* \
  ab-validator/reports/parser-ir/parser-rq-*.py
rg -n 'admitted_tuple_matches|admission[_-]boolean|retry' \
  abc/src/abc/tools/parser_rq_campaign.clj \
  abc/src/abc/tools/parser_release_qualification.clj \
  ab-validator/reports/parser-ir/parser-rq-*.py
git diff --check
```

- [ ] **Step 3: Fix before freeze, then commit and push**

Any implementation correction belongs here and repeats the full verification. When clean:

```bash
git status --short
git rev-parse HEAD
git push origin main
```

Record this resulting SHA as `candidate_git_rev`. No code, schema, policy, corpus, predicate, or instrument edit is permitted for this candidate after this point.

### Task 10: Prove the Candidate and Commit Its Sole Authorization

**Files:**
- Create: `abc/docs/reports/parser-rq/runs/$identity_dir/candidate.edn`, where the command below derives `identity_dir`.
- Create: `abc/docs/reports/parser-rq/runs/$identity_dir/executable-provenance.json`.
- Create: `abc/docs/reports/parser-rq/runs/$identity_dir/readiness-receipt.json`.
- Create: `abc/docs/reports/parser-rq/runs/$identity_dir/authorizations/$authorization_dir.edn`, where the command below derives `authorization_dir`.

- [ ] **Step 1: Prepare a detached clean worktree on hinoki**

On hinoki, set `repo_root` to its configured Soranoha checkout and derive the candidate from pushed `origin/main`:

```bash
repo_root=$(git rev-parse --show-toplevel)
evidence_tree="$repo_root"
git -C "$repo_root" fetch origin main
candidate_git_rev=$(git -C "$repo_root" rev-parse origin/main)
candidate_tree=$(mktemp -d /tmp/soranoha-p5-candidate.XXXXXXXX)
git -C "$repo_root" worktree add --detach "$candidate_tree" "$candidate_git_rev"
test -z "$(git -C "$candidate_tree" status --porcelain)"
test -z "$(git -C "$evidence_tree" status --porcelain)"
```

- [ ] **Step 2: Run runtime preflight**

The runtime descriptor is explicit JSON with schema version `2.0.0` and
contains only the lock, evidence-store, scratch, and corpus paths. It is not a
qualification identity. Stop unless the read-only runtime preflight passes.

```bash
: "${PARSER_RQ_SITE_DESCRIPTOR:?set the reviewed site-descriptor JSON path}"
graph="$candidate_tree/abc/data/parser-rq-production-graph-v1.json"
staging_root=$(mktemp -d /tmp/soranoha-p5-evidence.XXXXXXXX)
python "$candidate_tree/abc/tools/parser_rq_campaign_site.py" preflight-site \
  --site-descriptor "$PARSER_RQ_SITE_DESCRIPTOR" --graph "$graph" \
  --evidence-tree-clean true
```

- [ ] **Step 3: Perform and compare two independent realizations**

The provenance command seeds only the target's dependencies into each fresh
store, asserts the target is absent, and builds the concrete derivation
offline. It is the authority for independent realization; raw `nix build
--json` output is not executable provenance.

```bash
build_root=$(mktemp -d /tmp/soranoha-p5-builds.XXXXXXXX)
provenance_tool="$candidate_tree/ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py"
for build_id in build-a build-b; do
  python "$provenance_tool" realize-build --candidate-tree "$candidate_tree" \
    --build-id "$build_id" --store-root "$build_root/store-$build_id" \
    --build-log "$build_root/$build_id.log" --out "$build_root/$build_id.realization.json"
  python "$provenance_tool" capture-build \
    --realization "$build_root/$build_id.realization.json" --graph "$graph" \
    --parser-git-rev "$candidate_git_rev" --out "$build_root/$build_id.json"
done
python "$provenance_tool" compare-builds --first "$build_root/build-a.json" \
  --second "$build_root/build-b.json" --out "$build_root/provenance-proof.json"
```

Any realization or executable disagreement ends this candidate unavailable.

- [ ] **Step 4: Generate the candidate and bind provenance**

From the detached tree, load the live corpus, predicate set, admission coordinates, instrument policies, and verified executable record:

```bash
nix develop "$candidate_tree/abc" --command clojure -M:abc/parser-rq-campaign \
  candidate --repo "$candidate_tree" --parser-git-rev "$candidate_git_rev" \
  --provenance "$build_root/provenance-proof.json" \
  --out "$staging_root/candidate.edn"
candidate_ref=$(nix develop "$candidate_tree/abc" --command clojure -M:abc/parser-rq-campaign \
  candidate-ref --candidate "$staging_root/candidate.edn")
identity_ref=$(nix develop "$candidate_tree/abc" --command clojure -M:abc/parser-rq-campaign \
  qualification-identity-ref --candidate "$staging_root/candidate.edn")
python "$provenance_tool" bind-provenance --proof "$build_root/provenance-proof.json" \
  --candidate-ref "$candidate_ref" --qualification-identity-ref "$identity_ref" \
  --out "$staging_root/executable-provenance.json"
identity_dir=${candidate_ref#sha256:}
run_root="$evidence_tree/abc/docs/reports/parser-rq/runs/$identity_dir"
mkdir -p "$run_root/authorizations"
```

The run directory is keyed by the authenticated candidate value. The distinct
qualification identity authenticates every observation envelope and may be
shared only by candidates whose complete qualification tuple is identical.

- [ ] **Step 5: Seal readiness, then fix the sole execution window**

Choose the operational window before capture, write its concrete UTC timestamps into the authorization, and require ordinal one. The following derives a two-hour window beginning 30 minutes after generation:

```bash
not_before_utc=$(date -u -d '+30 minutes' '+%Y-%m-%dT%H:%M:%SZ')
not_after_utc=$(date -u -d '+150 minutes' '+%Y-%m-%dT%H:%M:%SZ')
provenance_core_ref=$(python -c 'import json,sys; print(json.load(open(sys.argv[1]))["provenance_core_ref"])' \
  "$staging_root/executable-provenance.json")
python "$candidate_tree/abc/tools/parser_rq_campaign_site.py" seal-readiness \
  --site-descriptor "$PARSER_RQ_SITE_DESCRIPTOR" --graph "$graph" \
  --candidate-ref "$candidate_ref" --qualification-identity-ref "$identity_ref" \
  --provenance-core-ref "$provenance_core_ref" --candidate-git-rev "$candidate_git_rev" \
  --evidence-base-git-rev "$candidate_git_rev" \
  --corpus-snapshot-hash sha256:63d8d53a9a0ef8ec80c921d7fb17d142f231fbc061066fb8056b951ffcfbe47e \
  --corpus-list-hash sha256:ace3fa3f4fb6565d46276d8276b4a2e183c58e595f27f0e3149d7395ca6554dd \
  --candidate-tree-clean true --evidence-tree-clean true \
  --out "$staging_root/readiness-receipt.json"
nix develop "$candidate_tree/abc" --command clojure -M:abc/parser-rq-campaign \
  authorize --candidate "$staging_root/candidate.edn" \
  --receipt "$staging_root/readiness-receipt.json" --ordinal 1 \
  --not-before "$not_before_utc" --not-after "$not_after_utc" \
  --out "$staging_root/authorization.edn"
authorization_ref=$(nix develop "$candidate_tree/abc" --command clojure -M:abc/parser-rq-campaign \
  authorization-ref --authorization "$staging_root/authorization.edn")
authorization_dir=${authorization_ref#sha256:}
cp "$staging_root/candidate.edn" "$run_root/candidate.edn"
cp "$staging_root/executable-provenance.json" "$run_root/executable-provenance.json"
cp "$staging_root/readiness-receipt.json" "$run_root/readiness-receipt.json"
cp "$staging_root/authorization.edn" "$run_root/authorizations/$authorization_dir.edn"
```

- [ ] **Step 6: Structurally verify, commit, and push before the window opens**

```bash
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  verify-authorization-record --candidate "$run_root/candidate.edn" \
  --provenance "$run_root/executable-provenance.json" --graph "$graph" \
  --receipt "$run_root/readiness-receipt.json" \
  --authorization "$run_root/authorizations/$authorization_dir.edn"
git add "$run_root"
git commit -m "evidence(parser-rq): authorize candidate capture"
git push origin main
```

If the push is not visible on hinoki before `not_before_utc`, or any volatile lane starts early, the authorization is invalid and this candidate ends unavailable. Select a new implementation candidate; do not rewrite the interval.

### Task 11: Execute and Publish the Sole Authorized Capture

**Files:**
- Create: immutable capture content beneath `abc/docs/reports/parser-rq/runs/$identity_dir/captures/$capture_dir/`.
- Modify: `abc/docs/reports/parser-release-qualification-measurements.edn` as a generated canonical projection.

- [ ] **Step 1: Re-authenticate the sealed explicit site configuration**

Continue with `candidate_git_rev`, `identity_dir`, `authorization_dir`, and `run_root` from Task 10. Require site configuration without embedding it in source:

```bash
: "${PARSER_RQ_SITE_DESCRIPTOR:?set the reviewed site-descriptor JSON path}"
test "$(git -C "$candidate_tree" rev-parse HEAD)" = "$candidate_git_rev"
test -z "$(git -C "$candidate_tree" status --porcelain)"
python "$candidate_tree/abc/tools/parser_rq_campaign_site.py" recheck-readiness \
  --site-descriptor "$PARSER_RQ_SITE_DESCRIPTOR" \
  --receipt "$run_root/readiness-receipt.json"
```

This rechecks the volatile lock and configured paths immediately before the first capture process.

- [ ] **Step 2: Execute all lanes once under the sole authorization**

```bash
capture_staging=$(mktemp -d /tmp/soranoha-p5-capture.XXXXXXXX)
bash "$candidate_tree/abc/bin/parser-rq-campaign-capture.sh" \
  --candidate "$run_root/candidate.edn" \
  --authorization "$run_root/authorizations/$authorization_dir.edn" \
  --provenance "$run_root/executable-provenance.json" \
  --readiness-receipt "$run_root/readiness-receipt.json" \
  --site-descriptor "$PARSER_RQ_SITE_DESCRIPTOR" \
  --candidate-tree "$candidate_tree" --evidence-tree "$evidence_tree" \
  --staging-root "$capture_staging" --production
```

The orchestrator verifies time and authorization before its first volatile process, retains the lock, runs three complete core repetitions, executes the remaining lanes serially, and writes one closed capture index. It records interruption or lock loss as unavailable and exits without starting a second attempt.

- [ ] **Step 3: Authenticate the controller-produced generation without re-execution**

```bash
nix develop "$candidate_tree/abc" --command clojure -M:abc/parser-rq-campaign \
  verify-capture --candidate "$run_root/candidate.edn" \
  --authorization "$run_root/authorizations/$authorization_dir.edn" \
  --capture-root "$capture_staging"
```

No parser command may run during these calls. Preserve any honest fail or unavailable envelope.

- [ ] **Step 4: Authenticate every logical blob and publish the receipt**

```bash
PARSER_RQ_EVIDENCE_STORE=$(python -c 'import json,sys; print(json.load(open(sys.argv[1]))["evidence_store_root"])' \
  "$PARSER_RQ_SITE_DESCRIPTOR")
capture_ref=$(nix develop "$candidate_tree/abc" --command clojure -M:abc/parser-rq-campaign \
  capture-ref --capture-index "$capture_staging/capture-index.edn")
CAPTURE_INDEX="$capture_staging/capture-index.edn" \
BLOBS_OUT="$capture_staging/blobs.json" \
nix develop "$candidate_tree/abc" --command clojure -M -e \
  '(require (quote [clojure.edn :as edn]) (quote [abc.tools.json :as json]))
   (json/write-deterministic-json-file!
    (System/getenv "BLOBS_OUT")
    (vec (vals (:members (edn/read-string (slurp (System/getenv "CAPTURE_INDEX")))))))'
nix develop "$candidate_tree/abc" --command python \
  "$candidate_tree/ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py" \
  verify-evidence --blobs "$capture_staging/blobs.json" \
  --evidence-root "$PARSER_RQ_EVIDENCE_STORE" \
  --candidate-ref "$candidate_ref" --capture-generation-ref "$capture_ref" \
  --out "$capture_staging/evidence-integrity-receipt.json"
```

This command streams and re-hashes the closed manifest-referenced set in the configured content store. Failure writes an unavailable receipt and blocks publication; changing a digest is forbidden.

- [ ] **Step 5: Publish the immutable generation and canonical measurements**

```bash
capture_dir=${capture_ref#sha256:}
mkdir -p "$run_root/captures"
publish_staging="$run_root/captures/.$capture_dir.tmp"
test ! -e "$publish_staging" && test ! -e "$run_root/captures/$capture_dir"
mkdir "$publish_staging"
cp -a "$capture_staging/." "$publish_staging/"
mv "$publish_staging" "$run_root/captures/$capture_dir"
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  project --runs-root abc/docs/reports/parser-rq/runs \
  --candidate-ref "$candidate_ref" --measurements-out \
  abc/docs/reports/parser-release-qualification-measurements.edn
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  verify-capture --candidate "$run_root/candidate.edn" \
  --authorization "$run_root/authorizations/$authorization_dir.edn" \
  --capture-root "$run_root/captures/$capture_dir"
```

The copy is staged under the destination filesystem and the final rename is
atomic. Once published and committed, the generation directory is immutable.

- [ ] **Step 6: Commit and push the capture before admission**

```bash
git add "$run_root/captures/$capture_dir" \
  abc/docs/reports/parser-release-qualification-measurements.edn
git commit -m "evidence(parser-rq): publish authorized candidate capture"
git push origin main
```

Do not create an evaluation yet: its strict admission result requires the independent full-corpus candidate produced in Task 12.

### Task 12: Audit, Resolve Admission, and Publish Current-Registry Evaluation

**Files:**
- Create: immutable evaluation content beneath `abc/docs/reports/parser-rq/runs/$identity_dir/evaluations/`.
- Modify only if strict status is missing: `abc/data/aat-parser-ir-compatibility.edn`.
- Modify: affected ADR evidence runs/catalogs derived from live registry reads.
- Modify: `abc/docs/reports/parser-release-qualification-report.json` as a generated canonical projection.

- [ ] **Step 1: Run the independent full-corpus audit**

Require the configured full admission AAT corpus and invoke the candidate-built executable recorded in provenance:

```bash
: "${PARSER_RQ_ADMISSION_AAT_DIR:?set the full ADR-0023 admission AAT root}"
audit_staging=$(mktemp -d /tmp/soranoha-p5-audit.XXXXXXXX)
aat_to_ir_bin=$(nix develop "$candidate_tree/abc" --command python \
  "$candidate_tree/ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py" \
  resolve-executable --provenance "$run_root/executable-provenance.json" \
  --name ab-aat-to-parser-ir)
"$aat_to_ir_bin" audit-corpus \
  --aat-dir "$PARSER_RQ_ADMISSION_AAT_DIR" \
  --mapping "$candidate_tree/ab-validator/data/aat-to-parser-ir-mapping-v2.json" \
  --summary-json "$audit_staging/summary.json" \
  --report-md "$audit_staging/report.md" \
  --compat-edn-out "$audit_staging/admission-candidate.edn" \
  --jobs 1 --abc-root "$candidate_tree/abc"
```

Authenticate the executable, mapping/version/hash, evidence scope, and candidate identity. Audit failure is an invalid/unavailable governance input; do not synthesize a row.

- [ ] **Step 2: Publish the pre-admission evaluation**

```bash
pre_eval=$(mktemp -d /tmp/soranoha-p5-pre-evaluation.XXXXXXXX)
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  evaluate --candidate "$run_root/candidate.edn" \
  --capture-root "$run_root/captures/$capture_dir" \
  --registry abc/data/aat-parser-ir-compatibility.edn \
  --admission-candidate "$audit_staging/admission-candidate.edn" \
  --out "$pre_eval"
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  publish-evaluation --candidate-root "$run_root" --evaluation-root "$pre_eval"
git add "$run_root/evaluations"
git commit -m "evidence(parser-rq): publish pre-admission evaluation"
git push origin main
```

Publish and push this immutable evaluation before any registry mutation, whether status is admitted, unadmitted, conflict, or invalid. A conflict/invalid result stops registry mutation and promotion after this honest evidence commit.

- [ ] **Step 3: Append only an exact missing candidate**

Read the machine status from `admission-report.edn`. If it is `missing`, create a new registry value without editing the input in place:

```bash
registry_staging=$(mktemp /tmp/aat-parser-ir-compatibility.XXXXXXXX.edn)
nix develop ./abc --command clojure -M:abc/aat-compat-admission -- \
  --registry abc/data/aat-parser-ir-compatibility.edn \
  --candidates "$audit_staging/admission-candidate.edn" \
  --append-out "$registry_staging"
mv "$registry_staging" abc/data/aat-parser-ir-compatibility.edn
```

If already admitted, skip the append. If conflict or invalid, commit the pre-admission evaluation and end P5 with ADR 0039 Proposed.

- [ ] **Step 4: Derive and recapture the complete affected evidence closure**

After an append, generate an inventory before governance recapture and assert its registry readers equal the four live catalogs and these eight focused runs: `adr-0009-c5-aat-conversion-compatibility`, base design bundle, parser-IR-publication design bundle, schema/RDF/TEI design bundle, temporal-person-ingest design bundle, `parser-import-boundary`, `parser-mapping-admission`, and `parser-phase5-frozen-tuple`. Include the pre-promotion snapshot if the generated inventory names the registry. Skip this step when the strict pre-evaluation was already admitted because no registry value changed.

```bash
inventory=$(mktemp /tmp/adr-evidence-inventory.XXXXXXXX.edn)
nix develop ./abc --command clojure -M:abc/adr-evidence-inventory -- \
  --output "$inventory"
```

Add a campaign command `verify-registry-closure` that compares the inventory's actual read graph to the committed expected set and fails on omission or a new reader. Then execute each descriptor through `abc.tools.adr-evidence-capture`, register its regenerated output, and run:

```bash
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  verify-registry-closure --inventory "$inventory"
just validate-migration
```

- [ ] **Step 5: Re-evaluate unchanged measurements against the current registry**

```bash
post_eval=$(mktemp -d /tmp/soranoha-p5-post-evaluation.XXXXXXXX)
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  evaluate --candidate "$run_root/candidate.edn" \
  --capture-root "$run_root/captures/$capture_dir" \
  --registry abc/data/aat-parser-ir-compatibility.edn \
  --admission-candidate "$audit_staging/admission-candidate.edn" \
  --out "$post_eval"
cmp "$run_root/captures/$capture_dir/measurements.edn" \
  abc/docs/reports/parser-release-qualification-measurements.edn
```

Require strict status `admitted` after an append. Publish only the new post-admission evaluation by its generated `evaluation_generation_ref`, then project the unique evaluation matching the current registry hash:

```bash
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  publish-evaluation --candidate-root "$run_root" --evaluation-root "$post_eval"
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  project --runs-root abc/docs/reports/parser-rq/runs \
  --candidate-ref "$candidate_ref" --registry abc/data/aat-parser-ir-compatibility.edn \
  --report-out abc/docs/reports/parser-release-qualification-report.json
```

If the pre-evaluation was already admitted and therefore has the current registry hash, do not run the post-evaluation commands or create a duplicate; project the unique committed pre-evaluation.

- [ ] **Step 6: Commit and push the admission transaction**

```bash
git add "$run_root/evaluations" abc/data/aat-parser-ir-compatibility.edn \
  abc/docs/reports/parser-release-qualification-report.json \
  abc/docs/adr/adr-evidence.edn
# Add only the exact catalog, descriptor, and run paths printed by recapture.
git commit -m "evidence(parser-rq): resolve candidate admission"
git push origin main
```

### Task 13: Resolve ADR 0040 and Conditionally Promote ADR 0039

**Files:**
- Modify: `abc/docs/adr/0040-process-tree-memory-qualification.md` only if its criteria pass.
- Modify: `abc/docs/adr/0039-custom-parser-release-qualification.md` only if promotion verification passes.
- Modify: generated ADR evidence and governance catalogs.

- [ ] **Step 1: Run the promotion verifier before editing governance**

```bash
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  verify-promotion --runs-root abc/docs/reports/parser-rq/runs \
  --candidate-ref "$candidate_ref" \
  --registry abc/data/aat-parser-ir-compatibility.edn \
  --measurements abc/docs/reports/parser-release-qualification-measurements.edn \
  --report abc/docs/reports/parser-release-qualification-report.json \
  --provenance "$run_root/executable-provenance.json" \
  --adr-0040 abc/docs/adr/0040-process-tree-memory-qualification.md \
  --adr-0041 abc/docs/adr/0041-parser-release-instrument-bindings.md
```

Record its exact errors. At this point ADR 0040 being Proposed is an expected promotion dependency error. Separately verify ADR-0040-C3 from the fresh authenticated memory envelope; other predicate failures do not block accepting the measurement mechanism, but they continue to block ADR 0039.

- [ ] **Step 2: Resolve ADR 0040 independently**

Evaluate ADR-0040-C3 against the fresh process-tree memory envelope and its authenticated campaign context. If all ADR 0040 criteria pass, set it Accepted with the actual date and evidence paths, recapture its governance closure, and commit. This decision does not require predicate 8 to pass the 2 GiB threshold; it accepts the governed measurement mechanism.

```bash
just validate-migration
git add abc/docs/adr/0040-process-tree-memory-qualification.md \
  abc/docs/adr/adr-evidence.edn
# Add only the exact ADR-0040 evidence paths printed by recapture.
git commit -m "docs(adr): resolve process-tree memory qualification"
git push origin main
```

If ADR 0040 criteria do not pass, retain Proposed, publish the blocker, and stop without editing ADR 0039.

- [ ] **Step 3: Re-run promotion verification**

Require zero errors, a byte-reproducible report with `gate_status = release-qualified`, `adr_0039_status = Accepted`, coherence `ok`, admission `admitted`, exactly nine passes, and zero fail/unavailable entries. Also require Accepted ADR 0040 and 0041, one capture, one current-registry evaluation, reproducible provenance, and one authenticated evidence-integrity receipt.

```bash
nix develop ./abc --command clojure -M:abc/parser-rq-campaign \
  verify-promotion --runs-root abc/docs/reports/parser-rq/runs \
  --candidate-ref "$candidate_ref" \
  --registry abc/data/aat-parser-ir-compatibility.edn \
  --measurements abc/docs/reports/parser-release-qualification-measurements.edn \
  --report abc/docs/reports/parser-release-qualification-report.json \
  --provenance "$run_root/executable-provenance.json" \
  --adr-0040 abc/docs/adr/0040-process-tree-memory-qualification.md \
  --adr-0041 abc/docs/adr/0041-parser-release-instrument-bindings.md
```

- [ ] **Step 4: Conditionally promote ADR 0039**

Only a zero exit permits the ordinary reviewed edit to ADR 0039: status, date, validation scope, release authority, dependency on ADR 0040/0041, implementation state, and immutable evidence paths. If the verifier is nonzero, do not edit ADR 0039; the committed honest result completes P5.

- [ ] **Step 5: Recapture governance and run final verification**

```bash
just check-no-build
just phase5-checkpoint
just monorepo-adr-governance
just python-quality
just nix-format-check
scripts/comment-hygiene-check.sh
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
just validate-migration
git diff --check
```

- [ ] **Step 6: Commit and push the honest terminal state**

```bash
git add abc/docs/adr/0039-custom-parser-release-qualification.md \
  abc/docs/adr/adr-evidence.edn
# Add only the exact ADR-0039 evidence paths printed by recapture.
git commit -m "docs(adr): resolve custom parser release qualification"
git push origin main
```

When the gate is not qualified, omit the ADR-0039 `git add`/commit, confirm it remains Proposed, and report the committed failing/unavailable predicate or admission blocker instead. That is a completed campaign, not an implementation failure.

## Final Self-Review Checklist

- [ ] Every design acceptance criterion maps to a task and executable check.
- [ ] Candidate selection happens after all implementation and predicate changes.
- [ ] Authorization is committed and pushed before the sole capture begins.
- [ ] Three core repetitions are mandatory, serial, and maximum-reduced.
- [ ] Capture publication precedes the audit; the audit candidate precedes every evaluation.
- [ ] Admission conflict overrides a matching nine-field projection inside the live gate.
- [ ] Registry append is exact, append-only, and followed by a live-read-graph closure check.
- [ ] Capture bytes do not change between pre- and post-admission evaluations.
- [ ] Canonical paths are mechanically resolved projections, never operator-selected paths.
- [ ] Two-build reproducibility and one closed evidence-integrity receipt are authenticated inputs to promotion.
- [ ] ADR 0041 is Accepted before freeze; ADR 0040 is resolved before ADR 0039.
- [ ] No command contains a hand-filled hash, report verdict, admission boolean, or machine-local source path.
- [ ] A failed, unavailable, interrupted, or conflicting one-shot run remains publishable without promotion.
