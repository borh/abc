# Parser RQ Diagnostic Authorization and R2 Activation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Authenticate and classify the custom `ab-aozora` parser's schema-v3
diagnostics into exact authorized decoded-byte intervals, then activate the
identity-bound R2 `:silent_drops` observation without allowing raw diagnostics
to excuse source loss directly.

**Architecture:** ABC owns a closed raw-diagnostic schema and an independently
reviewed immutable policy. A focused Rust crate implements a pure authorization
decision over validated values plus a separate authenticated corpus/CAS shell;
P1's existing pure reconciler remains policy-free. Clojure authenticates R1 and
the authorization aggregate before deriving R2.

**Tech Stack:** Rust 2024, serde/serde_json, sha2, jsonschema, clap, Hegel 0.28;
Clojure, Malli, Charred; Draft 2020-12 JSON Schema; Nix flakes and Kaocha.

## Global Constraints

- Scope is the custom `ab-aozora` parser only; third-party parser retirement is
  a later publication/cleanup milestone.
- Raw diagnostic capture, authorization decision, and reconciliation are three
  separate immutable values.
- ABC's canonical policy document is authority; parser code may expose
  vocabulary but may not approve its own dispositions.
- Raw schema ID/hash and version `3` are pinned and authenticated.
- Unknown codes, internal diagnostics, policy/vocabulary drift, malformed
  entries, identity mismatch, and invalid spans make authorization unavailable.
- Authorized spans are exact half-open `decoded_utf8` byte intervals; no line or
  construct expansion is permitted.
- Diagnostic endpoints must be valid UTF-8 boundaries in the authenticated
  decoded source value.
- An authenticated empty stream is available and records `vacuous = true`.
- R2 counts maximal connected silent byte intervals within each work, never
  constructs and never intervals merged across works.
- Corpus membership comes only from P1's authenticated record index.
- Corpus-scale artifacts remain in the external store; only schemas, policy,
  summaries, manifests, and small fixtures enter source.
- Full-corpus authoritative capture remains P5 work on
  `hinoki.hyakutake-barbel.ts.net`.
- Follow `docs/comment-standards.md`; no task/spec references in production
  comments.

---

### Task 1: Characterize every live diagnostic code and freeze the reviewed v1 matrix

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/diagnostic_authorization_characterization.rs`
- Create: `ab-validator/docs/superpowers/reports/2026-07-16-parser-rq-diagnostic-authorization-characterization.json`
- Create: `ab-validator/docs/superpowers/reports/2026-07-16-parser-rq-diagnostic-authorization-characterization.md`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml`
- Modify: `ab-validator/Cargo.lock`
- Delete before commit: `ab-validator/crates/ab-parser-rq-source-accountability/tests/diagnostic_authorization_characterization.rs`
- Restore before commit: `ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml`
- Restore before commit: `ab-validator/Cargo.lock`

**Interfaces:**
- Consumes: `ab_aozora_spec::Diagnostic::ALL_CODES`, `Diagnostic::explain`,
  `ab_aozora_aat::diagnostics_json_from_bytes`, and P1 work analysis.
- Produces: a committed 21-row characterization summary and one reviewed
  disposition per exact wire code. Later tasks must use that exact matrix.

The pre-registered hypothesis is:

```text
authorize_exact_span:
  source-contains-pua (warning, source)

observe_only:
  unclosed-bracket (error, source)
  unmatched-close (error, source)
  accent-decomposition-applied (note, source)
  unresolved-gaiji (warning, source)
  mismatched-container-close (error, source)
  empty-ruby-reading (error, source)
  nested-ruby (error, source)
  unrecognised-container-directive (warning, source)
  tcy-target-not-found (warning, source)
  bouten-target-ambiguous (warning, source)
  forward-referent-not-stylable (warning, source)
  break-in-single-line-container (warning, source)
  bracketed-kaeriten-no-pair (error, source)
  kaeriten-outside-kanbun (warning, source)
  mismatched-bouten-container (error, source)
  non-canonical-directive (warning, source)

reject_internal:
  residual-annotation-marker (error, internal)
  unregistered-sentinel (error, internal)
  registry-out-of-order (error, internal)
  registry-position-mismatch (error, internal)
```

- [ ] **Step 1: Write the disposable characterization test**

Add temporary dev dependencies:

```toml
ab-aozora-spec = { path = "../ab-aozora-spec" }
ab-aat-to-parser-ir = { path = "../ab-aat-to-parser-ir" }
```

The ignored test must enumerate `Diagnostic::ALL_CODES`, call `explain`, convert
the namespaced code to the live wire code with the same final-token
underscore-to-kebab rule as the facade, and emit one canonical row:

```rust
#[derive(serde::Serialize)]
struct Row {
    namespaced_code: String,
    wire_code: String,
    severity: String,
    source: String,
    repro: String,
    documented_recovery: String,
    emitted_by_repro: bool,
    diagnostic_span: Option<[u64; 2]>,
    r1_status: String,
    r1_uncovered: Vec<[u64; 2]>,
    diagnostic_intersection_with_r1_uncovered: Vec<[u64; 2]>,
    proposed_disposition: &'static str,
    rationale: &'static str,
}

#[test]
#[ignore = "disposable policy characterization; run explicitly"]
fn characterize_all_live_diagnostic_codes() {
    let rows = ab_aozora_spec::Diagnostic::ALL_CODES
        .iter()
        .map(|code| characterize(code))
        .collect::<Vec<_>>();
    assert_eq!(rows.len(), 21);
    assert_eq!(rows.iter().map(|r| &r.wire_code).collect::<BTreeSet<_>>().len(), 21);
    std::fs::write(
        std::env::var("CHARACTERIZATION_OUT").expect("CHARACTERIZATION_OUT"),
        canonical_json(&rows),
    ).unwrap();
}
```

For each source code, pass `DiagnosticInfo.repro` through the production chain:

```text
repro bytes
  -> ab_aozora_aat::aat_json_from_bytes
  -> ab_aat_to_parser_ir::convert with the checked-in v1 mapping/schema tuple
  -> ab_parser_rq_source_accountability::analyze_work
```

The temporary helper constructs `WorkInput` from the exact conversion tuple and
the decoded/source hashes, never a hand-written Parser-IR. It separately calls
`diagnostics_json_from_bytes`, selects the target code, and intersects that
exact span with the resulting R1 `record.uncovered`. Record both complete sets,
not just counts. This makes the probe answer the policy question: whether an
observed diagnostic span excuses bytes that R1 actually found unclaimed.
Internal codes are marked `emitted_by_repro = false` because ordinary source
must not manufacture internal failures. The rationale is copied from a closed
21-entry test map so absence is a compile/test failure, not prose added after
observation.

- [ ] **Step 2: Run the probe twice and prove determinism**

Run from `ab-validator/`:

```bash
CHARACTERIZATION_OUT=/tmp/diagnostic-authorization-1.json \
  cargo test -p ab-parser-rq-source-accountability \
  --test diagnostic_authorization_characterization \
  -- --ignored
CHARACTERIZATION_OUT=/tmp/diagnostic-authorization-2.json \
  cargo test -p ab-parser-rq-source-accountability \
  --test diagnostic_authorization_characterization \
  -- --ignored
cmp /tmp/diagnostic-authorization-1.json /tmp/diagnostic-authorization-2.json
```

Expected: both tests pass; `cmp` exits zero. Copy the first canonical JSON value
to the committed JSON report and have the disposable test render the Markdown
decision table from the same in-memory rows, so the two reports cannot diverge.

- [ ] **Step 3: Apply the fail-stop decision gate**

The task stops `BLOCKED` before Tasks 2-7 if any of these is true:

- the vocabulary is not exactly 21 unique codes;
- severity or source differs from the pre-registered matrix;
- a source repro unexpectedly emits a different target code and the behavior
  cannot be explained from live parser semantics;
- evidence shows a proposed `observe_only` recovery intentionally leaves its
  exact span unclaimed;
- evidence shows `source-contains-pua` is loss-free and fully attributable
  without authorization.
- a source repro cannot traverse the complete production conversion and R1
  analysis chain as `:ok`; such a failure is evidence to investigate, not a
  reason to substitute a synthetic Parser-IR.

If stopped, revise the design and this plan through user review; do not tune the
matrix inside the task. If the evidence supports the hypothesis, add to the
Markdown report:

```text
decision: accepted-v1-matrix
authorizing_codes: [source-contains-pua]
observe_only_codes: 16
reject_internal_codes: 4
```

- [ ] **Step 4: Remove the disposable code and commit only evidence**

Restore `Cargo.toml`, delete the temporary test, and run:

```bash
git diff -- ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml
git diff -- ab-validator/Cargo.lock
test ! -e ab-validator/crates/ab-parser-rq-source-accountability/tests/diagnostic_authorization_characterization.rs
git diff --check
git add ab-validator/docs/superpowers/reports/2026-07-16-parser-rq-diagnostic-authorization-characterization.*
git commit -m "docs(parser-rq): characterize diagnostic authorization semantics"
```

Expected: Cargo diff is empty; only the two small evidence reports are committed.

---

### Task 2: Land ABC-owned raw, policy, result, index, and aggregate protocols

**Files:**
- Create: `abc/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-authorization-policy.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-authorization-result.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-authorization-index.schema.json`
- Create: `abc/schemas/parser-rq-diagnostic-authorization-aggregate.schema.json`
- Create: `abc/data/parser-rq-ab-aozora-diagnostic-authorization-v1.json`
- Create: `abc/test/fixtures/parser-rq/diagnostic-authorization/*.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: Task 1's accepted exact matrix and P0 logical-ref conventions.
- Produces: five closed Draft 2020-12 protocols and the immutable canonical v1
  policy consumed by every later task.

- [ ] **Step 1: Write failing registration and closure tests**

Add tests that load all five live schemas and fixtures, assert root and nested
`additionalProperties: false`, and reject:

```clojure
(deftest diagnostic-authorization-policy-is-complete-test
  (let [policy (files/read-json
                "data/parser-rq-ab-aozora-diagnostic-authorization-v1.json")
        rows (get policy "diagnostics")]
    (is (= 21 (count rows)))
    (is (= 21 (count (set (map #(get % "code") rows)))))
    (is (= ["source-contains-pua"]
           (mapv #(get % "code")
                 (filter #(= "authorize_exact_span" (get % "disposition")) rows))))))

(deftest raw-diagnostic-codepoint-conditional-test
  (is (invalid? raw-schema
                (assoc-in valid-entry ["data" 0 "codepoint"] "x")))
  (is (invalid? raw-schema
                (update-in pua-entry ["data" 0] dissoc "codepoint"))))
```

Also reject duplicate policy codes, unknown dispositions, unknown raw fields,
`kind`/`code` mismatch, unavailable results containing interval/count claims,
and `ok` results with nonempty errors.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.validate-design-bundle-test/parser-rq-diagnostic-authorization
```

Expected: fail because the five schemas and policy do not exist.

- [ ] **Step 3: Implement exact closed contracts**

The raw schema requires:

```json
{
  "schemaVersion": 3,
  "data": [{
    "kind": "source_contains_pua",
    "code": "source-contains-pua",
    "severity": "warning",
    "source": "source",
    "span": {"start": 0, "end": 3},
    "codepoint": "\ue001"
  }]
}
```

Every object is closed. `codepoint` is required only for
`source-contains-pua`, forbidden otherwise. Policy rows require exact
`code,severity,source,disposition,rationale` fields. Result `ok` requires all
counts, vacuity, normalized intervals, decision-ledger logical ref, and empty
errors; `unavailable` prohibits those trusted observations and requires errors.
Index and aggregate follow P1's closed identity/work-count conventions.

Create the canonical policy with exactly the Task 1 matrix, including raw schema
ID/hash, parser `ab-aozora`, schema version `3`, coordinate system
`decoded_utf8`, 21 rows, and policy version
`parser-rq-ab-aozora-diagnostic-authorization-v1`.

- [ ] **Step 4: Run GREEN and drift checks**

```bash
cd abc
bin/kaocha --focus abc.tools.validate-design-bundle-test/parser-rq-diagnostic-authorization
cd ..
nix run .#schema-drift
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add abc/schemas/parser-rq-*diagnostic* \
  abc/data/parser-rq-ab-aozora-diagnostic-authorization-v1.json \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj \
  abc/test/fixtures/parser-rq/diagnostic-authorization
git commit -m "feat(parser-rq): define diagnostic authorization protocols"
```

---

### Task 3: Implement the pure diagnostic authorizer as a deep module

**Files:**
- Modify: `ab-validator/Cargo.toml`
- Modify: `ab-validator/Cargo.lock`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/Cargo.toml`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/model.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/authorize.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/authorize.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/policy_drift.rs`

**Interfaces:**
- Consumes:
  `authorize(ValidatedCapture, ValidatedPolicy, WorkContext) -> AuthorizationResult`.
- Produces: closed authorization values and full decision-ledger values; no I/O,
  locator, or raw JSON parsing in the decision function.

Define:

```rust
pub struct WorkContext<'a> {
    pub identity_ref: &'a str,
    pub work_id: &'a str,
    pub decoded_source: &'a str,
}

pub struct ValidatedCapture {
    pub raw_ref: LogicalBlobRef,
    pub diagnostics: Vec<Diagnostic>,
}

pub struct ValidatedPolicy {
    pub policy_ref: LogicalBlobRef,
    pub rows: BTreeMap<DiagnosticCode, PolicyRow>,
}

pub fn authorize(
    capture: &ValidatedCapture,
    policy: &ValidatedPolicy,
    context: WorkContext<'_>,
) -> AuthorizationAnalysis;
```

- [ ] **Step 1: Write RED examples and Hegel properties**

Examples must cover the 21-code matrix, unknown code, source/severity mismatch,
internal code, kind/code mismatch, conditional codepoint, empty/invalid/out-of-
bounds/mid-codepoint spans, duplicates, overlapping authorizing entries, and an
empty stream.

Add a Hegel property over already-validated diagnostics:

```rust
#[hegel::test]
fn authorization_is_order_invariant(tc: hegel::TestCase) {
    let diagnostics = draw_valid_diagnostics(&tc);
    let mut reversed = diagnostics.clone();
    reversed.reverse();
    assert_eq!(authorize_value(diagnostics), authorize_value(reversed));
}
```

Run:

```bash
cd ab-validator
cargo test -p ab-parser-rq-diagnostic-authorization
```

Expected: compile failure because the crate/API does not exist.

- [ ] **Step 2: Implement closed models and validation**

Use `serde(deny_unknown_fields)` and closed enums for every discriminator.
Parsing/validation converts raw values into `ValidatedCapture` and
`ValidatedPolicy`; `authorize` accepts no raw JSON type. Unknown/internal or any
invalid entry returns `unavailable` with no interval/count value.

For each accepted entry, write one full decision row. Only
`authorize_exact_span` contributes an interval. Normalize with P1's interval
module after all decisions validate. Set `vacuous = diagnostic_count == 0`.
Keep P1's existing `interval` module as the sole interval-algebra authority;
export only the already-pure `normalize`, `intersect`, and `subtract`
operations required by the new crate rather than duplicating them.

- [ ] **Step 3: Pin independent vocabulary drift**

`policy_drift.rs` compares the canonical policy code set and metadata with
`ab_aozora_spec::Diagnostic::ALL_CODES`/`explain`. It asserts the sole
authorizing code is `source-contains-pua`; it must not derive dispositions from
namespaces or parser metadata.

- [ ] **Step 4: Run GREEN and quality checks**

```bash
cargo test -p ab-parser-rq-diagnostic-authorization
cargo clippy -p ab-parser-rq-diagnostic-authorization --all-targets -- -D warnings
cargo fmt --all -- --check
```

Expected: pass with pristine output.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/Cargo.toml ab-validator/Cargo.lock \
  ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs \
  ab-validator/crates/ab-parser-rq-diagnostic-authorization
git commit -m "feat(parser-rq): authorize exact diagnostic intervals"
```

---

### Task 4: Authenticate diagnostic blobs and produce a deterministic corpus authorization index

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/blob_store.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/index.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/blob_store.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/corpus.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/main.rs`
- Modify: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/corpus.rs`

**Interfaces:**
- Consumes: P1 record index/store, authenticated decoded and diagnostic refs,
  exact qualification identity, canonical policy bytes, runtime store root.
- Produces:
  `authorize_corpus(CorpusAuthorizationInput) -> AuthorizationIndex` and CLI
  `authorize-corpus`.

- [ ] **Step 1: Write failing end-to-end trust tests**

Tests must assert:

- membership comes only from the P1 index;
- every diagnostic and decoded blob is re-read, byte-counted, and SHA-256
  authenticated before JSON/UTF-8 decoding;
- P1 work identity, work ID, decoded length, and diagnostic ref match;
- missing/extra/duplicate/unavailable works make the index unavailable;
- a locator cannot be absolute, traverse, or escape through any symlink ancestor;
- all results/ledgers are computed and existing CAS collisions verified before
  publication;
- index output may live separately from the CAS store;
- repeated runs are byte-identical and unrelated directory files are ignored.

Run:

```bash
cargo test -p ab-parser-rq-diagnostic-authorization --test corpus
```

Expected: compile failure on missing corpus APIs.

- [ ] **Step 2: Implement Capture -> Authorize storage**

Extract P1's existing private CAS logic from `index.rs` into a narrow shared
module with these value-oriented operations:

```rust
pub fn authenticate_blob(
    root: &Path,
    locator: &str,
    expected_sha256: &str,
    expected_bytes: u64,
) -> Result<Vec<u8>>;

pub fn publish_blob(root: &Path, media_type: MediaType, bytes: &[u8])
    -> Result<PublishedBlob>;

pub fn write_atomic_summary(path: &Path, bytes: &[u8]) -> Result<()>;
```

Move the current P1 tests onto this API before making the authorization crate
consume it. `authenticate_blob` rejects lexical escape, every symlink ancestor,
non-regular destinations, length mismatch, and digest mismatch before returning
bytes. `publish_blob` retains P1's unique same-directory temps, fsync, atomic
no-clobber publication, and winner re-verification. Do not expose path-building
or unchecked read/write primitives. The authorization index is the last atomic
summary write. Orphan immutable blobs are acceptable and unreferenced.

The CLI is:

```text
ab-parser-rq-diagnostic-authorization authorize-corpus
  --work-record-index <path>
  --work-record-store-root <path>
  --qualification-identity <path>
  --policy <path>
  --store-root <path>
  --index-out <path>
```

- [ ] **Step 3: Run GREEN and CLI smoke**

```bash
cargo test -p ab-parser-rq-diagnostic-authorization --test corpus
cargo run -p ab-parser-rq-diagnostic-authorization -- --help
```

Expected: tests pass and help lists `authorize-corpus`.

- [ ] **Step 4: Commit**

```bash
git add ab-validator/crates/ab-parser-rq-source-accountability \
  ab-validator/crates/ab-parser-rq-diagnostic-authorization
git commit -m "feat(parser-rq): capture corpus diagnostic authorization"
```

---

### Task 5: Aggregate authorization and derive the R2 envelope

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/aggregate.rs`
- Modify: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/aggregate.rs`
- Modify: `abc/src/abc/tools/parser_rq_source_accountability.clj`
- Modify: `abc/test/abc/tools/parser_rq_source_accountability_test.clj`

**Interfaces:**
- Consumes: authenticated R1 aggregate/work witnesses, authorization index/CAS,
  policy, qualification identity, and P1 `reconcile`.
- Produces: authorization aggregate and identity-bound `:silent_drops`
  observation envelope.

- [ ] **Step 1: Write RED aggregate and Clojure derivation tests**

Rust tests cover exact work-set equality, result/ledger authentication,
unavailable propagation, checked integer totals, per-work interval bounds, and
corpus vacuity defined as total diagnostic count zero.

Clojure tests assert:

```clojure
(is (= {:value 0
        :identity_ref identity-ref
        :diagnostic_count 0
        :vacuous true}
       (select-keys (derive-silent-drops-envelope inputs)
                    [:value :identity_ref :diagnostic_count :vacuous])))

(is (= 1 (:value (derive-silent-drops-envelope
                  (empty-diagnostics-with-one-uncovered-interval)))))
```

Also assert unknown/unavailable authorization leaves R2
`:instrument-missing`, a one-byte authorized overlap leaves the residual bytes
silent, and intervals from adjacent works never merge.

- [ ] **Step 2: Run RED**

```bash
cd ab-validator
cargo test -p ab-parser-rq-diagnostic-authorization --test aggregate
cd ../abc
bin/kaocha --focus abc.tools.parser-rq-source-accountability-test/silent-drops
```

Expected: missing aggregate and derivation APIs.

- [ ] **Step 3: Implement aggregate semantic revalidation and derivation**

The Rust aggregate must treat externally loaded result records as untrusted:
rehash bytes, validate schema/identity/policy/work ID, recheck decision count and
interval conservation, and require exact work-set equality before emitting
trusted totals.

Clojure authenticates the R1 aggregate, authorization aggregate/index, policy,
and qualification identity through the P0 manifest. It passes only the already
authorized interval values to P1 reconciliation and derives:

```text
silent_drop_count = sum(per_work_maximal_silent_intervals)
diagnosed_bytes + silent_bytes = R1 uncovered_eligible_bytes
```

Historical manifests without authorization evidence retain
`:instrument-missing`; no migration fabricates results.

- [ ] **Step 4: Run GREEN**

```bash
cd ab-validator
cargo test -p ab-parser-rq-diagnostic-authorization
cd ../abc
bin/kaocha --focus abc.tools.parser-rq-source-accountability-test
```

Expected: pass.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-parser-rq-diagnostic-authorization \
  abc/src/abc/tools/parser_rq_source_accountability.clj \
  abc/test/abc/tools/parser_rq_source_accountability_test.clj
git commit -m "feat(parser-rq): derive authorized silent-drop evidence"
```

---

### Task 6: Prove Capture -> Authorize -> Derive -> Drift with a complete fixture

**Files:**
- Create: `abc/test/fixtures/parser-rq/diagnostic-authorization-capture/*`
- Create: `ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/fixture_capture.rs`
- Modify: `abc/test/abc/tools/parser_rq_source_accountability_test.clj`

**Interfaces:**
- Produces: one complete small committed witness set generated only through
  production capture/authorization/aggregate paths.

- [ ] **Step 1: Write the unblessed fixture test**

The fixture contains at least:

- one covered clean work with an empty diagnostic stream;
- one work containing `source-contains-pua`;
- one work with an `observe_only` diagnostic and an artificial uncovered
  residual proving it remains silent.

The Rust test generates source, Parser-IR, P1 work records/index/aggregate, raw
diagnostics, policy, authorization results/ledgers/index/aggregate, identity,
and manifest. It compares the exact closed filename set and bytes with committed
fixtures. Initially fail because fixtures are absent.

- [ ] **Step 2: Bless once, then prove deterministic regeneration**

Generate into `/tmp/parser-rq-diagnostic-authorization-fixture`, inspect every
small file, copy with `cp --no-clobber`, and rerun normally twice. Expected:
byte-identical pass.

- [ ] **Step 3: Add real drift mutations**

Tests mutate separately:

- raw diagnostic bytes without manifest update;
- policy disposition/hash;
- qualification identity;
- R1 uncovered interval;
- authorization decision ledger;
- raw code to an unknown code.

Each mutation must traverse manifest verification/derivation and fail
unavailable; string inequality alone is insufficient.

- [ ] **Step 4: Commit**

```bash
git add abc/test/fixtures/parser-rq/diagnostic-authorization-capture \
  ab-validator/crates/ab-parser-rq-diagnostic-authorization/tests/fixture_capture.rs \
  abc/test/abc/tools/parser_rq_source_accountability_test.clj
git commit -m "test(parser-rq): drift-check diagnostic authorization capture"
```

---

### Task 7: Close API, governance, and full verification

**Files:**
- Modify if required by evidence closure:
  `abc/docs/evidence/adr-runs/*.json`, `abc/docs/adr/adr-evidence.edn`
- Modify: `abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md`

**Interfaces:**
- Produces: reviewed P4A completion evidence; P5 is unblocked with authoritative
  capture still unexecuted.

- [ ] **Step 1: Prove the trust API cannot be bypassed**

Add a compile-fail doctest or external integration compile test showing raw
diagnostic structs cannot be passed to P1 `reconcile`; only `Interval` values
from an available authorization result are accepted. Keep any injected parsing
or blob-reading seams private/test-only.

- [ ] **Step 2: Run focused and full verification**

From the monorepo root:

```bash
(cd ab-validator && cargo test -p ab-parser-rq-diagnostic-authorization)
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix run .#schema-drift
scripts/comment-hygiene-check.sh
NIX_CONFIG='eval-cache = false' just validate-migration
```

Expected: every command exits zero. Existing Python mypy informational notes and
post-build cache-hook warnings are not test failures; new warnings are findings.

- [ ] **Step 3: Recapture governance through the real tool if required**

If `monorepo-adr-governance` reports input drift, commit code first, push a
temporary capture branch to `hinoki.hyakutake-barbel.ts.net`, identify the exact
stale descriptor set in audit mode, and run
`clojure -M:abc/adr-evidence-capture` for each into external staging. Install
only outputs from successful commands, rebuild the candidate registry with the
repository tool, and rerun enforcement. Never edit evidence hashes by hand.

- [ ] **Step 4: Record completion and commit**

Update the roadmap to mark P4A implemented without claiming P5 capture or release
qualification. Commit any deterministic governance outputs and the roadmap:

```bash
git add abc/docs/superpowers/plans/2026-07-15-parser-release-qualification-campaign.md \
  abc/docs/evidence/adr-runs abc/docs/adr/adr-evidence.edn
git commit -m "docs(parser-rq): close diagnostic authorization implementation"
```

## Completion review

Before integration, request a whole-range review against the design and this
plan. The reviewer must specifically attempt to find:

- circular policy authority;
- raw-diagnostic-to-reconciler bypass;
- unknown-code or internal-diagnostic false availability;
- mid-codepoint/out-of-bounds span acceptance;
- cross-work interval merging;
- unauthenticated locator/CAS reads;
- incomplete fixture membership;
- historical evidence silently changing from unavailable.

Only merge after the review has no Critical/Important findings and fresh
`just validate-migration` passes on the exact final commit.
