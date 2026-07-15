# Parser RQ P1 Source Accountability Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the release-candidate source-accountability instrument that
derives exact Parser-IR decoded-byte coverage, produces a deterministic
corpus-record index, and provides a pure silent-region reconciler while leaving
R2 unavailable until P4 authorizes diagnostic intervals.

**Architecture:** ABC owns four closed JSON wire schemas and the empty v1
taxonomy. A focused Rust crate reuses `ab_aozora_aat::decode_source_bytes`,
implements pure interval algebra, emits immutable per-work/index/aggregate
values, and never infers corpus membership from a directory. A Clojure boundary
re-hashes the aggregate and derives the P0 observation envelope from exact
integer evidence.

**Tech Stack:** Rust 2024, serde/serde_json, sha2, clap, jsonschema, Hegel 0.28;
Clojure, Malli, Charred; Nix flakes and Kaocha.

## Global Constraints

- R1 coordinates are half-open decoded UTF-8 byte intervals into the exact full
  `DecodedSource.text` produced from pinned original bytes.
- `windows-31j-lossy` is unavailable; replacement bytes can never contribute to
  a passing denominator.
- Only `Parser-IR.nodes[*].span` counts. Paragraph and sentence spans never count.
- Every node must explicitly declare `coordinate_system = decoded_utf8`.
- The v1 ignored-region taxonomy is closed and empty; every decoded byte is
  eligible.
- Work completeness is exact corpus set/count equality, separate from the byte
  denominator.
- Record indexes are produced from corpus entries, never hand-authored or based
  on directory membership.
- P1 consumes only P4-authorized intervals for reconciliation and emits R2 as
  unavailable in this plan.
- Corpus-scale artifacts stay in the external store; only schemas, taxonomy,
  summaries, manifests, and small fixtures enter source.
- Full-corpus capture and candidate pinning remain P5 work on
  `hinoki.hyakutake-barbel.ts.net`.
- Follow `docs/comment-standards.md`; no task/spec references in production
  comments.

---

### Task 1: Prove Hegel works in the Cargo and offline Nix test boundary

**Files:**
- Modify: `ab-validator/Cargo.toml`
- Modify: `ab-validator/Cargo.lock`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/hegel_smoke.rs`

**Interfaces:**
- Consumes: repository Rust 2024 workspace and Nix `cargoLock` packaging.
- Produces: workspace crate `ab-parser-rq-source-accountability` and a proven
  Hegel test runtime. No measurement API yet.

- [ ] **Step 1: Add only the crate shell and failing Hegel smoke test**

Add the member after `ab-parser-study-report`:

```toml
"crates/ab-parser-rq-source-accountability",
```

Create `Cargo.toml`:

```toml
[package]
name = "ab-parser-rq-source-accountability"
version.workspace = true
edition.workspace = true
license.workspace = true
publish = false

[dependencies]
anyhow.workspace = true
clap.workspace = true
jsonschema.workspace = true
serde.workspace = true
serde_json.workspace = true
sha2.workspace = true

[dev-dependencies]
hegeltest = "0.28"
```

Create `src/lib.rs`:

```rust
#![forbid(unsafe_code)]

pub const fn hegel_probe_value(value: u16) -> u16 {
    value
}
```

Create `tests/hegel_smoke.rs` with an intentionally wrong property:

```rust
use ab_parser_rq_source_accountability::hegel_probe_value;
use hegel::generators;

#[hegel::test]
fn hegel_native_engine_shrinks_counterexamples(tc: hegel::TestCase) {
    let value = tc.draw(generators::integers::<u16>());
    assert_eq!(hegel_probe_value(value), 0);
}
```

- [ ] **Step 2: Update only the lockfile**

Run:

```bash
cargo check -p ab-parser-rq-source-accountability --tests
```

Expected: `Cargo.lock` gains Hegel packages without changing unrelated package
versions.

- [ ] **Step 3: Run RED locally**

Run from `ab-validator/`:

```bash
cargo test -p ab-parser-rq-source-accountability --test hegel_smoke
```

Expected: FAIL with a shrunk nonzero `u16` counterexample. If the engine cannot
start, stop: that is the design's explicit blocker, not permission to substitute
proptest.

- [ ] **Step 4: Make the property true**

Replace the assertion with:

```rust
assert_eq!(hegel_probe_value(value), value);
```

- [ ] **Step 5: Run GREEN locally and through Nix**

Run:

```bash
cargo test -p ab-parser-rq-source-accountability --test hegel_smoke
nix build ./ab-validator#checks.x86_64-linux.cargo-test
```

Expected: both pass without network access during the Nix build.

- [ ] **Step 6: Commit the proven test substrate**

```bash
git add ab-validator/Cargo.toml ab-validator/Cargo.lock \
  ab-validator/crates/ab-parser-rq-source-accountability
git commit -m "test(parser-rq): prove Hegel in the Nix boundary"
```

---

### Task 2: Land ABC-owned protocol schemas and the empty v1 taxonomy

**Files:**
- Create: `abc/schemas/parser-rq-ignored-regions.schema.json`
- Create: `abc/schemas/parser-rq-source-accountability-work.schema.json`
- Create: `abc/schemas/parser-rq-source-accountability-index.schema.json`
- Create: `abc/schemas/parser-rq-source-accountability-aggregate.schema.json`
- Create: `abc/data/parser-rq-ignored-regions-v1.json`
- Create: `abc/test/fixtures/parser-rq/parser-rq-ignored-regions.schema.json`
- Create: `abc/test/fixtures/parser-rq/parser-rq-source-accountability-work.schema.json`
- Create: `abc/test/fixtures/parser-rq/parser-rq-source-accountability-index.schema.json`
- Create: `abc/test/fixtures/parser-rq/parser-rq-source-accountability-aggregate.schema.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: P0 SHA-256 and qualification-identity conventions.
- Produces: four closed JSON protocols; taxonomy ID
  `parser-rq-ignored-regions-v1`; aggregate fields consumed by Tasks 3-7.

- [ ] **Step 1: Write schema-registration tests first**

Add to `validate_design_bundle_test.clj`:

```clojure
(deftest parser-rq-source-accountability-schemas-are-closed-test
  (let [pairs [["schemas/parser-rq-ignored-regions.schema.json"
                "test/fixtures/parser-rq/parser-rq-ignored-regions.schema.json"]
               ["schemas/parser-rq-source-accountability-work.schema.json"
                "test/fixtures/parser-rq/parser-rq-source-accountability-work.schema.json"]
               ["schemas/parser-rq-source-accountability-index.schema.json"
                "test/fixtures/parser-rq/parser-rq-source-accountability-index.schema.json"]
               ["schemas/parser-rq-source-accountability-aggregate.schema.json"
                "test/fixtures/parser-rq/parser-rq-source-accountability-aggregate.schema.json"]]]
    (doseq [[path fixture] pairs]
      (let [contract (files/read-json path)]
        (is (= false (get contract "additionalProperties")) path)
        (is (nil? (schema/validation-errors contract
                                            (files/read-json fixture))) path)))))

(deftest parser-rq-v1-taxonomy-is-empty-test
  (let [taxonomy (files/read-json "data/parser-rq-ignored-regions-v1.json")]
    (is (= "parser-rq-ignored-regions-v1" (get taxonomy "taxonomy_version")))
    (is (= "decoded_utf8" (get taxonomy "coordinate_system")))
    (is (= [] (get taxonomy "rules")))))
```

Create four minimal valid fixture documents under
`abc/test/fixtures/parser-rq/`, one per schema. The aggregate fixture must use:

```json
{
  "schema_version": "abc/parser-rq-source-accountability-aggregate/v1",
  "identity_ref": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
  "taxonomy_version": "parser-rq-ignored-regions-v1",
  "taxonomy_hash": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
  "coordinate_system": "decoded_utf8",
  "status": "ok",
  "work_completeness": {"expected": 1, "observed": 1, "complete": true},
  "eligible_bytes": 3,
  "covered_eligible_bytes": 2,
  "uncovered_eligible_bytes": 1,
  "uncovered": [{"work_id": "w1", "start": 2, "end": 3}]
}
```

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.validate-design-bundle-test/parser-rq-source-accountability-schemas-are-closed-test \
  --focus abc.tools.validate-design-bundle-test/parser-rq-v1-taxonomy-is-empty-test
```

Expected: FAIL because the schema/taxonomy files are absent.

- [ ] **Step 3: Create the closed schemas and taxonomy**

All schemas use Draft 2020-12, `additionalProperties: false`, required identity,
taxonomy, coordinate, status, and integer counter fields. Define `$defs.interval`
once per schema as:

```json
{
  "type": "object",
  "required": ["start", "end"],
  "additionalProperties": false,
  "properties": {
    "start": {"type": "integer", "minimum": 0},
    "end": {"type": "integer", "minimum": 0}
  }
}
```

The work schema additionally requires original/decoded/Parser-IR blob hashes, a
logical blob reference for the raw schema-v3 diagnostic capture (including
sanitizer diagnostics), detected
encoding enum `utf-8|utf-8-bom|windows-31j`, all four interval vectors, and exact
byte counters. The index schema requires an array of closed
`{work_id,sha256,bytes,media_type,locator}` entries. The aggregate schema matches
the fixture in Step 1 and permits status `ok|unavailable`; unavailable documents
carry nonempty string `errors` and no trusted numeric observation.

Create the exact taxonomy:

```json
{
  "$schema": "https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json",
  "schema_version": "abc/parser-rq-ignored-regions/v1",
  "taxonomy_version": "parser-rq-ignored-regions-v1",
  "coordinate_system": "decoded_utf8",
  "rules": []
}
```

Register all four paths in `validate_design_bundle.clj`'s schema path list.

- [ ] **Step 4: Run GREEN and schema drift checks**

```bash
cd abc
bin/kaocha --focus abc.tools.validate-design-bundle-test/parser-rq-source-accountability-schemas-are-closed-test \
  --focus abc.tools.validate-design-bundle-test/parser-rq-v1-taxonomy-is-empty-test
cd ..
nix run ./abc#schema-drift
```

Expected: focused tests and schema drift pass.

- [ ] **Step 5: Commit**

```bash
git add abc/schemas abc/data/parser-rq-ignored-regions-v1.json \
  abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj \
  abc/test/fixtures/parser-rq
git commit -m "feat(parser-rq): define source-accountability protocols"
```

---

### Task 3: Implement interval algebra with Hegel properties

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/interval.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/interval_properties.rs`

**Interfaces:**
- Produces:
  - `Interval::new(start: usize, end: usize, bound: usize) -> Result<Interval>`
  - `normalize(Vec<Interval>) -> Vec<Interval>`
  - `subtract(&[Interval], &[Interval]) -> Vec<Interval>`
  - `intersect(&[Interval], &[Interval]) -> Vec<Interval>`
  - `total_len(&[Interval]) -> Result<u64>`

- [ ] **Step 1: Write failing example and Hegel properties**

Use a generated bounded interval helper that draws `bound: u16`, two endpoints,
orders them with `min/max`, and widens to `usize`. Tests:

```rust
#[test]
fn subtraction_retains_residual_bytes() {
    let source = vec![Interval::new(0, 10, 10).unwrap()];
    let excused = vec![Interval::new(4, 5, 10).unwrap()];
    assert_eq!(subtract(&source, &excused), vec![iv(0, 4), iv(5, 10)]);
}

#[hegel::test]
fn normalization_is_order_and_duplicate_invariant(tc: hegel::TestCase) {
    let intervals = draw_intervals(&tc);
    let mut reversed_twice = intervals.clone();
    reversed_twice.reverse();
    reversed_twice.extend(intervals.clone());
    assert_eq!(normalize(intervals), normalize(reversed_twice));
}

#[hegel::test]
fn subtraction_partitions_source(tc: hegel::TestCase) {
    let source = normalize(draw_intervals(&tc));
    let removed = normalize(draw_intervals(&tc));
    let kept = subtract(&source, &removed);
    let cut = intersect(&source, &removed);
    assert_eq!(total_len(&source).unwrap(),
               total_len(&kept).unwrap() + total_len(&cut).unwrap());
    assert!(intersect(&kept, &cut).is_empty());
}
```

- [ ] **Step 2: Run RED**

```bash
cd ab-validator
cargo test -p ab-parser-rq-source-accountability --test interval_properties
```

Expected: compile failure because `interval` APIs do not exist.

- [ ] **Step 3: Implement the minimal pure sweep algorithms**

`Interval::new` rejects `start > end` and `end > bound`; zero-length intervals
are valid inputs but `normalize` removes them. `normalize` sorts by `(start,end)`
and merges overlap or adjacency. `subtract` and `intersect` operate only on
normalized values and return normalized values. All length addition uses checked
`u64` arithmetic and returns an error on overflow rather than wrapping.

- [ ] **Step 4: Run GREEN and clippy**

```bash
cargo test -p ab-parser-rq-source-accountability --test interval_properties
cargo clippy -p ab-parser-rq-source-accountability --all-targets -- -D warnings
```

Expected: pass.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-parser-rq-source-accountability
git commit -m "feat(parser-rq): add source interval algebra"
```

---

### Task 4: Analyze one work and fail closed at decode/coordinate boundaries

**Files:**
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/model.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/analyze.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/analyze_work.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- Use fixtures: `ab-validator/crates/ab-aozora-aat/tests/data/*.txt`

**Interfaces:**
- Consumes: original bytes, Parser-IR JSON, corpus entry, qualification identity,
  taxonomy identity.
- Produces:
  `analyze_work(WorkInput) -> WorkAnalysis`, where `WorkAnalysis` carries the
  closed work record plus raw schema-v3 diagnostic bytes for P4.
  Failures are serialized `status = unavailable` records with stable error codes.

- [ ] **Step 1: Add dependencies and failing contract tests**

Add:

```toml
ab-aozora-aat = { path = "../ab-aozora-aat" }
```

Write tests proving:

```rust
#[test]
fn lossy_shift_jis_is_unavailable_even_when_span_covers_replacement() { /* bytes [0xff,0x82] */ }

#[test]
fn absent_coordinate_system_is_unavailable() { /* node span {start:0,end:1} */ }

#[test]
fn paragraph_and_sentence_spans_do_not_cover_missing_nodes() { /* nodes:[] */ }

#[test]
fn nested_node_spans_count_union_bytes_once() { /* [0,10), [2,4) => 10 */ }

#[test]
fn rebased_body_and_tail_spans_slice_full_decoded_text() {
    // Use UTF-8 BOM + CRLF + body + 底本 tail fixture. Assert each accepted
    // node range indexes DecodedSource.text and the tail range is full-text,
    // not body-relative.
}
```

In `ab-aat-to-parser-ir/tests/integration.rs`, add a characterization that runs
the existing AAT-to-Parser-IR fixture conversion on a source with a nonzero body
offset and terminal provenance, then asserts every node span indexes the full
decoded text and the tail node start is greater than the body-relative length.
This pins the actual converter boundary instead of merely testing hand-authored
Parser-IR.

- [ ] **Step 2: Run RED**

```bash
cd ab-validator
cargo test -p ab-parser-rq-source-accountability --test analyze_work
```

Expected: compile failure because `analyze_work` and models do not exist.

- [ ] **Step 3: Implement closed serde models and analyzer**

Use `#[serde(deny_unknown_fields)]` on every wire struct. Hash original bytes,
decoded text, Parser-IR bytes, taxonomy JCS bytes, and the canonical qualification
identity. Reject `DecodedSource.encoding == "windows-31j-lossy"`. Validate every
node has integer start/end and explicit `decoded_utf8`; never read paragraph or
sentence spans. Normalize node intervals, compute complement against `[0,text.len())`,
and assert both conservation identities before returning `ok`.

Call `ab_aozora_aat::diagnostics_json_from_bytes` on the same original bytes.
Return those raw schema-v3 bytes alongside the record and place their
`sha256/bytes/media_type` logical reference in the record. Do not interpret or
authorize them in P1.

Validate Parser-IR `schema_id`, `schema_hash`, and every present `derived_from`
adapter/mapping coordinate against qualification identity. Producer executable
attestation remains P5-only and is not synthesized here.

- [ ] **Step 4: Run GREEN and full crate tests**

```bash
cargo test -p ab-parser-rq-source-accountability
```

Expected: all analyzer and interval tests pass.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/Cargo.toml ab-validator/Cargo.lock \
  ab-validator/crates/ab-parser-rq-source-accountability \
  ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat(parser-rq): measure per-work Parser-IR byte coverage"
```

---

### Task 5: Produce a deterministic corpus-driven record index

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/index.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/main.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/corpus_index.rs`

**Interfaces:**
- Produces:
  `analyze_corpus(CorpusInput) -> RecordIndex`
  and CLI commands `analyze-work`, `analyze-corpus`.
- Index entries are canonical corpus order and carry
  `work_id, sha256, bytes, media_type, locator`.

- [ ] **Step 1: Write failing deterministic-index tests**

Create a temp source/IR root with two corpus entries and an unrelated extra file.
Assert:

```rust
let first = analyze_corpus(input.clone()).unwrap();
let second = analyze_corpus(input).unwrap();
assert_eq!(canonical_json(&first), canonical_json(&second));
assert_eq!(first.records.iter().map(|r| &r.work_id).collect::<Vec<_>>(),
           vec!["work-a", "work-b"]);
assert!(!canonical_json(&first).contains("unrelated"));
```

Also assert duplicate corpus work IDs fail before writing any index.

- [ ] **Step 2: Run RED**

```bash
cd ab-validator
cargo test -p ab-parser-rq-source-accountability --test corpus_index
```

Expected: compile failure because index APIs do not exist.

- [ ] **Step 3: Implement corpus-driven production**

Iterate only corpus entries. Resolve source paths below `source_root` and
Parser-IR paths as `<parser_ir_root>/<work_id>.json`, rejecting canonical-path
escape. Write each canonical work record under
`sha256/<first-two>/<digest>.json`; write its raw diagnostic blob by
content identity as well; then write the canonical index atomically.
Do not glob roots or derive membership from files. `--index-out` is the only
committed summary path; `--store-root` remains runtime configuration.

- [ ] **Step 4: Run GREEN and CLI smoke**

```bash
cargo test -p ab-parser-rq-source-accountability --test corpus_index
cargo run -p ab-parser-rq-source-accountability -- --help
```

Expected: tests pass; help lists `analyze-work` and `analyze-corpus`.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-parser-rq-source-accountability
git commit -m "feat(parser-rq): generate corpus-bound record indexes"
```

---

### Task 6: Aggregate exact R1 evidence and implement pure R2 partitioning

**Files:**
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/aggregate.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/src/reconcile.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/lib.rs`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/main.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/aggregate.rs`
- Create: `ab-validator/crates/ab-parser-rq-source-accountability/tests/reconcile.rs`

**Interfaces:**
- Produces:
  - `aggregate(corpus, index, records, identity, taxonomy) -> AggregateRecord`
  - `reconcile(uncovered, authorized) -> Reconciliation`
- P1 aggregate always serializes `silent_drops = instrument-missing`.

- [ ] **Step 1: Write failing aggregate tests**

Tests must show a work-count denominator cannot pass:

```rust
#[test]
fn aggregate_uses_bytes_not_work_count() {
    let result = aggregate(two_records(9, 10, 100, 100)).unwrap();
    assert_eq!(result.covered_eligible_bytes, 109);
    assert_eq!(result.eligible_bytes, 110);
    assert!(!result.exactly_covered());
}
```

Add missing, duplicate, extra, blob-hash mismatch, identity mismatch, taxonomy
mismatch, unavailable work, and zero-denominator cases; all must yield aggregate
`unavailable`, never a partial number.

- [ ] **Step 2: Write failing R2 residual test**

```rust
#[test]
fn one_byte_authorization_does_not_excuse_a_large_gap() {
    let result = reconcile(&[iv(0, 10)], &[iv(4, 5)]);
    assert_eq!(result.diagnosed, vec![iv(4, 5)]);
    assert_eq!(result.silent, vec![iv(0, 4), iv(5, 10)]);
    assert_eq!(result.silent_drops, 2);
}
```

Add a test that adjacent source constructs remain one interval witness and label
the count as non-census metadata.

- [ ] **Step 3: Run RED**

```bash
cd ab-validator
cargo test -p ab-parser-rq-source-accountability --test aggregate --test reconcile
```

Expected: compile failure because aggregate/reconcile APIs do not exist.

- [ ] **Step 4: Implement exact folds and partitioning**

Authenticate index blob identities by streaming every record before decoding it.
Compare corpus/index/record work IDs as both set and count. Sum checked integers,
preserve uncovered witnesses with work IDs, and recompute conservation identities.
Never accept supplied totals/status fields as authority.

Implement R2 as `intersect(uncovered, authorized)` and
`subtract(uncovered, authorized)`; `silent_drops` is the normalized silent vector
length. Keep raw diagnostics out of this API. Add the `aggregate` CLI command
only after these tests pass.

- [ ] **Step 5: Run GREEN, fmt, and clippy**

```bash
cargo test -p ab-parser-rq-source-accountability
cargo fmt --check
cargo clippy -p ab-parser-rq-source-accountability --all-targets -- -D warnings
```

Expected: pass.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/crates/ab-parser-rq-source-accountability
git commit -m "feat(parser-rq): aggregate byte evidence and partition silent gaps"
```

---

### Task 7: Derive the P0 envelope and drift-test a fixture capture

**Files:**
- Create: `abc/src/abc/tools/parser_rq_source_accountability.clj`
- Create: `abc/test/abc/tools/parser_rq_source_accountability_test.clj`
- Create: `abc/test/fixtures/parser-rq/source-accountability/` small fixture set
- Modify: `abc/docs/reports/parser-release-qualification-measurements.edn`

**Interfaces:**
- Produces:
  `derive-source-span-envelope(store, manifest, aggregate, identity) -> envelope
  | unavailable`.
- R2 remains `{:value :instrument-missing :identity_ref ...}`.

- [ ] **Step 1: Write the first failing denominator test**

```clojure
(deftest byte-denominator-is-required-not-work-count
  (let [aggregate {:status "ok"
                   :coordinate_system "decoded_utf8"
                   :identity_ref identity-ref
                   :taxonomy_version "parser-rq-ignored-regions-v1"
                   :eligible_bytes 10
                   :covered_eligible_bytes 9
                   :uncovered_eligible_bytes 1}
        manifest {:blobs [aggregate-blob]
                  :denominator {:value 1 :unit "works"}}]
    (is (= :unavailable
           (:status (rq-source/derive-source-span-envelope
                     store manifest aggregate identity))))))
```

Add tests for a matching 10/10 aggregate (`:value 1.0M`), a 9/10 aggregate
(`:value 0.9M`), one-byte deficit with a large denominator (strictly below 1),
identity/taxonomy mismatch, counter non-conservation, blob mismatch, and missing
work completeness.

- [ ] **Step 2: Run RED**

```bash
cd abc
bin/kaocha --focus abc.tools.parser-rq-source-accountability-test
```

Expected: namespace/function missing.

- [ ] **Step 3: Implement the fail-closed derivation**

Call P0 `verify-manifest` first. Require denominator unit
`decoded_utf8_bytes` and value equal aggregate `eligible_bytes`; require full
identity ref, taxonomy version/hash, `work_completeness.complete`, and the two
counter conservation identities. Derive the decimal with:

```clojure
(defn- exact-display-ratio [covered eligible]
  (let [scale (inc (count (str eligible)))]
    (.divide (bigdec covered) (bigdec eligible)
             scale java.math.RoundingMode/DOWN)))
```

Return P0's closed envelope `{:value ratio :identity_ref expected}`. Never read a
reported floating ratio or reported pass status.

- [ ] **Step 4: Add fixture Capture -> Derive -> Drift**

Use two tiny UTF-8 sources and Parser-IR fixtures. Generate records/index/aggregate
twice into temporary stores and assert byte-identical index and aggregate JSON.
Commit only the small expected index/aggregate/manifest witnesses. Keep the
historical campaign measurement's R1 value `:instrument-missing`; P5 owns the
authoritative candidate capture.

- [ ] **Step 5: Run focused Rust and Clojure verification**

```bash
cd ab-validator
cargo test -p ab-parser-rq-source-accountability
cd ../abc
bin/kaocha --focus abc.tools.parser-rq-source-accountability-test \
  --focus abc.tools.parser-rq-capture-test \
  --focus abc.tools.parser-release-qualification-test
```

Expected: all pass; campaign report remains honestly not-qualified with R1/R2
unavailable until P5/P4 respectively.

- [ ] **Step 6: Run repository quality gates**

```bash
cd ..
clj-paren-repair abc/src/abc/tools/parser_rq_source_accountability.clj \
  abc/test/abc/tools/parser_rq_source_accountability_test.clj
scripts/comment-hygiene-check.sh
git diff --check
just validate-migration
```

Expected: comment hygiene, formatting, focused tests, Rust/Nix checks, schema
drift, and ADR governance pass.

- [ ] **Step 7: Commit**

```bash
git add abc/src/abc/tools/parser_rq_source_accountability.clj \
  abc/test/abc/tools/parser_rq_source_accountability_test.clj \
  abc/test/fixtures/parser-rq/source-accountability \
  abc/docs/reports/parser-release-qualification-measurements.edn
git commit -m "feat(parser-rq): derive source-span qualification evidence"
```

---

## Completion review

Before integration:

1. Confirm every schema field used by Rust matches the ABC schema spelling and
   type exactly.
2. Confirm `windows-31j-lossy`, absent coordinates, malformed intervals, missing
   work, and blob mismatches all produce unavailable results.
3. Confirm the only R1 pass path is exact integer numerator/denominator equality.
4. Confirm no raw diagnostic can reach the R2 reconciler.
5. Confirm record-index regeneration is byte-identical and corpus-driven.
6. Request an independent code review, run fresh verification, then use
   `superpowers:finishing-a-development-branch` for the user-authorized merge and
   push workflow.
