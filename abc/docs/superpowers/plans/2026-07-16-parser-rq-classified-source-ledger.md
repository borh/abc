# Parser RQ P4A1 Classified-Source Ledger Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Decomplect accepted text from verbatim recovery in the live tiled
classifier, freeze the ABC role/disposition vocabulary, and project one
authenticated classified-source ledger in the existing normalize/fold traversal.

**Architecture:** `ClassifyStream` remains the sole tiled consumption stream.
A behavior-preserving provenance split lands before new semantics; the fused
fold then produces immutable facts beside its current effects. ABC owns the
closed protocol and policy; capture publishes one generation manifest last.

**Tech Stack:** Rust 2024, serde/serde_json, sha2, Hegel 0.28; Clojure, Malli,
Charred; Draft 2020-12 JSON Schema; Nix flakes and Kaocha.

## Global Constraints

- Custom `ab-aozora` only; third-party parsers remain research artifacts.
- Preserve parser output behavior before adding ledger behavior.
- No second visitor, event bus, mutable claim collector, or reconstruction pass.
- Public spans are nonempty half-open `decoded_utf8` intervals.
- ABC owns public roles/dispositions; parser-private codes are evidence only.
- `DirectiveKind::Unknown` is `preserved_opaque`, never recognized.
- One content-addressed manifest binds source, parser output, diagnostics,
  ledger, and policy; members do not embed the manifest's `generation_ref`.
- Corpus-scale artifacts stay external.
- Follow `docs/comment-standards.md` and run comment hygiene.

---

### Task 1: Preserve and remove the falsifying diagnostic probe

**Files:**
- Create: `ab-validator/docs/superpowers/reports/2026-07-16-parser-rq-diagnostic-gap-falsification.json`
- Create: `ab-validator/docs/superpowers/reports/2026-07-16-parser-rq-diagnostic-gap-falsification.md`
- Delete: `ab-validator/crates/ab-parser-rq-source-accountability/tests/diagnostic_authorization_characterization.rs`
- Restore: `ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml`
- Restore: `ab-validator/Cargo.lock`

**Interfaces:** Produces immutable 21-row evidence explaining the old P4A stop.

- [ ] Run the disposable test twice with distinct output paths and `cmp`.
  Expected: byte-identical JSON and 21 unique codes.
- [ ] Assert every source row has `r1_status = "ok"` and empty `r1_errors`.
  Record the three non-emitting documented repros without rewriting them.
- [ ] Record exact observed counts: one proposed authorizer with zero gap
  intersection, twelve observe-only codes with gap intersection, three
  non-emitting source repros, and four internal codes.
- [ ] Delete the test and restore dependency files. Verify:

```bash
git diff -- ab-validator/Cargo.lock \
  ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml
test ! -e ab-validator/crates/ab-parser-rq-source-accountability/tests/diagnostic_authorization_characterization.rs
```

- [ ] Commit only the reports:

```bash
git add ab-validator/docs/superpowers/reports/2026-07-16-parser-rq-diagnostic-gap-falsification.*
git commit -m "docs(parser-rq): preserve diagnostic gap falsification"
```

### Task 2: Characterize the live tiled classifier

**Files:**
- Create temporarily: `ab-validator/crates/ab-aozora-pipeline/tests/classified_source_characterization.rs`
- Create: `ab-validator/docs/superpowers/reports/2026-07-16-classified-source-provenance.json`
- Create: `ab-validator/docs/superpowers/reports/2026-07-16-classified-source-provenance.md`
- Delete before commit: temporary test above

**Interfaces:** Proves where the classifier must preserve the binary distinction
between accepted text and recovered verbatim bytes.

- [ ] Write an ignored probe covering ordinary text, newline, literal
  quote/tortoise punctuation, solo refmark/bar/hash, unclosed open, unmatched
  close, declined gaiji, `DirectiveKind::Unknown`, every live `NodeKind`,
  block open, and block close. Record input bytes, pair events, classified
  spans, diagnostics, normalized SHA-256, and verbatim SHA-256.
- [ ] Run twice:

```bash
CLASSIFIED_SOURCE_OUT=/tmp/classified-source-1.json \
  cargo test -p ab-aozora-pipeline --test classified_source_characterization -- --ignored
CLASSIFIED_SOURCE_OUT=/tmp/classified-source-2.json \
  cargo test -p ab-aozora-pipeline --test classified_source_characterization -- --ignored
cmp /tmp/classified-source-1.json /tmp/classified-source-2.json
```

- [ ] Stop before Task 3 if recovered bytes cannot be distinguished before
  `flush_plain_up_to` or require AAT/Parser-IR reconstruction. The public output
  is intentionally binary; branch-specific explanations remain private
  evidence and diagnostics.
- [ ] Delete the probe and commit the two reports as
  `docs(parser-rq): characterize classified-source provenance`.

### Task 3: Split plain provenance without changing behavior

**Files:**
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/lexer/classify/mod.rs`
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/fold.rs`
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs`
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/lexer/instrumentation.rs`
- Test: `ab-validator/crates/ab-aozora-pipeline/tests/plain_provenance.rs`

**Interfaces:**

```rust
pub enum PlainProvenance { Text, RecoveredVerbatim }
pub struct PlainSpan { pub provenance: PlainProvenance }
pub enum SpanKind {
    Plain(PlainSpan),
    Aozora(Node),
    BlockOpen(RegionFormat),
    BlockClose(RegionClose),
    Newline,
}
```

Task 2 proved the binary distinction at classifier-local branches. Do not add a
reason enum: R1 consumes only binary provenance, while diagnostics retain
specific recovery explanations.

- [ ] Write RED tests asserting distinct provenance for every accepted recovery
  case while pinning normalized bytes, AST projection, diagnostics JSON, AAT
  JSON, HTML, and `to_source_verbatim`.
- [ ] Run `cargo test -p ab-aozora-pipeline --test plain_provenance`.
  Expected: compile failure because `PlainProvenance` is absent.
- [ ] Replace `pending_plain_start` with adjacent provenance segments; merge
  only equal adjacent provenance. Fold every `Plain(_)` byte-identically to
  the old `Plain`.
- [ ] Run:

```bash
cargo test -p ab-aozora-pipeline
cargo test -p ab-aozora-facade
cargo test -p ab-aozora-aat
```

- [ ] Commit only this behavior-preserving refactor as
  `refactor(aozora): separate plain recovery provenance`.

### Task 4: Define ABC classified-source policy and protocols

**Files:**
- Create: `abc/schemas/parser-rq-classified-source-policy.schema.json`
- Create: `abc/schemas/parser-rq-classified-source-ledger.schema.json`
- Create: `abc/schemas/parser-rq-capture-generation.schema.json`
- Create: `abc/data/parser-rq-ab-aozora-classified-source-v1.json`
- Create: `abc/test/fixtures/parser-rq/classified-source/*.json`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:** Produces closed schemas and policy consumed by Tasks 5-6/P4A2.

- [ ] Write RED tests rejecting unknown fields, duplicate rows,
  `preserved_opaque` with any role except `unrecognized_source_form`,
  semantic dispositions without target identity, structural claims without
  witnesses, member-content hash mismatches, and a mismatched manifest
  `generation_ref`.
- [ ] Run
  `cd abc && bin/kaocha --focus abc.tools.validate-design-bundle-test/parser-rq-classified-source`.
  Expected: missing-schema failure.
- [ ] Implement closed schemas. `target_identity` is exactly
  `{artifact_ref,value_hash,relation}`, relation `emits|preserves`. Define
  finite runtime construct/evidence/witness rules independently of Task 2/3;
  retain the complete matrix as characterization evidence and assert that each
  observation maps to one runtime rule. No wildcard role or disposition.
- [ ] Compute and assert schema/policy hashes via
  `abc.tools.hash/sha256-json-jcs`.
- [ ] Run focused Kaocha and `nix run .#schema-drift`; commit as
  `feat(parser-rq): define classified-source protocols`.

### Task 5: Project immutable facts in the existing fused fold

**Files:**
- Create: `ab-validator/crates/ab-aozora-syntax/src/ast/classified_source.rs`
- Modify: `ab-validator/crates/ab-aozora-syntax/src/ast/mod.rs`
- Modify: `ab-validator/crates/ab-aozora-syntax/src/ast/output.rs`
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/fold.rs`
- Modify: `ab-validator/crates/ab-aozora-pipeline/src/pipeline.rs`
- Test: `ab-validator/crates/ab-aozora-pipeline/tests/classified_source_facts.rs`

**Interfaces:** Adds
`LexOutput::classified_source_facts: Vec<ClassifiedSourceFact>`; facts remain
sanitized-coordinate internal values until Task 6 rebases them.

- [ ] Write RED tests for plain text, typed nodes, blocks, unknown directives,
  recovery, newline normalization, overlaps, and complete-value ordering.
- [ ] Add closed Rust enums mirroring policy. In the existing
  `Recorder::emit(&ClassifiedSpan)` match, produce the current normalization
  effect and fact candidates together. Add no callbacks or second pass.
- [ ] Add Hegel properties: permutation canonicalizes identically; semantic
  intervals are subsets of accounted; `PreservedOpaque` is never semantic.
- [ ] Run pipeline and syntax suites; commit as
  `feat(aozora): project classified-source facts`.

### Task 6: Extract the existing authenticated artifact-store boundary

**Files:**
- Create: `ab-validator/crates/ab-rq-artifact-store/Cargo.toml`
- Create: `ab-validator/crates/ab-rq-artifact-store/src/lib.rs`
- Create: `ab-validator/crates/ab-rq-artifact-store/tests/store.rs`
- Modify: `ab-validator/Cargo.toml`
- Modify: `ab-validator/Cargo.lock`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/Cargo.toml`
- Modify: `ab-validator/crates/ab-parser-rq-source-accountability/src/index.rs`

**Interfaces:**

```rust
pub fn authenticate_blob(
    root: &Path,
    locator: &str,
    expected_sha256: &str,
    expected_bytes: u64,
) -> anyhow::Result<Vec<u8>>;
pub fn publish_blob(root: &Path, extension: &str, bytes: &[u8])
    -> anyhow::Result<PublishedBlob>;
pub fn write_atomic_summary(path: &Path, bytes: &[u8]) -> anyhow::Result<()>;
```

- [ ] Move P1's existing CAS tests onto this API before moving implementation.
  Pin traversal, absolute path, symlink ancestor/destination, length/hash
  mismatch, collision, concurrent winner, and atomic-summary behavior.
- [ ] Run the new test and confirm RED because the crate is absent.
- [ ] Move—not copy—the current P1 functions into the new crate. Keep P1 output
  byte-identical and remove its private duplicates.
- [ ] Run artifact-store and complete P1 suites; commit only this
  behavior-preserving extraction as
  `refactor(parser-rq): share authenticated artifact storage`.

### Task 7: Capture one authenticated decoded-coordinate generation

**Files:**
- Create: `ab-validator/crates/ab-aozora-aat/src/classified_source.rs`
- Modify: `ab-validator/crates/ab-aozora-aat/src/lib.rs`
- Test: `ab-validator/crates/ab-aozora-aat/tests/classified_source_capture.rs`
- Create: `abc/test/fixtures/parser-rq/classified-source-capture/*`

**Interfaces:**

```rust
pub fn classified_source_ledger_from_bytes(bytes: &[u8]) -> anyhow::Result<Vec<u8>>;
pub fn capture_generation_from_bytes(bytes: &[u8]) -> anyhow::Result<CaptureGeneration>;
```

- [ ] Write RED tests for BOM, CRLF, accent, decorative insertion, PUA,
  typed syntax, opaque unknown, and malformed recovery. Assert UTF-8 boundaries,
  content-authenticated members, and one manifest `generation_ref` binding the
  exact member tuple without embedding that reference in member content.
- [ ] Rebase through existing `SpanContext`; emit reversible proofs for
  CRLF/accent, no claim for inserted blank lines, and a semantic gap for PUA.
- [ ] Publish immutable members first and canonical manifest last using
  `ab-rq-artifact-store`. Reject cross-generation replacement.
- [ ] Generate a small production fixture, bless once, and regenerate twice.
  Mutate source, ledger, policy, and generation reference; each must fail.
- [ ] Run the focused test and comment hygiene; commit as
  `feat(parser-rq): capture classified-source generations`.

## Completion Gate

Request whole-range review for parallel consumption history, recovered bytes
counted semantic, unknown directives counted recognized, sanitizer catch-alls,
unstable target identity, and cross-generation mixing. Run
`just validate-migration` on the exact final commit.
