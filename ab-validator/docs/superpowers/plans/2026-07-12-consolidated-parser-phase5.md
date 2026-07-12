# Consolidated Parser Phase 5 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the bare-toggle inline classifier as identity rotation C5 (`ab-aozora 0.5.0 → 0.6.0`) with the full gate ceremony, resolve the keigakomi denominator residual, confirm the ABC 0.3.0 contract so coverage reads COMPLETE, and clear the hygiene items — per the approved spec `ab-validator/docs/superpowers/specs/2026-07-12-consolidated-parser-phase5-bare-toggle-inline-design.md` (revision 2).

**Architecture:** The classifier is a per-line two-pass pairing pass over the adapter's already-built inline arrays (`crates/ab-aozora-aat`), emitting existing schema-v2 `inline_container` kinds `"yokogumi"`/`"keigakomi"`; no AAT schema bump. Instruments (delta-audit mode, checkpoint verifier) land before the identity closes. Mapping 0.3.0 is frozen as an immutable file before the 0.4.0 edit; dispatch binds the full generation tuple. Ceremony mirrors Phase 4: gates → registry row → admission → atomic run-set repoint → checkpoint.

**Tech Stack:** Rust (adapter/converter, proptest for the property target), Python 3 (report instruments), Clojure (abc admission tool), just, nix, hinoki (`hinoki.hyakutake-barbel.ts.net`) for corpus-scale runs.

## Global Constraints

- Exact recognition tokens, nothing else: yokogumi open `［＃横組み］` close `［＃横組み終わり］`; keigakomi open `［＃罫囲み］` close `［＃罫囲み終わり］`.
- Line grammar is the spec's Contract 1 two-pass algorithm over ONE global nesting stack; the normative Python model is `classify_tokens` (text wrapper `classify_line`) in `reports/aat-fidelity/bare-toggle-placement.py`; the Rust implementation mirrors it test-for-test.
- Corpus-bound expectations (grammar-true, placement report Revision 3 — four-class candidate classification over `reports/lib/corpus_reader.py`, the two tolerant-7zz-only recoveries verified toggle-free): adopted yokogumi pairs **1582**; adopted keigakomi pairs **25**; declined raw-preserved markers **24** = 10 orphan opens + 14 rollback markers; 0 orphan closes; 0 interleavings; 1 proper cross-construct nesting.
- Independence rule (review P5-4): the delta audit DERIVES expected adoptions and decline reasons from the baseline dump's raw marker nodes via `classify_tokens`; the placement report is preregistered design evidence, never a gate input.
- Invalid markers stay byte-identical raw nodes; no new warning codes; invalidation is construct-scoped per line; rewrites are atomic per line.
- Adopted-pair emission: one `inline_container` `{"kind": "yokogumi"|"keigakomi", "content": [...], "span": ...}`; span covers first byte of open marker through last byte of close marker in ADR 0024 decoded-source coordinates; marker tokens are consumed.
- No AAT schema bump — documents stay `version 2`. AAT schema files are untouched.
- Mapping: freeze `data/aat-to-parser-ir-mapping-v2.json` byte-exact as `data/aat-to-parser-ir-mapping-v2-0.3.0.json` (hash must remain `sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40`) BEFORE editing the live file to `mapping_version 0.4.0` (`source_aat_version` stays 2). Registry is append-only; existing rows and frozen files are never edited.
- Identity discipline: instruments land BEFORE the C5 identity closes; candidate commit recorded in `.superpowers/sdd/phase5-identity.json`; join key `ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3` (facade coordinate confirmed by Task 1; if Task 1 finds the facade must change, STOP and escalate — the plan assumes it does not); never derive candidates from `HEAD`; every hinoki run is candidate-commit-bound.
- Perf: a median regression greater than 10% on the pinned workset (`data/perf-workset.json`) blocks admission; `001562_56145` is individually watched.
- Conversion audit: 17886 attempted / 17886 succeeded / 0 failed under mapping 0.4.0.
- Conformance baselines to hold: ab-aozora full lane 127 vectors with 25/25 `must` and zero row drift vs C4; seed lane 30 vectors at 22 pass / 8 warning / 0 fail / 0 skip.
- Ceremony order: producer reports → registry row → admission (`:admitted`, byte-exact row copy) → ONE atomic repoint commit → checkpoint (`verify-phase5-checkpoint.py` → `CHECKPOINT OK`).
- Hygiene items (Tasks 12a–12c) are independent commits, never part of C5's identity.
- Frozen evidence reports (dated ≤ 2026-07-12 and marked frozen) are never edited. Hinoki dumps in the never-delete list are never deleted; retention is append-only.
- Every task leaves the repo green: `cargo check --workspace --all-targets`, `cargo fmt --check`, `just clippy`, and the Python test suites for any touched instrument.
- Heavy corpus runs happen on hinoki at `hinoki.hyakutake-barbel.ts.net` (the bare `hinoki` ssh alias resolves to the WRONG host — always use the FQDN). hinoki sshd uses PrivateTmp: persist outputs under `~` or `/db`, never `/tmp`. Launch multi-hour runs detached (`nohup … </dev/null &`) and poll synchronously.
- Ambient `AB_AAT_RUN_SET` / `SORANOHA_WORKSPACE_ROOT` may point at the MAIN checkout — unset or override them inside worktrees.

## File Structure (what this plan touches)

| Path | Role |
| --- | --- |
| `crates/ab-aozora-aat/src/lib.rs` | classifier pass + unit tests (Tasks 1, 4, 5) |
| `crates/ab-aozora-aat/Cargo.toml` | proptest dev-dep (Task 5); version bump (Task 8) |
| `crates/ab-aozora-aat/tests/goldens/*` | golden `adapter_version` bumps (Task 8) |
| `crates/ab-aozora-aat/tests/bare_toggle_model.rs` | Rust↔Python mirror + property tests (Task 5) |
| `reports/lib/corpus_reader.py` | shared source-reading contract (landed with the plan; consumed by the placement instrument and Task 11) |
| `reports/aat-fidelity/audit-aat-delta.py` (+ `tests/`) | `bare-toggle-adoption` mode (Task 2) |
| `reports/aat-fidelity/bare-toggle-model-vectors.json` | shared grammar test vectors (Task 2) |
| `reports/aat-fidelity/verify-phase5-checkpoint.py` (+ `tests/`) | checkpoint verifier (Task 3) |
| `data/aat-to-parser-ir-mapping-v2-0.3.0.json` | frozen 0.3.0 generation (Task 6) |
| `data/aat-to-parser-ir-mapping-v2.json` | mapping 0.4.0 (Task 6) |
| `crates/ab-aat-to-parser-ir/src/mapping.rs` + `README.md` | generation-binding preflight + docs (Task 6) |
| `reports/parser-ir/publication-coverage.py` | 0.3.0 contract pin (Task 7) |
| `ab-validator/justfile` | coverage recipe trusted-path fix (Task 7) |
| `crates/ab-aozora-facade/{src/lib.rs,Cargo.toml}` | stub-feature removal (Task 0) |
| `reports/aat-fidelity/keigakomi-residual-attribution.py` | 44-residual instrument (Task 11) |
| `reports/aat-fidelity/verify-golden-spans.py` | CRLF signature suppression (Task 12c) |
| `crates/ab-aozora-facade/tests/json_format.rs` | facade Segments-skip test (Task 12b) |
| `abc/data/aat-parser-ir-compatibility.edn` | C5 row (Task 10) |
| `reports/aat-fidelity/run-sets/current.json` | atomic repoint (Task 10) |

**EXECUTION ORDER (binding; review P5-3 moved the mapping freeze ahead of the verifier): 0 → 1 → 2 → 6 (mapping freeze + 0.4.0) → 3 (verifier) → 4 → 5 → 7 → 8 → 9 → 10 → 11 → 12 → 13.** Task numbers are stable identifiers, not execution order. Tasks 2, 6, 3 (instruments + mapping generations) all precede Task 8 (identity close) per the identity discipline. Task 7 (ABC confirmation) is independent and must land before Task 10's post-repoint coverage regeneration.

---

### Task 0: Repair the Phase 4 baseline

The integrated baseline is red under the repo's own gates. Two independent commits: (a) formatting, (b) the cst/query stub-feature contradiction.

**Files:**
- Modify: 6 files with fmt drift — `crates/ab-aozora/tests/wire.rs`, `crates/ab-aozora-aat/src/lib.rs` (24 hunks), `crates/ab-aozora-aat/tests/goldens.rs`, `crates/ab-aozora-pipeline/src/lexer/sanitize.rs`, `crates/ab-morph-analyzers/src/span_builder.rs`, `crates/ab-morph-analyzers/src/vibrato.rs`
- Modify: `crates/ab-aozora-facade/src/lib.rs` (delete lines 264–285 `cst` block+comment and 287–314 `query` block+comment)
- Modify: `crates/ab-aozora-facade/Cargo.toml` (delete the stub-feature rationale comment at ~84–93 and the `cst = []` / `query = []` lines at 94–95)

**Interfaces:**
- Consumes: nothing.
- Produces: a green baseline every later task inherits. No API changes — the deleted modules were uncompilable behind never-enabled features.

- [ ] **Step 1: Formatting commit**

```bash
cd ab-validator && cargo fmt && cargo fmt --check
```
Expected: second command exits 0, no output.

```bash
git add crates/ab-aozora/tests/wire.rs crates/ab-aozora-aat/src/lib.rs crates/ab-aozora-aat/tests/goldens.rs crates/ab-aozora-pipeline/src/lexer/sanitize.rs crates/ab-morph-analyzers/src/span_builder.rs crates/ab-morph-analyzers/src/vibrato.rs
git status --porcelain   # verify ONLY the six fmt files are staged; unstage anything else
git commit -m "chore(rust): clear committed formatting drift (fmt --check green)"
```
(If `cargo fmt` touched files beyond these six, list them in the report and stage them too — but investigate first; unexpected drift outside the known set may indicate uncommitted local work that must NOT be swept into this commit.)

- [ ] **Step 2: Verify the clippy failure is exactly the stub features**

```bash
cd ab-validator
cargo clippy --workspace --all-targets --all-features 2>clippy.err; echo "clippy_exit=$?"
grep -E "^error" clippy.err | grep -vE "aozora_cst|aozora_query|could not compile \`ab-aozora-facade\`" | head -10
rm clippy.err
```
Expected: `clippy_exit` NON-zero, and the second grep prints NOTHING (every error line names `aozora_cst`/`aozora_query` or the facade compile failure they cause). If the second grep prints anything, STOP and report BLOCKED — that is a different inherited failure the plan does not know about.

- [ ] **Step 3: Remove the stub features and their gated modules**

In `crates/ab-aozora-facade/src/lib.rs`, delete the `#[cfg(feature = "cst")] pub mod cst { … }` block together with its preceding explanatory comment (lines 264–285) and the `#[cfg(feature = "query")] pub mod query { … }` block with its comment (lines 287–314). In `crates/ab-aozora-facade/Cargo.toml`, delete the `cst = []` and `query = []` lines and the multi-line comment explaining them (the block starting "Stub features (Task 3 lift): …"). Do NOT touch the `entries`/`json`/`schema`/`proptest` features.

Decision context for the reviewer: these features exist only for upstream feature-name parity; enabling them has always failed to compile by design (Cargo.toml's own comment). The repo's `just clippy` gate runs `--all-features`, so the gate and the design contradict each other; removal is the YAGNI resolution the spec (Task 0, option 1) chose. The alternative (scoping the gate) was named in the spec; if you find a concrete reason removal breaks something, STOP and escalate rather than switching approaches silently.

- [ ] **Step 4: Verify all gates green**

```bash
cd ab-validator && cargo check --workspace --all-targets && cargo fmt --check && just clippy
```
Expected: all exit 0.

- [ ] **Step 5: Run the Phase 4 instrument tests and the live checkpoint**

```bash
cd ab-validator && python3 -m pytest reports/aat-fidelity/tests/ reports/lib/tests/ -q
```
Expected: all pass.

Then re-run the Phase 4 checkpoint exactly as recorded in `docs/superpowers/reports/2026-07-12-phase4-acceptance-wholesale.md` (§ "Checkpoint invocation", lines ~206–222 — copy the `verify-phase4-checkpoint.py` command verbatim from there, including `--activation-commit 1333b43dd103480d9064935cd1cf3027706531ee`).
Expected: `CHECKPOINT OK`, exit 0. Requires `/db` run-set dumps only if the run-set resolver is invoked; the checkpoint reads committed summaries + git trees + re-runs admission (needs `clojure` on PATH, cwd `abc/`).

- [ ] **Step 6: Commit**

```bash
git add crates/ab-aozora-facade/src/lib.rs crates/ab-aozora-facade/Cargo.toml
git commit -m "chore(facade): remove uncompilable cst/query stub features

just clippy runs --all-features; the stub features were documented as
intentionally failing to compile, so the gate and the design
contradicted each other. Phase 5 Task 0 resolution per spec."
```

---

### Task 1: Observability preflight — bare toggles are visible raw directive nodes

Pin, with a test, that the facade wire already delivers bare-toggle markers as in-line `directive` nodes whose spans recover the exact tokens — the precondition Contract 1's emission needs. If this test cannot be made to pass without facade changes, STOP: report BLOCKED (the facade 0.3.1 path requires a plan amendment, per spec).

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (add test in `mod tests`)
- Modify: `.superpowers/sdd/phase5-identity.json` (create, preflight fields only)

**Interfaces:**
- Consumes: `aat_value_for(src: &str) -> Value` (lib.rs:1980), `find_node`/`collect_spans` test helpers.
- Produces: the pinned wire contract Task 4 builds on: bare toggles surface in AAT inline content as `{"kind":"raw","source":"［＃横組み］","x-source-marker-kind":"directive",...}` nodes.

- [ ] **Step 1: Write TWO pin tests (review P5-5 — wire contract and AAT fallback are separate claims)**

Add to `crates/ab-aozora-aat/src/lib.rs` `mod tests`:

**Test A — facade WIRE contract** (the stream the classifier will consume): drive the facade exactly the way `projections` (lib.rs:408–428) does — read that function first and reuse its parse + `aozora_json::node_entries` invocation — over a PAIRED input, and assert on the wire nodes themselves.

> **Mid-execution amendment (Task 1 preflight finding, 2026-07-12):** the facade delivers bare-toggle markers as `containerOpen` (open tokens) / `containerClose` (close tokens) wire nodes, NOT `"directive"` as this plan originally assumed — one node per marker, exact span, source-ordered, raw-preserved fallback all hold, so the facade stays 0.3.0 and no design changes. Test A below must assert `kind == "containerOpen"` for the two open tokens and `kind == "containerClose"` for the two close tokens (replace the single `n.kind == "directive"` filter with this exact mapping). Task 4's marker identification is UNAFFECTED (it keys on raw-node `source`), but its prose mentions of `x-source-marker-kind: "directive"` read `"containerOpen"`/`"containerClose"` instead.

```rust
#[test]
fn bare_toggle_markers_arrive_as_ordered_directive_wire_nodes() {
    // Wire-level preflight: the facade must deliver each bare-toggle
    // marker as its own `directive` node whose span slices the exact
    // token, in source order — paired markers included (adoption later
    // consumes them, so THIS test, not the AAT fallback test, pins the
    // stream the classifier reads).
    let src = "ウサギ［＃横組み］（Hare）［＃横組み終わり］だ\n［＃罫囲み］三［＃罫囲み終わり］\n";
    // Follow `projections` (lib.rs:408) for decode + parse + node_entries.
    let decoded = /* same decode call `projections` uses on src.as_bytes() */;
    let nodes: Vec<AozoraNode> = /* same node_entries -> from_entries path */;
    let expected = ["［＃横組み］", "［＃横組み終わり］", "［＃罫囲み］", "［＃罫囲み終わり］"];
    let directives: Vec<&AozoraNode> =
        nodes.iter().filter(|n| n.kind == "directive").collect();
    let directive_sources: Vec<&str> = directives
        .iter()
        .map(|n| source_slice(&decoded.span_text, &n.span))
        .collect();
    for token in expected {
        assert!(
            directive_sources.iter().any(|s| *s == token),
            "token {token} missing from directive wire nodes: {directive_sources:?}"
        );
    }
    // Ordered by span (the classifier depends on source order):
    let starts: Vec<usize> = directives.iter().map(|n| n.span.start).collect();
    assert!(starts.windows(2).all(|w| w[0] < w[1]), "directive spans not ordered: {starts:?}");
}
```
(The two `/* … */` holes are deliberate: transcribe the exact two calls `projections` makes — do not invent a parallel decode path. If `AozoraNode`'s fields differ from `kind`/`span`, adjust field access, not the assertions.)

**Test B — AAT raw fallback for non-adopted markers** (uses ORPHAN inputs so it stays valid after the classifier lands):

```rust
#[test]
fn bare_toggle_orphan_markers_stay_raw_in_aat() {
    let src = "（例）［＃横組み］\nx［＃罫囲み終わり］y\n";
    let doc = aat_value_for(src);
    let mut raw_sources = Vec::new();
    collect_raw_sources(&doc, &mut raw_sources);
    for token in ["［＃横組み］", "［＃罫囲み終わり］"] {
        assert!(
            raw_sources.iter().any(|s| s == token),
            "orphan token {token} must stay a raw node; raw sources: {raw_sources:?}"
        );
    }
}

fn collect_raw_sources(v: &Value, out: &mut Vec<String>) {
    if let Some(obj) = v.as_object() {
        if obj.get("kind").and_then(Value::as_str) == Some("raw") {
            if let Some(s) = obj.get("source").and_then(Value::as_str) {
                out.push(s.to_owned());
            }
        }
        for key in ["blocks", "content", "children"] {
            if let Some(arr) = obj.get(key).and_then(Value::as_array) {
                for item in arr {
                    collect_raw_sources(item, out);
                }
            }
        }
    }
}
```

Note for Test A: if a marker surfaces merged into a wider node rather than one `directive` node per marker, the assertion fails — a merged shape means the classifier must split on marker boundaries, which is a design change: STOP and escalate with the observed node list.

- [ ] **Step 2: Run both**

```bash
cd ab-validator && cargo test -p ab-aozora-aat bare_toggle -- --nocapture
```
Expected: both PASS (facade delivers `directive` wire nodes per `crates/ab-aozora-syntax/src/degraded.rs`; the adapter's `_` fallback arm at lib.rs:1395 emits one `raw_node` per directive). On FAIL: report BLOCKED with the printed node/raw lists. Neither test needs modification when Task 4's classifier lands: Test A pins the wire (unaffected by AAT-side adoption), Test B uses only orphan markers.

- [ ] **Step 3: Record the preflight outcome**

Create `.superpowers/sdd/phase5-identity.json` (git-ignored scratch; the ledger cites it):

```json
{
  "phase": 5,
  "preflight": {
    "facade_version": "0.3.0",
    "facade_change_required": false,
    "pinned_by_test": "bare_toggle_markers_surface_as_raw_directive_nodes"
  },
  "join_key_template": "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3",
  "mapping_version": "0.4.0",
  "candidate_commit": null
}
```

- [ ] **Step 4: Commit**

```bash
git add crates/ab-aozora-aat/src/lib.rs && git commit -m "test(ab-aozora-aat): pin bare-toggle wire observability (Phase 5 preflight)"
```

---

### Task 2: Shared grammar vectors + `bare-toggle-adoption` delta-audit mode

Two deliverables: (a) a shared JSON vector file encoding the Contract 1 grammar's branch cases, consumed by BOTH the Python model tests and (later, Task 5) the Rust mirror tests; (b) the new delta-audit mode with its two checks (diff grammar over differing works; whole-candidate invariant scan for declined markers).

**Files:**
- Create: `reports/aat-fidelity/bare-toggle-model-vectors.json`
- Modify: `reports/aat-fidelity/audit-aat-delta.py`
- Test: `reports/aat-fidelity/tests/test_audit_aat_delta.py` (append) and `reports/aat-fidelity/tests/test_bare_toggle_model.py` (create)

**Interfaces:**
- Consumes: `classify_tokens(tokens: list[tuple[str, str]]) -> LineOutcome` and `TOKEN_KIND: dict[str, tuple[str, str]]` from `reports/aat-fidelity/bare-toggle-placement.py` (LineOutcome fields: `adopted_pairs: dict[str,int]`, `orphan_open`, `orphan_close`, `reopen` — all per-construct dicts; `interleave_events: int`, `proper_nestings: int`, `rollback_markers: int`, `invalid_constructs: set[str]`, `total_markers: int`). Import via importlib exactly as `tests/test_bare_toggle_model.py` does. Existing audit helpers: `die(msg)`, `load_dir`, `strip_identity`, the `summary` dict shape, mode handler signature `(base_doc, cand_doc, name, summary)`.
- Produces: mode name `bare-toggle-adoption` with CLI `audit-aat-delta.py bare-toggle-adoption BASELINE_DIR CANDIDATE_DIR --summary-json OUT [--expected-adopted-yokogumi N --expected-adopted-keigakomi N --expected-declined N]`; summary `classes` bucket `{identical, toggle_adopted}` plus `details {adopted_yokogumi_pairs, adopted_keigakomi_pairs, declined_markers, declined_by_reason: {orphan_open, orphan_close, reopen_rollback, interleave}}` — **all derived from the baseline dump via `classify_tokens`, never read from the placement report (review P5-4)**. Task 9's hinoki run and Task 3's verifier consume this summary shape.

- [ ] **Step 1: Write the vector file**

Create `reports/aat-fidelity/bare-toggle-model-vectors.json`. Every entry: the input line, and the expected `classify_line` outcome. These are the ten branch classes from the spec plus the corpus-observed shapes; expected values must match the normative model exactly.

```json
{
  "schema_version": "bare-toggle-model-vectors-v1",
  "tokens": {
    "yokogumi": ["［＃横組み］", "［＃横組み終わり］"],
    "keigakomi": ["［＃罫囲み］", "［＃罫囲み終わり］"]
  },
  "vectors": [
    {"name": "simple_pair", "line": "ab［＃横組み］xy［＃横組み終わり］cd",
     "adopted": {"yokogumi": 1, "keigakomi": 0}, "invalid": [],
     "orphan_open": {"yokogumi": 0, "keigakomi": 0}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 0, "rollback_markers": 0},
    {"name": "two_sequential_pairs", "line": "［＃罫囲み］a［＃罫囲み終わり］ b ［＃罫囲み］c［＃罫囲み終わり］",
     "adopted": {"yokogumi": 0, "keigakomi": 2}, "invalid": [],
     "orphan_open": {"yokogumi": 0, "keigakomi": 0}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 0, "rollback_markers": 0},
    {"name": "proper_cross_construct_nesting", "line": "［＃罫囲み］［＃横組み］x［＃横組み終わり］［＃罫囲み終わり］",
     "adopted": {"yokogumi": 1, "keigakomi": 1}, "invalid": [],
     "orphan_open": {"yokogumi": 0, "keigakomi": 0}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 1, "rollback_markers": 0},
    {"name": "improper_interleave", "line": "［＃横組み］［＃罫囲み］x［＃横組み終わり］［＃罫囲み終わり］",
     "adopted": {"yokogumi": 0, "keigakomi": 0}, "invalid": ["keigakomi", "yokogumi"],
     "orphan_open": {"yokogumi": 1, "keigakomi": 0}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 1, "proper_nestings": 0, "rollback_markers": 2},
    {"name": "same_construct_reopen", "line": "［＃横組み］a［＃横組み］b［＃横組み終わり］",
     "adopted": {"yokogumi": 0, "keigakomi": 0}, "invalid": ["yokogumi"],
     "orphan_open": {"yokogumi": 1, "keigakomi": 0}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 1, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 0, "rollback_markers": 2},
    {"name": "orphan_open", "line": "（例）［＃横組み］",
     "adopted": {"yokogumi": 0, "keigakomi": 0}, "invalid": ["yokogumi"],
     "orphan_open": {"yokogumi": 1, "keigakomi": 0}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 0, "rollback_markers": 0},
    {"name": "orphan_close", "line": "ab［＃横組み終わり］cd",
     "adopted": {"yokogumi": 0, "keigakomi": 0}, "invalid": ["yokogumi"],
     "orphan_open": {"yokogumi": 0, "keigakomi": 0}, "orphan_close": {"yokogumi": 1, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 0, "rollback_markers": 0},
    {"name": "valid_beside_invalid_other_construct", "line": "［＃罫囲み］x［＃罫囲み終わり］ ［＃横組み］",
     "adopted": {"yokogumi": 0, "keigakomi": 1}, "invalid": ["yokogumi"],
     "orphan_open": {"yokogumi": 1, "keigakomi": 0}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 0, "rollback_markers": 0},
    {"name": "valid_nested_inside_invalid_outer", "line": "［＃罫囲み］［＃横組み］x［＃横組み終わり］",
     "adopted": {"yokogumi": 1, "keigakomi": 0}, "invalid": ["keigakomi"],
     "orphan_open": {"yokogumi": 0, "keigakomi": 1}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 1, "rollback_markers": 0},
    {"name": "later_orphan_rolls_back_earlier_pair", "line": "［＃横組み］a［＃横組み終わり］［＃横組み］",
     "adopted": {"yokogumi": 0, "keigakomi": 0}, "invalid": ["yokogumi"],
     "orphan_open": {"yokogumi": 1, "keigakomi": 0}, "orphan_close": {"yokogumi": 0, "keigakomi": 0},
     "reopen": {"yokogumi": 0, "keigakomi": 0}, "interleave_events": 0, "proper_nestings": 0, "rollback_markers": 2}
  ]
}
```

- [ ] **Step 2: Write the model conformance test (Python side of the mirror)**

Create `reports/aat-fidelity/tests/test_bare_toggle_model.py`:

```python
"""The normative classify_line model must reproduce every shared vector.

The same vector file is consumed by the Rust mirror test
(crates/ab-aozora-aat/tests/bare_toggle_model.rs) — Task 5. Editing a
vector means BOTH sides re-verify; never edit expectations to match an
implementation."""
import importlib.util
import json
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve()
AAT_FIDELITY = HERE.parents[1]
VECTORS = json.loads((AAT_FIDELITY / "bare-toggle-model-vectors.json").read_text())

spec = importlib.util.spec_from_file_location(
    "bare_toggle_placement", AAT_FIDELITY / "bare-toggle-placement.py"
)
mod = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = mod
spec.loader.exec_module(mod)


def test_every_vector_matches_model():
    for vec in VECTORS["vectors"]:
        outcome = mod.classify_line(vec["line"])
        name = vec["name"]
        assert outcome.adopted_pairs == vec["adopted"], name
        assert sorted(outcome.invalid_constructs) == sorted(vec["invalid"]), name
        assert outcome.orphan_open == vec["orphan_open"], name
        assert outcome.orphan_close == vec["orphan_close"], name
        assert outcome.reopen == vec["reopen"], name
        assert outcome.interleave_events == vec["interleave_events"], name
        assert outcome.proper_nestings == vec["proper_nestings"], name
        assert outcome.rollback_markers == vec["rollback_markers"], name
```

- [ ] **Step 3: Run it (verifies the vector file against the already-committed model)**

```bash
cd ab-validator && python3 -m pytest reports/aat-fidelity/tests/test_bare_toggle_model.py -q
```
Expected: 1 passed. Any failure means a vector expectation was transcribed wrong — fix the vector, never the model.

- [ ] **Step 4: Write the failing tests for the audit mode**

Append to `reports/aat-fidelity/tests/test_audit_aat_delta.py` (reuse the file's existing `doc`/`para`/`text`/`write_dump`/`run` helpers; read them first — `run(mode, base, cand, tmp_path, *extra)` may need a small extension to pass extra flags; if `run` does not accept extra args, add `*extra` to its signature and `argv.extend(extra)` before invocation):

```python
# --- bare-toggle-adoption mode (Phase 5, rotation C5) ---

def raw_marker(source, line=1, bs=0, be=1):
    return {"kind": "raw", "source": source, "x-provenance": "parser-derived",
            "x-source-marker-kind": "directive",
            "span": {"line_start": line, "line_end": line, "byte_start": bs, "byte_end": be}}

def toggle_container(kind, content, line=1, bs=0, be=1):
    return {"kind": kind, "content": content,
            "span": {"line_start": line, "line_end": line, "byte_start": bs, "byte_end": be}}

def test_bare_toggle_identical_dumps_pass(tmp_path):
    docs = {"w1": doc([para([raw_marker("［＃横組み］", bs=0, be=12)])])}
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, summary, _ = run("bare-toggle-adoption", base, cand, tmp_path,
                           "--expected-adopted-yokogumi", "0",
                           "--expected-adopted-keigakomi", "0",
                           "--expected-declined", "1")
    assert code == 0
    assert summary["classes"]["identical"] == 1
    assert summary["details"]["declined_markers"] == 1

def test_bare_toggle_adoption_rewrite_passes(tmp_path):
    inner = {"kind": "text", "value": "（Hare）",
             "span": {"line_start": 1, "line_end": 1, "byte_start": 12, "byte_end": 22}}
    base_docs = {"w1": doc([para([
        raw_marker("［＃横組み］", bs=0, be=12), inner,
        raw_marker("［＃横組み終わり］", bs=22, be=40)])])}
    cand_docs = {"w1": doc([para([
        toggle_container("yokogumi", [inner], bs=0, be=40)])])}
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, summary, _ = run("bare-toggle-adoption", base, cand, tmp_path,
                           "--expected-adopted-yokogumi", "1",
                           "--expected-adopted-keigakomi", "0",
                           "--expected-declined", "0")
    assert code == 0
    assert summary["classes"]["toggle_adopted"] == 1
    assert summary["details"]["adopted_yokogumi_pairs"] == 1

def test_bare_toggle_wrong_kind_fails(tmp_path):
    inner = {"kind": "text", "value": "x",
             "span": {"line_start": 1, "line_end": 1, "byte_start": 12, "byte_end": 13}}
    base_docs = {"w1": doc([para([
        raw_marker("［＃横組み］", bs=0, be=12), inner,
        raw_marker("［＃横組み終わり］", bs=13, be=31)])])}
    cand_docs = {"w1": doc([para([
        toggle_container("keigakomi", [inner], bs=0, be=31)])])}
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, _, err = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2

def test_bare_toggle_content_loss_fails(tmp_path):
    inner = {"kind": "text", "value": "x",
             "span": {"line_start": 1, "line_end": 1, "byte_start": 12, "byte_end": 13}}
    base_docs = {"w1": doc([para([
        raw_marker("［＃横組み］", bs=0, be=12), inner,
        raw_marker("［＃横組み終わり］", bs=13, be=31)])])}
    cand_docs = {"w1": doc([para([toggle_container("yokogumi", [], bs=0, be=31)])])}
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, _, _ = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2

def test_bare_toggle_declined_marker_mutation_fails(tmp_path):
    base_docs = {"w1": doc([para([raw_marker("（例）［＃横組み］", bs=0, be=20)])])}
    cand_docs = {"w1": doc([para([raw_marker("（例）［＃横組み］", bs=0, be=21)])])}  # span drifted
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, _, _ = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2

def test_bare_toggle_expected_counter_mismatch_fails(tmp_path):
    docs = {"w1": doc([para([text("plain\n")])])}
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, _, _ = run("bare-toggle-adoption", base, cand, tmp_path,
                     "--expected-adopted-yokogumi", "1582")
    assert code == 2
```

- [ ] **Step 5: Run the new tests — expect failure (mode unknown)**

```bash
cd ab-validator && python3 -m pytest reports/aat-fidelity/tests/test_audit_aat_delta.py -q -k bare_toggle
```
Expected: errors — argparse rejects the unknown mode.

- [ ] **Step 6: Implement the mode**

In `reports/aat-fidelity/audit-aat-delta.py`:

1. Add `"bare-toggle-adoption"` to the mode `choices` and the `handler` dict; add the three optional args `--expected-adopted-yokogumi`, `--expected-adopted-keigakomi`, `--expected-declined` (type=int, default None); initialize its summary buckets `{"identical": 0, "toggle_adopted": 0}` and `summary["details"] = {"adopted_yokogumi_pairs": 0, "adopted_keigakomi_pairs": 0, "declined_markers": 0, "declined_by_reason": {"orphan_open": 0, "orphan_close": 0, "reopen_rollback": 0, "interleave": 0}}`.
2. Add the mode function. Core algorithm — the diff-grammar check and the invariant scan in one pass per work:

```python
BARE_TOGGLE_TOKENS = {
    "［＃横組み］": ("yokogumi", "open"),
    "［＃横組み終わり］": ("yokogumi", "close"),
    "［＃罫囲み］": ("keigakomi", "open"),
    "［＃罫囲み終わり］": ("keigakomi", "close"),
}
TOGGLE_KINDS = {"yokogumi", "keigakomi"}


def iter_inline_arrays(node, path=""):
    """Yield (array, path) for every blocks/content/children list, depth-first."""
    if isinstance(node, dict):
        for key in ("blocks", "content", "children"):
            arr = node.get(key)
            if isinstance(arr, list):
                yield arr, f"{path}/{key}"
                for i, item in enumerate(arr):
                    yield from iter_inline_arrays(item, f"{path}/{key}[{i}]")


def is_bare_toggle_raw(node):
    return (
        isinstance(node, dict)
        and node.get("kind") == "raw"
        and node.get("source") in BARE_TOGGLE_TOKENS
    )


def collect_bare_toggle_raws(doc):
    """All raw nodes whose source is exactly one bare-toggle token."""
    found = []
    for arr, path in iter_inline_arrays(doc):
        for i, node in enumerate(arr):
            if is_bare_toggle_raw(node):
                found.append((f"{path}[{i}]", node))
    return found


def normalize_adoption(node):
    """Rewrite one candidate inline_container back to its baseline raw
    sequence: [raw open marker, *content..., raw close marker]. Returns
    None if the node is not a toggle adoption."""
    if not isinstance(node, dict) or node.get("kind") not in TOGGLE_KINDS:
        return None
    kind = node["kind"]
    open_token, close_token = {
        "yokogumi": ("［＃横組み］", "［＃横組み終わり］"),
        "keigakomi": ("［＃罫囲み］", "［＃罫囲み終わり］"),
    }[kind]
    span = node.get("span")
    if not isinstance(span, dict):
        die(f"toggle container missing span")
    content = node.get("content")
    if not isinstance(content, list):
        die(f"toggle container missing content")
    return kind, open_token, close_token, span, content


def bare_toggle_adoption_mode(base_doc, cand_doc, name, summary):
    base = strip_identity(base_doc)
    cand = strip_identity(cand_doc)
    if base == cand:
        summary["classes"]["identical"] += 1
        count_declined(base, name, summary)
        return
    # Difference exists: it must consist EXACTLY of adopted-pair rewrites.
    # Strategy: project the candidate back to baseline shape by expanding
    # every yokogumi/keigakomi inline_container into
    # [raw open, *content, raw close], counting adoptions; the projection
    # must equal the baseline byte-for-byte.
    adopted = {"yokogumi": 0, "keigakomi": 0}
    projected = expand_adoptions(cand, base, name, adopted)
    if projected != base:
        die(f"{name}: candidate differences are not pure toggle adoptions")
    if adopted["yokogumi"] + adopted["keigakomi"] == 0:
        die(f"{name}: differs from baseline but contains no toggle adoption")
    summary["classes"]["toggle_adopted"] += 1
    summary["details"]["adopted_yokogumi_pairs"] += adopted["yokogumi"]
    summary["details"]["adopted_keigakomi_pairs"] += adopted["keigakomi"]
    count_declined(cand, name, summary)
```

`expand_adoptions(cand, base, name, adopted)` recursively copies `cand`; whenever it meets a dict with `kind in TOGGLE_KINDS` inside an inline array, it calls `normalize_adoption`, increments `adopted[kind]`, and splices `[make_raw(open_token, span_head), *recursed_content, make_raw(close_token, span_tail)]` in its place. The two synthetic raw markers' spans are RECOVERED FROM THE BASELINE: find in `base` the raw node with `source == token` whose span lies inside the container's span (fail-closed `die` if zero or >1 match) and use it verbatim — this makes the projection byte-exact without the audit re-deriving span arithmetic.

**Independent expectation derivation (review P5-4 — this is the load-bearing check):** add `derive_expected(base_doc) -> tuple[dict[str,int], dict[str,int]]`: collect the BASELINE's bare-toggle raw markers via `collect_bare_toggle_raws`, group them by line (`span["line_start"]`; `die` if any marker's `line_start != line_end`), order each group by `span["byte_start"]`, map each to its `(construct, kind)` via `TOKEN_KIND[source]`, and run `classify_tokens` per line. Sum into `(expected_adopted: {yokogumi, keigakomi}, expected_reasons: {orphan_open, orphan_close, reopen_rollback, interleave})` where `reopen_rollback` accumulates `rollback_markers` and the orphan counters sum both constructs. **Per LINE** (amended after Task 2 review — per-work totals admit a compensating false-pass where a missed valid adoption masks a wrong invalid-line adoption; probe-confirmed): attribute each observed adoption to its line via the recovered markers' `line_start`, and assert observed-per-line == expected-per-line for every line carrying markers (`die` on mismatch, naming work and line); accumulated into `details`, with `declined_by_reason` from the derivation and `declined_markers` from `count_declined`. A passing-path test must assert the four `declined_by_reason` buckets directly (interleave/orphan/rollback baseline). Cross-check: `declined_markers == expected_reasons["orphan_open"] + expected_reasons["orphan_close"] + expected_reasons["reopen_rollback"] + <markers consumed by interleave events per the derivation>` — `die` if the arithmetic does not close. `count_declined(doc, name, summary)` runs `collect_bare_toggle_raws` over the CANDIDATE and requires each found marker to exist in the BASELINE with identical `source` and `span` (raw-preservation invariant; `die` otherwise). The placement report is NOT an input to this mode.
3. Wire the three `--expected-*` flags: after the walk loop in `main`, if a flag is not None and the accumulated detail differs, `die(f"expected adopted yokogumi {n}, found {m}")` etc.
4. Unit tests must include one INDEPENDENCE case: a baseline whose markers imply (via `classify_tokens`) one adoption, paired with a candidate that did NOT adopt it (markers still raw) → exit 2 ("candidate failed to adopt an expected pair"), and the converse (candidate adopts a pair the derivation says is invalid) → exit 2. Add both to the Step 4 test list.

- [ ] **Step 7: Run the tests**

```bash
cd ab-validator && python3 -m pytest reports/aat-fidelity/tests/test_audit_aat_delta.py -q
```
Expected: all pass, including the pre-existing modes' tests (regression check).

- [ ] **Step 8: Commit**

```bash
git add reports/aat-fidelity/bare-toggle-model-vectors.json reports/aat-fidelity/audit-aat-delta.py reports/aat-fidelity/tests/test_audit_aat_delta.py reports/aat-fidelity/tests/test_bare_toggle_model.py
git commit -m "feat(reports): bare-toggle-adoption delta-audit mode + shared grammar vectors"
```

### Task 3: `verify-phase5-checkpoint.py` skeleton

Model on `reports/aat-fidelity/verify-phase4-checkpoint.py` (read it first — reuse its helpers verbatim where they fit: `die`, `load`, `git_show`, `git_show_names`, `repo_root_for`, `check_admission`). Phase 5 differences: ONE candidate stage (C5: delta + conformance + perf), the bare-toggle audit summary (both checks), the mapping-generation binding, and the repoint-commit tree check.

**Files:**
- Create: `reports/aat-fidelity/verify-phase5-checkpoint.py`
- Test: `reports/aat-fidelity/tests/test_verify_phase5_checkpoint.py`

**Interfaces:**
- Consumes: gate summary shapes from Phase 4 (each has `stage`, `gate`, `verdict`, `candidate.commit`, `candidate.bin_sha256`, `version`); the Task 2 audit summary (`classes.toggle_adopted`, `details.*`); `run-sets/current.json` shape (§Task 10).
- Produces: CLI `verify-phase5-checkpoint.py --c5-gates DELTA CONF PERF --audit AUDIT_SUMMARY --conversion CONVERSION_SUMMARY --admission-capture TXT --admission-cmd CMD --repoint-commit SHA --run-set PATH --coverage COVERAGE_SUMMARY --c5 COMMIT --mapping-file data/aat-to-parser-ir-mapping-v2.json --mapping-version 0.4.0 --mapping-hash sha256:… --frozen-mapping data/aat-to-parser-ir-mapping-v2-0.3.0.json`; prints `CHECKPOINT OK` exit 0 / `CHECKPOINT FAIL: …` exit 1. Task 10 runs it.

Checks to implement (each its own function, mirroring phase4's granularity):
1. `check_stage`: the three C5 gate summaries carry `verdict: "PASS"`, one shared `candidate.commit == --c5`, one shared `bin_sha256`, `version` matching `^ab-aozora 0\.6\.0 aat-schema 2 facade 0\.3\.0 wire-schema 3 \(git <c5>\)$`.
2. `check_audit`: audit summary `verdict == "PASS"`, `mode == "bare-toggle-adoption"`, `compared == 17886`, `details.adopted_yokogumi_pairs == 1582`, `details.adopted_keigakomi_pairs == 25`, `details.declined_markers == 24`, `details.declined_by_reason == {"orphan_open": 10, "orphan_close": 0, "reopen_rollback": 14, "interleave": 0}` (these are independently derived by the audit from the baseline dump — Task 2).
3. `check_conversion` (REAL summary shape — copy paths from `docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json`): `summary["totals"]["files_attempted"] == summary["totals"]["files_succeeded"] == 17886`, `summary["totals"]["files_failed"] == 0`, `summary["mapping"]["mapping_version"] == "0.4.0"`, `summary["mapping"]["mapping_hash"] == --mapping-hash`.
4. `check_mapping_generation`: the registry/converter hash is the CANONICAL document hash — `abc_legacy_json_hash` (`crates/ab-aat-to-parser-ir/src/mapping.rs:63–68`), whose Python mirror is `reports/lib/legacy_json_c14n.py` (`canonical_json` → sha256 → `"sha256:" + hex`). Use the lib (import it; do NOT hash raw file bytes): canonical hash of `--mapping-file`'s parsed JSON == `--mapping-hash`; canonical hash of `--frozen-mapping`'s parsed JSON == `sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40`; `--mapping-file`'s `mapping_version == --mapping-version`; frozen file's `mapping_version == "0.3.0"`. (Task 6 Step 1 verifies the 7249cd72 reproduction BEFORE this task executes — see EXECUTION ORDER.)
5. `check_admission`: verbatim from phase4 (capture contains `:status :admitted`; re-run `--admission-cmd` with cwd `<repo-root>/abc`, require exit 0 + `:admitted`).
6. `check_repoint` (adapted `check_activation`): `git show --name-only` of `--repoint-commit`; changed paths must be EXACTLY `{"ab-validator/reports/aat-fidelity/run-sets/current.json"}` (Task 10 commits only that file; retention notes go in the closure commit — review P5-8 alignment); parent blob's `ab-aozora` entry must contain the C4 commit `27772b1b…`, child blob and live file must contain `adapter_version_contains == --c5` and exactly 5 adapters.
7. `check_coverage`: coverage summary asserts `source_region_contract.verdict == "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"`, `custom_contract.verdict == "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"`, `tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"`, top-level `verdict == "IR_PUBLICATION_COVERAGE_COMPLETE"`, `parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_COMPLETE"`, and the three source-authority occurrence counters == 0 (copy the counter names from `check_coverage` in verify-phase4-checkpoint.py:331).

- [ ] **Step 1: Write failing tests** — `tests/test_verify_phase5_checkpoint.py` builds fixtures + a tiny git repo (`git init`; commit a parent `current.json` with the C4 entry, then a child commit swapping to a C5 entry) and asserts: all-green fixtures → exit 0 + `CHECKPOINT OK`; then one negative per check (10+ tests): wrong adopted count, wrong mapping hash, frozen-file canonical-hash drift, missing `:admitted`, extra path in repoint commit, coverage verdict not COMPLETE, stage commit mismatch, conversion failure count, declined_by_reason mismatch, admission command with no output. The conversion fixture is a COPY of the real `docs/superpowers/reports/2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json` with only `totals`/`mapping` fields mutated per test — never an invented simplified shape (review P5-3). The green-path admission stub is `--admission-cmd "echo ':status :admitted'"` (the verifier requires `:admitted` in stdout; `true` prints nothing and MUST fail — that is the tenth negative test).
- [ ] **Step 2: Run tests — expect failure (file absent).** `python3 -m pytest reports/aat-fidelity/tests/test_verify_phase5_checkpoint.py -q`
- [ ] **Step 3: Implement** the script per the check list above (argparse exactly as in Interfaces; helpers copied from phase4 verifier with a comment naming the origin).
- [ ] **Step 4: Run tests — all pass.** Also rerun the phase4 verifier tests if any helper was extracted rather than copied (prefer copying; do NOT refactor phase4's frozen instrument).
- [ ] **Step 5: Commit** — `git add reports/aat-fidelity/verify-phase5-checkpoint.py reports/aat-fidelity/tests/test_verify_phase5_checkpoint.py && git commit -m "feat(reports): phase 5 checkpoint verifier (C5 gates, mapping generation, repoint binding)"`

---

### Task 4: The classifier — same-line bare-toggle pairing in `ab-aozora-aat`

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` — new pairing pass + call site, unit tests in `mod tests`.

> **Mid-execution amendment 2 (Task 9 delta-gate BLOCK, 2026-07-12):** the original placement — last step of `inline_content`, BEFORE block classification — is wrong: consuming marker nodes changes how `blocks_from_inline_content` segments paragraphs/jizume blocks (83 works regressed: merged burasage paragraphs, dropped 字下げ terminators). The pass must run AFTER block classification, applied recursively to every built block's/container's content array, so block segmentation sees the original node stream and adoption rewrites ONLY the toggle span. A regression test must pin the corpus shape that caught this (a bare toggle inside a compound 字下げ block, per work 000026_55738): block structure byte-identical to the pre-classifier output except the adopted container.

**Interfaces:**
- Consumes: the inline array `Vec<Value>` built by `inline_content` (nodes carry `kind`, `span` `{line_start,line_end,byte_start,byte_end}`); `raw_node` emission shape (`kind:"raw"`, `source`, `x-source-marker-kind:"directive"`); Task 1's pinned observability.
- Produces: `fn pair_bare_toggles(content: Vec<Value>) -> Vec<Value>` applied as the LAST step of `inline_content` before return. Emits `{"kind": "yokogumi"|"keigakomi", "content": [...], "span": {...}}` (an `inline_container` — same shape family as `style_node`'s output, no extra fields). Tasks 5 and 9 depend on this exact behavior.

Implementation contract (mirrors `classify_line` exactly):

1. Identify marker indices: a node is a bare-toggle marker iff `kind == "raw"` and `source` is exactly one of the four tokens. Group markers by line: `span.line_start` (markers never span lines; assert `line_start == line_end` and treat violation as non-marker).
2. Per line group, run Pass 1 (one global stack over that line's markers in array order; the array order matches source order because `inline_content` sorts by span): reopen → mark construct invalid + push; orphan close → invalid; matched close → pop + candidate `(open_idx, close_idx, kind)`; mismatched close → both constructs invalid, no pop; leftover frames → invalid.
3. Pass 2: adopted candidates = those whose construct is not invalid on that line. Splice innermost-first (sort candidates by descending `open_idx`): replace `content[open_idx..=close_idx]` with one container node whose `content` is the nodes strictly between the markers and whose span is `{line_start: open.span.line_start, line_end: close.span.line_end, byte_start: open.span.byte_start, byte_end: close.span.byte_end}`. Innermost-first splicing keeps outer candidates' indices valid after inner splices ONLY if indices are re-tracked — simplest correct approach: process one line's adoptions by rebuilding the whole array once via a recursive descent (build a small tree from the candidate ranges, then emit), or repeatedly splice the innermost remaining candidate and recompute the other candidates' indices by shifting. Either is acceptable; the tests below pin observable behavior, not the mechanism.
4. Invalid constructs' markers: untouched (they remain the raw nodes they were).

- [ ] **Step 1: Write failing unit tests** in `mod tests` — one per shared vector, driving the full adapter via `aat_value_for` and asserting on the produced AAT (find containers via `find_node`, raw markers via the Task 1 `collect_raw_sources` helper). Name them `bare_toggle_<vector name>` (e.g. `bare_toggle_simple_pair_adopts_inline_container`). Three examples to transcribe (write ALL ten vectors as tests; the remaining seven follow the same pattern with the vector file's lines and expectations):

```rust
#[test]
fn bare_toggle_simple_pair_adopts_inline_container() {
    let doc = aat_value_for("ab［＃横組み］xy［＃横組み終わり］cd\n");
    let node = find_first_node(&doc, "yokogumi");
    let content = node["content"].as_array().unwrap();
    assert_eq!(content.len(), 1);
    assert_eq!(content[0]["kind"], "text");
    assert_eq!(content[0]["value"], "xy");
    let mut raws = Vec::new();
    collect_raw_sources(&doc, &mut raws);
    assert!(raws.iter().all(|s| !s.starts_with("［＃横組み")), "markers must be consumed: {raws:?}");
}

#[test]
fn bare_toggle_orphan_open_stays_raw() {
    let doc = aat_value_for("（例）［＃横組み］\n");
    assert!(find_node(&doc, "yokogumi").is_none());
    let mut raws = Vec::new();
    collect_raw_sources(&doc, &mut raws);
    assert!(raws.iter().any(|s| s == "［＃横組み］"));
}

#[test]
fn bare_toggle_nested_pair_becomes_child_container() {
    let doc = aat_value_for("［＃罫囲み］［＃横組み］x［＃横組み終わり］［＃罫囲み終わり］\n");
    let outer = find_first_node(&doc, "keigakomi");
    let inner = find_node(outer, "yokogumi").expect("nested yokogumi child");
    assert_eq!(inner["content"][0]["value"], "x");
    // span containment: parent covers child
    let (po, pc) = (outer["span"]["byte_start"].as_u64().unwrap(), outer["span"]["byte_end"].as_u64().unwrap());
    let (io, ic) = (inner["span"]["byte_start"].as_u64().unwrap(), inner["span"]["byte_end"].as_u64().unwrap());
    assert!(po < io && ic < pc);
}
```

Also add the invariants tests (review P5-6 — STRUCTURAL EQUALITY, not marker survival). Make `pair_bare_toggles` `pub(crate)` so tests call it directly:

```rust
#[test]
fn bare_toggle_zero_adoption_input_is_structurally_unchanged() {
    // For every shared vector with zero adoptions, the pass must return
    // the input array UNCHANGED — full structural equality, so no text,
    // span, provenance, ordering, or field can drift unnoticed.
    for line in [
        "（例）［＃横組み］",                                          // orphan open
        "ab［＃横組み終わり］cd",                                       // orphan close
        "［＃横組み］a［＃横組み］b［＃横組み終わり］",                    // reopen
        "［＃横組み］［＃罫囲み］x［＃横組み終わり］［＃罫囲み終わり］",     // interleave
        "［＃横組み］a［＃横組み終わり］［＃横組み］",                     // rollback
    ] {
        let content = inline_array_for(line); // helper: build the pre-pass
        // inline array for a one-line paragraph (extract the array the
        // adapter feeds pair_bare_toggles — factor a small test hook or
        // reuse inline_content directly with the same inputs).
        let out = pair_bare_toggles(content.clone());
        assert_eq!(out, content, "zero-adoption line must be identity: {line}");
    }
}
```

plus `bare_toggle_multi_line_isolation` (two lines: valid pair on line 1, orphan on line 2 → line 1 adopts, line 2's nodes structurally unchanged — corpus case `000106_55753`).

- [ ] **Step 2: Run — expect all new tests fail** (`cargo test -p ab-aozora-aat bare_toggle`): no containers are produced yet. Task 1's two preflight tests keep passing unmodified before AND after this task (the wire test is unaffected by AAT-side adoption; the fallback test uses only orphan markers).
- [ ] **Step 3: Implement `pair_bare_toggles`** per the contract above, called at the end of `inline_content`. Keep it a pure function `Vec<Value> -> Vec<Value>`; no `serde_json::Value` round-trips beyond the array manipulation itself (perf: the pass must be O(n) when a paragraph contains no markers — early-return on "no raw node with a bare-toggle source").
- [ ] **Step 4: Run the full adapter suite** — `cargo test -p ab-aozora-aat`. Expected: all pass INCLUDING goldens (the five golden fixtures contain no bare toggles — verify with `grep -l "横組み］" crates/ab-aozora-aat/tests/goldens/` returning nothing; if a golden does contain one, regenerate it per `tests/goldens.rs`'s documented regeneration flow and include the diff in the commit).
- [ ] **Step 5: Full workspace green** — `cargo check --workspace --all-targets && cargo fmt --check && just clippy`.
- [ ] **Step 6: Commit** — `git commit -m "feat(ab-aozora-aat): classify same-line bare-toggle pairs as inline yokogumi/keigakomi containers"`

---

### Task 5: Rust↔Python mirror + property target

**Files:**
- Create: `crates/ab-aozora-aat/tests/bare_toggle_model.rs`
- Modify: `crates/ab-aozora-aat/Cargo.toml` (add `[dev-dependencies] proptest.workspace = true` — the workspace pins proptest 1.5 at `ab-validator/Cargo.toml:201`; never add a second literal version — and `serde_json` if not already a dev-dep)

**Interfaces:**
- Consumes: `reports/aat-fidelity/bare-toggle-model-vectors.json` (Task 2), the adapter's public AAT entry point used by `aat_value_for` (check `mod tests` — it calls `aat_json_from_bytes`; the integration test must use the crate's public API: verify `aat_json_from_bytes` (or its public wrapper) is exported; if it is `pub(crate)`, use the `ab-aozora` binary via `tests/goldens.rs`'s pattern instead — read goldens.rs first and reuse its invocation helper).
- Produces: the mirror + property evidence the spec's Contract 1 requires. No downstream consumers.

- [ ] **Step 1: Mirror test.** `bare_toggle_model.rs` reads the vector file (path: `concat!(env!("CARGO_MANIFEST_DIR"), "/../../reports/aat-fidelity/bare-toggle-model-vectors.json")`), and for each vector: build a one-line document `format!("{}\n", vector.line)`, run the adapter, and assert: number of `yokogumi`/`keigakomi` containers == `adopted` counts; every token of an invalid construct present as a raw node; adopted lines contain no marker raw nodes. (The Rust side asserts the OBSERVABLE outcome — containers and raw survivors — not the internal counters; the Python side owns counter-level assertions.)
- [ ] **Step 2: Property tests** (same file, `proptest!` block, `#![proptest_config(ProptestConfig { cases: 512, ..ProptestConfig::default() })]`):

Generator: token soup lines — `prop::collection::vec(prop_oneof![Just("［＃横組み］"), Just("［＃横組み終わり］"), Just("［＃罫囲み］"), Just("［＃罫囲み終わり］"), "[a-zあ-ん]{1,4}".prop_map(String::from)], 0..12)` joined into a line, then 1–3 such lines joined with `\n`.

Properties (each a separate `#[test]` inside `proptest!`):
1. `every_marker_consumed_or_preserved_exactly_once`: count of token occurrences in input == (2 × containers of that construct… careful: containers consume one open+one close each) + raw-node survivors of that token. Exact check per token string: input occurrences == raw survivors + (containers of the construct, counted once for the open token and once for the close token).
2. `invalid_only_lines_structurally_unchanged`: if a generated line has zero adoptions (recompute expected adoptions in Rust with a tiny reimplementation of pass 1/2 over the token list — 30 lines, kept INSIDE the test file as the oracle), then `pair_bare_toggles(content.clone()) == content` — full structural equality of the pre-pass inline array (review P5-6), not mere marker survival.
3. `deterministic`: running the adapter twice on the same input yields identical JSON.
4. `line_isolation`: for a 2-line input, the AAT of line 1's paragraph content equals the AAT produced from line 1 alone (modulo spans' line numbers — compare kinds+values+sources only).
5. `nesting_well_formed`: recursively, every `yokogumi`/`keigakomi` container's children spans lie within the parent span, and no container of the same kind is its own direct descendant on the same line without an intervening… (keep simple: parent span strictly contains child spans — that is the spec property).

- [ ] **Step 3: Run** — `cargo test -p ab-aozora-aat --test bare_toggle_model`. Expected: pass, 512 cases per property.
- [ ] **Step 4: Workspace green + commit** — `git commit -m "test(ab-aozora-aat): bare-toggle model mirror + proptest property target"`

---

### Task 6: Mapping generation freeze + 0.4.0 + generation-binding dispatch

**Files:**
- Create: `data/aat-to-parser-ir-mapping-v2-0.3.0.json` (byte-exact copy, committed BEFORE any edit to the live file — separate commit)
- Modify: `data/aat-to-parser-ir-mapping-v2.json` (version 0.4.0 + inline toggle rules)
- Modify: `crates/ab-aat-to-parser-ir/src/mapping.rs` (preflight: expected-generation check)
- Modify: `crates/ab-aat-to-parser-ir/src/main.rs` (new `--expect-mapping-version` / `--expect-mapping-hash` args threaded to preflight)
- Modify: `crates/ab-aat-to-parser-ir/README.md` (generation table)
- Test: converter integration tests (`crates/ab-aat-to-parser-ir/tests/integration.rs`)

**Interfaces:**
- Consumes: `MappingDocument::from_path` (mapping.rs:63–74), `MappingDocument::preflight` (mapping.rs:76–172), CLI arg plumbing in main.rs (4 `from_path` call sites: lines ~122/158/219/251).
- Produces: frozen file with `mapping_version "0.3.0"`; live file `mapping_version "0.4.0"`, `source_aat_version 2`; converter flags `--expect-mapping-version <v>` and `--expect-mapping-hash <sha256:…>` that fail-closed when the loaded mapping does not match; the audit summary already carries `mapping_version`/`mapping_hash` (registry row fields) — verify and rely on it. Tasks 3, 9, 10 consume the flags/hashes.

- [ ] **Step 1: Freeze commit.**

```bash
cd ab-validator && cp data/aat-to-parser-ir-mapping-v2.json data/aat-to-parser-ir-mapping-v2-0.3.0.json
cmp data/aat-to-parser-ir-mapping-v2.json data/aat-to-parser-ir-mapping-v2-0.3.0.json && echo BYTE-IDENTICAL
git add data/aat-to-parser-ir-mapping-v2-0.3.0.json && git commit -m "chore(mapping): freeze mapping generation 0.3.0 as immutable file (pre-0.4.0)"
```
Then verify the frozen generation's registry hash. The scheme is known: `abc_legacy_json_hash` (`crates/ab-aat-to-parser-ir/src/mapping.rs:63–68`), Python mirror `reports/lib/legacy_json_c14n.py`. Reproduce:

```bash
python3 -c "
import json, hashlib, sys
sys.path.insert(0, '.')
from reports.lib.legacy_json_c14n import canonical_json
doc = json.load(open('data/aat-to-parser-ir-mapping-v2-0.3.0.json'))
print('sha256:' + hashlib.sha256(canonical_json(doc).encode()).hexdigest())
"
```
Expected output: `sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40` (check `canonical_json`'s exact API in the lib first — if callers are expected to use a provided hash helper, use that). If it does NOT reproduce, STOP: the hashing assumption is wrong and Task 3's `check_mapping_generation` must not be built on it. Record the command + output in the task report.

- [ ] **Step 2: Failing converter test for generation binding.** In `tests/integration.rs` add: running the converter with `--expect-mapping-version 0.9.9` against the live mapping exits non-zero with a message containing `mapping generation mismatch`; with the correct version+hash it succeeds. (Follow the file's existing test harness pattern for invoking the binary/library.)
- [ ] **Step 3: Implement**: add the two optional args, thread to `preflight`, add checks (after the existing tuple checks at mapping.rs:96–107):

```rust
if let Some(expected) = expect_mapping_version {
    if self.mapping_version != *expected {
        bail!(
            "mapping generation mismatch: loaded mapping_version {} but --expect-mapping-version {}",
            self.mapping_version, expected
        );
    }
}
if let Some(expected_hash) = expect_mapping_hash {
    if &self.computed_hash != expected_hash {
        bail!(
            "mapping generation mismatch: loaded mapping hash {} but --expect-mapping-hash {}",
            self.computed_hash, expected_hash
        );
    }
}
```
(`computed_hash` = whatever the audit already computes for the registry row — reuse that code path, do not introduce a second hashing scheme.)
- [ ] **Step 4: Edit the live mapping to 0.4.0 — schema-valid fields ONLY (review P5-2).** Set `mapping_version: "0.4.0"`. Add transform-rule descriptions for the two new path families, following the file's existing entry shape EXACTLY (copy an S-rule entry — e.g. S-05 — as a template and keep ONLY the properties the mapping schema admits; `MappingRule` in `crates/ab-aat-to-parser-ir/src/mapping.rs:12` has six properties and the schema sets `additionalProperties: false` — adding any other key fails the mapping's own preflight): one rule for inline `yokogumi` containers, one for inline `keigakomi` containers (next free S-numbers), with the path pattern format used by existing inline_container rules for `style`, a `disposition` matching how existing inline_container kinds convert, and a description that STATES THE SPAN PROJECTION: "AAT container span is marker-inclusive; parser-IR spans are constructed from visible decoded text (convert.rs `append_visible_content_text`); marker bytes are not projected." **No occurrence counters in the mapping — empirical counts live in the conversion-audit report only. Mapping 0.4.0 is FROZEN by this commit, before the authoritative conversion run, and is never rewritten from its own run's observations** (the hash committed here is the hash the registry row binds). Verify the mapping passes its own preflight (`cargo test -p ab-aat-to-parser-ir`). Do NOT renumber or edit any existing rule. Record the 0.4.0 canonical hash (same command as Step 1, against the live file) in the task report and in `.superpowers/sdd/phase5-identity.json`.
- [ ] **Step 5: README generation table.** In `crates/ab-aat-to-parser-ir/README.md`'s v2 section add a two-row generation table: `0.3.0 — frozen at data/aat-to-parser-ir-mapping-v2-0.3.0.json, hash sha256:7249cd72… (registry coordinate of the 0.2.0-era…C4 rows)` / `0.4.0 — live at data/aat-to-parser-ir-mapping-v2.json, hash computed by audit-corpus, binds C5+`. Keep the existing "computed, never hand-written" note for the live file.
- [ ] **Step 6: Tests + green.** `cargo test -p ab-aat-to-parser-ir && cargo check --workspace --all-targets && cargo fmt --check && just clippy`. Also re-run the converter's full test suite and `python3 -m pytest reports/aat-fidelity/tests/ -q` (no instrument reads the live mapping's version yet, but the canary tests in ab-check/ab-oracle read schema files — confirm untouched).
- [ ] **Step 7: Commit** — `git commit -m "feat(converter): mapping 0.4.0 with inline toggle rules + generation-binding preflight"`

---

### Task 7: ABC 0.3.0 integration confirmation (independent of C5)

Root cause (verified during planning): `publication-coverage.py:41` pins `ABC_PRESERVATION_SCHEMA_VERSION = "0.2.0"` while the contract on BOTH sides is 0.3.0 and byte-identical (`abc/schemas/parser-ir-publication-preservation.schema.json` ≡ `ab-validator/data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json`, sha256 prefix `b073df01…`); additionally the coverage recipe passes the VENDORED snapshot path while `TRUSTED_ABC_PRESERVATION_SCHEMA_PATH` resolves (via `reports/lib/paths.py:38-51`) to the abc-side sibling when present, so `trusted_schema_path_match` is false. Both must be fixed for `custom_contract_block` (publication-coverage.py:962–1048) to return CONFIRMED.

**Files:**
- Modify: `reports/parser-ir/publication-coverage.py:41` (pin 0.2.0 → 0.3.0)
- Modify: `ab-validator/justfile` `parser-ir-publication-coverage-report` recipe (~line 502): change `CUSTOM_CONTRACT_SCHEMA` handling so the script receives the TRUSTED path (simplest: make the recipe's default empty and have the script default the arg to `TRUSTED_ABC_PRESERVATION_SCHEMA_PATH` when unset — check how the arg is parsed and pick the smaller diff; the requirement is `custom_contract_schema.resolve() == TRUSTED_ABC_PRESERVATION_SCHEMA_PATH`)
- Create: provenance record inside the task report + a dated report `docs/superpowers/reports/2026-07-XX-abc-contract-0.3.0-confirmation.md` (+ regenerated coverage summary alongside)
- Test: `reports/parser-ir/tests/` (if a test dir exists for publication-coverage — check; else add assertions to the smoke script path used by `parser-ir-publication-coverage-smoke`, justfile:396–397)

**Interfaces:**
- Consumes: `custom_contract_block`, `tei_profile_contract_block` (publication-coverage.py:1074+), the coverage recipe's other default inputs (matrix/source/delta/bundle/next-work summaries — reuse the same input set the Phase 4 postactivation run used; copy the invocation from `docs/superpowers/reports/2026-07-12-phase4-postactivation-coverage` report header).
- Produces: a coverage summary with the four verdicts of spec Contract 5. Task 10's `--coverage` input REQUIRES this state under C5 wiring (Task 10 regenerates; this task proves the machinery).

- [ ] **Step 1: Semantic diff review (the provenance record).** Diff the 0.2.0-era contract against 0.3.0: `git log --oneline -- abc/schemas/parser-ir-publication-preservation.schema.json` and `git diff <0.2.0-commit> HEAD -- abc/schemas/parser-ir-publication-preservation.schema.json`. Write into the report: source ABC commit, per-artifact source/destination hashes (`sha256sum` of abc-side and vendored files — must be equal), what changed 0.2.0→0.3.0 (enum additions of record/coverage classes, constructs — enumerate them), why compatible, the commands run, and the conclusion labeled **integration confirmation** (not independent semantic proof). If the diff shows anything OTHER than additive class/construct/metadata changes (e.g. a removed record class), STOP and escalate.
- [ ] **Step 2: Failing check.** Regenerate coverage with the CURRENT code (`just parser-ir-publication-coverage-report` with the Phase 4 postactivation input set) into scratch paths; assert `custom_contract.verdict == CANDIDATE_PROVIDED` (reproduces the defect).
- [ ] **Step 3: Fix the pin and the path.** `ABC_PRESERVATION_SCHEMA_VERSION = "0.3.0"` (with a one-line comment naming this task's report as the rotation review). Recipe/script change so the custom-contract arg resolves to the trusted path.
- [ ] **Step 4: Regenerate + assert.** Re-run the recipe; assert in the produced summary: the four Contract 5 verdicts (`SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`, `CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`, `TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`, top-level `IR_PUBLICATION_COVERAGE_COMPLETE`), three occurrence counters 0, `FIVE_PARSER_EVIDENCE_COMPLETE`, and `closure_gaps.classified_but_not_admitted.count == 0` (the 151 rows must fold into `admitted_by_custom_contract`/`admitted_by_tei_profile`). If TEI-profile still reports missing classes/constructs (`tei_profile_contract_block`'s missing_* fields non-empty), the 0.3.0 contract does not admit the required evidence — STOP and escalate with the missing lists (this is a genuine spec-level outcome, not an implementation bug).
- [ ] **Step 5: Commit** (independent commit, not part of C5): `git commit -m "fix(coverage): confirm ABC preservation contract 0.3.0 (pin + trusted path); coverage COMPLETE"` including the report + summary evidence.

---

### Task 8: Close the C5 identity

Only after Tasks 1–6 are merged into the phase branch (instruments-before-identity).

**Files:**
- Modify: `crates/ab-aozora-aat/Cargo.toml:4` (`version = "0.6.0"`)
- Modify: `crates/ab-aozora-aat/src/lib.rs` — doc comment ~:1682, expected string ~:1699, comment ~:2254, join-key assertion ~:2258 → `"ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3"`; rename test `c4_identity_join_key_and_document_version` → `c5_identity_join_key_and_document_version`
- Modify: all 5 goldens `crates/ab-aozora-aat/tests/goldens/*.expected.json` line 1 `meta.adapter_version` `0.5.0` → `0.6.0`
- Modify: `.superpowers/sdd/phase5-identity.json` (fill `candidate_commit` AFTER the commit exists)

- [ ] **Step 1:** Apply the version edits; `cargo test -p ab-aozora-aat` green; workspace green.
- [ ] **Step 2:** Commit: `git commit -m "feat(ab-aozora-aat): close C5 identity — ab-aozora 0.6.0"`. Record `git rev-parse HEAD` as **C5** in `.superpowers/sdd/phase5-identity.json` (`candidate_commit`) and in the ledger. Every subsequent build/run uses `git checkout <C5>`-derived binaries; NEVER a later HEAD. If ANY defect is found after this point, the fix commit MOVES C5 (update the identity file + ledger, rerun all gates from scratch — the Phase 4 f71432ac→c2b9b396 precedent).

---

### Task 9: hinoki full run + four gates

All commands run on `hinoki.hyakutake-barbel.ts.net` unless marked local. Sync the phase branch to hinoki first (`git fetch` a pushed branch, or `git push hinoki-remote` — hinoki has `~/Projects/soranoha` tracking origin; push the phase branch to origin). Build candidate-bound: `git -C ~/Projects/soranoha checkout <C5> && cd ab-validator && nix develop … cargo build --release -p ab-aozora -p ab-aat-to-parser-ir` (use the same build environment the Phase 4 runs used — copy from `docs/superpowers/reports/2026-07-12-phase4-c4-perf.md`'s recorded build lines).

**Deliverables (all frozen under `docs/superpowers/reports/`, dated at run time):**
1. **Dump**: full-corpus AAT run of the C5 binary into `/db/ab-validator/aat-corpus/ab-aozora-phase5-c5-<c5-short>/aat/ab-aozora` (mirror the Phase 4 dump layout exactly — `<out>/aat/<adapter>` plus `metadata.json`; copy the runner invocation from the Phase 4 confinement report's header). Detached `nohup`, log under `~`, poll synchronously.
2. **Delta audit** (both checks): local or hinoki —
```bash
python3 reports/aat-fidelity/audit-aat-delta.py bare-toggle-adoption \
  /db/ab-validator/aat-corpus/ab-aozora-phase4-c4-27772b1/aat/ab-aozora \
  /db/ab-validator/aat-corpus/ab-aozora-phase5-c5-<short>/aat/ab-aozora \
  --summary-json ~/phase5-delta.summary.json \
  --expected-adopted-yokogumi 1582 --expected-adopted-keigakomi 25 --expected-declined 24
```
Expected: exit 0, `classes.toggle_adopted` == number of works carrying ≥1 adoption, `identical == 17886 - toggle_adopted`, details exactly 1582/25/24 with `declined_by_reason == {orphan_open: 10, orphan_close: 0, reopen_rollback: 14, interleave: 0}` — all DERIVED by the audit from the C4 baseline dump (the placement report is not an input; agreement with its preregistered numbers is the independent confirmation). **Any exit 2 blocks the phase: diagnose, fix, MOVE C5, rerun.**
3. **Conformance**: `just aozora-notation-spec-comparison` (full, 127 vectors) and `just official-docs-seed-comparison` (30 vectors) with the C5 binary wired as the ab-aozora adapter (the recipe builds from the checked-out tree — confirm the checkout IS C5); then `reports/parser-conformance/compare-adapter-rows.py` against the C4 summaries (`2026-07-12-phase4-c4-conformance{,-seed}.summary.json`) → gate summary with `must_fail: 0, must_skip: 0, differing_full: 0, differing_seed: 0`. Bare toggles appear in no conformance vector — any row drift is a regression.
4. **Perf**: `python3 reports/aat-fidelity/run-perf-workset.py --workset data/perf-workset.json --corpus /db/ab-validator/perf-workset-corpus-v1 --baseline-cmd "<C4 binary> --mode aat" --baseline-id-bin <C4 binary> --candidate-cmd "<C5 binary> --mode aat" --candidate-id-bin <C5 binary> --runs 5 --out ~/phase5-perf.runner.json` — baseline is the C4 binary (build it from commit `27772b1b…` in a scratch checkout). Gate: median regression ≤ 10% (expect ≈0: the pass is O(n) skip for 17,542 untouched works); record `001562_56145` individually.
5. **Conversion audit**: `just aat-to-parser-ir-full-audit` equivalent with explicit `--mapping data/aat-to-parser-ir-mapping-v2.json --expect-mapping-version 0.4.0 --expect-mapping-hash <0.4.0 canonical hash from Task 6>` over the C5 dump → summary with `totals.files_attempted == totals.files_succeeded == 17886`, `totals.files_failed == 0`, `mapping.mapping_version == "0.4.0"`, `mapping.mapping_hash == <that hash>` + `--compat-edn-out` for Task 10. **The mapping is frozen (Task 6) and is NEVER edited from this run's observations (review P5-2)** — the new rules' empirical occurrence counts appear in the audit report only.
6. **Durable identity record (review P5-7)**: commit `docs/superpowers/reports/<dated>-phase5-identity.json` containing: `candidate_commit` (C5), `candidate_bin_sha256`, the full join key, `facade_version`/`wire_schema`/`aat_schema` coordinates, `mapping_version` + canonical `mapping_hash` (0.4.0) + `frozen_mapping_hash` (0.3.0), corpus store path, baseline dump identity (`ab-aozora-phase4-c4-27772b1` + its run-set `content_hash`), candidate dump identity (path + `hash_aat_dir` content hash). `.superpowers/sdd/phase5-identity.json` remains git-ignored WORKING state; the committed record is the reviewable identity.
7. **Gate summaries**: wrap each raw output in the Phase 4 gate-summary shape (`stage: "c5"`, `gate`, `verdict`, `candidate {commit, bin_sha256}`, `version` = full join key with `(git <C5>)`).

Commit all frozen reports + summaries locally under `docs/superpowers/reports/` (dated), plus the runner JSONs verbatim.

---

### Task 10: Registry row → admission → atomic repoint → checkpoint

- [ ] **Step 1: Registry row.** Append the C5 row to `abc/data/aat-parser-ir-compatibility.edn` by copying the row from Task 9's `--compat-edn-out` file VERBATIM (byte-exact whole-row equality is the admission rule — never hand-edit). Expected row fields: `:aat_adapter_version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git <C5>)"`, `:mapping_version "0.4.0"`, `:mapping_hash` = Task 9's final hash, evidence scope 17886/17886/0. Commit (abc-side commit).
- [ ] **Step 2: Admission.** `cd abc && clojure -M:abc/aat-compat-admission -- --candidates <repo>/ab-validator/docs/superpowers/reports/<dated>-ab-aozora-phase5-c5-compat.edn | tee <repo>/ab-validator/docs/superpowers/reports/<dated>-phase5-admission-report.txt` (`set -o pipefail`). Expected `:status :admitted`. Commit the capture.
- [ ] **Step 3: Atomic repoint commit.** ONE commit touching EXACTLY ONE file, `ab-validator/reports/aat-fidelity/run-sets/current.json` (the verifier's `check_repoint` requires exactly this set — Task 3): the `ab-aozora` entry's `aat_dir`/`run_descriptor` → the C5 dump paths, `adapter_version_contains` → `<C5>`, `content_hash` → `sha256:<hash_aat_dir of the C5 dump>` (compute with the same `reports/aat-fidelity/lib/aat_hash.py` function `resolve-run-set.py` uses; verify by running `python3 reports/aat-fidelity/resolve-run-set.py` → lock resolves green). Retention notes, docs, and any other change go in OTHER commits.
- [ ] **Step 4: Post-repoint coverage regeneration** (C5 wiring + Task 7 confirmation): run the coverage recipe; expect ALL Contract 5 assertions (top-level COMPLETE). Freeze as the phase's postactivation coverage report.
- [ ] **Step 5: Checkpoint.** Run `verify-phase5-checkpoint.py` with every argument bound (gates from Task 9, audit summary, conversion summary, admission capture + live re-run cmd, `--repoint-commit <sha>`, `--run-set reports/aat-fidelity/run-sets/current.json`, `--coverage <postactivation summary>`, `--c5 <C5>`, mapping args from Task 6/9) `| tee` into the frozen checkpoint report (`set -o pipefail`). Expected: `CHECKPOINT OK` exit 0. Then run ONE negative probe (e.g. `--c5 <wrong sha>`) and record its `CHECKPOINT FAIL` in the report — proving the verifier bites.
- [ ] **Step 6: Retention.** Record in the closure doc + memory: `ab-aozora-phase5-c5-<short>` joins the never-delete dump list (append-only; C4 dump stays).

---

### Task 11: Keigakomi 44-residual — attribute or errata

**Files:**
- Create: `reports/aat-fidelity/keigakomi-residual-attribution.py`
- Create: frozen report `docs/superpowers/reports/<dated>-keigakomi-residual-attribution.md` + `.summary.json`

**Interfaces:**
- Consumes: the reader import pattern from `bare-toggle-placement.py` (import the split-script reader the same way — copy the importlib block verbatim); `decoration.keigakomi.source_patterns` from `data/aozora-syntax-coverage.toml:2471` (13 patterns — parse the TOML with `tomllib`); the frozen totals: matrix-exact 673, frozen denominator 717.
- Produces: per-pattern, per-work occurrence table over the exact 17,886-entry universe; the diff target is the Rust scanner's counting semantics. The script must ALSO reproduce the Rust source-inventory count if possible: read how `crates/ab-coverage/src/bin/source_inventory.rs` counts keigakomi occurrences (per-line? per-match? which regex crate semantics?) and implement that counting mode alongside the matrix-alternation mode. The 44 = difference between the two countings; localize to named works/lines/forms.

- [ ] **Step 1:** Read `source_inventory.rs`'s keigakomi counting (find the function; note regex dialect differences — Rust `regex` vs Python `re`: `(?i)`, unicode classes, overlapping alternations) and write the script with BOTH counting modes + a `--diff` output listing every (work, line, matched-form) present in one mode and not the other.
- [ ] **Step 2:** Run on hinoki against the pinned corpus. Localize the 44. Outcomes: (a) attributed → report names the forms/works and the semantic cause (e.g. a pattern matching overlapping occurrences, a form counted by the Rust scanner outside the matrix alternation); (b) NOT attributable → errata report per spec Contract 4: preserve BOTH figures (673 scanner-defined / 717 frozen-instrument / 44 unresolved), require future keigakomi rates to name their denominator definition or report an interval; NO single authoritative figure designated.
- [ ] **Step 3:** Commit script + frozen report. Not phase-blocking — do not gate Task 10 on this.

---

### Task 12: Hygiene items (three independent commits)

**12a — README v1 hash line.** `crates/ab-aat-to-parser-ir/README.md:84-85`: replace `sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340` with `sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2` (the hash the frozen v1 mapping and the registry rows actually bind — verify by grepping `a1e1b506` in `abc/data/aat-parser-ir-compatibility.edn`). Commit: `docs(converter): correct v1 parser-IR schema hash citation`.

**12b — Facade Segments-skip test.** Add to `crates/ab-aozora-facade/src/json.rs` `mod tests` (beside `ruby_entries_exposes_side_base_reading`, json.rs:756):

```rust
#[test]
fn ruby_entries_skips_segments_base_ruby() {
    // A gaiji-reference base (※［＃…］) is segmented content, not a single
    // plain run; content_range_as_plain returns None and the entry must be
    // omitted (the AAT adapter then keeps its own typed emission — pinned
    // adapter-side by gaiji_base_ruby_keeps_v1_typed_emission).
    let src = "※［＃「木＋吶のつくり」、第3水準1-85-57］《かい》\n";
    let tree = parse(src); // use the same constructor ruby_entries_exposes_side_base_reading uses
    let entries = ruby_entries(&tree);
    assert!(
        entries.iter().all(|e| e.reading != "かい"),
        "Segments-base ruby must not project into ruby_entries: {entries:?}"
    );
}
```
(Adjust `parse` to the constructor the neighboring test actually uses.) Run `cargo test -p ab-aozora-facade ruby_entries`. Commit: `test(facade): pin ruby_entries exclusion of Segments-base ruby`.

**12c — CRLF suppression in `verify-golden-spans.py`.** First REPRODUCE: run the tool over a CRLF golden (`full-markup-shift_jis.txt` if CRLF; find the failing pair by running the tool over each golden and recording which spans FAIL and exactly how — the documented artifact is a `line_start` computed from `\n`-counting vs a value baked under CRLF normalization; capture the precise signature: value matches bytes exactly BUT `line_start` off by the number of `\r\n` line breaks before the span, or value differs ONLY by `\r` characters). Then implement: classify each mismatch; if and only if it matches the captured signature exactly, count it in `crlf_artifact_suppressed` and do not fail; any other mismatch still exits 1. Print `crlf_artifact_suppressed: N` in the output. Add a pytest with a synthetic CRLF source + golden reproducing both a suppressible artifact and a REAL span error (must still fail). Commit: `fix(reports): verify-golden-spans suppresses documented CRLF artifact by signature, counted`.

---

### Task 13: Closure

- Append a Phase 5 closure section to `docs/handoffs/2026-07-10-parser-fork-provenance.md`: C5 identity + join key, gates table, mapping generation table (0.3.0 frozen file / 0.4.0 live), repoint commit, checkpoint result, coverage COMPLETE, keigakomi residual outcome, retention list (append C5 dump), rollback (revert repoint commit; mapping 0.3.0 remains in-tree).
- Ledger: final Phase 5 section entries in `.superpowers/sdd/progress.md`.
- Verify workspace green one last time: `cargo check --workspace --all-targets && cargo fmt --check && just clippy && python3 -m pytest reports/ -q`, plus the monorepo's primary gate at the REPO ROOT: `just validate-migration` (root `justfile:52` — review P5-8 named this as the omitted primary check). `nix flake check` at repo root if `ab-validator/flake.nix` inputs changed (they should NOT this phase — no flake input edits are planned; if any task touched one, run `nix flake update ab-validator` at the monorepo root per the Phase 4 lesson).
- Commit closure docs.

## Review round 2 (plan review P5-1…P5-8) — incorporated

- P5-1: placement evidence regenerated as Revision 3 over the shared `reports/lib/corpus_reader.py` contract (four-class candidate classification; windows-31j member names; tolerant 7zz). Empirical outcome: 17,886 works + 5 non-work + 2 recovered-extra (BOTH toggle-free — verified, so 1582/25/24 stands over the full readable universe) + 2 unreadable (named, no known reader). Note the review's premise was partially wrong: the four excluded files were not SJIS-name ZIPs (Python reads those; Java rejected them), and ABC's own strict 7zz fallback throws on the nonzero exits these need — recovery required a MORE tolerant reader than production has, which is why the two recoveries are a separate class outside the gate universe.
- P5-2: no `observed` field in mapping rules (schema forbids it); counts live in the audit report; mapping 0.4.0 frozen before the authoritative run, never rewritten from its own observations.
- P5-3: verifier binds `.totals.files_attempted/succeeded/failed` and `.mapping.mapping_version/mapping_hash`; canonical hash via `reports/lib/legacy_json_c14n.py` (mirror of `abc_legacy_json_hash`); real conversion-summary fixture; mapping freeze ordered before the verifier (see EXECUTION ORDER).
- P5-4: the audit derives expected adoptions + decline reasons from the baseline dump via `classify_tokens` (token-level entry point added to the instrument); placement report demoted to preregistered evidence; independence negative-tests added.
- P5-5: preflight split into a facade-wire test (paired markers, exact sources, ordered spans) and an AAT orphan-fallback test; neither changes when the classifier lands.
- P5-6: zero-adoption identity asserted as full structural equality on `pair_bare_toggles` input/output, in unit tests and the property target.
- P5-7: durable committed identity record (Task 9 deliverable 6); the `.superpowers` file is working state only.
- P5-8: pipefail-safe clippy verification with an exact-crate filter; explicit-path staging (no `git add -u`); admission stub echoes `:admitted` (+ negative test for silent success); repoint commit = exactly `current.json` in both Task 3 and Task 10; root `just validate-migration` added to the final gate; `proptest.workspace = true`.

## Self-review notes (writing-plans checklist applied)

- Spec coverage: Task 0 ↔ spec Task 0; Task 1 ↔ observability preflight; Tasks 2–3 ↔ instruments-before-identity; Task 4–5 ↔ Contract 1 (grammar, mirror, property target); Task 6 ↔ Contract 2; Task 7 ↔ Contract 5; Tasks 8–10 ↔ Contract 3 (identity, gates, ceremony); Task 11 ↔ Contract 4; Task 12 ↔ Contract 6; Task 13 ↔ closure. Non-goals honored: no inline-attr work, no cross-line pairing, no schema edit, no warning enrichment.
- Known unknowns made explicit rather than papered over: the registry `mapping_hash` computation scheme (Task 6 Step 1 verifies before Task 3's check is finalized); the golden regeneration flow (Task 4 Step 4); the `run()` test-helper extension (Task 2 Step 4); the coverage recipe arg plumbing (Task 7 Step 3); the dump runner invocation (Task 9 — copied from the Phase 4 report header, which records it).
- Type consistency: `classify_line`/`LineOutcome` field names match the committed instrument; audit summary keys (`toggle_adopted`, `details.adopted_yokogumi_pairs`, `declined_by_reason.reopen_rollback`) are used identically in Tasks 2, 3, 9; the join key string is identical in Tasks 3, 8, 9, 10.
