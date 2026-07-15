# Parser Release-Qualification Measurement Campaign — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a real verdict for every ADR 0039 release predicate from a
committed instrument on one admitted build, so the gate renders an honest
qualification result. Success is *honest verdicts*, not a green gate: an honest
`fail` or a resolved `:conflict` that leaves ADR 0039 `Proposed` is a correct
outcome. The only failure is a fabricated/imputed number or a weakened predicate.

**Design:** `docs/superpowers/specs/2026-07-15-parser-release-qualification-campaign-design.md`
(read it first — this plan executes it). Two tracks: **Track R** (release
qualification → ADR 0039) is the spine; **Track S** (neutral-study axes → the
study, never gates ADR 0039) is a separable appendix. The gate moves to
**Capture → Derive → Drift** (decision B): corpus-scale evidence is captured once
on hinoki into a three-tier store (committed manifests + external
content-addressed blobs + a rebinding verifier), and each predicate observation
is a deterministic projection of the committed manifests, drift-tested — no
hand-authored numbers. Coherence is a **gate-level precondition** (not a tenth
predicate) that fails closed unless every observation shares one
`qualification_identity` and that tuple exact-matches an admitted ADR-0023 row.

**Tech Stack:** Clojure (`abc.tools.parser-release-qualification`,
`abc.tools.aat-parser-ir-compat`, Malli schemas), Rust
(`ab-aat-to-parser-ir audit-corpus`, `ab-check`), Python (capture/derivation
harnesses, ruff), Nix checks, EDN/JSON manifests. Heavy captures and full-corpus
audits run on hinoki (`.superpowers/sdd/hinoki-exec.md`); governance recapture per
`.superpowers/sdd/task-6-report.md`.

## Global Constraints

- **Integrity mandate (overrides everything):** never fabricate or impute a
  measurement. A predicate with no instrument stays `:unavailable`; an observed
  value below threshold is `:fail`; `0.969` fails a `1.0` predicate mechanically.
  Do not weaken, round, or reinterpret a predicate to force a pass — changing a
  predicate requires its own evidenced ADR (ADR 0039's rule).
- **Evidence-class separation is structural.** Neutral-comparison /
  parser-selection evidence can never satisfy admission or release (enforced by
  `abc.tools.parser-evidence`). Track S never feeds the gate.
- **One coherent tuple.** All Track-R evidence describes one pinned build; the
  gate precondition *derives* admission equality from the ADR-0023 registry — it
  never trusts a hand-asserted `:admitted_tuple_matches`.
- **No corpus-scale artifacts in the repo.** Commit manifests, hashes,
  inventories, summaries, and small witnesses only; corpus-scale blobs live in the
  external content-addressed store, rebound and verified on hinoki. `artifact_root`
  is a content address, not a machine-local path.
- **Immutable history.** Admission appends a *new* registry generation; never
  rewrite the existing `ab-aozora 0.6.0 … 004deaf … 0.4.0` row or any committed
  manifest.
- **Governance stays green.** Touching an ADR evidence-closure file (the gate
  report, citations, registry) requires recapture through the real tool on hinoki;
  `just validate-migration` must exit 0 (see `.superpowers/sdd/task-6-report.md`).
- **Heavy work on hinoki.** Full-corpus audits, capture runs, memory/latency
  measurement, and regeneration run on hinoki, not locally.

## Task sequencing (dependencies)

```
Phase A (foundation, serial):   T1 pin+decide → T2 data contracts → T3 gate precondition
Phase B (instruments):          T4 (R3) ┐  T5 (R4) ┐        each: build → capture(hinoki)
                                T6 (R1) → T7 (R2)  │        → derive → flip predicate
                                T8 (R5 hardening) depends on T6 (span) + T1 (pred-4 decision)
Phase C (admission+promotion):  T9 (R6 admit)  ‖ Phase B   → T10 (R7 barrier: recapture+gate+ADR)
Phase D (Track S, separable):   S2 → S1 → S3 → S4          parallel to Track R; never gates it
```

- **T3 must precede all instruments** — every instrument emits into the
  envelope/manifest contract T2 defines and T3 enforces.
- **R1 (T6) precedes R2 (T7);** R2 also needs the diagnostic plumbing settled (T1).
- **R3, R4 are independent** and may interleave.
- **R6 (T9)** is independent of the instruments but shares the pinned tuple (T1);
  it can run in parallel with Phase B.
- **T10 is the barrier** — it consumes T3–T9 and must not run until all feed one
  coherent tuple.
- **Track S** is end-to-end independent; S4 (host-controlled latency) is the long
  pole and is scheduled last.

## File Structure

- `abc/src/abc/tools/parser_release_qualification.clj` + test — the gate:
  `evaluate`, `gate-status`, report schema; **T2/T3 migrate this** to
  envelope-shaped observations + the coherence precondition.
- `abc/data/parser-release-qualification-predicates.edn` — the nine predeclared
  predicates (do not change the set without an ADR; T8 touches only predicate 4's
  disclosure, T1 decides its identity).
- `abc/data/parser-release-qualification-corpus.edn` — pinned qualification corpus.
- `abc/docs/reports/parser-release-qualification-measurements.edn` → migrates to a
  **generated** envelope bundle; `…-report.json` regenerated by the gate.
- `abc/data/aat-parser-ir-compatibility.edn` — ADR-0023 registry (T9 appends).
- `abc/src/abc/tools/aat_parser_ir_compat.clj` (alias `abc/aat-compat-admission`)
  — admission; its match-key logic is reused by the T3 precondition.
- `ab-validator/crates/ab-aat-to-parser-ir` — `audit-corpus` (T9 full-corpus audit).
- `abc/schemas/parser-ir-publication-preservation.schema.json` +
  `ab-validator/reports/parser-ir/publication-bundle-validate.py` — R3 instrument.
- `abc/schemas/parser-ir.schema.json` — predicate 5 validation target.
- New: a capture-manifest schema, the observation-envelope bundle schema, the
  external-store config + rebinding verifier, and one harness per instrument
  (R1–R4) under `abc/` (Clojure/Python) or `ab-validator/reports/…`.
- `abc/docs/adr/0039-custom-parser-release-qualification.md` (T10 promotion),
  `abc/docs/adr/0023-owned-aat-parser-ir-mapping.md` (T9 admission context).

---

## Phase A — Foundation

### Task 1: Pin the release candidate and resolve the two scope decisions

**Files:** the design spec (record the pinned tuple + decisions); a disposable
probe (throwaway, not committed as production code).

**Interfaces:**
- Produces: the frozen `qualification_identity` tuple (git rev, baked adapter
  coordinates, mapping id/version/hash, parser-IR schema id/hash, corpus hashes,
  predicate-set hash) and a recorded decision on predicate 4's identity.

- [ ] **Step 1: Pin the release candidate.** Choose the exact `ab-aozora` git rev
  that is the release candidate (a real, buildable HEAD), and record its baked
  adapter coordinates, the mapping version it converts under, and the parser-IR
  schema hash it validates against. This tuple is the single input to T3–T9.
- [ ] **Step 2: Disposable diagnostic-plumbing probe.** Run `ab-aozora --mode
  diagnostics` over a diagnostic-triggering fixture and observe whether it can
  emit, and whether a capture can record, diagnostics with `{code,severity,span}`.
  Label the probe disposable; it settles *plumbing only*, not semantics.
- [ ] **Step 3: Decide predicate 4's identity** (grounded, recorded). Either
  **(a) keep envelope-completeness** — zero-over-corpus is a legitimate pass, but
  the vacuity MUST be disclosed in the observation itself; or **(b) diagnostic
  recall** over a versioned challenge corpus — a *different* predicate that needs
  an oracle and its own evidenced ADR. If (b), open that ADR as separate work; do
  not swap it into the nine silently. Default to (a) unless the probe/grounding
  shows the parser is expected to diagnose conditions it silently ignores.
- [ ] **Step 4: Record** the pinned tuple and the predicate-4 decision in the
  design spec's "Settled decisions" section. Commit
  `docs(parser): pin release candidate + resolve predicate-4 identity`.

### Task 2: Data contracts — capture manifests, observation envelopes, three-tier store

**Files:**
- Create: a capture-manifest schema and an observation-envelope bundle schema
  (Malli in the gate ns and/or committed JSON/EDN schema files).
- Create: external-store config + a rebinding-verifier interface (model on
  `ab-validator/reports/parser-study/freeze_run_evidence.py` `verify_external`).
- Test: schema-validation tests (Clojure + the verifier's tests).

**Interfaces:**
- Consumes: the pinned tuple (T1).
- Produces: the versioned data contracts every instrument and the gate depend on.

- [ ] **Step 1: Failing test** for the capture-manifest schema (per-work +
  aggregate: content hashes, inventories/denominators in explicit units, small
  witnesses, external blob content-addresses) and the observation-envelope schema
  (each observation = `{:value … :identity <qualification_identity ref/hash>}`).
- [ ] **Step 2: Run → FAIL.**
- [ ] **Step 3: Implement** the schemas + the three-tier evidence model: committed
  manifests, an external content-addressed store config, and a rebinding verifier
  that binds committed manifest hashes to store blobs and confirms integrity on
  hinoki (the count-authenticity analogue of the study's `verify_external`).
- [ ] **Step 4: Run → PASS**; ruff/clippy/fmt as applicable.
- [ ] **Step 5: Commit** `feat(parser-rq): capture-manifest + observation-envelope contracts`.

### Task 3: Gate migration — the coherence precondition

**Files:**
- Modify: `abc/src/abc/tools/parser_release_qualification.clj` (bundle shape,
  `evaluate-predicate`, `gate-status`, report schema/version) + test.
- Modify: `abc/docs/reports/parser-release-qualification-measurements.edn` →
  envelope form (4 predicates still `:instrument-missing`).

**Interfaces:**
- Consumes: envelope bundle (T2), the ADR-0023 registry, `aat-parser-ir-compat`
  match-key logic.
- Produces: a gate that is `:release-qualified` only when the coherence
  precondition holds **and** all nine verdicts are `:pass`.

- [ ] **Step 1: Failing tests.** (a) `gate-status` is `:not-qualified` when the
  observed tuple does not exact-match an admitted registry row, *even if all nine
  verdicts were `:pass`* (this is the current code's gap — `gate-status:156`
  ignores admission). (b) The precondition fails closed when observation envelopes
  disagree on `qualification_identity`. (c) Admission equality is *derived from the
  registry*, not read from a `:admitted_tuple_matches` field.
- [ ] **Step 2: Run → FAIL.**
- [ ] **Step 3: Implement** the precondition ahead of the verdict tally; migrate
  `evaluate-predicate` to read `.value` from envelopes; make `gate-status` require
  precondition ∧ nine `:pass`; bump the report-schema version. Re-express the
  committed bundle in envelope form (predicates 2/3/6/8 stay `:instrument-missing`
  → gate stays `:not-qualified`, now for coherent reasons).
- [ ] **Step 4: Regenerate** `…-report.json`; run the gate + kaocha. If the report
  is in an ADR evidence closure, recapture on hinoki and confirm
  `just validate-migration` exits 0.
- [ ] **Step 5: Commit** `feat(parser-rq): coherence precondition derives admission from the registry`.

---

## Phase B — Release instruments (each flips one predicate `unavailable → real`)

Every task here: write the failing derivation test → build the instrument →
capture on hinoki into the three-tier store → derive the observation as a pure
projection → regenerate the bundle/report → the predicate flips to `:pass`/`:fail`
from real evidence. No task weakens a threshold or imputes a value.

### Task 4 (R3): Publication-structure instrument (predicate 6)

**Rationale first:** lowest uncertainty — the schema
(`parser-ir-publication-preservation.schema.json`) and validator
(`publication-bundle-validate.py`) already exist; the work is a committed real run.

- [ ] **Step 1: Failing test** that the publication-structure observation derives
  from a capture manifest (per-work + aggregate pass ratio against the preservation
  schema), denominator in works, with every work contributing a record.
- [ ] **Step 2: Run → FAIL.**
- [ ] **Step 3:** wire the validator over the pinned build's publication output for
  the corpus (on hinoki); emit the capture manifest (committed) + external blobs;
  derive the ratio observation into the bundle.
- [ ] **Step 4:** regenerate report; predicate 6 = `:pass` (1.0) or `:fail` (<1.0,
  listing incomplete works). Governance green.
- [ ] **Step 5: Commit** `feat(parser-rq): publication-structure instrument`.

### Task 5 (R4): Per-work memory instrument (predicate 8)

- [ ] **Step 1: Failing test** that peak-RSS observation derives from a capture
  manifest carrying per-work peak RSS + host coordinates (host-sensitive predicate).
- [ ] **Step 2: Run → FAIL.**
- [ ] **Step 3:** build a per-work peak-RSS harness (GNU `time -v` / `getrusage` /
  cgroup peak) on a pinned, disclosed host; capture on hinoki; derive corpus-max
  observation.
- [ ] **Step 4:** regenerate; predicate 8 = `:pass` (≤ 2 GiB) or `:fail`. Host
  pinned in the manifest so the bound is interpretable.
- [ ] **Step 5: Commit** `feat(parser-rq): per-work memory instrument`.

### Task 6 (R1): Source-span coverage instrument (predicate 2)

**Files:** a byte-coverage analyzer + a **versioned ignored-region taxonomy**
(taxonomy version ∈ instrument identity).

- [ ] **Step 1: Failing test** pinning the denominator UNITS: coverage =
  `covered_eligible_bytes / eligible_bytes` where `eligible_bytes = total_source_bytes
  − ignored_regions(taxonomy_version)`. Assert the denominator is a **byte** count
  (a work-count denominator must fail the test), and that "every work contributed a
  record" is a *separate* work-unit assertion.
- [ ] **Step 2: Run → FAIL.**
- [ ] **Step 3:** build the per-work analyzer over `ab-aozora` parser-IR output;
  commit/hash per-work eligible-byte counts + aggregate; classify uncovered bytes by
  the versioned taxonomy; capture on hinoki; derive the ratio.
- [ ] **Step 4:** regenerate; predicate 2 = `:pass` (1.0) or `:fail` (<1.0, with the
  uncovered-region taxonomy naming what is dropped).
- [ ] **Step 5: Commit** `feat(parser-rq): source-span coverage instrument (byte denominator + taxonomy)`.

### Task 7 (R2): Silent-drop instrument (predicate 3)

**Depends on:** T6 (uncovered-region set) and T1 (diagnostic plumbing settled).

- [ ] **Step 1: Failing test** that silent-drops = count of uncovered source regions
  (from R1) that lack a corresponding diagnostic — the ADR-0002 source definition,
  explicitly NOT the AAT→parser-IR mapping-layer LOSS count.
- [ ] **Step 2: Run → FAIL.**
- [ ] **Step 3:** join R1's uncovered-region set with the captured diagnostic stream;
  count uncovered-without-diagnostic per work + aggregate; capture/derive.
- [ ] **Step 4:** regenerate; predicate 3 = `:pass` (0) or `:fail` (>0, enumerating
  the silently dropped constructs).
- [ ] **Step 5: Commit** `feat(parser-rq): silent-drop reconciliation instrument`.

### Task 8 (R5): Weak-pass hardening (predicates 4 and 5) — no predicate reinterpretation

**Depends on:** T1 (predicate-4 decision), T6/diagnostic capture, and the pinned
admitted tuple (for predicate 5's re-measure).

- [ ] **Step 1: Failing tests.** (4) Per the T1 decision: if envelope-completeness,
  the observation must **disclose vacuity** when the corpus emits zero diagnostics
  (a bundle note is insufficient); if recall was chosen, that lives in its own ADR,
  not here. (5) Schema validation is measured against the **admitted tuple's**
  mapping + schema hash, not the live-drifted 0.5.0/`43a6a6d8…`.
- [ ] **Step 2: Run → FAIL.**
- [ ] **Step 3:** implement the predicate-4 disclosure in the derived observation;
  re-measure predicate 5 under the pinned admitted tuple. Do **not** subsume
  predicate 5 into coherence — they remain independent.
- [ ] **Step 4:** regenerate; both predicates carry honest, disclosed observations.
- [ ] **Step 5: Commit** `fix(parser-rq): de-weaken predicates 4 and 5 without reinterpretation`.

---

## Phase C — Admission and conditional promotion

### Task 9 (R6): Admit the exact release-candidate tuple (ADR 0023)

**Files:** `abc/data/aat-parser-ir-compatibility.edn` (append),
`ab-aat-to-parser-ir audit-corpus`, alias `abc/aat-compat-admission`.

- [ ] **Step 1:** Run a **fresh full-corpus conversion audit** for the pinned tuple
  on hinoki:
  `ab-aat-to-parser-ir audit-corpus --aat-dir <corpus> --mapping
  data/aat-to-parser-ir-mapping-v2.json --summary-json … --report-md …
  --compat-edn-out <candidate-row> --jobs 32 --abc-root data/abc-schemas`.
- [ ] **Step 2:** Append the `--compat-edn-out` candidate as a **new immutable
  registry generation** (never rewrite the existing row). Current-schema tuple:
  current git rev + mapping 0.5.0 + parser-IR schema hash `43a6a6d8…` (the admitted
  `004deaf`/0.4.0 row pins the drifted `a1e1b506…` and fails closed against the
  current schema — that is why a new generation is required).
- [ ] **Step 3:** Admit via `clojure -M:abc/aat-compat-admission -- --candidates
  <candidate-row>`; require overall status `:admitted` (exit 0). An honest
  `:conflict`/`:missing` is a real blocker — record it, do not force it.
- [ ] **Step 4:** Recapture affected ADR evidence bundles on hinoki; confirm
  `just validate-migration` exits 0.
- [ ] **Step 5: Commit** `feat(parser): admit the release-candidate parser/adapter/mapping tuple`.

### Task 10 (R7): Recapture, re-run, conditional ADR 0039 promotion — THE BARRIER

**Do not start until T3–T9 all feed one coherent tuple.**

- [ ] **Step 1:** Generate the measurement bundle from the committed capture
  manifests (decision B) via the deterministic projection tool; all nine
  observations are envelope-shaped projections, predicate 4/5 hardened; admission
  equality left for the gate precondition to derive.
- [ ] **Step 2:** Regenerate `…-report.json` via the gate; add a **drift test** that
  the bundle regenerates byte-identically from the committed manifests.
- [ ] **Step 3: Conditional promotion.** If the coherence precondition holds AND the
  tally is all-`:pass`: promote ADR 0039 to **Accepted** (Status + `Accepted:` date,
  release authority toward publication, criteria citing the new instruments as
  executable evidence). Otherwise leave it **Proposed** with the named blocker (an
  honest failing predicate or unresolved admission). Never weaken a predicate to
  promote.
- [ ] **Step 4:** Recapture evidence closure; `just validate-migration` exits 0; ABC
  governance + focused Nix checks green.
- [ ] **Step 5: Commit** `feat(parser): qualify (or honestly decline) the parser via captured evidence`.

---

## Phase D — Track S (neutral-study axes; separable, never gates Track R)

In scope, but end-to-end independent of Track R and its state never blocks the
gate. May be split into its own plan. Each axis lands the same Rigor Bar as the
one currently-`measured` study axis (content-hashed inputs, pinned denominators in
their own units, non-imputation, host capture where host-sensitive); a missing
link stays a disclosed blocker row, never a zero.

- [ ] **Task S2 — Diagnostic scoring (nearly ready).** Commit a per-parser
  diagnostic-capture run feeding the study, scored only on the predeclared
  label/severity/UTF-8 span from the frozen fixture
  (`parser-comparison-diagnostics-v1.json`) via the existing conformance scorer
  (`run-aozora-notation-spec.py`). Promote the study's `diagnostics` axis to
  `measured` for lanes that emit diagnostics.
- [ ] **Task S1 — Fidelity oracle.** Build a content-hashed reference rendering +
  per-lane fidelity runs (visible-text byte agreement, structure agreement,
  ruby/gaiji/note counts) feeding the study generator; emit blocker rows where a
  reference is absent.
- [ ] **Task S3 — Span accuracy (may honestly end `non_comparable`).** Build the
  instrument for span-bearing lanes; disclose adapter lanes (which omit spans) as
  `non_comparable` with the reason — never as zero, never forced to `measured`.
- [ ] **Task S4 — Kaplan–Meier latency + frozen bootstrap (long pole).** (a) A
  controlled single-host re-run capturing per-repetition wall time with committed
  `host_capture` for the compared lanes (the current blocker: host comparability is
  `unavailable` for all but one lane); (b) implement the preregistered KM estimator +
  frozen bootstrap (seed 20260714, 10000 resamples, right-censored at 300 s). Report
  median/p95/CI or an honest `unavailable` bound.

---

## Self-review

- Every Track-R task's acceptance gate is a real predicate verdict from a committed
  instrument, not a hand-typed number; the drift test (T10) is a change-detector,
  not the honesty gate. No task flips a status or changes a count by hand; no
  predicate is weakened (integrity mandate + ADR 0039's predicate-change rule).
- The two review blockers are addressed structurally: coherence is a gate
  precondition with a real data contract (T2/T3), and R1's denominator is in bytes
  (T6). The two artifact/scope suggestions are honored: three-tier capture (T2, no
  corpus-scale commits) and the plumbing-vs-semantics split for predicate 4 (T1/T8).
  Predicate 5 is retained, not subsumed (T8).
- Foundation precedes instruments (T3 before Phase B); R1 precedes R2; T10 is a hard
  barrier; admission (T9) is separable but shares the pinned tuple; Track S never
  gates Track R.
- Honest outcomes are first-class: a failing predicate or a `:conflict` admission
  leaves ADR 0039 Proposed with a named blocker, and that is recorded as success,
  not patched over.
- Heavy captures and the full-corpus audit run on hinoki; every evidence-closure
  touch carries the governance-recapture step and a `just validate-migration` exit-0
  check.
- Open at plan start (resolved in T1, not assumed): which git rev is the release
  candidate, and predicate 4's identity (envelope-completeness vs an ADR-gated recall
  predicate). Neither is forced to a false "decided."
