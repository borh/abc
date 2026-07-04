# Code-as-Spec: Making the Formal Models Useful

> Status: Proposed
> Date: 2026-07-04
> Companion to: `docs/handoffs/formal-verification-assessment-critique.md`
> Supersedes (in intent): the hand-authored SMT pattern in `docs/adr/r{1,2,3}-*.smt2`

## 1. Problem

The critique established, empirically, that the three committed SMT "invariant
proofs" are mode-B vacuous: each is `P ∧ ¬P`, true for any claim, carrying no
information about the real system (manifests, schemas, code). The single design
defect behind every vacuous model in this repo is **a translation step between
what the author wrote and what the machine checks**: hand-transcribed SMT
restates a prose ADR, and nothing reads the real artifact. Box's usefulness
test fails because the verdict cannot change in response to a real regression.

The call-graph work, by contrast, is already code-as-spec: it reads real source,
no transcription, and it has falsified a handoff claim. The strategic move is
to **generalize that pattern and retire hand-transcribed input**.

## 2. Design rule (the one invariant of this design)

> **Every formal model must read a real artifact** — a JSON Schema, source
> code, a manifest corpus, or git history. **No model may take a hand-transcribed
> restatement of a prose spec as its input.** A model whose verdict cannot
> change in response to a real regression is not a model; it is ceremony.

Every layer below is an instance of this rule. Layers that cannot satisfy it
are retired.

## 3. Layered architecture

```
ADR prose  ──acceptance gate (F)──▶  condemned fixture (real artifact)
                                          │
                  ┌───────────────────────┼───────────────────────┐
                  ▼                       ▼                       ▼
            Schema (A)              Validator (D)           Fact corpus (C)
            the model              the oracle              the relational
            (JSON Schema /          (test.check             model (Prolog over
             SHACL / Schematron)     generators call         emitted facts from
             reads real schema      the real validator)     real manifests/drift)
             ▼                       ▼                       ▼
        fixture battery          property tests         CI Nix gate
        (A) rejects each          (D) finds shrunk        (C) queries facts,
        condemned fixture          counterexample by       asserts cross-artifact
        via real validator         running real code      invariants hold
```

Each layer reads a real artifact; none transcribes prose. The layers compose
(defense in depth) but each is independently useful.

## 4. Layer A — Schemas-as-spec + fixture battery (the workhorse)

### 4.1 Idea

The repo's JSON Schemas *already are* formal models: they enforce cardinality,
enums, `minItems`/`maxItems`, `additionalProperties`, `pattern`. The R3
property test shipped in commit `48c4c65` proves the pattern: a real schema, six
fixture counterexamples (one per cardinality-violation mode), a verdict that
flips if the schema regresses. Generalize this to every ADR invariant expressible
in a single schema.

### 4.2 Conventions

- **Condemned fixtures** live under `fixtures/v0/invalid/<invariant-name>/`,
  mirroring the existing drift-fixture layout. Each fixture is a concrete
  artifact the real validator must reject.
- A **condemnation test** asserts the real validator rejects the fixture and
  reports the expected failure code(s). This is the smoke-test pattern already
  used by `validate-drift-fixtures-smoke-test`.
- The schema is the model; the fixtures are the proof obligations; the test gate
  is the proof. No SMT.

### 4.3 Retrofits (replaces vacuous SMT)

| Old SMT | New fixture + test |
|---|---|
| R1 (reproducibility conflict) | superseded by Layer D (property test), not a fixture — see §6 |
| R2 (non-circularity of `artifact_id`) | `fixtures/v0/invalid/manifest-with-nested-artifact-id/` — a manifest whose `manifest_identity_object` contains an `artifact_id` field; `validate-design-bundle` must reject |
| R3 (drift cardinality) | **already shipped** (`test/abc/tools/person_drift_test.clj`, six modes); optionally also materialize as `fixtures/v0/invalid/drift/{split,merge}-cardinality-*` condemned fixtures |

### 4.4 Boundaries

- **In scope:** any invariant a single JSON Schema / SHACL shape / Schematron
  rule can express.
- **Out of scope (routed to C):** cross-artifact invariants — "a drift event
  must not rotate `manifest_identity_object`," "every `person_id` in a
  manifest's contributors resolves to a `person_record` or a drift successor."
  These span files no single schema can see.

## 5. Layer F — ADR-acceptance process gate

### 5.1 Idea

The cheapest, highest-leverage change: make the next vacuous model structurally
impossible to land **at review time**, rather than policed after the fact.

### 5.2 Change to the ADR template

The "Acceptance Criteria" section of every ADR must include, for each
invariant stated in prose:

- a **condemned fixture path** (`fixtures/v0/invalid/...`) that the real
  validator rejects, OR
- a **property test** path (`test/...`) whose generator drives the real
  validator, OR
- a **Prolog fact-corpus query** path (Layer C) for cross-artifact invariants.

An ADR whose acceptance criteria are prose "MUST" clauses with no executable
condemnation is not accepted. The R3 property test (`48c4c65`) is the exemplar
referenced by the template.

### 5.3 Enforcement (ratcheted)

A repo-wide scan finds 17 existing ADRs with an "Acceptance Criteria" section
but no `fixtures/|test/|facts/` path. Retrofitting all of them is out of scope.
Instead the lint is **ratcheted with an allowlist**:

- **Allowlist:** `docs/adr/.acceptance-legacy-allowlist` lists the 17 existing
  ADRs (0001, 0002, 0003, 0004, 0005, 0006, 0007, 0008, 0009, 0010, 0012,
  0013, 0014, 0017, 0018, 0020, 0022) that pre-date this gate. Each carries a
  ref-returning exception explaining which of its invariants are covered by
  which new/old executable path and which are genuinely prose-only.
- **Hard rule (forward):** any ADR added or modified after this gate lands MUST
  satisfy the executable-p condemnation per invariant; the build fails otherwise.
- **Migration set (retroactive):** the ADRs whose invariants this design
  re-routes to executable gates (0001, 0020 — and any others the plan touches)
  are removed from the allowlist in the same commit that lands their
  replacement, so a later regression can't quietly drop the executable path.
- **Review time:** reviewers cite the matching executable path when approving;
  the lint is a machine backstop, not a substitute for review.

## 6. Layer D — Property-based testing against the real validator

### 6.1 Idea

For invariants a single schema can't express *and* that are about a function's
input/output behavior (rather than cross-artifact relations), `test.check`
generators drive the real validator as the oracle. This is the closest thing to
formal verification that cannot suffer transcription drift: the generator
derives inputs from the schema, real code is the oracle, shrinking surfaces
concrete minimal counterexamples the SMT never could. It feels like a proof and
is falsifiable by real regressions.

### 6.2 Boundary (per the agreed decision)

**In-process:** the generator and the validator call live in one test file;
facts are emitted ephemerally (in-memory), the validator is called in-process.
No separate emitter lifecycle, no checked-in generated artifact. This is the
tightest loop and matches how `48c4c65`'s mutation test already worked.

### 6.3 Retrofit: R1 (reproducibility conflict)

The highest-profile vacuous SMT. Replacement:

- **Generator:** produce pairs of success manifests sharing an
  `manifest_identity_object` (generated from `schemas/manifest.schema.json`)
  but with mutated `content.content_hash`. Also generate the negative space
  (same identity, same hash → must pass; different identity → must pass).
- **Oracle:** the real release validator (`validate-design-bundle` /
  `validate-release` path).
- **Property:** `for all (m1, m2) generated as above, release_validation(m1, m2)`
  rejects iff identity matches and hash differs.
- **Shrinking** gives a minimal counterexample if a regression lets a conflict
  through — exactly what R1's SMT could never produce.

### 6.4 Boundaries

- **In scope:** invariants that are about a function's behavior over a
  generated input space (R1 reproducibility; future: canonicalization/JCS
  null-vs-omitted — the old "R3 null-dimension" that the assessment §3 correctly
  punted out of SMT).
- **Out of scope:** cross-artifact relational invariants → Layer C.

## 7. Layer C — Prolog over facts emitted from real artifacts

### 7.1 Idea

Generalize the call-graph pattern from code to data. For cross-artifact
invariants — drift events must not rotate `manifest_identity_object`
(ADR 0020 Position L); person_id referential integrity across
manifest↔drift↔person-record — emit Prolog facts from the *actual*
`manifest.json`, drift-event files, and person records, then query. The verdict
depends on real data, not a transcribed model.

### 7.2 Boundary (per the agreed decision)

**Checked-in generated facts** at `fixtures/v0/facts/prolog/`, governed by ADR
0011 (generated-fixture policy): regenerated from source artifacts in CI,
byte-stable, diffable, reviewable. This is the cross-artifact relational model
made inspectable — a Prolog query failure can be debugged against concrete
facts committed to the repo.

Concretely: a Clojure emitter (`abc.tools.facts.emit-prolog`, new) reads the
real manifest/drift/person-record corpus and writes
`fixtures/v0/facts/prolog/*.pl`. A Nix gate runs SWI-Prolog (see §7.5) against
the fact set plus a committed query file `docs/adr/<invariant>.pl`.

### 7.5 Dialect (pinned): SWI-Prolog

The CI dialect is **SWI-Prolog** (`swi-prolog` in nixpkgs), pinned at plan
time to a concrete nixpkgs revision for reproducibility. The pin is made now,
not deferred, because dialect affects negation (`\+`), tabling (`:- table`),
module syntax, CLI behavior, and Nix reproducibility — all of which shape the
emitter and query authoring. `chiasmus_verify` prolog remains available as an
**exploratory/manual path** for ad-hoc queries during development, but it is
not the CI dialect; the gate runs `swipl` directly so the dialect is fixed in
the Nix derivation, not in an MCP server.

Deferred to plan level only: file layout (one `.pl` per artifact type vs one
corpus-wide `.pl`) and the Kaocha test selector for any Clojure-side property
tests. Dialect itself is settled here.

### 7.3 Cross-artifact invariants to cover (the reason this layer exists)

1. **Position L (ADR 0020):** `manifest_identity_object` is unchanged by the
   presence of any drift event. Facts: `manifest_identity/2` from the real
   manifest, `drift_event/1` from the drift log. **Identity facts are emitted
   by the Clojure emitter using the repo's real manifest-identity function**
   (the same code path that computes `artifact_id`), and Prolog only *compares*
   emitted facts — it never recomputes identity. This avoids hidden
   hand-translation: a Prolog `recompute_identity_after_event/2` would risk
   restating manifest identity logic by hand; instead the emitter writes
   `manifest_identity_after(Manifest, Event, Hash)` facts for the post-event
   state computed in Clojure, and the query is
   `manifest_identity(M, H1), manifest_identity_after(M, E, H2), H1 \= H2`
   → must fail (no drift event rotates manifest identity). All identity
   arithmetic stays in the single real implementation.
2. **Person_id referential integrity:** every `person_id` in a manifest's
   `contributors[]` Either resolves to a committed `person_record` or to a
   successor in the drift log. Facts: `contributor/2`, `person_record/1`,
   `drift_successor/2`. Query: `contributor(M, P), \+ person_record(P), \+
   drift_successor(P, _)` → must fail (no dangling contributor).

### 7.4 Boundaries

- **In scope:** invariants spanning ≥2 artifact files (manifest + drift;
  manifest + person-record; drift + person-record).
- **Out of scope:** single-schema invariants (Layer A); function-behavior
  invariants (Layer D).
- **Predicated on `chiasmus_verify` prolog** (already demonstrated in the
  assessment) or a Nix-provided SWI-Prolog. Cycles need tabling; the
  `graph-reachability` template's caveats apply.

## 8. Layer G — Retire hand-authored SMT (cleanup, bundled)

### 8.1 Disposition (agreed): delete on replacement

Once each of R1/R2/R3 has its real-artifact replacement landed and green in CI:

- **Delete** `docs/adr/r1-reproducibility-conflict.smt2`,
  `docs/adr/r2-non-circularity.smt2`, `docs/adr/r3-drift-cardinality.smt2`.
- **Delete** `docs/adr/0001-invariants.README.md` (its content is retired; the
  invariants live in the schemas/tests/Prolog, documented in-line).
- **Remove** the `checks.adr0001-invariants` and `checks.adr0020-drift-cardinality`
  Nix gates (they enforced the vacuous files).
- **Delete** `checks.adr-invariants-vacuity` alongside the SMT files (no SMT
  → no vacuity to guard; dormant guardrails become ceremony). If a future
  cross-rule SMT (§8.2) is introduced, reintroduce a scoped vacuity gate **in
  the same commit** as that SMT. Do not leave it lying dormant.

### 8.2 Reservation (the one regime SMT is honest)

Reserve an SMT path *only* for cross-rule entailment where the axioms and the
goal are genuinely different objects (e.g., the *interaction* of drift
vocabulary + PROV graph shape + cardinality + identifier policy + Position L).
For single-rule files SMT is always trivially consistent; do not reintroduce it.

## 9. What is explicitly NOT pursued

- **LLM-as-judge for ADR↔code drift** (`chiasmus_solve` end-to-end). The
  critique already warned: adds nothing a property test can't do better with
  higher trust.
- **JSON-Schema→SMT generator** (mechanical derivation). Removes transcription
  drift but, once derived, mostly re-checks what the schema already enforces;
  not worth the mapping complexity (`$ref`, conditional `allOf`, `format`).
- **Hand-authored SMT for any new invariant.** Period.

## 10. Migration order (summary; full plan in writing-plans output)

1. **A + F** (foundation): generalize the R3 pattern; retrofit R2 as a
   condemned fixture; add the ADR-acceptance-template gate. Lowest cost, uses
   existing infra.
2. **D**: `test.check` reproducibility model (R1 replacement). Kills the
   highest-profile vacuous SMT.
3. **C**: fact emitter + Prolog gate for Position L and person_id referential
   integrity.
4. **G**: delete R1/R2/R3 `.smt2` + gates + README once steps 1–3 are green.

## 11. Success criteria

- Every ADR invariant has an executable condemnation (fixture, property test,
  or Prolog query) that reads a real artifact. **No invariant is "proven" by a
  hand-transcribed model.**
- A regression that widens a schema bound / lets a drift event rotate manifest
  identity / accepts a reproducibility conflict **flips a CI gate red** —
  verified by the same mutation discipline applied to the R3 test in `48c4c65`.
- R1/R2/R3 `.smt2` and their gates are deleted; `0001-invariants.README.md` is
  retired.
- The critique's mode-B vacuity class is structurally impossible to reintroduce:
  the ADR template (F) rejects prose-only acceptance criteria, and no path
  exists for hand-transcribed SMT to enter a gate.

## 12. Open questions deferred to plan (not design) level

Two previously-deferred items are now pinned in §7.5: the CI Prolog dialect
(SWI-Prolog) and the fact-emitter location (`fixtures/v0/facts/prolog/`).
What remains for the plan:

- File layout: one `.pl` per artifact type vs one corpus-wide `.pl` (not a
  design decision; both satisfy the design rule).
- Kaocha test selector for the Layer D property tests (separate suite vs
  in-tree).
