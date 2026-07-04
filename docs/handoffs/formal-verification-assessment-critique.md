# Critique: Are the Formal Models Useful?

> Companion to `formal-verification-assessment.md`. That assessment argued
> chiasmus "deepens the investigation." This critique asks the harder question
> the assessment barely touched: of the formal artifacts we actually committed,
> which carry real information about the system and which are `P ∧ ¬P`
> tautologies dressed as proofs. Verdict: **the call-graph work earns its keep;
> the three SMT "invariant proofs" do not, in their current form.**

## 0. The question, restated

"All models are wrong, but some are useful" (Box). A formal model is *useful*
when its verdict can *falsely* change — when there exists a real variant of the
system under which the model would give a different answer. A model that returns
the same verdict regardless of what the system does is a tautology: not wrong,
exactly, but carrying zero information about the system. This critique applies
that test to each committed formal artifact.

## 1. Inventory of committed formal artifacts

| Artifact | Tool | What it claims to prove |
|---|---|---|
| `docs/adr/r1-reproducibility-conflict.smt2` | z3 | ADR 0001 R1: two success manifests sharing identity & differing on content ⇒ release invalid |
| `docs/adr/r2-non-circularity.smt2` | z3 | ADR 0001 R2: `artifact_id` not among the 12 identity fields |
| `docs/adr/r3-drift-cardinality.smt2` | z3 | ADR 0020: split = 1 pred / ≥2 succ; merge = ≥2 pred / 1 succ |
| `flake.nix` checks `adr0001-invariants`, `adr0020-drift-cardinality` | `z3 file.smt2` | CI gate: Z3 must print `unsat` |
| `graph-query-verification.md` (call graph over real Rust) | `chiasmus_graph` | aozora2html does not reach ab-ir serialization; `ab-diff-utils` not a fake seam |

## 2. The three SMT proofs are `P ∧ ¬P` tautologies (mode-B vacuity)

### 2.1 Structure

Every committed `.smt2` has the shape:

```text
(declare ...)                 ; unconstrained SMT constants
(assert Ax)                   ; "geometry": e.g. both manifests success
(assert P)                    ; the invariant, RESTATED as a bare fact
(assert ¬P)                   ; the counterexample = literal negation of P
(check-sat)                   ; → unsat, by the law of non-contradiction
```

`P` is the invariant. `¬P` is the counterexample, and it is the *logical
negation* of `P` (restated). The solver reports `unsat` because `P ∧ ¬P` is a
propositional contradiction — true for **any** `P`, including `P`'s negation,
including claims that have nothing to do with manifests or drift events.

### 2.2 Empirical confirmation (re-ran all three through chiasmus 2026-07-04)

I re-ran the three committed files through `chiasmus_verify` and confirmed:

- **R1** → `unsat`, core = `[r1_both_success, r1_same_identity, r1_content_differs, r1_conflict_implies_invalid, r1_counterexample_release_valid]`
- **R2** → `unsat`, core = `[r2_artifact_id_distinct_from_fields, r2_counterexample_field_equals_artifact_id]`
- **R3** → `unsat`, core = `[dr_split_cardinality, dr_merge_cardinality, dr_counterexample]`

R2 and R3 cores are **exactly `{P, ¬P}`** — no independent axiom participates;
the unsat is pure non-contradiction. R1's core additionally includes the three
geometry axioms, which only *set up the conflict scenario*; the invariant
itself is still the definitional equation `release_valid = ¬conflict`,
*asserted*, not derived from any manifest-validation logic.

### 2.3 The decisive test: strip the invariant assertion

If `P` were *derived* from lower-level axioms, removing the `(assert P)` line
would not change the verdict (the derivation still holds). If `P` is merely
*stipulated*, removing it leaves a satisfiable counterexample.

I stripped `P` from each file and re-ran:

| File | Stripped `P` | Verdict | Model |
|---|---|---|---|
| R1 | drop `r1_conflict_implies_invalid` | **sat** | `release_valid=true`, conflict holds |
| R2 | drop `r2_artifact_id_distinct_from_fields` | **sat** | `f_schema = artifact_id = "A"` (circular identity *present*) |
| R3 | drop `dr_split_cardinality`, `dr_merge_cardinality` | **sat** | `split`, `pred_count=0`, `succ_count=0` |

**This is the smoking gun.** With the invariant stripped, the counterexample
becomes satisfiable — meaning the invariant was the *only* thing refuting it,
and the invariant was itself a bare assertion. The committed `unsat` is the
truth of `P ∧ ¬P`, not a property of manifests, schemas, or code. Concrete
illustration: R2's stripped mutation models *circular identity present*
(`f_schema = artifact_id = "A"`) and is satisfiable — the SMT cannot tell that
this is exactly the situation the invariant forbids, because no SMT constant is
linked to a manifest field.

### 2.4 What this means, precisely

These files prove **"the author's transcription of the invariant is
self-consistent with its own negation."** That is a real but trivial property.
They do **not** prove the invariant holds of:

- `schemas/manifest.schema.json` (no SMT constant reads the schema)
- `manifest_identity_object` (the 12 `String` constants are unconstrained)
- the `validate-design-bundle` runtime path (the gate never runs the code)
- `sha256`/JCS (R2 models distinctness as `String ≠`, unrelated to the hash
  mechanism that actually prevents circularity)

A schema bug that added `artifact_id` as a 13th identity field would not flip
any of these `.smt2` files to `sat`. The models cannot fail on real regressions
because they do not read the real artifacts.

## 3. Silent transcription drift (independent defect)

Even taking the SMT at face value as "the spec, formalized," the formalization
silently changes meaning from the source ADR:

- **R1 vs ADR 0001.** The ADR's acceptance criterion (line ~95) is "*identical
  `manifest_identity_object`* and different `content.content_hash` ⇒ release
  fails." R1's SMT substitutes "**same `artifact_id`**" for "identical
  `manifest_identity_object`." But ADR 0001 *defines*
  `artifact_id = sha256(JCS(identity_object))`, so "same artifact_id" is a
  *derived* property of "identical object" — and a *weaker* one (it cannot see
  hash collisions in the other direction). R1 validates a different, weaker
  claim than the ADR states, and nothing in the pipeline flags the
  substitution.
- **R2 vs ADR 0001.** The ADR's mechanism for non-circularity is *structural*
  ("`artifact_id` is never nested inside `manifest_identity_object`" — a
  schema-level field-alphabet separation). R2 models it as `String` value
  distinctness over 12 free constants. Value-distinctness has no relationship
  to the field-alphabet property; the model wouldn't catch a schema that
  declared `artifact_id` as a field, because the constants are unconstrained.
- **The assessment's "two layers" defense collapses.** `formal-verification-
  assessment.md` §5 defends: "SMT proves the *spec*; the test gate proves the
  *code* — two layers." But the SMT is a hand-retyped copy of the spec, not the
  spec itself. Two copies of one author's belief is not triangulation; it is
  duplication presented as evidence.

## 4. What is genuinely useful (keep and expand)

- **`chiasmus_graph` reachability/impact/dead-code** (Avenue B in the
  assessment; see `graph-query-verification.md`). Machine-confirmed that
  `aozora2html` does not reach `blocks_to_aat_projection`, and *refuted* the
  `crate-classification.md` claim that `ab-diff-utils` is a fake seam (its
  public API has a real production consumer in `ab-compare`). These are useful
  Box-models: their verdict depends on real source, they can be wrong about
  reality, and they told us something we did not already assume.
- **§6 calibration honesty** in the assessment (don't solver-verify policy
  choices or empirical distributions). Correct and worth preserving.
- **R3's content** (split/merge cardinality) *is* a real invariant — it is just
  "proven" in a way that proves nothing. The fix is the test gate: assert the
  runtime validator *rejects* a fabricated `split` with `pred_count=2`.

## 5. Two distinct vacuity notions (be precise)

This critique distinguishes two failure modes; conflating them produces a
check that overclaims:

### Mode-A — counterexample unreachable for orthogonal reasons
The counterexample `¬P` is unsatisfiable *without* `P` (self-refuting, or refuted
by axioms alone). Removing `P` leaves `unsat`. The invariant did no work. This
**is** automatable: strip `P`, require `sat`. See §6.

### Mode-B — invariant stipulated, not derived (the current defect)
`P` asserted as a bare fact; `¬P` is its negation; `unsat` is `P ∧ ¬P`.
Removing `P` leaves a *satisfiable* counterexample (`sat`), so the committed
files **pass** mode-A. The defect is that `P` is not entailed by independent
axioms about the real system. **No pure-Z3 gate catches mode-B** — it is a
modeling-adequacy judgment (is `P` linked to the schema/code?). The remedy is
items §7.2–§7.4 (bind to real artifacts, or honestly relabel), not a stricter
solver invocation.

### Empirical mode-A results (all three pass — counterexamples are reachable)
| File | Strip `P` → | Verdict |
|---|---|---|
| R1 | drop invariant equation | `sat` ✅ passes mode-A |
| R2 | drop `distinct` | `sat` ✅ passes mode-A |
| R3 | drop cardinality rules | `sat` ✅ passes mode-A |

So the committed files are *not* mode-A-vacuous; they are mode-B-vacuous. The
Nix check in §6 is a **forward-looking guardrail** against mode-A regressions in
future specs, not a detector of the current defect. Stating otherwise would be
the same kind of overclaim this critique is written to call out.

## 6. The mutation-vacuity Nix check (item from §7.1)

Implemented in `flake.nix` as `adr-invariants-vacuity`. For each invariant
`.smt2`, it strips the `; BEGIN-INVARIANTS … ; END-INVARIANTS` block (the
invariant assertion `P`), keeps axioms + counterexample, and requires Z3 to
report **`sat`** — i.e. the counterexample must be reachable once `P` is gone.
If a future spec authors a counterexample that is self-contradictory or already
refuted by the axioms, this check fails (`unsat` after stripping) and surfaces
the vacuity.

**Scope, stated honestly:** this catches mode-A only. It does **not** flag R1,
R2, or R3 (all pass). It does **not** catch mode-B (stipulated invariants),
which is the actual defect of the current files. Mode-B requires binding `P` to
the real schema/code (§7.2–§7.3) or honest relabeling (§7.4); no automatic
solver invocation substitutes.

The value of shipping the check anyway: it is cheap, it prevents a *different*
class of future vacuity, and — by requiring each spec to delimit its invariant
block — it makes the "which assertion is the stipulated `P`?" question
*explicit and reviewable*, which is itself a mild corrective to mode-B
(§7.4 makes the same point at the doc level).

## 7. To make the models genuinely useful (ranked)

1. **Ship the mutation-vacuity guardrail (§6).** Done. Catches mode-A
   regressions; forces explicit invariant delimitation. Does *not* fix the
   current files.
2. **Separate axioms from conjecture.** A meaningful `.smt2` encodes
   *lower-level* axioms (where the 12 fields come from, how `artifact_id` is
   computed as `sha256(JCS(...))`, the hash-alphabet vs field-alphabet
   separation) and then *negates the goal*. For R2 that means modeling the
   hash construction and *deriving* `artifact_id ∉ fields`, not asserting
   `distinct` over free strings. This is the assessment-§3 "needs refinement"
   work that was deferred; without it the files should not claim to prove.
3. **Bind to the real artifact.** Either (a) generate SMT datatypes from
   `schemas/manifest.schema.json` so a schema change to identity fields
   auto-propagates into the model, or — far cheaper and stronger — (b) write a
   property test that constructs an offending manifest (`artifact_id` nested in
   `manifest_identity_object`; two success manifests with same id / different
   hash) and asserts `validate-design-bundle` *rejects* it. That test, not the
   `.smt2`, is what actually guards the invariant today; the `.smt2` currently
   gives it false cover.
4. **Honest relabeling.** Rename the directory and README from "solver-checked
   invariants" / "the invariant holds in the model" to "ADR self-consistency
   checks (mode-A guarded)." Stop presenting `unsat` as "genuine formal
   verification of the invariant" (assessment §2). Reserve *proof* for claims
   derived from independent axioms. This is the cheapest single change and the
   one most directly suggested by Box's framing.
5. **Aim solver effort at cross-rule entailment.** Single-rule files are always
   trivially consistent. The useful regime is *interaction*: the drift
   vocabulary + PROV graph shape + cardinality + identifier policy + Position-L
   ("drift events must not rotate manifest identity") interacting. There,
   `unsat` of `(all axioms ∧ ¬cross-cutting-invariant)` actually means
   something, because the axioms and the goal are different objects and neither
   restates the other.

## 8. Bottom line

- The **call-graph** artifacts are useful: verdict depends on real source, can
  be falsified, has falsified a handoff claim.
- The **three SMT invariant files** are not useful in their current form: they
  are `P ∧ ¬P` tautologies that pass for any claim, including circular identity
  (the R2 stripped mutation models exactly that, satisfiably). They prove the
  author's transcription contradicts itself, nothing about the system.
- The **Nix gate** enforces "my file returns `unsat`" — true by construction
  for this file shape — so it cannot fail on a real regression. The new
  `adr-invariants-vacuity` check (§6) is an honest *forward-looking* guardrail
  for a different vacuity class; it does not retroactively fix the current
  files. Fixing those requires §7.2–§7.4 (derive from independent axioms / bind
  to schema or property tests / relabel), or retirement of the files in favor
  of the property tests that already do the real work.

---

*Tools: chiasmus MCP (`chiasmus_verify` z3; `chiasmus_graph` for §4 context).
Probes run 2026-07-04. All findings reproducible by re-running the tool calls
in §2.2, §2.3, and §5. Companion implementation: `flake.nix`
`checks.adr-invariants-vacuity`; block markers added to the three committed
`.smt2` files.*
