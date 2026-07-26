# Accepted-Criteria Conjunct Review (disposition-review caveat 2)

Date: 2026-07-23
Scope: all 32 Accepted ADRs, 156 acceptance criteria, decomposed into 453
atomic conjuncts; each conjunct checked against the criterion's *cited*
evidence, then against the wider suite. Executed as six parallel review
passes over disjoint ADR slices, with adversarial spot-verification of every
severe claim against the actual test files.

Classification per conjunct:

- **COVERED** — a cited assertion fails if the conjunct is false.
- **PARTIAL** — cited evidence asserts a strictly weaker version.
- **UNCOVERED** — no assertion anywhere checks it.
- **UNCITED-COVERED** — an assertion exists, but not in any cited evidence
  path (the caveat-2 "omitted conjunct" in its benign form).
- **NOT-MACHINE-CHECKABLE** — inherently a human/prose obligation.

## Headline

The corpus is much healthier than caveat 2 feared: **no criterion's core
claim is false**, and only **14 conjuncts across 9 ADRs are asserted by
nobody**. The dominant defect is not missing tests but **missing citations**:
~113 conjuncts (across 15 ADRs) are precisely covered by tests the criteria
do not cite, because governance requires evidence paths per *ADR*, not per
*criterion*. ADRs 0009, 0010, 0024, 0029, 0031, and 0033 are the worst
offenders (0033: 10 of 11 criteria cite nothing).

## Tier 1 — genuinely uncovered conjuncts (14)

Nothing in the repo fails if these become false:

1. **0018-C2(e)** — `dcterms:rights` must be an IRI: `sh:nodeKind sh:IRI`
   exists in `schemas/manifest.shacl.ttl` but no test adds a literal-valued
   rights statement and asserts rejection. *(verified by grep)*
2. **0020-C2(f)** — canonical participant/edge order is part of the hashed
   drift-event value: `drift-event-id-omits-only-id-field-test` checks
   general content-sensitivity but never reordering. *(verified)*
3. **0032-C2(d,f) + vacuous (e)** — the detach *repository* is asserted only
   in the handoff, never in the ADR text; no assertion requires lifted-crate
   provenance headers to *exist* (the doseq is guarded by
   `:when (str/includes? text "Forked from")` — deleting every header passes)
   or to carry the detach *revision*. *(verified at
   `test/abc/tools/parser_relations_provenance_evidence_test.clj:55`)*
4. **0006-C1(b), 0007-C4(b), 0008-C2(a), 0010-C5(b)** — every "the supported
   `nix run .#validate-design-bundle` application exits zero" conjunct: no
   test invokes the app or asserts an exit code; enforcement is CI-only
   (wiring pinned by `validation-workflow-wiring-test`, result asserted by
   nobody). One systemic gap, stated four times.
5. **0008-C4(a,b)** — `evidence-input-paths` is a hand-maintained list;
   nothing reconciles it against the helpers' actual read set (the only test
   asserts membership of 3 paths, not equality).
6. **0010-C3(d)** — nothing asserts the materializer's identity fields are
   produced by the canonical JCS serializer the criterion describes; the
   linkage is source-level only.
7. **0038-C2(b)** — "Release authority: development" for ADR 0038: governance
   validates the vocabulary, but no assertion pins this ADR's specific value.
8. **0042-C2(d)** — rejection of an *extra/undeclared* member beyond the
   closed evidence manifest is asserted nowhere (duplicate rejection exists
   in the uncited ab-validator Python tests; extra-member does not).
9. **0042-C3(c)** — promotion *requiring* the evidence-integrity receipt: the
   campaign test always writes a valid receipt and never deletes it, so
   promotion could stop requiring it without failing anything.

**Distinct semantic staleness (verified):**
`fixtures/canonicalization/manifest-identity-object.canonical.json` (ADR
0001-C2) carries the historical 13-key identity set — it lacks
`tokenizer_profile_hash` and `annotation_policy_hash`, which the current
manifest schema's identity object requires. The null-dimension canonical
pinning therefore does not cover the current field inventory.

## Tier 2 — uncited coverage (~113 conjuncts, the systemic finding)

Tests exist and would fail, but the criteria cite nothing (or the wrong
file). Root cause: `abc.tools.adr` enforces evidence-path existence, and at
least one path per Accepted ADR is guaranteed by the section rules — but a
criterion with *zero* backticked paths raises no problem. Affected worst:
0001 (C2–C5), 0002 (C1–C3), 0006 (C3, C6–C9), 0007 (C1–C3), 0009 (C2–C7),
0010 (C2–C5), 0013 (C2), 0023 (C1–C4), 0024 (C1–C3, C5), 0029 (C1–C4), 0030
(C1–C2), 0031 (C1–C3), 0032 (C1), 0033 (C2–C11), 0038 (C2), 0039 (C4), 0040
(C3), 0041 (C3), 0042 (C2 partially).

Notable systematic sub-pattern: for ADRs 0015/0016/0020, all "death-side" /
second-field assertions live only in the uncited
`test/abc/tools/temporal_evidence_input_test.clj` (e.g. the
participant-required-fields dissoc sweep at line 701 that closes 0021-C2(a),
gYearMonth EDTF echo, SHACL death-property cardinality). Citing that one
file from those ADRs would convert ~10 findings to COVERED.

Genuine **miscitations** (cited file asserts none of the criterion's
conjuncts):

- **0031-C4** cites `test/abc/tools/adr_test.clj`; the workflow-fixture
  rejection tests live in `test/abc/tools/diagram/workflow_graph_test.clj`.
- **0012-C1** cites `test/abc/tools/tei_test.clj`; the mechanism is the
  `tei-profile-drift` flake check (and `nix/tei-profile-artifacts.nix`).
- **0041-C3** cites `parser_rq_admission_promotion_drift_test.clj`, whose
  mutation tests compare inert Clojure maps (exercise zero campaign code);
  the real rejections live in `parser_rq_campaign_test.clj`.
- **0033-C9**'s only executable evidence (the `source-bundle-corpus` flake
  check) lives in `flake.nix`, outside the four citable evidence roots.

## Tier 3 — notable PARTIAL coverage (~36 conjuncts; strongest first)

- The only test executing full `validate-design-bundle!`
  (`design-bundle-does-not-run-repository-history-checks-test`) stubs
  `validate-publication-output!`, `validate-xml!`, `validate-tei!`, and
  `validate-tei-schematron!` — so "the bundle exercises the publication/TEI
  gates" (0006-C1, 0024-C6, 0025-C1/C6) is asserted only per-step, never
  through the orchestration. Deleting a step from the orchestration breaks
  no cited assertion (same weakness for materialization: 0009-C6, 0011-C1).
- **0024-C4** — ruby direction `"left"` is asserted nowhere (only `"right"`),
  and no TEI fixture carries a `rend` direction, so profile-validity of the
  emitted value is unasserted.
- **0042-C1** — the no-machine-identity/topology/site/replication invariant
  is a closed denylist of 11 tokens; same concept under another name passes.
- **0014-C1(b)** — "validates against the 2020-12 meta-schema" is near-vacuous:
  the meta-schema reference used imposes no assertions.
- **0039-C2(c)** — "thresholds fixed before measurement" is temporal history;
  identity-rotation coherence approximates it, ordering itself unprovable.
- **0040-C1(a,d)** — predicate-8 measurement *mechanism* (`memory.peak`
  transient service) is structure-checked only; `wrapper_identity_hash` is
  regex-shaped, never recomputed from wrapper content.
- **0013-C3(e)** — external-context fetch refusal asserts only
  `(thrown? Exception ...)`; any error satisfies it.

## Not machine-checkable (11)

0038-C1(a–e) (the external ownership assessment's content — by design, D(a)
immutable historical evidence), 0040-C2(c) provenance-of-run (self-reported
host metadata), 0042-C3(d) cross-ADR non-change claim, plus scope
disclaimers in 0007-C2(c), 0015-C2(j)/C6(e), 0021-C8(b). All acceptable.

## Fixed during this review (committed)

The sweep found one regression from today's apparatus deletion: the prose
"Evidence" sections of ADRs 0039–0042 still cited five
`docs/evidence/adr-runs/*.json` files deleted with the typed-evidence
apparatus (ungoverned because path-checking covers Acceptance Criteria
only). Fixed by:

- re-pointing all four ADRs' Evidence prose at the living Kaocha tests those
  runs executed;
- porting the deleted acceptance authenticator into an ordinary test —
  `hinoki-resource-witness-binds-the-campaign-capture` in
  `test/abc/tools/parser_rq_resource_test.clj` now authenticates the Hinoki
  smoke witness against the committed nine-envelope P5 campaign capture and
  the qualification report (closing the 0040-C2 "witness has no consumer"
  gap);
- deleting the four now-orphaned apparatus capture directories
  (`docs/evidence/parser-rq-{instrument-bindings,portable-integrity,resource,resource-acceptance}/`),
  which had zero remaining consumers.

## Follow-ups (executed 2026-07-23, same day)

All ranked follow-ups were performed in four single-axis commits:

1. **Done — citation backfill** (`docs(adr): cite evidence per criterion
   across the Accepted corpus`): all 72 uncited criteria across 21 ADRs now
   cite their actual covering test files; the four miscitations (0012-C1,
   0031-C4, 0040-C2, 0041-C3) corrected. 0033-C9 is now citable via a new
   corpus-maxima pin test in `test/abc/tools/source_bundle_test.clj`.
2. **Done — machine check** (`feat(adr): require an evidence path per
   Accepted criterion`): `abc.tools.adr` now rejects any Accepted criterion
   citing nothing (`:missing-criterion-evidence`), turning this review
   class into standing governance.
3. **Done — canonicalization fixture** (`fix(canonicalization): pin the
   current identity inventory`): fixture regenerated to the 15-key set,
   both digests repinned (the README digest had already drifted), and a new
   test ties the fixture's keys to the schema's identityObject required
   list so it cannot silently go stale again.
4. **Done — Tier-1 tests** (`test(evidence): close uncovered conjuncts from
   conjunct review`): literal-rights rejection + InC sole-value acceptance;
   drift-event-id order sensitivity; crate provenance headers required
   (guard dropped; repository/revision/licence asserted per crate; ADR 0032
   pinned to name `P4suta/aozora` — the full URL genuinely appears only in
   the handoff); extra-member rejection both directions; receipt-required
   promotion; orchestration asserted to invoke all four publication/TEI
   gates; ruby "left" rendering; ADR 0038 release-authority pin.
5. **Done — mirror-layer retirement** (same day, follow-on audit wave):
   the apparatus-era evidence-mirror test files this review kept citing as
   "uncited coverage" were themselves audited clause-by-clause and
   dissolved into the domain tests: `temporal_evidence_input_test.clj`
   (967 lines; 64/79 clauses duplicate, 14 unique predicates migrated),
   `schema_validation_evidence_test.clj` (six-domain grab-bag; 7 moves,
   12 duplicate deletions), `parser_publication_evidence_test.clj`
   (whole-file duplicate), four diagram/policy mirrors and the inert
   admission-promotion drift test (several confirmed-vacuous clauses).
   All ADR citations re-pointed; file paths named elsewhere in this
   report reflect the pre-consolidation tree.
6. **Decision — app exit-code claims** (0006/0007/0008/0010): kept the
   claims and cited the CI wiring test (`validation-workflow-wiring-test`)
   as best-available evidence rather than rewording Accepted criteria or
   adding a slow subprocess Nix test. The in-process orchestration is now
   asserted to invoke every gate; the process-level exit code remains
   enforced operationally by CI. This is the one residual documented gap.
