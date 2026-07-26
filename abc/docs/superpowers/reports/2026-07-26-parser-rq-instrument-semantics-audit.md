# Parser-RQ Instrument Semantics, Identity, and Authority

Date: 2026-07-26 (revision 3)
Classification: **protocol design** — instrument semantics, identity, and
authority. Revision 1 filed this as simplification; that was the wrong frame
and produced two unsound blockers. Revision 2 corrected the findings but
proposed a sequence that was not implementable; revision 3 corrects the
sequence.
Mode: **audit only** — no source, policy, or governance file was modified.

Companion to `2026-07-24-system-simplification-audit.md`. Label note: the
deferred list in
`docs/superpowers/plans/2026-07-25-publication-slug-collision-integrity.md`
uses F-numbers from a different audit than that report's F1–F6. This report
uses letters to avoid a third colliding scheme.

## Claims withdrawn from revision 1

Recorded so the earlier draft's conclusions do not propagate.

1. **"The declared semantics fields are read by no code; nothing links the
   policy to the implementation."** False. `predicate-hardening-identity.py`
   *generates* those fields, and `validator_semantics_hash` is computed over a
   reviewed source closure that includes the implementation itself
   (`ab-validator/reports/parser-ir/predicate-hardening-identity.py:150-191`).
   The linkage exists and is stronger than a runtime interpreter would be. It
   is, however, currently broken — see finding A.
2. **"The vacuous pass is a defect."** It is governed, declared, and
   deliberately tested behaviour. See finding C.
3. **"`predicate_set_hash` does not rotate, and requiring ≥1 diagnostic is an
   optional independent change."** Self-contradictory: mapping empty to failure
   *is* the requirement. Withdrawn entirely.
4. **"No check compares any policy's membership to the governed corpus."**
   False for three of five instruments. See finding D.
5. **"The resource checks are tautologies that could be deleted."**
   Reclassified — see finding E.
6. **"Step 1 makes the old gate visibly close."** False: evaluation consumes
   the stored measurement bundle (`parser_rq_campaign.clj:869-887`), so
   changing instrument code does not re-decide a published capture.

Withdrawn from revision 2:

7. **"The test that would have caught both"; "expect A1 and A2 to go red."**
   The identity suite contains no committed-artifact comparison, so it can
   surface A1 only. See finding A3.
8. **"Step 1 touches no governed value and rotates no identity."** Impossible:
   both repairs move `validator_semantics_hash` and `policy_hash`.
9. **"The `jsonschema` crate version is already declared separately."**
   Declared, but as a hardcoded literal — not derived, not verified. See A2.
10. **"Five instrument policies carry membership."** Four do;
    `parser-rq-publication-policy-v1.json` carries none.

## What must not change

- Candidate identity derivation and the published run artifacts under
  `docs/reports/parser-rq/runs/`.
- The accepted P5 verdict as a **historical** fact about the implementation and
  corpus it measured.
- Each instrument's existing fail-closed behaviour.
- The production graph and its `policy_hash`.

---

## Finding A — the semantic-closure identity mechanism is broken

**Severity: blocker.** This is the highest-priority item in the system and it
was invisible to every gate.

The mechanism is sound in design. `predicate-hardening-identity.py` builds a
manifest of the reviewed source closure plus declared artifacts, hashes it into
`validator_semantics_hash`, and folds that into `policy_hash`. A change to the
implementation therefore rotates the instrument's policy identity. Three
independent faults currently defeat it.

### A1. The diagnostic-completeness reviewed closure names a deleted file

Running the generator against the working tree fails:

```
ValueError: reviewed semantic closure differs: unreviewed=[], stale=[
  'abc/src/abc/annotation/schema.clj',
  'abc/src/abc/tools/evidence_io.clj',
  'abc/src/abc/tools/malli.clj',
  'abc/src/abc/tools/path_containment.clj']
```

Four files are declared reviewed but are no longer reachable in the require
closure from `parser_rq_diagnostic_completeness.clj`. The policy cannot be
regenerated at all until `INSTRUMENTS` is corrected.

Three of the four still exist and are merely out of the closure. The fourth,
`abc/src/abc/tools/evidence_io.clj`, **no longer exists**: it was deleted by
`b042c7cd refactor(adr): delete typed-evidence apparatus, cite ordinary tests
(ADR 0043)`. So the committed policy's `validator_semantics_hash` is taken over
a source set that includes bytes absent from the repository — a governed
simplification silently changed an instrument's semantic identity.

*Repaired 2026-07-26. The closure is the seven sources that survive; the other
three were reachable only through `evidence_io.clj`, so one deletion accounts
for the whole discrepancy. Identity rotated.*

### A2. The parser-IR conformance policy has already drifted

The generator *does* run for this instrument, and produces a different policy
than the one committed:

| Field | Generated today | Committed |
|---|---|---|
| `validator_semantics_hash` | `sha256:d66b1a13…` | `sha256:28ffc4e4…` |
| `policy_hash` | `sha256:7868a31d…` | `sha256:7b2af979…` |

**The cause is not a semantic change.** `git diff 2acfcce4..HEAD --
ab-validator/crates/ab-aat-to-parser-ir/src/` is empty: no reviewed Rust source
has changed since 2026-07-17, and the policy was written 2026-07-19. The drift
comes from the manifest's `artifacts` list, which binds
`ab-validator/Cargo.lock` wholesale. That lockfile has changed three times
since — most recently `ba9837bc`, a crate *extraction* refactor that altered no
instrument semantics.

The diff is precisely that: it adds the workspace package `ab-aozora-capture`
and dependency edges. No external dependency version changed.

So `validator_semantics_hash` is **over-scoped**: it is bound to a build
artifact rather than to semantics, and rotates on workspace-membership changes
that cannot affect the converter. This is why regeneration cannot simply be
made load-bearing today — an equality check would go red on every lockfile
edit.

**The replacement is not obvious, and must be decided before it is coded.**
Revision 2 suggested leaning on the manifest's declared `jsonschema_crate`
version. That was wrong: `"jsonschema_crate": "0.46.9"` is a **hardcoded string
literal** (`predicate-hardening-identity.py:175`), while the workspace declares
only `jsonschema = "0.46"` (`ab-validator/Cargo.toml:185`). It is a
hand-maintained assertion that can silently disagree with what actually builds
— the same class of fault as finding F. The qualification path also depends on
`serde`, `serde_json`, `sha2`, `regex`, and others capable of changing
observable results.

Dropping the lockfile without first defining the relevant dependency closure
would trade false-positive rotation for **undetected semantic drift**, which is
the worse failure. What is needed is a package-specific projection — the
transitive locked closure of `ab-aat-to-parser-ir`, with resolved versions and
enabled features — tested in both directions: an unrelated workspace package
must not rotate it, and a relevant version or feature change must.

*Repaired 2026-07-26, with one correction to the paragraph above: **features
cannot be part of it.** `Cargo.lock` records resolved packages, not feature
selection, so a feature change that alters behaviour without changing the
resolved set rotates nothing — under the old whole-lockfile binding as much as
under the new projection. That is now a stated gap in ADR
`package-scoped-instrument-dependency-identity`, alongside the larger one:
twenty-two workspace-local crates sit in this closure carrying no checksum, so
their bytes were never covered either. Whole-lockfile hashing bought the
appearance of dependency coverage without the substance.*

*Confirmed while deciding: `ab-aat-to-parser-ir`'s own lock entry is
byte-identical since `2acfcce4`, no resolved version changed anywhere in the
file, and `ab-aozora-capture` is absent from the package's 480-entry transitive
closure — so the projection provably would not have rotated on the change that
caused this drift, while still covering `jsonschema 0.46.9`.*

### A3. The identity suite has been erroring, not running — and would not have caught A2 anyway

`ab-validator#checks.x86_64-linux.parser-rq-publication-pytest` **fails today**:

```
24 passed, 6 errors in 0.19s
ERROR reports/parser-ir/tests/test_predicate_hardening_identity.py::… -
  FileNotFoundError: '/build/tmp.…/ab-validator/reports/parser-ir/predicate-hardening-identity.py'
```

All six items error at import. `REPO_ROOT` is computed as `parents[4]`
(`test_predicate_hardening_identity.py:11`), which resolves above the subtree
the check copies, so the module under test is not found.

**Fixing `REPO_ROOT` alone is insufficient.** `ab-validator/flake.nix:125`
stages `source = cleanProjectSource ./.` — the `ab-validator` subtree only —
while the generator requires a root containing **both** `ab-validator/` and
`abc/` (`INSTRUMENTS` reviewed sources span both, and `_json_logical_hash`
reads `abc/schemas/…`). The check must stage both trees, or the tests must take
the two roots explicitly.

**And the suite would not have caught A2.** Its five test functions cover
closure discovery, mutation sensitivity, and self-consistency of *generated*
values (`test_predicate_hardening_identity.py:25,31,45,73,92`). **None compares
a regenerated manifest or policy against the committed artifact.** So repairing
the staging surfaces A1 — via
`test_reviewed_sources_equal_discovered_owned_closure[diagnostic-completeness]`
— and leaves the committed parser-IR drift of A2 exactly as invisible as it is
now. Revision 2's "the test that would have caught both" and "expect A1 and A2
to go red" were both wrong.

Detecting A2 needs a **new, distinct** committed-regeneration equality test.
That test cannot be added green until the scope question in A2 is settled and
the affected governed identities are regenerated — see the sequence below.

The root flake does not aggregate `ab-validator`'s checks, so this check is
reached only by running `nix flake check ./ab-validator` directly.

---

## Finding B — qualification identity binds instrument *descriptions*

**Severity: blocker for any semantics change.**

`parser_rq_campaign.clj:203-206` derives the candidate's `instrument_versions`
from the predicate set's human-readable `:instrument` strings:

```clojure
:instrument_versions
(into (sorted-map) (map (juxt (comp name :observed_key) :instrument))
      (:predicates predicates))
```

For diagnostic-completeness that string is
`"ab-aozora --mode diagnostics envelope entries {code,severity,source,span}"`
(`data/parser-release-qualification-predicates.edn:38`). It is not
`policy_hash`, not `validator_semantics_hash`, and not `algorithm_version`.

**Consequence.** A change to instrument semantics rotates `policy_hash`, but
`qualification_identity_ref` — which is what `candidate_ref` and the whole
admission/promotion chain bind — does **not** rotate unless the descriptive
string is also edited. The coordinate intended to carry instrument identity
carries prose instead.

**Editing the `:instrument` string is not the only remedy, and is the worst
one.** It rotates `predicate_set_hash` as a side effect, braiding predicate
identity with instrument identity — the two are different things:

| Layer | Question it answers | Coordinate today |
|---|---|---|
| Predicate identity | what must be true | `predicate_set_hash` |
| Instrument identity | how it was measured | *none* — prose in `instrument_versions` |
| Executable provenance | which binary ran | `provenance_core_ref`, per-executable `sha256` |

The middle row is the hole. A separate machine coordinate — say
`instrument_policy_hashes`, binding each instrument's governed `policy_hash` —
would fill it while leaving predicate prose and `predicate_set_hash` alone.
Adding it changes `qualification_identity_ref` and therefore `candidate_ref`,
which is correct and intended; it does **not** need to rotate the predicate
contract.

Any decision under finding C must state which coordinate carries the change.

---

## Finding C — vacuity is a governed decision, not a defect

**Severity: open decision. Not a defect.**

The current behaviour is declared, generated, and deliberately tested:

- `data/parser-rq-diagnostic-completeness-policy-v1.json:26` —
  `vacuity_semantics: "valid_empty_passes_with_disclosure"`, emitted by the
  identity generator (`predicate-hardening-identity.py:217`).
- `test/abc/tools/parser_rq_diagnostic_completeness_test.clj:58` —
  `valid-empty-envelope-is-an-explicit-vacuous-pass`, asserting
  `(:value observation)` is `1.0` and details are
  `{:diagnostic_count 0 :works_with_diagnostics 0 :vacuous true}`.
- The predicate reads "Every emitted diagnostic carries a stable code,
  severity, and span when available"
  (`data/parser-release-qualification-predicates.edn:35-41`) — a universally
  quantified statement, vacuously true over an empty set.

Revision 1 argued from the parser-IR sibling's
`no_output_semantics: "available_failure_when_generated_outputs_zero"`. That
analogy is not decisive: absent parser-IR output means an **expected product is
missing**, whereas absent diagnostics may legitimately mean **nothing required
diagnosis**. The two empty cases are not the same kind of empty.

**The actual question.** Must a release-qualified parser have emitted at least
one diagnostic over the qualification corpus? That is a policy question about
what the release gate is for, and it is the reader's to answer.

If **yes**, it is a new qualification requirement and must be represented as
one — in the predicate contract (a changed dimension, rotating
`predicate_set_hash`) or in a newly versioned instrument contract (a bumped
`algorithm_version`, with finding B's binding resolved). It must not be
smuggled in as an instrument status while the predicate text still reads
"Every emitted diagnostic…". Either route requires identity rotation and
requalification.

If **no**, the current behaviour is correct and what needs fixing is only the
disclosure: a corpus that provably exercises no diagnostic path is weak
evidence, and that is an argument for corpus expansion, not for a gate change.

---

## Finding D — membership propagation, instrument by instrument

**Severity: strong suggestion**, scoped to one instrument.

Revision 1 claimed no policy membership is ever compared against the governed
corpus. That is false. The correct picture:

| Instrument | What drives measurement | Index `expected_work_ids` origin | Comparison point | Divergence behaviour |
|---|---|---|---|---|
| diagnostic-completeness | corpus `entries` (`parser-rq-predicate-hardening-capture.py:70`) | corpus-derived (`:142`) | `parser_rq_diagnostic_completeness.clj:165` | **genuine cross-authority**; `unavailable / index_incomplete` |
| parser-IR conformance | same capture, same corpus rows | corpus-derived | `parser_rq_parser_ir_conformance.clj:181` | **genuine cross-authority**; `unavailable` |
| publication | corpus `entries` | — | `publication-rq-capture.py:55-63`, "works do not have exact pinned corpus membership" | **genuine cross-authority**; explicit error |
| core-attempt | corpus-derived ab-check index and work-ids file (`parser_rq_campaign_orchestrator.py:684-687`) | policy `expected_sources` (`parser-rq-core-attempt-capture.py:416`) | `parser_rq_core_attempt.clj:105` compares policy to a policy-derived index | indirect: a policy work absent from the corpus yields no report, and `_closed_reports` raises "report output is missing" (`:248-250`) |
| **resource** | policy `work_ids` (`parser-rq-resource-capture.py:39`) | policy-derived (`:49`) | `parser_rq_resource.clj:21` compares policy to itself | **none** — a stale policy silently measures its own subset |

Revision 1 also cited `parser_rq_predicate_hardening.clj:82-83` as a
cross-check. `install-observations` has **no production caller** — a repository
search finds only tests.

**The gap is the resource instrument alone.** It is the one path where corpus
membership never reaches the measurement, so a corpus that grows while the
resource policy does not would qualify a memory ceiling over the old subset
without any diagnostic. Core-attempt fails closed, but by accident of report
completeness rather than by an explicit membership comparison; making it
explicit is cheap and worth doing in the same change.

---

## Finding E — the resource checks are coherence checks, not corpus authority

**Severity: strong suggestion. Do not delete them.**

`parser_rq_resource.clj:21` and `:24` compare the policy against values the
capture copied or stamped from that same policy
(`parser-rq-resource-capture.py:39,49,132`), reading the same file at
`parser_rq_campaign_orchestrator.py:504` and `:938`. They therefore cannot
establish that the measured membership is the *corpus's* membership.

They are not worthless. They are **self-originated provenance and coherence
checks**: they still detect an index or record set that has been corrupted,
truncated, or mixed between capture, persistence, and projection time — the
three moments those values must survive.

Correct classification: sufficient for transport coherence, insufficient for
corpus authentication. Keep them; add the independent corpus comparison from
finding D alongside.

---

## Finding F — core-attempt declares three hashes it never reads

**Severity: follow-up.**

`schemas/parser-rq-core-attempt-policy.schema.json:7` requires
`expected_work_set_hash`, `capture_semantics_hash`, and
`analyzer_semantics_hash`. `parser_rq_core_attempt.clj` reads none of them, and
no repository search finds a reader for the latter two anywhere.

Unlike its two siblings, the core-attempt policy has **no generator** —
`INSTRUMENTS` in `predicate-hardening-identity.py` covers only
diagnostic-completeness and parser-IR conformance. So its three semantic hashes
are hand-maintained assertions bound to nothing, in the instrument that
measures fatal failures, wall time, and timeouts.

Bringing it under the same generator is the coherent repair, but only after
finding A is fixed — otherwise it inherits a broken mechanism.

---

## Finding G — per-operation knowledge scattered by lifecycle phase

**Severity: follow-up — do not act now.**

Each campaign operation's knowledge is spread across three dispatch chains in
`parser_rq_campaign_orchestrator.py` (argv `:386-496`, projection inputs
`:839-928`, execution `:1167-1177`) plus its `_prepare_*` function,
`EXPECTED_GRAPH`, and `PUBLICATION_ARTIFACTS`. Changing one instrument means
four or five edits in a 1,321-line module. Real, but this is the production
capture path and the payoff does not justify the risk mid-migration.

---

## Revised sequence

Revisions 1 and 2 are both withdrawn. Revision 2's step 1 claimed to repair the
identity mechanism while rotating no identity; those cannot both hold, because
removing four stale reviewed sources changes the diagnostic manifest hash and
changing the artifact scope changes the parser-IR manifest hash, and both flow
into `validator_semantics_hash` and `policy_hash`. A regeneration equality gate
cannot be green against the *committed* policies without regenerating them. As
written, a structural test repair would have silently become an
identity-semantics migration — the exact mixing this work is meant to avoid.

The repair therefore splits into four steps with a decision in the middle.

**Step 1a — repair test staging and characterize (no identity change).**
**Executed 2026-07-26.**

`REPO_ROOT` needed no change: `parents[4]` is correct for a checkout, and the
derivation was staging wrongly. `parser-rq-publication-pytest` now stages
`${source}` as `repo/ab-validator` and `${abcSource}` as `repo/abc`, which is
what the reviewed closure spanning both trees requires. The suite went from
**24 passed, 6 errors** to **27 passed, 5 xfailed**.

Three tests failed on execution, all rooted in A1, and one sharpened it:
`abc/src/abc/tools/evidence_io.clj` is not merely outside the require closure —
it was **deleted** by `b042c7cd refactor(adr): delete typed-evidence apparatus`
(ADR 0043). The committed diagnostic-completeness policy's
`validator_semantics_hash` therefore covers bytes that no longer exist in the
repository. A governed simplification changed an instrument's semantic identity
and nothing noticed, because this suite was erroring at import.

A1 and A2 are recorded as `xfail(strict=True)` rather than left red, so the
gate is green while both faults stay explicit and machine-tracked; step 1c
removes the markers. Strictness was verified, not assumed: a deliberately
passing test carrying the marker fails with `[XPASS(strict)]`, so a forgotten
marker cannot outlive its fault.

The missing test now exists —
`test_committed_policy_equals_its_regenerated_form`, marked `COMMITTED_DRIFT`.
It reads work ids from the committed policy on purpose, isolating semantic
identity from corpus membership (finding D's separate authority). Un-marking it
is step 1d.

The check had been built by **no recipe at all**: `check-no-build` only
evaluates, as its name says. A `parser-rq-instrument-identity` recipe now
builds it inside `validate-migration`, following the `phase5-checkpoint`
precedent; without that the repair would have been inert. The eval-cache smoke's
pinned counts moved from five builds / three evaluations to six / four.

**Step 1b — decide the dependency-identity boundary.** Open question 3, moved
ahead of any scope edit. What belongs in an instrument's semantic identity: the
reviewed sources, plus which projection of the dependency graph? This is a
protocol decision and it determines whether the committed policies are stale or
correctly frozen.

*Decided 2026-07-26: a package-scoped locked projection, with the
workspace-local-source and Cargo-feature gaps named and deferred rather than
closed. Recorded as ADR `package-scoped-instrument-dependency-identity`.*

**Step 1c — change the generator and regenerate atomically.** Correct the
reviewed closure (A1) and the artifact scope per 1b (A2) in one commit with the
regenerated manifests and policies, since every one of those edits moves the
same hashes. This *is* an identity rotation and must be governed as one.

**Step 1d — add the equality gate.** A new committed-regeneration test,
asserting the committed policies equal the generator's output, against the
artifacts 1c produced. Green from the first commit, red forever after on
undeclared drift.

### Execution record — steps 1b through 1d (2026-07-26)

1c and 1d could not be separate commits. Both faults were held open by strict
xfail markers, so the commit that repairs either one must remove its marker in
the same change or the suite fails on `XPASS(strict)`. The rotation is atomic by
construction; splitting it would have left an intermediate commit with a failing
gate.

Three things the plan did not anticipate:

- **The manifests of record had drifted too, and nothing checked them either.**
  `ab-validator/data/parser-rq-*-validator-v1.json` are the generator's `--out`
  artifacts. No active code reads them and no test compared them. They are
  regenerated, and `test_committed_manifest_equals_its_regenerated_form` now
  holds them to the same standard as the policies.
- **The predicate-hardening capture fixture had to be regenerated.** It embeds
  `policy_hash` in content-addressed records, so rotating the policies
  re-addressed ten blobs and the manifest. The sanctioned path already existed:
  `ab-validator/tests/parser-rq-predicate-hardening-capture-smoke.sh` takes a
  write target and generates twice, diffing for reproducibility. Verified before
  writing that every blob is byte-identical once `policy_hash` is projected out,
  and that the two index blobs differ solely in the refs their re-addressed
  records produced — so no measured value moved.
- **Regenerated fixture blobs are invisible to the flake until staged.** The
  `abc` source derivation is git-backed, so `clj-nix-focused-tests` failed on the
  old fixture while the same tests passed locally. Same class as the
  missing-narrative failure in the previous tranche.

`jsonschema_crate` is now read from the projection rather than restated; a crate
absent from the closure fails closed. The manifest schema is
`parser-rq-predicate-validator-identity-v2`, and both instruments rotate
together — `diagnostic-completeness` for A1, `parser-ir-conformance` for the
scope change.

Suite: 39 passed, no xfails remaining. The five new projection tests check both
directions — an unrelated sibling package is ignored, a transitive version move
and a newly acquired dependency both rotate, local members are marked uncovered,
and the schema crate is read rather than restated.

**Step 2 — decide finding C explicitly**, and with it finding B's coordinate
question. A reader decision, recorded as a decision record before code moves.

*Decided 2026-07-26 and recorded as the `:proposed` ADR
`governed-diagnostic-expectation-and-instrument-coordinate`, which becomes
accepted when step 3 supplies its evidence. Neither answer was the binary the
finding offered; see the execution record below.*

### Execution record — step 2 (2026-07-26)

Three facts moved the decision off the yes/no axis finding C posed.

- **The accepted verdict passed this predicate over zero evidence.**
  `measurements.json` records `diagnostic_completeness 1.0`,
  `diagnostic_count 0`, `works_with_diagnostics 0`, `vacuous true`. One of nine
  release predicates contributed nothing.
- **The corpus governs that, and nothing enforces it.** All three entries
  declare `:expected_diagnostics []`, so the clean result matches intent. But
  that field is read by no capture, instrument, or test, and is absent from
  `corpus-entry-identity-keys` (`[:work_id :source_path :source_sha256
  :category :expected_status]`). It changes no behaviour and rotates no
  identity — decoration, the same class as finding F.
- **This report described the instrument wrongly, and so does the predicate.**
  Finding C calls it a ratio and the contract declares `:unit "ratio"`.
  `diagnostic_completeness` is the literal `1.0` at
  `parser_rq_diagnostic_completeness.clj:185`, and `complete_diagnostics` is
  assigned equal to `emitted_diagnostics` unconditionally at `:130` — the two
  cannot diverge. The predicate's real content is that the envelope validates
  against the v3 schema, which requires code, severity, source, and span. It
  does discriminate, against a malformed envelope; it had no diagnostics to
  discriminate over.

So the decision is neither branch. The global floor is rejected — satisfiable
by any single diagnostic anywhere, silent about which works produced which, and
it would fail a corpus governed to be clean. Instead the **already-governed
per-work expectation becomes the authority**: observed diagnostics are compared
against `:expected_diagnostics`, which joins the corpus entry identity keys.
The check stops being vacuous on its own as the corpus grows, with no second
gate redesign and no predicate dimension rewritten to restate what the corpus
already says.

Finding B is answered as the report recommended: a new
`instrument_policy_hashes` coordinate, not an edit to the `:instrument` prose.
One mechanical fact the finding did not record — the candidate schema declares
`identity` with `additionalProperties: false` and a closed `required` list, so
this is a schema change plus an honest `schema_version` bump off `1.0.0`, not
only a code edit.

**Step 3a — add cross-authority membership checks on the current corpus.**
Resource corpus authentication and core-attempt's explicit membership
comparison (finding D) change failure behaviour, so they are characterized
against the present three-work corpus first, where the expected result is "no
change in outcome."

**Step 3b — rotate membership atomically.** Corpus artifact, the **four**
policies that carry membership (core-attempt, diagnostic-completeness,
parser-IR conformance, resource), the publication fixtures, and the
capture-index fixture, in one commit as data and generated-artifact work. Note
that `parser-rq-publication-policy-v1.json` carries no membership at all —
publication membership lives in the fixtures and capture input — so revision
2's "five instrument policies" was wrong. Regenerate the two generated policies
rather than hand-editing them, which step 1 has made possible.

**Step 4 — capture, evaluate, requalify.** A new governed capture is the only
thing that can demonstrate any changed behaviour, since evaluation replays
stored measurements.

**Step 5 — governance.** Amend `custom-parser-release-qualification.md` only
once a new capture exists. Preserve the existing verdict as historical truth
about the implementation and corpus it measured; append a superseding note
rather than retroactively redefining a governed result. If the vacuity should
be disclosed before then, the honest instrument is a note citing the existing
empty-envelope test as characterization evidence — not an amendment claiming a
failure no capture has produced.

### Not scheduled

- Deleting restated membership as duplication — it is closed-membership
  authentication.
- Deleting the resource coherence checks (finding E).
- Restructuring the orchestrator's dispatch (finding G).
- Any edit to the production graph or its `policy_hash`.

## Open questions for the reader

All three are answered. Retained with their answers so the reasoning that
settled them is not re-litigated.

1. ~~**Finding A2 — the dependency-identity boundary (step 1b).**~~ **Answered
   2026-07-26.** A package-scoped locked projection over the transitive
   `Cargo.lock` closure of `ab-aat-to-parser-ir`, as name, version, and
   checksum. Features are not in it: `Cargo.lock` records no feature selection,
   so the revision-2 phrasing "with versions and features" was not implementable
   as stated, and that gap is now named in the ADR rather than assumed away.

   The governance fact it also decided: the committed policies were **stale**,
   not correctly frozen. They read as live capture inputs
   (`tools/parser_rq_campaign_orchestrator.py:861,865`), so a policy that no
   longer matches the code the next capture runs is a false claim, not a
   historical record. The accepted verdict's own capture agreed with them
   exactly before this rotation — nothing had drifted silently — so
   requalification is owed for the rotation itself, and the published capture
   stays accepted as of the inputs it names.

2. ~~**Finding C.**~~ **Answered 2026-07-26.** Neither branch. A global
   diagnostic floor is rejected as blunt and as failing a corpus governed to be
   clean; keeping the vacuity untouched is rejected because it leaves
   `:expected_diagnostics` dead. The governed per-work expectation becomes the
   authority and joins `corpus-entry-identity-keys`. The question as posed
   assumed the instrument computes a ratio; it does not.

3. ~~**Finding B.**~~ **Answered 2026-07-26: yes.** `instrument_policy_hashes`
   is added, binding each instrument's governed `policy_hash`; the `:instrument`
   prose stays descriptive and `predicate_set_hash` is untouched. One mechanical
   fact this question omitted: `parser-rq-candidate.schema.json` declares
   `identity` with `additionalProperties: false` and a closed `required` list,
   so the coordinate is a schema change plus a `schema_version` bump off
   `1.0.0`, not only a code edit.
