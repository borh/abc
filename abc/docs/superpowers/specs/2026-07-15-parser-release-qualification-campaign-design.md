# Parser Release-Qualification Measurement Campaign Design

Date: 2026-07-15
Status: Proposed design for review
Parent: `2026-07-11-parser-qualification-remediation-design.md`
Depends on: ADR 0002, ADR 0023 (Accepted), ADR 0030, ADR 0038, ADR 0039 (Proposed)
Realizes: the "Deferred — requires its own spec" section of
`docs/superpowers/plans/2026-07-15-parser-study-followups.md`

## Purpose

Turn the deferred, research-scale measurement work behind **ADR 0039 (Custom
Parser Release Qualification)** into a reviewable design. The parser-study
follow-ups closed the report-honesty polish; they explicitly did **not** produce
any new measurement. This spec defines the instruments, evidence, and admission
work that would let ADR 0039's gate render a real verdict for every predicate on
an admitted build — and states precisely, up front, that the campaign's success
is *honest verdicts*, not a guaranteed promotion to Accepted.

This is a design for review, not an implementation plan. Each instrument below
is an independently specifiable subsystem; a task-by-task plan follows only after
this design is accepted (see Deliverables).

## Terminal Objective and the Honesty Constraint (read this first)

The tempting one-line goal — "make ADR 0039 Accepted" — is a trap, and naming it
as the objective would corrupt every downstream decision. ADR 0039 is a
must-pass gate over exact numeric predicates; its own Decision says a predicate
with no instrument is `unavailable`, **never** `pass`, and a `0.969` observation
fails a `1.0` predicate mechanically. Weakening a failing or unavailable
predicate to force a pass is prohibited by the ADR itself.

Therefore the campaign's **deliverable is a real verdict for every predicate
from a committed instrument, plus a resolved admission** — not a green gate. Two
outcomes are both successes of this campaign:

- Every predicate observation is captured and `pass`, admission matches → ADR
  0039 promotes to **Accepted**.
- Some predicate is honestly `fail` (e.g. source-span coverage `0.97 < 1.0`) or
  an axis is genuinely `non_comparable` → ADR 0039 stays **Proposed**, the gate
  reports `not-qualified`, and the blocker is named. This is a correct, valuable
  result: it tells the truth about the parser.

The only *failure* of this campaign is a fabricated or imputed number, a
weakened threshold, or an instrument that reports `pass` without measuring what
the predicate claims. This integrity mandate overrides every other goal here and
is inherited verbatim from the parent design and the follow-ups plan.

## Current State (grounded, 2026-07-15)

### The release-qualification gate

The gate (`abc.tools.parser-release-qualification`, entry `-main` →
`report-from-bundle` → `build-report` → `evaluate`) consumes a **measurement
bundle** `{:report_id :identity :measurements}` and evaluates **nine**
predeclared predicates (`abc/data/parser-release-qualification-predicates.edn`)
over the pinned corpus (`abc/data/parser-release-qualification-corpus.edn`).
Verdicts are exactly `:pass | :fail | :unavailable`; an observation of `nil`,
`:unavailable`, or `:instrument-missing` yields `:unavailable` **by
construction** (`unavailable-observations`, non-imputation), so no missing
instrument can masquerade as a pass. The captured report
(`abc/docs/reports/parser-release-qualification-report.json`) currently tallies
**5 pass, 4 unavailable**, so `gate_status = not-qualified` and
`adr_0039_status = Proposed`.

| # | predicate_id | observed_key | expected | current observed | verdict |
|---|---|---|---|---|---|
| 1 | `:fatal-failures` | `:fatal_failures` | `<= 0` | `0` | pass |
| 2 | `:source-span-coverage` | `:source_span_coverage` | `= 1.0` | `:instrument-missing` | **unavailable** |
| 3 | `:silent-drops` | `:silent_drops` | `<= 0` | `:instrument-missing` | **unavailable** |
| 4 | `:diagnostic-completeness` | `:diagnostic_completeness` | `= 1.0` | `1.0` | pass (weak — see below) |
| 5 | `:parser-ir-schema-validation` | `:parser_ir_schema_validation` | `= 1.0` | `1.0` | pass (weak — see below) |
| 6 | `:publication-structure` | `:publication_structure` | `= 1.0` | `:instrument-missing` | **unavailable** |
| 7 | `:wall-time` | `:wall_time_seconds` | `<= 300` | `0.06` | pass |
| 8 | `:memory` | `:peak_rss_bytes` | `<= 2147483648` | `:instrument-missing` | **unavailable** |
| 9 | `:timeout-policy` | `:timeouts` | `<= 0` | `0` | pass |

The four `unavailable` predicates (2, 3, 6, 8) are exactly the "four missing
release-qualification instruments." Each predicate's own `:instrument` string
names the blocker (no committed per-work span-coverage, silent-drop, publication
bundle, or peak-RSS instrument for `ab-aozora`).

### Two weak passes (honesty debt inside the green rows)

The captured bundle's own `:notes` disclose two passes that are technically
`= 1.0` but do not yet mean what the predicate claims:

- **`:diagnostic-completeness` passes vacuously.** The parser emitted **0
  diagnostics** over the corpus (`:diagnostic_count 0`), so "every diagnostic
  carries code/severity/span" is true over an empty set. A vacuous pass is not a
  measurement of diagnostic quality.
- **`:parser-ir-schema-validation` was measured against the wrong tuple.** It
  validated against the **current** `parser-ir.schema.json`
  (`sha256:43a6a6d8…`) via the **live mapping 0.5.0**, not the admitted 0.4.0
  tuple. It is a real validation, but of the release candidate under current
  schema, not of the admitted build.

### The admission gap (independent of instruments)

The bundle's `:identity` records `:admitted_tuple_matches false`. This is **not**
a missing-registry-row problem — the ADR-0023 compatibility registry
(`abc/data/aat-parser-ir-compatibility.edn`) already contains a row for
`ab-aozora 0.6.0 … git 004deaf548… mapping 0.4.0` (last entry). The gap is
twofold and sharper:

1. **Build divergence.** The release candidate was built at git `90967157…`; the
   admitted row pins git `004deaf…`. Different builds → not the admitted tuple.
2. **Schema drift since admission.** The admitted row pins parser-IR schema hash
   `a1e1b506…`, but the current `parser-ir.schema.json` computes `43a6a6d8…`.
   Converting under the admitted mapping 0.4.0 **fails closed** against the
   current schema. So even rebuilding at `004deaf` would not produce a coherent
   current-schema admission.

Admitting the actual release candidate therefore requires a **fresh full-corpus
conversion audit of the exact current tuple** (current git rev + mapping 0.5.0 +
current schema hash `43a6a6d8…`), producing a new immutable registry generation
— not an edit of the existing row.

### The neutral-comparison study (separate evidence class)

The neutral study (`ab-parser-study-report`, committed
`comparison-result.json`) has **nine axes**; exactly **one** is `measured`
(robustness, corpus parse-completion arm only). The other eight are `failed` /
`missingness: unavailable` for existing parsers, or `non_comparable` for the
owned `ab-aozora` baseline on the two owned-contract axes (diagnostics, spans).
Its generator refuses to impute and emits an explicit blocker caveat per missing
axis. **This study is a different evidence class** (`:neutral-comparison`) that
`abc.tools.parser-evidence` structurally forbids as release evidence — building
out its axes does **not** move ADR 0039. That decomposition is the next section.

## The Two Orthogonal Gates to Accepted (decomplection)

ADR 0039 promotes to Accepted only when **both** of the following hold, and they
are independent — closing one does nothing for the other:

- **Instrument gate:** all nine predicate observations resolve to `pass` from
  committed instruments (fixing predicates 2, 3, 6, 8 and de-weakening 4, 5).
- **Admission gate:** the exact release-candidate tuple is admitted through ADR
  0023, backed by a fresh full-corpus conversion audit — and the gate precondition
  *derives* that equality from the registry (the live gate does not check this
  today; see Coherence).

Bundling these — or bundling either with the neutral-study axes — is the central
entanglement this spec exists to prevent. Even a perfect instrument suite leaves
the gate `not-qualified` while admission is unresolved, and vice versa.

## Scope: Two Independent Tracks

The follow-ups plan's "Deferred" list named four items as one campaign. They are
not one thing. This spec splits them by the ADR and evidence class they serve:

- **Track R — Release qualification (→ ADR 0039).** The instrument gate, the
  weak-pass hardening, the admission gate, and the conditional promotion. This is
  the campaign's spine.
- **Track S — Neutral-comparison study completion (→ the study, NOT ADR 0039).**
  The four richer comparison-axis instruments. **In scope for this campaign**
  (confirmed 2026-07-15), but structurally barred from release qualification and
  **not a prerequisite for ADR 0039**: it runs in parallel on its own schedule
  and its state never gates Track R. It shares the evidence-provenance rigor bar
  and — per the capture-convergence follow-up below — may share one `ab-aozora`
  corpus capture with Track R's owned-parser measurements.

A reviewer who accepts only Track R still unblocks ADR 0039. A reviewer who wants
the neutral study finished adds Track S. Keeping them separable is a hard
requirement of this design.

## Coherence Requirement: One Tuple (a mechanized identity invariant)

Every Track-R observation must describe **one identical build**. The nine
predicate measurements, the two de-weakened passes, and the admission audit must
all be produced from the *same* pinned `ab-aozora` git revision, the *same*
mapping version, and validated against the *same* parser-IR schema hash. The
current bundle already violates this (candidate `90967157` vs admitted `004deaf`;
mapping 0.5.0 vs admitted 0.4.0).

**Name the equivalence relation** (parent program invariant 3: every identifier
states the relation it implements). A qualification run's identity is the tuple:

```
qualification_identity =
  { parser_git_rev, adapter_coordinates,          ; the build
    mapping_id, mapping_version, mapping_hash,     ; the transform
    parser_ir_schema_id, parser_ir_schema_hash,    ; the target contract
    corpus_snapshot_hash, corpus_list_hash,        ; the subject (already pinned)
    predicate_set_hash,                            ; the rules (already pinned)
    instrument_versions }                          ; the observers (new — see below)
```

Two qualification results are the same run iff all fields are equal. The gate
today records some of these (`:baked_git_rev`, schema hashes) but **enforces none
of them**: `gate-status` (`parser_release_qualification.clj:156`) computes
`:release-qualified` purely from the nine predicate verdicts and never consults
`:identity` or the admission registry. So ADR-0039-C5 ("release-qualified only …
for an admitted build") is *aspirational in code* — the runtime gate would
green-light a fully-passing run of an unadmitted build. And each predicate result
(`evaluate-predicate:127`) carries only a bare scalar `:observed`, with no
observation-level identity to bind it to a tuple.

**Blocker (correctness/trust): the coherence requirement is asserted as an
invariant, but the spec has not chosen the data contract or enforcement semantics
that make it real.** Promoting a prose invariant without settling those is the
weakness. This design settles them:

1. **Coherence is a gate-level qualification *precondition*, distinct from the
   nine scientific predicates — not a tenth predicate.** A tenth predicate would
   change the predeclared nine-predicate set and its hash (violating ADR-0039-C2,
   which pins exactly nine dimensions). A CI-only check would not make the runtime
   gate fail closed. A bundle schema constraint can validate *shape* but cannot by
   itself derive *registry equality*. So coherence lives at the gate as a
   precondition evaluated **before** the verdict tally, alongside — not inside —
   the predicate set.
2. **Structured observation envelopes.** The measurement bundle migrates from a
   flat map of scalars to per-observation envelopes: each observation carries its
   value **and** a reference/hash to the `qualification_identity` it was captured
   under. `evaluate-predicate` reads `.value`; the precondition checks every
   envelope's identity reference is equal.
3. **Admission is a *projection* of the identity, not full-identity equality.**
   `qualification_identity` is a superset of the ADR-0023 match key: the registry
   row has no corpus, predicate-set, or instrument-version fields (`match-keys`,
   `aat_parser_ir_compat.clj:11` — nine fields: aat version/adapter/adapter
   version, mapping id/version/hash/schema-hash, parser-IR schema id/hash). So
   "the tuple matches a row" is two *separate* relations, and conflating them
   would demand impossible map equality or silently drop fields:
   - **Observation coherence:** every observation envelope references the same
     complete `qualification_identity` (`coherent-observations?`).
   - **Admission:** the *admission projection* of that identity onto the nine
     `match-keys` fields (`admission-query`) is present in the registry —
     `(compatible? registry (admission-query identity))`, the match-key membership
     predicate, **not** a human-typed `:admitted_tuple_matches`. (R6 separately
     uses the stricter full-evidence `admission-report` to *add* the row.)
4. **`gate-status` requires *both*.** The gate is `:release-qualified` only when
   the coherence precondition holds **and** all nine verdicts are `:pass`. Any
   incoherent bundle is `:not-qualified` regardless of the tally — this is the
   fix for the current code's admission-blind qualification.

This is a **bundle/schema version migration** (flat scalars → identity-bearing
envelopes; a new gate precondition). The migration cost is real but *already
follows from the claimed invariant* — you cannot enforce one-tuple coherence over
a data shape that cannot express per-observation identity. Note this makes the gate
precondition and predicate 5 (schema validation) **complementary, not
overlapping**: the precondition asserts predicate 5's observation was captured
against the *admitted* schema hash; predicate 5 independently asserts the outputs
*validate*. A coherent run can still emit schema-invalid output, so both are
required (see R5).

The campaign must first **pin the release candidate** (an exact git rev + baked
coordinates + mapping version + schema hash), then produce admission and all
instrument evidence against that single frozen tuple, and the gate precondition
must mechanically reject any bundle whose parts disagree.

## Track R — Release-Qualification Instruments

Each instrument below states: what the predicate demands, the current blocker,
the instrument to build, the evidence chain it must land (see Rigor Bar), and the
honest outcomes it may legitimately produce.

### R1 — Source-span coverage instrument (predicate 2)

- **Demands:** parsed-source span coverage `= 1.0` — every source byte of every
  corpus work is covered by an emitted parser-IR source span, with an
  ignored-region taxonomy for anything deliberately uncovered.
- **Blocker:** no committed per-work span-coverage instrument for `ab-aozora`.
  Note this is a *self-measurement* of the owned parser, distinct from the
  cross-parser span-accuracy axis in Track S (S3).
- **Build:** a per-work analyzer over `ab-aozora`'s parser-IR output that sums
  covered source bytes over the **eligible source-byte inventory** — total source
  bytes minus the regions removed by a **versioned ignored-region taxonomy**. The
  denominator is a *byte* count, not a work count: `covered_eligible_bytes /
  eligible_bytes`. Commit and hash both the per-work eligible-byte counts and the
  aggregate eligible-byte total; **separately** assert (a distinct, work-unit
  check) that every work in the qualification corpus contributed exactly one
  record, so completeness of *works* and the byte-ratio *denominator* are two
  independent authentications, never conflated.
- **Instrument identity:** the ignored-region taxonomy version is part of the
  instrument's identity (Design Deepening → identity). Changing what counts as
  "eligible" changes the result, so a taxonomy revision is a new instrument
  version, not a silent re-measure.
- **Honest outcomes:** `1.0` (every eligible source byte covered → pass); `< 1.0`
  (fail, with the uncovered-region taxonomy naming exactly what is dropped). A
  fail here is a truthful, publishable result, not a campaign failure.

### R2 — Silent-drop detection instrument (predicate 3)

- **Demands:** silent drops `<= 0` — zero source constructs dropped **without a
  diagnostic**. Per ADR 0002 this is the source-construct definition, explicitly
  *not* the AAT→parser-IR mapping-layer LOSS count (the bundle note is emphatic:
  the 1/work divergence LOSS signal is a mapping-layer artifact, not the source
  silent-drop instrument).
- **Blocker:** no committed silent-drop instrument. It requires reconciling
  span-coverage (R1) against emitted diagnostics: a source region that is neither
  covered by output nor accompanied by a diagnostic is a silent drop.
- **Build:** an instrument that joins R1's uncovered-region set with the parser's
  diagnostic stream and counts uncovered regions lacking a corresponding
  diagnostic. Depends on R1 and on R5's diagnostic emission being real (a vacuous
  0-diagnostic stream makes every uncovered region a silent drop).
- **Honest outcomes:** `0` (pass); `> 0` (fail, enumerating the silently dropped
  constructs). Strongly coupled to R1 and R5 — see Sequencing.

### R3 — Publication-structure validation instrument (predicate 6)

- **Demands:** required publication structures present `= 1.0`.
- **Blocker:** the schema exists
  (`abc/schemas/parser-ir-publication-preservation.schema.json`) and a
  `publication-bundle-validate.py` is referenced, but nothing is wired into the
  capture pipeline to produce an observation.
- **Build:** wire the publication-bundle validator over the release candidate's
  parser-IR publication output for the corpus, emitting a per-work + aggregate
  pass ratio against the preservation schema; commit the validator invocation and
  its output as the instrument. This is the lowest-uncertainty of the four — the
  schema and validator largely exist; the work is committing a real run.
- **Honest outcomes:** `1.0` (pass); `< 1.0` (fail, listing works whose
  publication structure is incomplete).

### R4 — Per-work memory instrument (predicate 8)

- **Demands:** peak resident memory `<= 2147483648` bytes (2 GiB).
- **Blocker:** neither `ab-aozora` nor `ab-check` emits a peak-RSS field.
- **Build:** a measurement harness capturing per-work peak RSS (e.g. GNU
  `time -v` / `getrusage` / cgroup peak) over the corpus on a pinned host, with
  the host coordinates captured (this predicate is host-sensitive; record the
  measurement host as the study already does for latency). Emit per-work and
  corpus-max observations.
- **Honest outcomes:** `<= 2 GiB` (pass); `> 2 GiB` (fail). Host must be pinned
  and disclosed so the bound is interpretable.

### R5 — Weak-pass hardening (predicates 4 and 5)

Not new predicates — de-weakening two existing green rows so their `pass` means
what the predicate claims, under the coherent tuple. **Neither predicate is
subsumed by coherence; both remain independent must-pass claims.**

- **Diagnostic completeness (4) — separate the plumbing from the semantics.**
  Predicate 4's dimension is literally *"every **emitted** diagnostic carries
  code/severity/span"* (`predicates.edn:38`). Under that definition, zero emitted
  diagnostics is **mathematically a pass** — vacuously true over an empty set. So
  the fix is *not* "measure a real diagnostic stream to make it non-vacuous"; that
  conflates two distinct questions:
  - **Plumbing (settle with a disposable probe):** can `ab-aozora --mode
    diagnostics` emit and can the capture record diagnostics at all? Run the
    parser over a diagnostic-triggering fixture and observe. This validates the
    *instrument*, nothing more.
  - **Semantics (a design decision, possibly ADR-gated):** *should* predicate 4
    stay envelope-completeness (in which case zero-over-corpus is a legitimate
    pass, but the **vacuity must be disclosed in the observation itself**, not a
    bundle note), **or** should the release claim actually be *diagnostic recall*
    — "the parser diagnoses every expected unsupported/malformed construct"? Recall
    is a **different predicate**, needs an oracle + a versioned challenge corpus,
    and — per ADR 0039's own rule that predicate changes require a separately
    evidenced ADR — cannot be swapped in silently. The probe cannot answer this;
    it is a scope decision for the plan.
- **Parser-IR schema validation (5) — keep it, re-measure against the pinned
  candidate tuple.** Predicate 5 independently asserts that successful outputs
  *validate* against the schema, and it is measured against the **pinned
  candidate identity** (its own mapping + parser-IR schema hash) — capturing
  schema validation needs only the candidate, *not* registry admission, so this
  does not depend on R6. The coherence precondition and R7 separately derive
  whether that same candidate is admitted; a coherent run can still emit
  schema-invalid output, so predicate 5 is not redundant with coherence and must
  not be folded into it.

### R6 — Admit the exact release-candidate tuple (ADR 0023)

- **Blocker:** `:admitted_tuple_matches false` — build divergence + schema drift
  (see Current State). The existing 0.4.0/`004deaf` row does not describe the
  current-schema release candidate.
- **Build:**
  1. **Pin** the release-candidate tuple (git rev, baked adapter coordinates,
     mapping version, parser-IR schema hash) — the same tuple Track-R instruments
     measure.
  2. Run a **fresh full-corpus conversion audit** for that exact tuple:
     `ab-aat-to-parser-ir audit-corpus --aat-dir <corpus> --mapping
     data/aat-to-parser-ir-mapping-v2.json --summary-json … --report-md …
     --compat-edn-out <candidate-row> --jobs 32 --abc-root data/abc-schemas`
     (run on hinoki; heavy). The `--compat-edn-out` is the candidate registry
     row.
  3. **Append** the candidate as a new immutable registry generation in
     `abc/data/aat-parser-ir-compatibility.edn` (never rewrite the existing row —
     ADR-0023/program invariant: manifests in a citation set are replaced by new
     generations, not mutated).
  4. **Admit** via `clojure -M:abc/aat-compat-admission -- --candidates
     <candidate-row>` and require overall status `:admitted` (exit 0).
- **Honest outcomes:** `:admitted` (admission gate closes); `:conflict` (the
  match key exists with differing evidence — a real finding that blocks
  promotion) or `:missing`. Admission touches an ADR evidence-closure file, so it
  carries the governance-recapture cost (`just validate-migration` must exit 0;
  see `.superpowers/sdd/task-6-report.md`).

### R7 — Recapture, re-run, conditional promotion

- Produce the measurement bundle for the single pinned tuple by **generating it
  from committed capture manifests** (decision B) via a deterministic tool with a
  drift test — not hand-authored — with R1–R4 observations as pure projections in
  identity-bearing envelopes, R5's hardened passes, and admission equality left
  for the gate to derive from the registry.
- Enforce the **coherence precondition** (Coherence section): `gate-status` is
  `:release-qualified` only when the precondition holds (every observation shares
  one `qualification_identity`, and that tuple exact-matches an admitted ADR-0023
  row) **and** all nine verdicts are `:pass`; an incoherent bundle is
  `:not-qualified` regardless of tally.
- Regenerate `parser-release-qualification-report.json` via the gate.
- **Conditional promotion:** promote ADR 0039 to Accepted (Status +
  `Accepted:` date, release authority toward publication, typed acceptance
  criteria remain as committed) **only if** the tally is all-`pass` and admission
  matches. Otherwise leave it Proposed with the named blocker recorded. Per ADR
  governance, an Accepted ADR requires `## Decision`, `## Implementation Status`,
  and `## Acceptance Criteria` each citing an existing executable evidence path —
  the new instruments become those evidence paths.

## Track S — Neutral-Comparison Axis Instruments (independent of ADR 0039)

These promote `unavailable` study axes to `measured` (or to a defensible
`non_comparable`). They do not feed release qualification. Each must land the same
Rigor Bar as the one currently-`measured` axis.

### S1 — Fidelity oracle

Visible-text byte agreement, structure agreement, and exact ruby/gaiji/note
counts against an authoritative reference rendering (absent from committed
artifacts today). Adjacent tooling exists (`reports/aat-fidelity/*`, the
`ab-oracle` crate) but produces no study-consumable artifact. Build: a
content-hashed reference oracle + per-lane fidelity runs feeding the study
generator; emit a blocker row if any link is absent rather than imputing.

### S2 — Diagnostic code/severity/span scoring

The frozen fixture
(`ab-validator/docs/studies/fixtures/parser-comparison-diagnostics-v1.json`) and
a working scorer (`reports/parser-conformance/run-aozora-notation-spec.py`,
`run_diagnostics`) both already exist; the missing piece is a **committed
per-parser diagnostic-capture run** feeding the study, scored only on the
predeclared label/severity/UTF-8 span. Lowest-uncertainty Track-S axis.

### S3 — Span accuracy

Exact/overlapping source-span, covered-byte, and invalid-span counts against an
authoritative span-bearing reference. **Structural blocker:** AAT adapter output
carries no spans (production adapters omit them), so the adapter lane is
genuinely `non_comparable` for span accuracy, and the honest terminal state for
several candidates may be `non_comparable`, not `measured`. Build the instrument
for the lanes that *do* emit spans; disclose the rest as non-comparable with the
reason, never as zero.

### S4 — Kaplan–Meier latency with the frozen bootstrap

Fully preregistered already (`performance_protocol`: workset
`performance-largest-six-v1`, 1 warmup + 5 measured reps, right-censored at 300 s,
KM estimator, bootstrap `seed 20260714`, 10000 resamples, percentile CI or
`unavailable` bound). **Hard blocker:** host comparability is `unavailable` for
every lane except the one `aozora-rs-native` execution — a KM statistic cannot be
computed until per-repetition wall-time is captured for the compared lanes on a
single controlled host. Build: (a) a controlled single-host re-run capturing
per-repetition latency with committed `host_capture`; (b) the KM + frozen
bootstrap estimator (does not exist — a wall-time harness exists but computes no
survival statistic). Largest Track-S cost; the host-comparability capture is the
gating dependency.

## Evidence & Provenance Rigor Bar (every instrument, both tracks)

An observation may only leave `unavailable`/`instrument-missing` if it lands the
same evidence chain that backs the one currently-`measured` study axis. This is
the non-negotiable protocol:

- **Content-hashed inputs, durable locators.** Any oracle/fixture/reference is
  committed and content-addressed; runs record `execution_sha256`,
  `manifest_sha256`, `program_sha256`. `artifact_root` must be a **content
  address into the evidence store, not a machine-local path** — a bare filesystem
  locator is not durable evidence and must not be what binds a count to its bytes
  (see Capture tiering below).
- **Pinned denominators, in the ratio's own units.** Every ratio's denominator
  equals a committed, hashed inventory **measured in the ratio's own unit** — byte
  ratios get a byte-inventory denominator, work ratios a work-count denominator
  (the R1 blocker was a byte-ratio given a work-count denominator). "Every work
  contributed a record" is a **separate** completeness assertion, not the ratio
  denominator. The gate/study hard-errors on denominator mismatch — do not weaken
  this.
- **Per-mode provenance.** Native vs adapter provenance is stamped, never
  conflated; ownership grants no pass (owned axes with no analogue stay
  `non_comparable`, competitors are never assigned zero).
- **Host capture where host-sensitive.** Memory (R4) and latency (S4) commit host
  coordinates and a comparability predicate; an axis stays `unavailable` until
  host capture exists for the compared lanes.
- **Non-imputation.** A missing link produces an explicit blocker row/observation
  with a named cause — never a fabricated or zero value. The generator/gate must
  continue to refuse to impute.
- **Exact comparison preserved.** `0.969` fails `1.0`; rounded composite coverage
  is descriptive and never substitutes for a must-pass predicate.

## Design Deepening (hammock + Hickey review)

Applying the design lenses to *this spec* surfaced a root cause that reframes
Track R: the current gate consumes a **hand-authored** measurement bundle, and
that is where the honesty debt entered. The two weak passes are not incidental —
they are the predictable failure mode of a trust boundary that sits inside a
file a human types. This section records the decomplection that removes it —
**adopted** (decision B), and hardened by a second expert review round into the
three-tier capture policy and the gate-precondition coherence contract below.

### The root complection: capture braided with derivation, mediated by a human

`parser-release-qualification-measurements.edn` today entangles three concerns in
one hand-edited artifact: (1) *what was run* (the tuple), (2) *what was observed*
(the numbers), and (3) *the interpretation* (the prose `:notes` explaining
vacuity, schema drift, silent-drop semantics). A person reconciles all three by
hand. The gate then evaluates the result "honestly" — but its honesty is bounded
by the honesty and accuracy of the hand-authored inputs. The `:instrument-missing`
sentinels, the `1.0` diagnostic pass, and the mapping-version note are all human
keystrokes, not machine outputs. **The trust boundary is inside the file, which
is exactly the fake-seam pattern:** the gate *looks like* a control on the build,
but it is really a control on a document about the build.

### Prior art already in this repo: the study's Capture → Derive → Drift shape

Do not invent a parallel architecture — the neutral study already solved this
exact shape and the follow-ups plan just hardened it. The study is: **immutable
raw run manifests (capture) → deterministic report regeneration (derive) →
byte-identical drift test (change-detector).** No human types a robustness count;
it is derived from committed outcomes with pinned denominators, and a stale
literal cannot survive because the report regenerates from data. Release
qualification should adopt the same three-part shape:

- **Capture (once, expensive, place-bound, on hinoki):** run the pinned build
  over the corpus and produce, per work, the parser-IR output, diagnostic stream,
  resource stats (peak RSS, wall time), and publication bundle. This is the raw
  evidence — but it is **corpus-scale generated data and MUST NOT be committed to
  the repository**. It follows the study's own model (the parser-study README
  documents exactly why corpus-scale bytes are not committed) via a **three-tier
  split**:
  1. **In-repo (committed):** capture *manifests*, per-work and aggregate
     *hashes/inventories/summaries*, and *small diagnostic witnesses* — never the
     corpus-scale blobs.
  2. **External evidence store (not in repo):** the corpus-scale per-work blobs,
     placed in an explicitly configured **content-addressed store**, addressed by
     the committed manifest hashes (not by machine-local paths).
  3. **Rebinding verifier (on hinoki):** a checkable step that re-binds the
     committed manifest hashes to the external blobs and confirms integrity —
     mirroring the study's `verify_external` count-authenticity gate. This is what
     makes the external evidence *verifiable* without bloating the repo.
- **Derive (pure, cheap):** each predicate observation is a **deterministic
  function of the committed capture manifests** (and, where a blob is needed, of
  the store contents rebound by hash on hinoki). The measurement bundle is
  *generated* by a tool that projects each predicate from the manifests, not
  typed. Span coverage, silent drops, diagnostic completeness, memory, wall time,
  and timeouts become pure projections.
- **Drift test:** the generated bundle regenerates byte-identically from the
  committed manifests — the same change-detector the study uses, catching a
  hand-edited bundle or a manifest/bundle desync.

This makes coherence **structural rather than aspirational**: all nine predicates
provably read the *same* committed manifests of the *same* build, because they are
projections of one manifest set. It also removes the human from the observation
trust path — a person can still write interpretation prose, but the numbers are
machine-derived and drift-tested. Tradeoff (acknowledged): derivation is **not**
reproducible from a bare clone, since the corpus-scale blobs live in the external
store; but *evidence identity and verification* remain reproducible on hinoki
without committing or machine-binding corpus-scale artifacts.

### Decision: provenance of predicate observations

The one fork worth a matrix. Cells carry the reason and the falsifier, not a
glyph.

| Criterion | (A) Hand-authored bundle (status quo) | (B) Generated from captures (recommended) | (C) Hybrid: instruments emit, human assembles |
|---|---|---|---|
| Integrity / trust | Weak — trust boundary is inside a human-typed file; the two weak passes are the demonstrated leak. *Falsifier: if review reliably caught such errors, the weak passes wouldn't have shipped — they did.* | Strong — numbers are machine-derived; a wrong number requires a wrong capture, which is hash-addressed and drift-tested. *Falsifier: a capture that silently measures the wrong build — mitigated by the coherence precondition.* | Medium — observations are machine-made but re-keyed by hand; assembly can still mis-bind an observation to the wrong tuple. |
| Coherence guarantee | None — nothing forces all observations to describe one build. | Structural — all predicates are projections of one capture. | Partial — depends on assembler discipline. |
| Reproducibility | Low — re-deriving means re-typing. | High — pure derivation, drift-tested, matches study. | Medium. |
| Build cost | Zero new machinery, but the instruments (R1–R4) must exist anyway. | Marginal cost over (A): a capture schema + a projection tool. The instruments are the same work; only *bundle assembly* moves from human to tool. | Between the two. |
| Consistency with repo | Diverges from the study's architecture. | Reuses the study's proven Capture→Derive→Drift shape. | Diverges partially. |

**Decision: (B), adopted (user-confirmed 2026-07-15).** The instruments R1–R4
have to be built regardless; the only real delta is committing a per-work capture
schema and a deterministic bundle-projection tool with a drift test. The
integrity payoff (machine-derived, coherent-by-construction, drift-tested
observations) is exactly the payoff the follow-ups plan bought for the study
report, applied to the gate that actually governs release. This is not
gold-plating — it removes the mechanism that produced the current honesty debt.
The follow-on plan builds Track R on this shape unconditionally.

### State, time, and identity (made explicit)

- **Identity:** a qualification result is identified by `qualification_identity`
  (Coherence section). Admission identity is exact-tuple value equality (already
  correct in ADR 0023). Instruments get **versions** folded into identity: if a
  span-coverage instrument changes how it counts, a prior capture's derived
  observation can change, so the instrument version is part of what a result
  means. *(New requirement — the current design has no instrument-version
  identity.)*
- **Time:** measurements have a **validity window bound to the schema/mapping/build
  hashes**, not a wall-clock date. A `pass` is valid only while the tuple it was
  captured against is still the release tuple. Schema drift (the `a1e1b506…` →
  `43a6a6d8…` event) *invalidates* a prior pass — and under the coherence
  invariant, the gate detects this as a hash mismatch rather than silently
  serving a stale green. The captured reports are **append-only immutable
  snapshots** (like the study's), giving an audit trail of what was true for which
  tuple.
- **Time-modeling level (escalate to the risk, not past it):** this needs
  *current state + immutable append-only captured snapshots* — roughly level 3
  (audit log via committed reports). It does **not** need domain events, an
  event-sourced log, or a database. Each promotion is a discrete, human-reviewed
  ADR event backed by one immutable captured report; that is sufficient and
  proportionate. Reflexively reaching for event sourcing here would add
  complexity the risk does not justify.

### Severity-tagged findings (two review rounds)

First-round findings (structural, resolved in this spec):

- **Blocker (trust/composition):** Hand-authored bundle puts the observation
  trust boundary inside a human-typed file; the two weak passes are the
  demonstrated leak. *Resolved:* Capture→Derive→Drift, decision (B).
- **Strong suggestion (composition):** Decomplect place-bound capture from pure
  derivation — one capture-manifest set, N predicate projections.
- **Strong suggestion (identity):** Fold instrument versions into
  `qualification_identity`; a changed observer changes what an observation means.

Second-round findings (from expert review, resolved above — these are the ones
that made "coherence-as-invariant" real rather than aspirational):

- **Blocker (protocol, resolved):** Coherence was promoted to an invariant
  without a data contract or enforcement semantics; the live gate ignores
  admission entirely (`gate-status:156`) and observations are bare scalars
  (`evaluate-predicate:127`). *Resolved:* coherence is a **gate-level
  precondition** (not a tenth predicate, not CI-only, not mere schema), backed by
  **identity-bearing observation envelopes** and **registry-derived admission
  equality**, with `gate-status` requiring precondition ∧ nine passes. A
  bundle/schema version migration follows and is accepted (Coherence section).
- **Blocker (units, resolved):** R1 gave a byte-coverage ratio a work-count
  denominator. *Resolved:* the denominator is the eligible **source-byte**
  inventory under a versioned ignored-region taxonomy; "every work contributed a
  record" is a separate work-unit completeness check (R1, Rigor Bar).
- **Strong suggestion (artifact policy, resolved):** "commit per-work captures"
  would check corpus-scale generated blobs into source, against repo policy and
  the study's own model. *Resolved:* three-tier capture — committed
  manifests/hashes/summaries/small witnesses; external content-addressed blob
  store; hinoki rebinding verifier. `artifact_root` is a content address, not a
  machine path (Capture section, Rigor Bar).
- **Strong suggestion (probe scope, resolved):** the diagnostic probe can only
  validate capture *plumbing*, not the *semantic* question of whether zero-over-
  corpus is a legitimate completeness pass. *Resolved:* probe = plumbing only;
  predicate 4's identity (envelope-completeness-with-disclosed-vacuity vs
  diagnostic-recall-over-a-challenge-corpus, the latter an ADR-gated predicate
  change) is a separate scope decision (R5).
- **Strong suggestion (predicate 5, resolved):** do not subsume predicate 5 into
  coherence — they answer different questions and subsumption would delete an
  independent must-pass and change the predicate set (a non-goal). *Resolved:*
  predicate 5 retained; coherence is complementary (R5, Coherence section).

- **Follow-up (not this campaign):** the study's `missing` axes (Track S) and the
  gate share a provenance model; consider whether the two capture-manifest formats
  should converge, so one `ab-aozora` corpus run serves both the release gate and
  the study's owned-parser appendix. Track, don't bundle.

### Settled decisions and remaining open questions

Settled (user-confirmed 2026-07-15, and hardened by the second review round):

- **Capture→Derive→Drift (decision B) is adopted**, with the **three-tier capture
  policy** (committed manifests + external content-addressed blobs + hinoki
  rebinding verifier) — no corpus-scale artifact is committed.
- **Track S is in scope**, running in parallel and never gating Track R.
- **Coherence is a gate-level precondition** (not a tenth predicate, not CI-only,
  not mere schema): identity-bearing observation envelopes + registry-derived
  admission equality + `gate-status` requiring precondition ∧ nine passes. This
  answers the earlier open "where does the coherence check live" question.
- **Predicate 5 is retained** as an independent must-pass; coherence is
  complementary, not a replacement. (Earlier open question, now closed.)
- **R1 denominator is the eligible source-byte inventory** under a versioned
  ignored-region taxonomy; work-completeness is a separate check.

What remains genuinely open — the plan must resolve or explicitly defer each,
and none should be forced to a false "done":

- **The bundle/schema migration shape** — the exact envelope schema, how the
  precondition is expressed (Malli precondition fn ahead of the tally), and the
  report-schema version bump. Design when the capture-manifest schema is designed.
- **Whether Track S and Track R share one `ab-aozora` capture-manifest format** —
  an attractive simplification that must not delay Track R.

**Settled since the second review round:**

- **Predicate 4 stays envelope-completeness for this campaign.** Zero-over-corpus
  is a legitimate pass whose vacuity is disclosed in the observation; a *diagnostic
  recall* claim (does the parser diagnose every expected construct?) is a
  different predicate needing an oracle + a versioned challenge corpus + its own
  evidenced ADR, and is recorded as a **separate future workstream** that does not
  change this campaign's predicate set or dependency graph. The disposable probe
  tests only plumbing and cannot justify switching to recall.
- **External-store lifecycle.** A capture manifest commits a **logical blob
  identity** — `sha256:<digest>` + byte length + media type — never a
  machine-local path; `artifact_root` is a *configured store locator resolved at
  runtime*, distinct from the committed content identity, and must not be called a
  content address. Resolution goes through explicit runtime configuration (who
  sets the store root is a config concern, not committed evidence). A blob that is
  **absent or hash-mismatched yields `:unavailable`/error, never a derived
  observation**; the rebinding verifier streams and re-hashes blobs (it does not
  trust store metadata), and durability is a stated retention guarantee of the
  store, not of the repo.

Honest status: Track R's *what, why, architecture, and the five review findings*
are settled; what remains is one genuine scope decision (predicate 4's identity)
and the concrete schema/migration shapes — work for the plan's first steps, not a
claim that everything is decided.

## Program Invariants (inherited, binding)

1. **Integrity mandate overrides everything.** No fabricated or imputed
   measurement; unmeasured stays `unavailable`/`non_comparable`/`missing` with a
   named blocker. (See Terminal Objective.)
2. **No predicate weakening.** Changing a threshold or predicate requires a
   separately evidenced ADR — never done to force a pass.
3. **Evidence-class separation is structural.** Neutral-comparison and
   parser-selection citations can never satisfy admission or release (enforced by
   `abc.tools.parser-evidence`). Track S never feeds ADR 0039.
4. **Historical manifests are immutable.** Admission adds a new registry
   generation; it does not rewrite the existing `004deaf`/0.4.0 row.
5. **One coherent tuple.** All Track-R evidence describes one identical build
   (Coherence Requirement); the gate precondition *derives* admission equality
   from the ADR-0023 registry — it does not trust a hand-asserted
   `:admitted_tuple_matches` boolean.
6. **Governance stays green.** Any ADR-evidence-closure change is recaptured
   through the real tool on hinoki; `just validate-migration` exits 0.
7. **Heavy work on hinoki.** Full-corpus audits, memory/latency captures, and
   regeneration run on hinoki, not locally.

## Sequencing & Dependencies

Within Track R:

- **R1 precedes R2** (silent-drop reconciles uncovered regions against
  diagnostics).
- **R5-diagnostic precedes/co-designs with R2** (a vacuous diagnostic stream
  makes every uncovered region a silent drop; the diagnostic-emission decision
  must be settled before silent-drop counts mean anything).
- **R3, R4 are independent** of R1/R2 and of each other.
- **R6 (admission) is independent of R1–R5** but shares the pinned tuple; it can
  proceed in parallel once the tuple is pinned.
- **R7 is the barrier** — it consumes R1–R6 and must not run until all feed one
  coherent tuple.

Track S is independent of Track R end-to-end. Within Track S, S4 depends on the
host-comparability capture; S2 is nearly ready; S1 and S3 need reference
artifacts (S3 may terminate `non_comparable` for adapter lanes).

Cross-cutting: pin the release-candidate tuple **first** — it is the shared
input to R1–R6 and its choice (which git rev is "the release candidate") is a
decision the plan must record before any measurement.

## Acceptance Criteria (campaign-level)

The campaign is **complete** (independent of whether ADR 0039 promotes) when:

- Every one of the nine release predicates resolves to `pass` **or** `fail` from
  a committed instrument over the coherent tuple — zero `unavailable` remain, or
  any remaining `unavailable` has an ADR-recorded, evidenced reason it cannot be
  instrumented.
- The two weak passes are de-weakened without predicate reinterpretation:
  predicate 4's identity (envelope-completeness with disclosed vacuity, or an
  ADR-gated recall predicate) is decided and the chosen observation discloses its
  meaning; predicate 5 is retained and measured against the admitted tuple's
  schema.
- The release-candidate tuple is pinned and its admission is resolved through ADR
  0023 to `:admitted` (or an honest `:conflict`/`:missing` blocker is recorded).
- The regenerated report reflects real verdicts, and the gate precondition
  confirms admission equality by *deriving* it from the registry (not a
  hand-asserted boolean).
- ADR 0039 is promoted to **Accepted** iff the tally is all-`pass` **and**
  admission matches; otherwise it stays Proposed with the blocker named.
- Every new instrument lands the Rigor Bar (content-hashed inputs, pinned
  denominators, non-imputation) and is cited as executable evidence.
- The **coherence precondition** is mechanized at the gate: `gate-status`
  requires the precondition (identity-envelope agreement ∧ registry-derived
  admission equality) **and** nine passes, so a `pass` provably describes the
  admitted build; an incoherent bundle is `not-qualified` regardless of tally.
- The measurement bundle is generated from committed capture **manifests** (not
  corpus-scale blobs, which live in the external content-addressed store) and
  regenerates byte-identically under a drift test; no predicate observation is
  hand-keyed (decision B).
- **(Track S, in scope)** each of the four study axes is `measured` or a
  defensible `non_comparable` with committed provenance; no axis is imputed.
- `just validate-migration` exits 0; ABC governance green after any
  evidence-closure recapture.

## Non-Goals

- Promoting ADR 0039 without a fully passing captured run on an admitted build.
- Weakening, rounding, or reinterpreting any predicate to obtain a pass.
- Using neutral-comparison evidence for admission or release qualification.
- Rewriting historical registry rows or manifests.
- Selecting a different parser base (ownership is settled by ADR 0038,
  independent of comparison rankings).
- Treating Track S as a prerequisite for ADR 0039.

## Safe Fallback

If instruments reveal failing predicates or admission surfaces a conflict, the
custom parser continues under **development** release authority without
publication authority — the ADR 0038 safe fallback. The neutral comparison is
preserved as evidence. No predicate is weakened to qualify the parser; a genuine
change to a predicate requires a separately evidenced ADR.

## Risks & Open Questions

- **Which build is "the release candidate"?** The pinned tuple must be chosen and
  recorded before measurement; the current bundle's `90967157` is stale. Decide
  during planning.
- **Diagnostic vacuity (R5):** is "0 diagnostics over the corpus" a legitimate
  pass or evidence the instrument is missing? This is a genuine design decision,
  not a mechanical fix — resolve it explicitly and ground it in what the parser
  is expected to diagnose.
- **Schema drift (R6):** admission must target the current schema hash
  `43a6a6d8…`; confirm mapping 0.5.0 is the intended live mapping and that the
  audit binds it, so the admitted tuple and the schema-validation predicate agree.
- **Host comparability (R4, S4):** memory and latency need a pinned,
  disclosed host; S4's KM statistic is fully blocked until per-repetition
  latency is captured for the compared lanes on one host.
- **Span non-comparability (S3):** adapter lanes may legitimately end
  `non_comparable`; do not force a `measured` label.
- **Cost:** full-corpus audit + four instrument captures + the Track-S latency
  re-run are substantial hinoki jobs. Track S is in scope but sequenced so it
  never blocks Track R; if capacity is tight, S4 (host-controlled latency) is the
  natural item to run last since its host-comparability capture is the long pole.

## Deliverables & Follow-on Plan

This design produces:

- Track R: four release instruments (R1–R4), weak-pass hardening (R5), a fresh
  ADR-0023 admission (R6), a generated bundle + regenerated report + the mechanized
  coherence **precondition**, and a conditional ADR 0039 promotion (R7). Under
  decision (B): a **capture-manifest schema** (three-tier: committed manifests +
  external content-addressed blob store + hinoki rebinding verifier), the
  identity-bearing observation-envelope bundle migration, and a deterministic
  bundle-projection tool with a drift test.
- Track S (in scope): four study-axis instruments feeding the neutral report.

Next step is a **task-by-task implementation plan** (a
`docs/superpowers/plans/2026-…-parser-release-qualification-campaign.md`) that
sequences R1→R7 as the spine, pins the release candidate first, and treats Track
S as a separable appendix or its own plan. As the parent deferral instructed,
each instrument is a substantial, independently specifiable subsystem — the plan
should carry one owner per instrument with the Rigor Bar as the shared
acceptance net, and run all heavy captures on hinoki.
