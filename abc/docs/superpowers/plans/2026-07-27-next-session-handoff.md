# Handoff: Parser-RQ Corpus Tiering (2026-07-27)

This is an operational checkpoint for the next session, not an implementation
plan and not a new source of governance authority. The governing design remains
`docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`;
hand-authored records in `docs/adr/decisions.edn` become authoritative once
accepted.

> **Q15 was decided 2026-07-27** — the body and the packaging metadata are two
> populations measured separately, with conjunctive clearance over a declared
> partition. See *Q15 — decided* below and
> `plans/2026-07-27-q15-region-partition.md`. The standing caution survives the
> decision: do not add a `publication_metadata` policy rule as the frame fix. The
> ledger never lexes those bytes, so a rule alone classifies nothing — metadata
> attribution needs a fact producer first.

## Start here

1. Recalculate live repository state rather than trusting historical counts:

   ```sh
   git status --short
   git log --oneline --decorate main..HEAD
   git diff --stat main...HEAD
   ```

2. Read these design sections in order:

   - *Status*
   - *Q14 mechanism, corrected again 2026-07-27 — a coordinate mismatch, not a
     missing rule*
   - *Q16, settled 2026-07-27*
   - *What each open item blocks*
   - the Q15 and Q13 entries under *Open Questions*

3. Q15's contract is decided; its implementation is not started. Treat the later
   Q15 rescope as the current diagnosis. The design's superseded
   passages were reconciled on 2026-07-27 and their old diagnoses kept behind
   explicit history markers — see *Design self-contradiction — reconciled* below
   for what changed and for the figures that remain indicative rather than
   authoritative.

The durable checkpoint is commit `a2c8ad73` (the original handoff).
`06f6cd37` added the initial design plus non-authoritative corpus inventory/tail
tools and their quality wiring. `3a329ea0` is the only later change to active
parser-RQ qualification code; it binds the classified-source policy into the
qualification identity. Counts such as “commits ahead of `main`” and working-tree
cleanliness are deliberately not recorded here because they become false as soon
as work resumes.

## Historical verification baseline

At the checkpoint, the focused Clojure run reported **162 tests / 816
assertions / 0 failures / 2 environment errors** across:

- `abc.tools.parser-rq-campaign-test`
- `abc.tools.parser-release-qualification-test`
- `abc.tools.parser-rq-source-accountability-test`
- `abc.tools.validate-design-bundle-test`

Both errors were `TEI_SCHEMA_PATH must be set` in
`validate-design-bundle-test`; they were reproduced on the then-clean tree and
were not caused by this branch. The raw command below is therefore diagnostic
unless that variable is configured:

```sh
(cd abc && clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-campaign-test \
  --focus abc.tools.parser-release-qualification-test \
  --focus abc.tools.parser-rq-source-accountability-test \
  --focus abc.tools.validate-design-bundle-test)
```

The standard Nix-backed checks below are the verification authority. They supply
their governed dependencies and must pass before completion; do not recast the
raw command's two errors as a successful full test run.

During the handoff review, the Nix-backed full focused check passed **1,145 tests
/ 11,754 assertions / 0 failures**:

```sh
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests --no-link \
  --print-build-logs
```

## Q15 — decided 2026-07-27

The traced mechanism is:

- `ab-aozora-capture/src/classified_source.rs::build_ledger` lexes
  `decoded.span_text`;
- `decoded.span_text` is the `aozora_body_range` body projection;
- the recognition instrument currently takes `eligible_bytes` from the entire
  decoded file.

The result divides a body-derived numerator by a whole-file denominator.

**The decision rejected the framing this section previously used.** Options A and
B offered two competing denominators for one ratio. The body and the packaging
metadata are **two populations**: the body is the work — prose and annotations —
and the header and `底本：` colophon are metadata *about* the work. A single ratio
over their union averages parser fidelity against packaging attribution and can
mean neither.

- The decoded file is partitioned into declared regions derived from
  `aozora_body_range` **alone**.
- `source_span_coverage` becomes **body-projection coverage**.
- A **separate metadata-attribution predicate** covers the header and tail in
  their own coordinate. Header and tail are distinct regions so a failure
  localizes; they qualify under one conjunctive predicate.
- **A work clears only when both clear.**
- Neither threshold is carried forward by default. Build, measure under an
  explicitly non-authoritative exploratory campaign, then predeclare — Q14's rule
  applied to both predicates.

This is **not** a denominator reduction: no byte leaves the accounting. The
metadata bytes move to a different *accounted* region. And the partition makes
conservation assertable — `header + body + tail == decoded file`, disjoint —
which is the first check in this area that *can* fail. Every defect traced here
shared one property: no invariant existed that could catch it.

Honest cost: this is Option A's work **plus** Option B's. It is not a middle
path; the extra cost buys the invariant.

Task sequence, traced constraints, and a draft `decisions.edn` entry are in
`plans/2026-07-27-q15-region-partition.md`. Two findings from writing it:

- **`aozora_body_range` does not currently partition the file.** `body_end` is
  trim-adjusted while `tail_start` is not (`ab-source-syntax/src/lib.rs:146-147`),
  so whenever a tail exists at least one byte belongs to no region. The
  implementation must declare where those bytes go — widening `body_end` to
  `tail_start` is the wrong answer, because body coverage would then depend on
  how many blank lines a transcriber left.
- **Q15 tasks 1–5 are now done too** (2026-07-27), and task 6 is deliberately
  not executed because its precondition is unmet: the metadata instrument
  classifies one form and leaves ~70% of packaging bytes unattributed, so a
  threshold fixed now would encode that gap as a permanent governed allowance.
  What is owed before it can run is listed in
  `plans/2026-07-27-q15-region-partition.md`.
- **Q13 is now done** (2026-07-27, all six tasks), so partition task 3 no longer
  inherits it. `analyze.rs`'s whole-file eligibility is gone, `aggregate.rs` and
  the P1 aggregate schema are deleted, and the work record is at
  `abc/parser-rq-source-accountability-work/v2` carrying no measurement at all.
  Q15 declares region eligibility on its own terms with nothing to undo first.

### Acceptance conditions

- Numerator facts and denominator intervals inhabit one declared coordinate, per
  region.
- Region boundaries have one authority: `aozora_body_range`.
- Conservation and disjointness are asserted and fail closed.
- Works without two separator lines, without a `底本：` line, and with bare-CR
  line endings are covered by tests.
- The governed three-work corpus remains the control and returns its expected
  result.
- A real-source sample is re-measured through the built
  `ab-parser-rq-source-accountability` binary. The ≈0.9889 estimate came from a
  disposable separator heuristic and is not an acceptance value.
- The affected policy/schema hashes and `qualification_identity_ref` rotate
  through their intended coordinates. `predicate_set_hash` moves when the
  predicate contract changes — and under this decision it does.
- Old evidence is rejected as stale or protocol-incompatible, as appropriate.

## Design self-contradiction — reconciled 2026-07-27

**Severity was: blocker for implementation from the design. Now discharged.**

The design previously carried the traced diagnosis in *Q14 mechanism, corrected
again* and the Q15 *Open Questions* entry while earlier text still instructed the
superseded fix — add `publication_metadata`, `warichu_close`, `framed_close`, and
a `底本` rule. Those passages were reconciled in the commit that follows this
handoff's rewrite. Each superseded diagnosis was kept as history behind an
explicit marker rather than deleted, so the record of being wrong twice survives:

| Passage | Now reads |
|---|---|
| *Status* | The traced coordinate mismatch; Q15's decided contract |
| *Q14, decided* | Decision stands; a banner marks the whole section's mechanism as history and voids Part A |
| *Part B, diagnosed* | Discriminator and verdict stand; scope corrected to twelve variants; the A+B merger withdrawn |
| *What each open item blocks*, row Q15 | Blocked on the coordinate choice, not on missing rules (since decided) |
| *Current blocker status*, B5 | Coordinate mismatch named; the superseded rule claim marked as such |
| *Governance Path*, step 1 | Two separable deliverables: the coordinate, then the twelve `node_policy` arms |
| Q14 *Open Questions* | Superseded causal claim removed from the entry itself |
| Q15 *Open Questions* | Part (i) now records the decided contract; the earlier proposal is marked superseded |

One stale claim was also found in code and corrected: the comment in
`classified-source-policy-edits-rotate-the-qualification-identity` called its
fixture edit "exactly the Q15 amendment in miniature". The test is sound — it
proves a policy edit rotates the identity without moving `predicate_set_hash` —
but its edit is a *representative* policy edit, not the Q15 fix, which the
comment now says.

Two verification notes for whoever implements Q15. The design's ≈0.9889 and
≈0.9892 coverage estimates, its 57.1/39.6/3.3 frame split, and the 96.7% and 65%
figures all derive from the disposable separator heuristic; the surviving text
marks them indicative, and none is an acceptance value. And re-check for drift
before trusting any single passage:

```sh
grep -n 'publication_metadata\|warichu_close\|framed_close\|Part A' \
  abc/docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md
```

## Governance records — three written 2026-07-27, three still owed

`decisions.edn` is edited by hand, never generated. Records are kept distinct so
that decision, implementation, and measurement do not collapse into one event.

Written 2026-07-27, all **`:proposed`** with narrative files, `INDEX.md` and
`adr-graph.mmd` regenerated, `clojure -M:abc/adr-governance` clean:

| Slug | Covers | Note |
|---|---|---|
| `parser-rq-instrument-before-threshold` | Q14 | The rule plus the traced coordinate mismatch. The two superseded causal claims are deliberately **not** preserved as claims |
| `parser-rq-source-region-partition` | Q15 | Partition, both measurement contracts, conjunctive clearance, and the `aozora_body_range` gap |
| `parser-rq-classified-source-policy-binding` | Q16 **and** the instrument-binding amendment | `:amends parser-release-instrument-bindings` scoped to the `source_recognition` policy closure — the amends relation *is* the amendment, so these were one record, not two |

Still owed:

| Record | Current state | Required action |
|---|---|---|
| Q11 | Decided in a prior session | Record tier 2 as the publication's successfully-selected projection, named the *publication-workload snapshot*, with `candidates_considered`, `derive_failures`, and `rejected` declared |
| D7 | Decided in a prior session | Record `unexpected-fatal-failures <= 0` and its safe initial expected-outcome vocabulary |
| Q13 | **Decided and implemented 2026-07-27** | Recorded as `parser-rq-retire-node-span-coverage`; all six implementation tasks done. Still `:proposed` — c1, c2, c4 and c5 lack evidence paths |

Q11 and D7 were left alone deliberately: their reasoning belongs to the sessions
that decided them, and writing their claims second-hand would put words in those
decisions' mouths.

### Why Proposed rather than Accepted

`:accepted` requires an accepted date, a validation scope, a release authority,
and **`:kind` plus non-empty `:evidence` on every claim**, with evidence paths
that exist under `test/`, `fixtures/`, `nix/`, or `docs/evidence/external/`.
(Since 2026-07-31 those roots are monorepo-root-relative — `abc/test/`, … —
and `ab-validator/crates/<crate>/tests/` is additionally citable.)
Q14's and Q15's claims have no such evidence — nothing is implemented, and the
measurements behind them were taken under a synthesized qualification identity
that authenticates nothing. Promoting either is rejected by the schema, which is
the correct outcome.

`parser-rq-classified-source-policy-binding` **does** promote cleanly, verified
by loading the corpus, setting `:status :accepted` with an accepted date in
memory, and checking both `shape-problems` and `semantic-problems` return empty.
Acceptance is a governance act, so the record is offered for promotion rather
than asserting it. This was not true when the record was first written: claim c3
carried no evidence and review caught it. c3 now cites
`test/abc/tools/validate_design_bundle_test.clj`, which authenticates the
authority descriptor's `raw_bytes_hash` and `identity_hash` against the policy's
exact bytes — the fact c3 rests on.

### Regeneration and gate notes

Adding a record invalidates **two generated files**, and requires a third
artifact that is authored rather than generated:

| Artifact | How it is produced |
|---|---|
| `docs/adr/INDEX.md` | generated — `clojure -M:abc/adr-governance --write-index` |
| `docs/adr/adr-graph.mmd` | generated — `clojure -M:abc/diagrams` |
| `docs/adr/<slug>.md` | **authored by hand**; mandatory |

`narrative-problems` reports both missing narratives and orphan files, so the
narrative is not optional. Validate with `clojure -M:abc/adr-governance`, which
exits 1 on any problem. The stale diagram is caught by
`clj-nix-focused-tests`, not by ADR governance — running only the latter will
pass while the branch gate fails.

Records do **not** rotate `qualification_identity_ref`. `decision_statuses` is a
promotion-gate input, not an identity field. `parser-rq-p5-promotion-audit` was
run and passes against the edited corpus.

The Q16 implementation widened `instrument-policy-paths` from member → path to
member → ordered vector. `:source_recognition` now binds both:

- `data/parser-rq-ab-aozora-classified-source-v1.json`
- `data/parser-rq-ignored-regions-v1.json`

The wire shape remains `member -> sha256`: a single-document member retains its
document hash byte-for-byte, while a multi-document member folds the ordered
document hashes. Only `source_recognition` moved
(`sha256:c099072a…` → `sha256:81547652…`). This rotated
`qualification_identity_ref`, so captures made against the old identity are
stale.

## Work not blocked by the Q15 implementation

Q15's contract is decided; its implementation is not started. Partition tasks 1
and 2 are unblocked and are the shortest path to a check that can fail — see
`plans/2026-07-27-q15-region-partition.md`. Task 2 became executable only on
2026-07-27, when the region set was closed at three regions with the tail
extending back to `body_end`; before that the ADR asserted conservation over a
set that provably did not conserve, and review caught it. The streams below are independent of
it, and each still needs its own scoped plan before code changes. **Q13 is
settled and implemented**, so the partition no longer inherits `analyze.rs`'s
whole-file eligibility.

1. **Q13: decided and implemented 2026-07-27 — quantity retired.** Recorded as
   `docs/adr/parser-rq-retire-node-span-coverage.md`; plan at
   `plans/2026-07-27-q13-node-span-coverage.md`. Parser-IR node spans are running
   offsets over emitted visible text; the emitter hard-codes their label to
   `decoded_utf8`, `sentences.rs` uses that string for the visible-text
   projection, and `analyze.rs` reads it as the decoded source file. Confirmed
   through the built binary: the governed works of 43/58/76 bytes all report
   `covered_eligible_bytes` of 25, and on one real work the decoded source under a
   node's span matches that node's text 7 times in 8,137.

   **Scoped to the quantity, not the analyzer.** The enumeration found every
   consumer of the coverage number is a test — `install-source-recognition-observation` and `derive-source-span-envelope` have zero production callers, and
   the key is absent from published measurements — but `analyze_corpus` also
   produces the membership index the release-authoritative recognition path
   authenticates against. The span union and coverage fields go; the membership
   derivation and its per-work parser-IR authentication stay. Implementation plan
   written: `plans/2026-07-27-q13-implementation.md`. It establishes that
   **`qualification_identity_ref` does not rotate** — `instrument_versions` is
   derived from the predicate set and no predicate names this instrument — while
   `membership_ref` does, making prior captures protocol-incompatible.

   **Implemented 2026-07-27, all six tasks** (`c993b685`…`a536c829`). Both open
   scope questions were settled by taking the recommendations: the record also
   sheds eligibility and the P1 aggregate, and the v1 schemas are frozen
   alongside v2. Re-measured on the governed corpus under the promoted identity:
   `qualification_identity_ref` **unchanged**, `membership_ref`
   `746b9989…` → `93e9dbd4…`, per-work record 1,524 → 1,225 bytes, recognition
   still `ok` on all three works. The published-artifact count was 18 (9 work,
   3 aggregate, 6 index), not the 22 carried by earlier drafts.

   Two things a reader should carry forward. **The 9.80 ms/work saving is not
   claimed** — it was not re-measured, and the retirement rests on the coordinate
   defect alone. **The record is still `:proposed`**: c3 and c6 now carry
   evidence, c1, c2, c4 and c5 do not, and the Rust tests holding most of the
   implementation cannot be cited at all, because the governance schema admits
   only `test/`, `fixtures/`, `nix/` and `docs/evidence/external/` prefixes while
   those files live under `ab-validator/crates/`. Promotion needs that gap
   addressed, not worked around. **Gap closed 2026-07-31**: evidence paths are
   now monorepo-root-relative and `ab-validator/crates/<crate>/tests/` is
   citable; the governance nix check stages both trees. c1's evidence question
   remains the record's own (see its Evidence section).

   **One thing went wrong and is worth repeating as a warning.** Task 2 deleted
   `abc/test/fixtures/parser-rq/source-accountability` as orphaned; it was not,
   because a Rust integration test reads it as a committed witness set. The
   enumeration behind that task was scoped to Clojure and missed a cross-language
   consumer, and the branch gate never ran the Rust tests —
   `nix build …#cargo-test` **cannot build at all** on this tree (verified by
   stashing and rebuilding on the unmodified tree: `ab-aozora-capture`
   `include_bytes!`s four files from `abc/`, which the ab-validator flake source
   does not stage), so `just validate-migration` only ever *evaluates* it. A
   check that is registered, evaluated, and never built reads exactly like a
   passing check. Use `cargo test --workspace` directly.

2. **D7 prerequisites:** separate record `status` from measured `disposition`,
   then specify closed expected-outcome vocabularies. Initially only `parsed` and
   `fatal_error` may be expected; `adapter_timeout`, `protocol_error`, and
   `unavailable` remain unconditional failures.
3. **Q9:** authenticate set equality with the publication's selected projection
   and produce an accounted set difference against admission and conversion-audit
   populations. Do not demand equality with admission.
4. **Q3:** trace `LOSS` and `AMBIGUITY` paths. Directly measured campaign results
   do not prove those mechanisms preserve coverage.
5. **D8:** decide whether repetitions remain fixed at three or become
   tier-relative. The latter is a schema and core-attempt protocol migration, not
   an analyzer flag.
6. **Q5/Q6:** define the governed host class and the capture retention policy.
   One measured capture generation is approximately 12 GB; retention across
   generations and closed-membership verification cost remain unknown.

Do not combine these merely to minimize the number of commits. Combine identity
rotations only after the semantics of every constituent change are independently
reviewed.

## Post-Q15 sequence

Superseded by `plans/2026-07-27-q15-region-partition.md`, which carries the
task-by-task sequence against the decided contract. Two rules from the earlier
sequence that the plan inherits and that are easy to lose:

- The twelve unmapped `DirectiveKind` variants in
  `ab-validator/crates/ab-aozora-pipeline/src/fold.rs::node_policy` are a
  **separate reviewable deliverable**, body-side, not blocked by the partition.
  **Done 2026-07-31**; `policy_hash` rotated and the corpus sweep re-verified.
- Rebuild after any policy change: the classified-source policy is
  `include_bytes!`-embedded and `parser-rq-classified-source-authority-v1.json`
  fails closed on `raw_bytes_hash` and `identity_hash`.

The exploratory and confirmatory campaigns must have distinct predicate-set
identities. A post-hoc threshold must not be presented as preregistered
confirmation. Under the Q15 decision this applies to **both** predicates —
body-projection coverage and metadata attribution — and neither threshold is
carried forward by default.

## Measurements worth carrying

- Q2: 2,421 `ParserResidue` occurrences over 299 works; 100% use the
  `x-provenance == "parser-derived"` arm and 100% carry non-zero source spans.
  The string heuristic under Q4 did not fire.
- Q12: no sampled real work reached `source_span_coverage = 1.0` (fold 0.9640,
  worst 0.2466), while all three governed works did.
- Q1: capture costs 232.4 ms/work, **9.25×** all of serial `ab-check`; only 2.2%
  is parsing and 4.2% is the non-authoritative node-span analysis.
- Capture cost is approximately byte-proportional at 5,400 ms/MB decoded:
  **78–85 minutes** for one serial full-snapshot pass.
- Capture storage is 662 KB/work, **15.2×** decoded source, or about **12 GB**
  per 17,878-work generation.

These are measurements from a 299-work sample and synthesized identity. They
measure behavior and cost; they do not authenticate a campaign.

## Failure modes to keep visible

- The frame mechanism was stated three times and was wrong twice because causes
  were inferred from correlations. Require a source-line trace for future
  mechanism claims.
- Searching for `b'-' * 40` found overlapping offsets inside one 55-hyphen
  separator. Never restore that detector; use `aozora_body_range`.
- Always run the governed three-work corpus as a control. It exposed a
  node-span reconstruction that measured the wrong quantity.
- `tools/corpus_inventory.py` and `tools/corpus_tail_set.py` are exploratory,
  non-authoritative tools. They must not become governance inputs.
- Published manifests are immutable.
- Add new tracked files before quality checks: Python quality and Nix evaluation
  intentionally do not see untracked sources in the same way.
- Do not commit one-off capture probes as release-crate examples. If a probe is
  recreated, keep it in configured scratch state and record its exact source or
  method in the resulting evidence.

## Reproduction and verification

Use the pinned `aozorabunko-src` flake input. If an extracted checkout is needed
for an exploratory probe, pass its path explicitly through task-local
configuration and first verify revision
`0e9ea3e586eb0aa34039fabfc85a407d2f98b165`; do not add a machine-local path to
tracked code or documentation.

Focused checks should match the files changed. The final branch gate remains:

```sh
just comment-hygiene
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
just validate-migration
```

For documentation-only edits to this handoff, inspect the rendered structure and
run `git diff --check`; do not claim that historical code tests were rerun.
