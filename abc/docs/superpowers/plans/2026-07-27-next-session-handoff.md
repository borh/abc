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
- **Settle Q13 before partition task 3.** Dropping the `NodeSpans` path removes
  `analyze.rs`, `aggregate.rs`, two schemas and four test files from this
  change set.

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
| Q13 | **Owner decision required** | Record one of the three options in `plans/2026-07-27-q13-node-span-coverage.md`; state the quantity actually measured, not "renamed for clarity" |

Q11 and D7 were left alone deliberately: their reasoning belongs to the sessions
that decided them, and writing their claims second-hand would put words in those
decisions' mouths.

### Why Proposed rather than Accepted

`:accepted` requires an accepted date, a validation scope, a release authority,
and **`:kind` plus non-empty `:evidence` on every claim**, with evidence paths
that exist under `test/`, `fixtures/`, `nix/`, or `docs/evidence/external/`.
Q14's and Q15's claims have no such evidence — nothing is implemented, and the
measurements behind them were taken under a synthesized qualification identity
that authenticates nothing. Q16's claims *do* carry evidence and would validate
as `:accepted`, but acceptance is a governance act, so the record is offered for
promotion rather than asserting it.

### Regeneration and gate notes

Adding a record requires three derived artifacts to be regenerated, or the branch
gate fails:

```sh
clojure -M:abc/adr-governance --write-index   # docs/adr/INDEX.md
clojure -M:abc/diagrams                       # docs/adr/adr-graph.mmd
clojure -M:abc/adr-governance                 # validate; exit 1 on any problem
```

A narrative file `docs/adr/<slug>.md` is mandatory: `narrative-problems` reports
both missing narratives and orphan files.

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
`plans/2026-07-27-q15-region-partition.md`. The streams below are independent of
it, and each still needs its own scoped plan before code changes. **Q13 should be
settled first**: dropping the `NodeSpans` path removes `analyze.rs`,
`aggregate.rs`, two schemas and four test files from the partition change set.

1. **Q13: scoped 2026-07-27 — see
   `plans/2026-07-27-q13-node-span-coverage.md`; awaiting an owner selection
   among three options.** The traced diagnosis is stronger than the naming
   objection first recorded: parser-IR node spans are running offsets over
   emitted visible text, the emitter hard-codes their label to `decoded_utf8`,
   `sentences.rs` uses that string for the visible-text projection, and
   `analyze.rs` reads it as the decoded source file. The published number is
   approximately visible-text bytes ÷ decoded-source bytes. It costs 9.80 ms/work
   and is authoritative for no predicate. The plan recommends dropping it, and
   the diagnosis is **confirmed through the built binary** (governed corpus
   0.5814 / 0.4310 / 0.3289 with `covered_eligible_bytes` = 25 for all three
   despite sources of 43/58/76 bytes; one real work at 0.8646 where the decoded
   source under a node's span matches that node's text 7 times in 8,137). Every
   record returned `status: "ok"` with no errors. The probe also found that
   parser-IR mixes two coordinates in one document, both labelled `decoded_utf8`
   — the `source-note` arm emits genuine source spans while every other arm emits
   accumulator offsets — which is a parser-IR emitter question, not Q13's.
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
