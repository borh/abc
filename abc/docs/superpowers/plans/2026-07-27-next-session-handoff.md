# Handoff: Parser-RQ Corpus Tiering (2026-07-27)

This is an operational checkpoint for the next session, not an implementation
plan and not a new source of governance authority. The governing design remains
`docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`;
hand-authored records in `docs/adr/decisions.edn` become authoritative once
accepted.

> **Stop condition:** Do not implement Q15 until the owner chooses the coordinate
> represented by `source_span_coverage`. In particular, do not add a
> `publication_metadata` policy rule as the frame fix: the ledger never lexes
> those bytes, so that rule cannot change the result.

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

3. Treat the later Q15 rescope as the current diagnosis. Before using the design
   as an implementation source, reconcile the stale passages named under
   *Known contradiction in the design* below.

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

## Owner decision: Q15 source coordinate

The traced mechanism is:

- `ab-aozora-capture/src/classified_source.rs::build_ledger` lexes
  `decoded.span_text`;
- `decoded.span_text` is the `aozora_body_range` body projection;
- the recognition instrument currently takes `eligible_bytes` from the entire
  decoded file.

The result divides a body-derived numerator by a whole-file denominator. The
owner must choose which contract the predicate is meant to express:

### Option A — body-projection coverage

Make the eligible region exactly the projection supplied to the parser. Header
and tail intervals must come from `aozora_body_range`, never from separator or
`底本：` heuristics maintained elsewhere.

This is the direction proposed by the latest Q15 text. It is a principled
denominator reduction because it removes bytes outside the parser's input
coordinate, not unrecognized bytes inside that coordinate. It also changes the
meaning of `source_span_coverage = 1.0` from whole-file coverage to body-input
coverage and therefore requires an explicit owner decision.

This is **not** a taxonomy-data-only edit. The v1 schema requires `rules` to be
empty and the Rust taxonomy reader rejects non-empty rules. More importantly,
the release-authoritative recognition path hard-codes the eligible interval to
the complete decoded source, and `RecognitionInput` does not accept the taxonomy
whose rules would justify exclusions. The older node-span accountability path
also requires empty ignored regions and whole-file eligibility. Adopting this
option therefore needs an authenticated taxonomy input and a versioned
instrument/protocol change across at least:

- `abc/schemas/parser-rq-ignored-regions.schema.json`
- `abc/data/parser-rq-ignored-regions-v1.json`
- `ab-validator/crates/ab-parser-rq-source-accountability/src/main.rs`
- `ab-validator/crates/ab-parser-rq-source-accountability/src/recognition.rs`
- `ab-validator/crates/ab-parser-rq-source-accountability/src/recognition_aggregate.rs`
- `ab-validator/crates/ab-parser-rq-source-accountability/src/analyze.rs`
- `ab-validator/crates/ab-parser-rq-source-accountability/src/aggregate.rs`
- `abc/schemas/parser-rq-source-recognition-work.schema.json`
- `abc/schemas/parser-rq-source-recognition-index.schema.json`
- `abc/schemas/parser-rq-source-recognition-aggregate.schema.json`
- `ab-validator/crates/ab-parser-rq-source-accountability/tests/recognition.rs`
- `ab-validator/crates/ab-parser-rq-source-accountability/tests/recognition_corpus.rs`
- `ab-validator/crates/ab-parser-rq-source-accountability/tests/aggregate.rs`
- `ab-validator/crates/ab-parser-rq-source-accountability/tests/analyze_work.rs`
- `abc/test/abc/tools/parser_rq_source_accountability_test.clj`
- `abc/test/abc/tools/validate_design_bundle_test.clj`

Write a dedicated implementation plan after this option is accepted; do not
infer a v1 wire shape or an ambient taxonomy dependency from the current empty
taxonomy.

### Option B — whole-file coverage

Keep the denominator as the complete decoded file and extend classified-source
capture so header and tail produce attributable facts in the same coordinate as
body facts. This preserves the whole-file reading of the predicate and avoids a
denominator reduction, but it broadens the instrument beyond the parser's body
input and requires explicit roles, dispositions, spans, and tests for packaging
metadata.

A policy rule alone is insufficient under this option too: facts for the
currently unlexed bytes must first exist.

### Acceptance conditions for either option

- Numerator facts and denominator intervals inhabit one declared coordinate.
- Header and tail boundaries have one authority: `aozora_body_range`.
- Works without two separator lines or without a `底本：` line are covered by
  tests.
- The governed three-work corpus remains the control and returns its expected
  result.
- A real-source sample is re-measured through the built
  `ab-parser-rq-source-accountability` binary.
- If ignored regions exist, the exact taxonomy content that derives them is
  authenticated into every work record and aggregate that consumes them.
- The affected policy/schema hashes and `qualification_identity_ref` rotate
  through their intended coordinates; `predicate_set_hash` moves only if the
  predicate contract itself changes.
- Old evidence is rejected as stale or protocol-incompatible, as appropriate.

## Known contradiction in the design

**Severity: blocker for implementation from the design.**

The later *Q14 mechanism, corrected again* and Q15 *Open Questions* entry contain
the traced diagnosis above. Earlier text still says the frame is fixed by adding
`publication_metadata`, `warichu_close`, `framed_close`, and a `底本` rule. The
stale instructions remain in at least:

- the opening *Status* discussion of Q14/Q15;
- the Q14 “Part A” sequence;
- *What each open item blocks*, row Q15;
- *Current blocker status*, B5;
- *Governance Path*, step 1.

Those passages also say only two close-marker rules are missing. The traced
scope is broader: `node_policy` maps only `DirectiveKind::Unknown` and
`DirectiveKind::WarichuOpen`; the other **twelve** variants return `None`.

Reconcile the design before or in the same reviewed change that records the Q15
decision. Preserve the superseded diagnosis as history only if it is clearly
marked and cannot be mistaken for current instructions.

## Decisions and governance records still owed

`decisions.edn` is edited by hand, never generated. Keep these records distinct
so that decision, implementation, and measurement do not collapse into one
event:

| Record | Current state | Required action |
|---|---|---|
| Q11 | Decided | Record tier 2 as the publication's successfully-selected projection, named the *publication-workload snapshot*, with `candidates_considered`, `derive_failures`, and `rejected` declared |
| D7 | Decided | Record `unexpected-fatal-failures <= 0` and its safe initial expected-outcome vocabulary |
| Q14 | Decided | Record “fix the instrument before setting the residual threshold”; do not preserve the superseded causal claim |
| Q16 | Decided and implemented | Record the classified-source policy as a bound source-recognition authority |
| Instrument-binding amendment | Implied by Q16 | Amend `parser-release-instrument-bindings` so its documented authority closure matches the code |
| Q15 | **Owner decision required** | Record Option A or B before implementation |

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

## Work available before Q15 is decided

These streams do not require choosing the source coordinate, but each still
needs its own scoped plan before code changes:

1. **Q13:** drop `:parser_ir_node_span_coverage` or rename it only after defining
   the quantity it actually measures. It costs 9.80 ms/work and is authoritative
   for no predicate; its current ratio-like name invites the exact
   misinterpretation that exposed D6.
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

Once the owner decision is recorded:

1. Write a task-by-task implementation plan for the selected coordinate.
2. Characterize the current three-work control and add failing boundary tests
   before changing the instrument.
3. Align numerator and denominator using the selected contract.
4. Map all twelve currently unmapped `DirectiveKind` variants in
   `ab-validator/crates/ab-aozora-pipeline/src/fold.rs::node_policy`, adding the
   required `ConstructId` values and classified-source policy rules. Treat this
   as a separate reviewable deliverable from the coordinate change.
5. Rebuild because the classified-source policy is `include_bytes!`-embedded and
   `parser-rq-classified-source-authority-v1.json` fails closed on
   `raw_bytes_hash` and `identity_hash`.
6. Re-run the governed control, then re-measure the real-source sample through
   the built accountability binary. The earlier ≈0.9889 estimate came from a
   disposable separator heuristic and is not an acceptance value.
7. Only after the instrument measures its declared contract, use an explicitly
   non-authoritative exploratory campaign to choose and predeclare any residual
   confirmatory threshold.

The exploratory and confirmatory campaigns must have distinct predicate-set
identities. A post-hoc threshold must not be presented as preregistered
confirmation.

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
