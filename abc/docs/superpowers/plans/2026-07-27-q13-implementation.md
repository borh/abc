# Q13 implementation plan — retire parser-IR node-span coverage

Executes the decision recorded in
`docs/adr/parser-rq-retire-node-span-coverage.md` (`:proposed`). Diagnosis,
measurement, and consumer enumeration are in
`docs/superpowers/plans/2026-07-27-q13-node-span-coverage.md` and are not
repeated here.

Scope: remove the node-span coverage quantity. **Retain** the membership index
and the per-work authentication it carries, because the release-authoritative
recognition path depends on both.

> **Both open scope questions were settled 2026-07-27, taking the
> recommendations:** the per-work record also sheds its eligibility fields and
> the P1 aggregate (Option A), and the v1 schemas are retained frozen alongside
> v2. Both are recorded in `docs/adr/parser-rq-retire-node-span-coverage.md`.

## What does not move, and why that is worth stating first

The natural assumption is that retiring an instrument rotates the qualification
identity. **It does not.** Traced:

- `instrument_versions` is built from the predicate set —
  `(map (juxt (comp name :observed_key) :instrument) (:predicates predicates))`
  at `parser_rq_campaign.clj:274`. The nine `observed_key`s are
  `fatal_failures`, `wall_time_seconds`, `timeouts`, `source_span_coverage`,
  `silent_drops`, `diagnostic_completeness`, `parser_ir_schema_validation`,
  `publication_structure`, `peak_cgroup_memory_bytes`. **None names this
  instrument**, and `parser-rq-source-accountability-v1` is not among the six
  declared `:instrument` values.
- `instrument-policy-paths` (`parser_rq_campaign.clj:45`) is closed over
  `member-observed-keys` and has no source-accountability entry, so
  `instrument_policy_hashes` does not move.
- No predicate declaration changes, so `predicate_set_hash` does not move.

Therefore `qualification_identity_ref` **does not rotate**, and no capture
becomes stale *against the identity*.

There is a consequence of this worth noticing. `source-accountability-identity-valid?`
(`parser_rq_source_accountability.clj:149`) requires
`instrument_versions[:source_accountability]` to equal
`parser-rq-source-accountability-v1`. Since no predicate produces that key, a
campaign-built identity never carries it, so that gate can never pass in the live
path — which is the structural reason `derive-source-span-envelope` is reachable
only from tests. The dead code and the dead gate are the same fact seen twice.

**What does move is `membership_ref`.** It is the sha256 of the membership index
bytes, and `RecordIndexEntry` carries each work record's `sha256`. Shrinking the
work record changes those hashes, so the index bytes change, so `membership_ref`
changes — and it is recorded into every recognition record
(`recognition_corpus.rs:205`, `:271`). Prior captures therefore become
**protocol-incompatible**, not merely stale, and that is the real migration cost.

## Traced change surface

### Rust — `ab-parser-rq-source-accountability`

| File | Change |
|---|---|
| `src/model.rs:27` | Delete `CoverageBasis` (single variant `NodeSpans`) |
| `src/model.rs:130` | `WorkRecord`: drop `coverage_basis`, `covered_eligible`, `uncovered_eligible`, `covered_eligible_bytes`, `uncovered_eligible_bytes` |
| `src/model.rs:15` | Bump `WorkSchemaVersion` to a v2 wire string |
| `src/analyze.rs:100-133` | Delete node-span collection, `normalize`/`subtract` union, and the coverage conservation checks. **Keep** the parser-IR `schema_id`/`schema_hash` and `derived_from` validation at `:70-99` |
| `src/analyze.rs:182-183` | Drop `coverage_basis` from the emitted record |
| `src/aggregate.rs` | See *Open scope decision* |
| `src/main.rs` | Adjust `AnalyzeWork`/`AnalyzeCorpus`/`Aggregate`/`CaptureCorpus` wiring |
| `tests/analyze_work.rs` | 6 coverage references |
| `tests/aggregate.rs` | 18 coverage references |

**Stays untouched.** `src/interval.rs` and `src/reconcile.rs` — `Interval` is used
by `recognition.rs`, and `reconcile` is called by
`ab-parser-rq-diagnostic-authorization/src/aggregate.rs:156` over the **R1**
recognition record's semantic gaps (`r1.semantic_gap_bytes`), not over node-span
intervals. `silent_drops` is unaffected; that crate contains no reference to
`coverage_basis`, `uncovered_eligible`, or `NodeSpans`.

`RecordIndex` (`model.rs:191`) is **already coverage-free** — entries carry
`work_id`, `sha256`, `bytes`, `media_type`, `locator`. Its shape need not change;
only the hashes inside it do.

Four of the twelve Rust test files already touch no coverage —
`corpus_index.rs`, `fixture_capture.rs`, `hegel_smoke.rs`, `identity.rs` — so the
membership and authentication paths are already covered independently of the
quantity being removed. That is the evidence that the retained half stands on its
own.

### Schemas — `abc/schemas`

- `parser-rq-source-accountability-work.schema.json` — remove the coverage
  properties, bump `schema_version`.
- `parser-rq-source-accountability-index.schema.json` — likely unchanged.
- `parser-rq-source-accountability-aggregate.schema.json` — see *Open scope
  decision*.

### Clojure — `abc/src`

| Symbol | File | Action |
|---|---|---|
| `install-source-recognition-observation` | `parser_release_qualification.clj:190` | Delete; 0 production callers |
| `derive-source-span-envelope` | `parser_rq_source_accountability.clj:180` | Delete; 0 production callers |
| `valid-aggregate?` | `parser_rq_source_accountability.clj:155` | Delete; only caller is the above |
| `aggregate-schema` | `parser_rq_source_accountability.clj:15` | Delete with the aggregate |
| `source-accountability-identity-valid?` | `parser_rq_source_accountability.clj:149` | Delete; only caller is the above |
| `source-accountability-instrument-version` | `parser_rq_source_accountability.clj:55` | Delete |

Tests: `parser_release_qualification_test.clj:140` (one demotion assertion) and
`parser_rq_source_accountability_test.clj` (nine `derive-source-span-envelope`
references). `validate_design_bundle.clj:509,575` lists the aggregate schema in
its closure check and follows the aggregate's fate.

## Open scope decision — does the record also shed eligibility?

After the coverage fields go, `WorkRecord` still carries `ignored`, `eligible`,
`ignored_bytes`, `eligible_bytes` — a declared whole-file eligibility with
nothing measured against it, and an `ignored` list that is always empty because
the v1 taxonomy's `rules` are empty.

**Option A — reduce P1 to a provenance-and-authentication record (recommended).**
Also drop `ignored`/`eligible` and their byte counts, and delete the P1 aggregate
(`aggregate.rs`, its schema, `source-accountability-aggregate.json`, and the
Clojure `aggregate-schema`/`valid-aggregate?` pair). What remains is: which
source, which parser-IR, which diagnostics, which taxonomy, validated against
which identity, plus status and errors. A coherent thing with a name that
describes it.

This also makes true a claim the Q15 plan already relies on — that retiring this
path removes `analyze.rs`'s whole-file eligibility from what Q15 task 3 must
migrate. Under Option B it stays, and Q15 inherits it.

**Option B — remove coverage only.** Smaller diff, keeps `eligible`/`ignored` and
a reduced aggregate. But it leaves a denominator with no numerator and hands Q15
a migration this plan could have retired.

**Decided 2026-07-27: A.** The eligibility fields exist to support a coverage
computation that is going away; keeping them preserves the shape of a measurement
without the measurement. Q15 will declare region eligibility on its own terms,
and it should not inherit a whole-file one it has to undo. Retain
`taxonomy_version` and `taxonomy_hash` regardless — they record which taxonomy
was in force, which is provenance rather than measurement.

## The v1 retention question — decide before task 1

Measured 2026-07-27: **18** artifacts under `docs/reports/parser-rq/runs/`
carry a v1 P1 wire version — 9 work records, 3 aggregates, 6 indexes, exactly 6
per run across three runs, one of them the promoted `24d61fc7…`. The 9 work
records are the ones carrying the `parser_ir.nodes[*].span` basis. The
"twenty-two" figure carried by earlier drafts was unmeasured and wrong.
Published manifests are immutable, so none are rewritten.

**Decided 2026-07-27: retain the v1 schemas as frozen alongside v2.** Deleting them
would leave published evidence that nothing can validate, which is the outcome
the decision record explicitly forbids. Freezing them costs three files that
never change again and keeps the historical captures checkable.

The alternative — declaring those artifacts protocol-incompatible and dropping
v1 — is only defensible if some other check still authenticates them. Establish
that before choosing it, do not assume it.

Note the Rust wire enums are single-variant (`WorkSchemaVersion { V1 => … }`) with
`deny_unknown_fields`, so a v2 reader cannot read a v1 document and vice versa.
That is the desired fail-closed behaviour; it is also why the version string must
actually change rather than the fields quietly disappearing.

## Task sequence

Each task is separately reviewable and separately committable.

1. **Characterize — DONE 2026-07-27.** Record the current per-work record, index
   and aggregate for the governed three-work corpus, and add a test asserting
   the membership index authenticates and recognition succeeds — the property
   that must survive every later task. See *Task 1 result*.
2. **Delete the dead Clojure — DONE 2026-07-27.**
   `install-source-recognition-observation`,
   `derive-source-span-envelope`, `valid-aggregate?`,
   `source-accountability-identity-valid?`,
   `source-accountability-instrument-version`, and their tests. **No Rust, no
   schema, no wire change** — nothing outside `abc/src` and `abc/test` moves, and
   no artifact hash changes. Doing this first proves the enumeration: if
   something breaks here, the "zero production callers" finding was wrong, and
   that is worth learning before touching the wire format.
3. **Remove the coverage quantity from the Rust record — DONE 2026-07-27**,
   in one commit with task 4. See *Tasks 3 and 4 result*.
4. **Apply the open scope decision — DONE 2026-07-27.** Option A.
5. **Freeze v1 — DONE 2026-07-27**, ahead of tasks 3 and 4 rather than after
   them. Freezing is purely additive, so doing it first means published v1
   evidence is validatable at every commit; doing it last would leave a window
   where it was not. See *Task 5 result*.
6. **Re-measure and record the rotation — DONE 2026-07-27.** See *Task 6
   result*.

**Task 2 is the one to do first even if the rest stalls.** It is pure deletion
with no wire consequences, and it removes the two functions that would otherwise
keep reading a quantity nobody should trust.

## Verification

Per task, plus the full gate before the branch closes:

```sh
just comment-hygiene
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./abc#checks.x86_64-linux.parser-rq-p5-promotion-audit
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
(cd ab-validator && cargo test --workspace)
just validate-migration
```

**Running the Rust tests was missing from this list and its absence cost a
regression.** Task 2 deleted `abc/test/fixtures/parser-rq/source-accountability`
as orphaned; it was not, because `tests/fixture_capture.rs` reads that directory
as its committed witness set. Nothing in the gate caught it: `cargo-check`,
`cargo-clippy` and `cargo-fmt` do not execute tests. Restored in `c993b685`.

**And the obvious fix does not work.** `nix build
./ab-validator#checks.x86_64-linux.cargo-test` cannot build at all — verified
2026-07-27 by stashing this branch's changes and building it on the unmodified
tree, where it fails identically. `ab-aozora-capture` `include_bytes!`s four
files from `abc/` (`classified_source.rs:21-27`) and the `ab-validator` flake
source does not stage `abc/`, so the derivation fails at compile. That is why
`just validate-migration` reaches `cargo-test` only through `flake check
--no-build`, which evaluates it without building: the check is registered but
has never been runnable. So the gate step is `cargo test --workspace` run
directly, which does work.

Worth stating plainly, because it is the more general lesson: a check that is
registered, evaluated, and never built is indistinguishable from a passing check
when you read the gate output.

Two lessons, both cheap to apply and both about this plan rather than the code.
A consumer enumeration scoped to one language misses cross-language consumers —
a Rust integration test reading a path under `abc/test` is exactly the shape
that hides from a Clojure-side search. And a verification list that omits the
one check which runs the tests will report green while a test is broken.

`parser-rq-p5-promotion-audit` is the one to watch. It verifies the **current**
promotion against `decisions.edn` and reads the published projections in
`docs/reports/`; it is the check most likely to notice if task 5's retention
choice is wrong.

`validate-migration` runs `nix build --rebuild`, which errors rather than builds
when a derivation has no prior output. Any Rust change here needs
`./ab-validator#ab-aozora` and `#ab-aat-to-parser-ir` built once first.

## Risks

- **The enumeration is a claim about the code as it stands.** Task 2 is designed
  to falsify it cheaply if it is wrong.
- **`membership_ref` rotation is the migration**, and it is easy to under-state:
  every recognition record embeds it, so every published recognition artifact
  from before this change describes a membership index that no longer exists.
  This must be declared, not discovered.
- **The mixed-coordinate emitter finding is not fixed by this plan.** Claim c7 of
  the decision record carries it. Retiring the consumer removes what silently
  unioned across two coordinates; it does not make the parser-IR emitter's
  coordinate determinate, and no task here does.
- **Do not fold Q15 into this.** Region eligibility is Q15's; this plan removes a
  quantity and, under Option A, an eligibility that Q15 would otherwise have to
  undo. It does not declare a new one.

## Status

Written 2026-07-27 against a recorded decision. Both scope questions settled.
**Task 2 done**; tasks 1 and 3–6 not started.

### Task 1 result

**The before-image is the promoted run, not a fresh probe.** Run
`24d61fc7…`, capture `e27fa29f…`, lane `source` — an immutable published
artifact set, which is a stronger baseline than anything re-derived under a
synthesized identity, because it is the state the release actually qualified
against.

| Coordinate | Value |
|---|---|
| `identity_ref` | `sha256:8c1716f0…d960d366` |
| membership index sha256 | `746b9989…6d2e78` |
| `membership_ref` in every recognition record | `sha256:746b9989…6d2e78` |
| per-work record bytes | 1,524 for each of `000001_1`, `000002_2`, `000003_3` |
| P1 aggregate | `eligible_bytes` 177, `covered_eligible_bytes` **75**, `uncovered_eligible_bytes` 102 |

`membership_ref` is the sha256 of the index file verbatim — confirmed by hashing
the committed file — so the rotation task 6 must record is exactly the movement
of that one digest.

The aggregate confirms the characterization at run scale without re-running
anything: 43 + 58 + 76 = 177 source bytes, and 3 × 25 = **75** covered. The
numerator is three identical 25-byte contributions from works of three different
sizes.

The second baseline is the fixture witness set restored in `c993b685` —
`abc/test/fixtures/parser-rq/source-accountability`, two works, `eligible_bytes`
9 and `covered_eligible_bytes` 9. That one is checked byte-for-byte by
`tests/fixture_capture.rs` on every run, so it is the baseline that will *fail*
when tasks 3 and 4 land, and re-blessing it via `BLESS_PARSER_RQ_FIXTURE` is the
visible record of the shape change.

**The seam test the task called for did not exist and now does.**
`tests/membership_seam.rs`. Every existing recognition test drives
`analyze_recognition_corpus` from a **hand-written** membership index
(`tests/recognition_corpus.rs:119`), so no test asserted that a membership index
the production `analyze_corpus` actually produces is accepted. That is precisely
the seam this retirement stresses: the coverage fields leave the work record →
record hashes move → index bytes move → `membership_ref` rotates. Three
assertions:

- a real `analyze_corpus` index drives recognition to `Ok`, and
  `membership_ref` equals the sha256 of the exact bytes written to disk;
- a semantically identical but pretty-printed index is **rejected** —
  `membership_ref` is a claim about bytes, not about meaning;
- the index's own key set is provenance-only, so retiring coverage moves the
  hashes it lists without changing its shape.

Without this, tasks 3–5 could have shrunk the work record and broken the handoff
to recognition with nothing failing.

### Tasks 3 and 4 result

**Done as one commit, deliberately.** Task 3 bumps `WorkSchemaVersion` and task
4 removes eligibility; splitting them would have published an intermediate v2
carrying eligibility but no coverage — a wire version that never ships and that
the frozen-schema discipline would then owe a schema. One version bump, one
shape.

**`WorkRecord` v2 is 13 fields.** Gone: `coverage_basis`, `covered_eligible`,
`uncovered_eligible`, `covered_eligible_bytes`, `uncovered_eligible_bytes`,
`ignored`, `eligible`, `ignored_bytes`, `eligible_bytes` — and
`decoded_source_bytes`, which was not on the plan's list. It duplicated
`decoded_source.bytes`; it existed as the conservation denominator, and
`aggregate.rs` carried an explicit check that the two agreed. With the
conservation gone the duplicate is just a second copy of a number the record
already carries, so it went with them.

`coordinate_system` **stays**, and the reason is worth recording because it
looks like an oversight. It no longer qualifies any interval — the record
publishes none — but it does qualify `decoded_source.bytes`, which survives.
Same for the index, whose shape does not move at all.

**The node-span walk went with the quantity, not just the union.** v1 bounded
each span against the decoded source and could mark a work `unavailable` on
`node-span-out-of-bounds`, `node-span-coordinate-system-mismatch` and four other
codes. Those checks were the defect, not a casualty of removing it: parser-IR
spans are offsets into emitted visible text, which is shorter than its source,
so bounding them against the decoded file admitted everything. Parser-IR node
structure has its own instrument — `parser-rq-parser-ir-conformance-*` — and
that is where it belongs. What this instrument still authenticates about
parser-IR is its schema identity and its `derived_from` coordinates, untouched.

`node_spans_no_longer_affect_the_record_at_all` pins that: three parser-IR
documents — no nodes, spans with no declared coordinate, spans running past the
end of the source — now produce equal records. Two of the three were
`unavailable` under v1. `parser_ir` is excluded from the comparison and asserted
*different*, because the record still records which document it saw; that is
provenance, and it is the half being kept.

**Deleted:** `src/aggregate.rs`, `tests/aggregate.rs` (459 lines), the
`Aggregate` subcommand, the P1 aggregate from `capture-corpus` output,
`AggregateRecord`, `AggregateSchemaVersion`, `WorkCompleteness`, `WorkInterval`,
`WireInterval`, `NodeSpan`, `CoverageBasis`, and
`schemas/parser-rq-source-accountability-aggregate.schema.json`. `interval.rs`
and `reconcile.rs` stay: `recognition.rs` uses `Interval`, and `reconcile` is
called from `ab-parser-rq-diagnostic-authorization` over R1 semantic gaps.

**Two retirements are asserted rather than assumed.**
`the_retired_aggregate_subcommand_is_gone` invokes `aggregate --help` and
requires failure; the CLI-shape unit test now requires the same parse to be an
error where it previously required success. A subcommand can come back by
accident; a test that says it must not is cheap.

**One test was replaced rather than deleted.**
`aggregate_cli_resolves_index_locators_against_separate_store_root` tested a
property of the *index* — that it is written outside the store and every locator
it carries resolves and authenticates inside it — through the aggregate CLI,
which is now gone. The property outlives its old vehicle, so it is now asserted
against `analyze_corpus` directly.

**Fixture witnesses re-blessed.** `abc/test/fixtures/parser-rq/source-accountability`
loses `aggregate.json`; `index.json` and `manifest.json` move because the record
hashes inside them move. That movement is `membership_ref` rotating in miniature,
and it is the visible record of the shape change.

### Task 6 result

**Re-measured on the actual governed corpus under the actual promoted
identity**, not on a synthesized one. The inputs are all in-tree: the three
sources under `ab-validator/crates/ab-index/tests/fixtures/corpus/cards/`, the
qualification identity and corpus manifest from the promoted capture
`e27fa29f…`, and each work's parser-IR recovered from the predicate lane store
by the `parser_ir.sha256` its published v1 record names. Run through
`capture-corpus` on the release binary.

| Coordinate | Before (`24d61fc7…`) | After |
|---|---|---|
| `qualification_identity_ref` | `sha256:8c1716f0…d960d366` | **unchanged** |
| `membership_ref` | `sha256:746b9989…6d2e78` | `sha256:93e9dbd4…39c9f18` |
| work record bytes | 1,524 × 3 | 1,225 × 3 |
| P1 aggregate | `eligible` 177 / `covered` 75 | **not produced** |
| recognition | ok | **ok**, 3 records, no errors |

**The identity does not rotate.** That was traced in this plan and is now
observed: the same identity file yields the same `identity_ref` across the
change. Nothing becomes stale *against the identity*.

**`membership_ref` does rotate, and that is the migration.** It is recorded into
every recognition record, so every published recognition artifact from before
this change names a membership index that no longer exists. Prior captures are
**protocol-incompatible, not stale**: a v2 reader cannot read a v1 work record
and vice versa, so there is no reinterpretation available, only re-capture.

**Recognition still authenticates across the rotation** — the property task 1's
seam test was written to protect, here confirmed on the governed corpus rather
than a synthetic one.

**One finding worth separating out.** On this corpus the release-authoritative
recognition instrument reports `eligible_bytes` 177, `recognized_bytes` 177,
`semantic_gap_bytes` **0**. The retired P1 quantity reported 75 of 177 covered.
Same three works, same identity, same coordinate *name* — and a disagreement of
more than 2×, with no guard anywhere that could notice it, because the two
numbers were never compared. R1 is the one that is right. This is the clearest
statement available of what was being published.

**The cost saving is not claimed.** The acceptance condition requires the
9.80 ms/work figure to be re-measured if claimed, and it was not re-measured, so
it is not claimed. The governed corpus is 43–76 bytes per work, where process
startup dominates any span-union cost, and a meaningful measurement needs a real
work — which means regenerating parser-IR against the pinned `aozorabunko`
checkout. The retirement stands on the coordinate defect on its own; the cost
argument was always secondary, and an unmeasured saving is worth nothing here.

### Task 5 result

Done before tasks 3 and 4, not after. Freezing is purely additive, so taking it
first means published v1 evidence is validatable at every commit on the branch.
Taken last it would have left a window where it was not, which is the outcome
the decision record forbids.

Added `schemas/parser-rq-source-accountability-work-v1.schema.json` and
`-aggregate-v1.schema.json` — standalone copies with their own `$id`, not
`$ref` aliases. An alias would track whatever the live schema became, which is
the opposite of a freeze. Both registered in `validate_design_bundle.clj`'s
schema closure.

**The index is deliberately not frozen.** `RecordIndex` never carried a coverage
quantity, so its shape does not move; the live schema keeps validating both the
6 published v1 indexes and everything produced after the retirement. Freezing it
would assert a version boundary that does not exist.

**A frozen schema has no live producer to keep it honest.** That is precisely
how one drifts out of agreement with what it claims to validate, unnoticed, and
it is why the freeze needed evidence rather than just files. Two tests in
`parser_rq_source_accountability_test.clj`:

- `published-v1-evidence-still-validates-against-the-frozen-schemas` walks
  `docs/reports/parser-rq/runs/`, groups every JSON by its declared
  `schema_version`, and validates each against the schema for that wire version.
  It also asserts the counts — 9 work, 3 aggregate, 6 index — so that a run
  losing an artifact fails loudly instead of silently shrinking the validated
  set toward zero.
- `the-frozen-v1-schemas-are-frozen-copies-and-not-aliases` asserts each frozen
  document still *requires* the retired fields it exists to validate.

### Task 2 result

Deleted, with zero production callers as the enumeration predicted:
`install-source-recognition-observation`, `derive-source-span-envelope`,
`valid-aggregate?`, `valid-uncovered?`, `valid-taxonomy?`,
`source-accountability-identity-valid?`, `source-accountability-instrument-version`,
`aggregate-schema`, `taxonomy-schema`, `expected-locators`; the nine P1
span-envelope deftests and one demotion deftest; the helpers left dead by them;
and the orphaned `test/fixtures/parser-rq/source-accountability` fixtures.

`exact-display-ratio` and `valid-identity?` were **kept** — both are also used by
the recognition path. `write-blob!`, `capture`, `with-capture`, `aggregate-value`,
`taxonomy-text` and `taxonomy-hash` were kept for the same reason.

One surviving test needed rewriting rather than deleting.
`malformed-or-missing-recognition-evidence-cannot-fall-back-to-node-spans`
asserted that a P0-only capture yields `instrument-missing` from the recognition
envelope *while* the node-span envelope would have returned `0.9M`. With the
node-span envelope gone the comparison cannot be written, but the property is
still worth holding, so it now asserts the `instrument-missing` result alone and
is renamed `…-reports-instrument-missing`. The fallback is now structurally
impossible rather than merely rejected.

**Test counts moved as predicted, and the one surprise is explained.**
1,145 → 1,135 tests (ten deftests removed) and 11,754 → 11,727 assertions.
Deleting the orphaned fixtures dropped assertions again, 11,727 → 11,672, without
failing anything — because `active-parser-rq-surface-has-no-site-or-backup-policy`
scans every file under `abc/test` for each of 11 forbidden tokens, and the
directory held 5 files: 5 × 11 = 55. The scanned population shrank; no assertion
weakened. Worth knowing before reading a future assertion-count drop as coverage
loss.
