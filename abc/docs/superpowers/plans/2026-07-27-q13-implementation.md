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

Twenty-two artifacts under `docs/reports/parser-rq/runs/` carry the
`parser_ir.nodes[*].span` basis, six in the promoted run
`24d61fc7…`. Published manifests are immutable, so they are not rewritten.

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

1. **Characterize.** *(not started)* Capture the current per-work record, index, and aggregate
   for the governed three-work corpus through the built binary, and record their
   hashes. This is the before-image any later movement is attributed against.
   Add a test asserting the membership index authenticates and recognition
   succeeds — the property that must survive every later task.
2. **Delete the dead Clojure — DONE 2026-07-27.**
   `install-source-recognition-observation`,
   `derive-source-span-envelope`, `valid-aggregate?`,
   `source-accountability-identity-valid?`,
   `source-accountability-instrument-version`, and their tests. **No Rust, no
   schema, no wire change** — nothing outside `abc/src` and `abc/test` moves, and
   no artifact hash changes. Doing this first proves the enumeration: if
   something breaks here, the "zero production callers" finding was wrong, and
   that is worth learning before touching the wire format.
3. **Remove the coverage quantity from the Rust record.** Fields, basis enum,
   span union, conservation checks. Bump `WorkSchemaVersion`. Update
   `analyze_work.rs` and `aggregate.rs` tests.
4. **Apply the open scope decision** — eligibility and the P1 aggregate, per
   Option A or B.
5. **Freeze v1 and update the schemas.** Per the retention decision.
6. **Re-measure and record the rotation.** Re-run the governed control, confirm
   recognition still authenticates against the new membership index, and record
   the `membership_ref` movement explicitly as a protocol-incompatibility for
   prior captures.

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
just validate-migration
```

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
