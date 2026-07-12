# Drift-Sidecar Interplay Simulation Design

Date: 2026-07-12
Status: Approved (design approved in session; spec pending user review)

## Problem

ADR 0022 added the drift-participant update check to the upstream history
audit: `aozora-history-audit --drift-persons-dir DIR` validates the drift
sidecars under `DIR` (`_events/`, `_indexes/`) and reports a
`drift_participant_updates[]` entry for every drift participant whose
generated person record was added, removed, or whose `person_record_hash`
changed between the audited corpus snapshots.

That path is implemented (`abc.tools.aozora-history-audit/drift-participant-updates`)
and unit-tested with hand-built sidecars
(`test/abc/tools/aozora_history_audit_test.clj`), but the aozora evolution
simulation harness (spec
`2026-07-11-aozora-evolution-simulation-testing-design.md`) never exercises
it: no generated history is ever audited with a `--drift-persons-dir`, so
the *interplay* between evolving upstream state and accepted drift sidecars
has no generative coverage. This was the explicitly deferred "drift-sidecar
interplay" Future Work item.

## Scope decisions (user-resolved)

- **Sim coverage only.** No production behavior change. Anything the
  properties surface becomes a known-divergences table entry (D7+),
  triaged as separate bug/decision work — the same protocol as D1–D6.
  The ADR 0022 "pre-overwrite guard" production idea stays future work.
- **Intent-derived lifecycle.** Sidecars are authored from the *applied*
  forced drift intent in a generated history — the realistic editorial
  workflow (upstream drifts → editor records the accepted event → later
  upstream edits to participants must surface in the audit).
- **Sampled invalid-sidecar faults.** A small representative sample of
  `abc.tools.person-drift` failure codes is injected to assert the
  fail-loud-before-report contract; exhaustive per-code coverage stays in
  `person_drift_test.clj`.

## Architecture decision: observer-layer module

Sidecars are ABC-editorial artifacts, not upstream state. The sim model
(`abc.sim.model`) is ground truth for *upstream* evolution and stays
untouched; so do the generators (`abc.sim.gen`). The fold's applied-intents
log already records each forced drift event with its pids — exactly what an
editor adjudicates. A new test-side namespace authors a sidecar *from the
applied intent*, and properties run the existing audit entry points
(`audit!`, `scan-history!`) with `:drift-persons-dir`.

Rejected alternatives:

- **Model-integrated sidecars** (an `:author-sidecar` model event): would
  complect the editorial layer into upstream ground truth, and the sidecar
  would ride the shrinking machinery for no benefit (one static sidecar per
  run).
- **More hand-built unit cases**: already exist; the gap is generative
  interplay with evolving histories.

## Key facts the design rests on (verified)

1. `person_record_hash` is a pure function of the person's own CSV fields.
   `aozora_ingest/build-person-record` whitelists person body keys plus
   `external_links`; `person-record/canonical-identity-form` drops only
   `source_csv_provenance`. Work attachments never enter the hash. So the
   oracle predicts `change_type` from the model alone — no hash prediction
   needed.
2. Generated corpora contain exactly the *attached* persons (the CSV has
   one row per work-contributor pair), i.e. the sim's existing
   `oracle/projection`. Presence in the projected endpoint states decides
   added/removed.
3. `validate-drift-events!` never checks participant `person_record_hash`
   values against any corpus (pattern check only), so authored sidecars may
   carry deterministic placeholder hashes.
4. On invalid sidecars, `drift-index-map` throws
   `ex-info {:persons-dir … :failures …}` before any participant-update
   reporting (ADR 0022 acceptance criterion).
5. `scan-history!` re-validates the sidecar dir per pair
   (`drift-participant-updates` runs inside each `pair-report`); the
   summary key is `"drift_participant_updates"` (sum of per-pair counts).

## Components

### 1. `abc/test/abc/sim/sidecar.clj` (new, test-side, pure + writer)

Authoring:

- `placeholder-hash [person-id]` →
  `(hash/format-sha256 (hash/sha256-json-jcs person-id))` — well-formed,
  deterministic, corpus-independent (fact 3).
- `event-for-intent [intent]` — takes an applied intent from the fold log
  (`{:intent :clean-split|:clean-merge :event e …}`) and returns a *valid*
  drift event map:
  - `drift_event_type`: `"split"` for `:clean-split`, `"merge"` for
    `:clean-merge`.
  - Participants: for split, source pid with snapshot id `pre-<pid>` and
    each target pid with `post-<pid>`; for merge, each source pid `pre-`
    and the target pid `post-`. Participants sorted by `snapshot_id`
    (lexicographic; note `post-…` sorts before `pre-…`). Each participant
    carries `person_id`, `placeholder-hash`, `snapshot_id`.
  - `prov.used` = sorted `pre-` snapshot ids; `prov.was_generated_by` =
    sorted `post-` snapshot ids; `qualified_association` uses the example
    constants (`agent` `https://w3id.org/abc/agents/editorial-board`,
    `had_role` `abc:DriftEditor`).
  - `evidence`: `["https://example.org/abc/drift-evidence/sim-<type>"]`.
  - `date`: `"2026-07-12"` (fixed).
  - `schema_id`/`schema_hash`: live values (`drift/event-schema-id`,
    `manifest/schema-hash drift/event-schema-path`).
  - Finished with `drift/materialize-event-id`.
- `indexes-for-event [event]` — one index map per distinct participant
  `person_id`: `{person_id, drift_event_ids [id], schema_id, schema_hash}`
  with live index-schema values.
- `write-sidecars! [dir event]` — writes `_events/<drift_event_id>.json`
  and `_indexes/<person_id>.json` with the deterministic JSON writer used
  by `aozora_history_audit_test.clj`.

Fault injection — `corrupt [dir event fault]` rewrites the already-written
sidecar dir per fault keyword (each maps to a documented
`person-drift/failure-codes` member):

- `:schema-hash-mismatch` — event `schema_hash` replaced with
  `sha256:000…0`.
- `:orphan-event-file` — delete all index files.
- `:index-target-missing` — add an extra index file for an unrelated pid
  whose `drift_event_ids` names a nonexistent event id.
- `:participants-not-sorted` — reverse the event's `participants` array
  (content-hash id mismatch is not validated, so only the sortedness
  coherence failure fires).

### 2. Oracle extension (`abc.sim.oracle`)

`expected-participant-updates [prev cur participant-pids]` → sorted vector
of `{person_id, change_type}`:

- pid attached in `(projection prev)` only → `"removed"`.
- pid attached in `(projection cur)` only → `"added"`.
- attached in both with **equal model person maps** → no entry.
- attached in both with differing maps → `"hash_changed"`.

This relies on render/parse being injective on the model's person fields —
the same assumption the existing metadata-correction properties already
make. A violation shows up as a property failure, which is the point.

Null-ness contract (asserted alongside, from ADR 0022): `removed` ⇒
`current_hash` null and `previous_hash` matching `^sha256:[0-9a-f]{64}$`;
`added` ⇒ mirrored; `hash_changed` ⇒ both non-null hex-pattern hashes and
`previous_hash ≠ current_hash`. Every entry's `drift_event_ids` equals the
authored event's id in a one-element vector.

### 3. `abc/test/abc/sim/drift_sidecar_sim_test.clj` (new)

All properties assert **desired** behavior. Divergences found go into
`abc.sim.divergences/table` as D7+ `:open` entries wrapped with
`div/expected-failure`, exactly like D1–D6.

- **P15.lifecycle** (generative; run for `forced :clean-split` and
  `:clean-merge`; sizes like P10: `{:length [2 4] :works [3 5]}`, 10 runs
  per variant, harness seeds/soak unchanged):
  1. Generate a history with the forced drift intent; append one extra
     `:edit-person` event on the first successor pid (field
     `:family_name`, value `"変"` — outside the generator name pool) so
     `hash_changed` coverage is guaranteed when the successor survives
     attached to the end; the model's no-op totality keeps the append
     harmless otherwise.
  2. Fold; author the sidecar from the applied forced intent
     (`sgen/find-applied`); write sidecars to a temp dir. If shrinking
     yields a history where the forced intent was not applied
     (`find-applied` returns nil), the property passes vacuously — there
     is no adjudicated event for an editor to record.
  3. Locate the forced event's position in the history's event vector
     (equality match on the applied intent's `:event`; the states vector
     has one entry per event, so state index = event index + 1). Commit
     three states: `s-before` (immediately before the forced event),
     `s-after` (immediately after it), and `s-final`. Run `audit!` with
     `:drift-persons-dir` over two explicit windows:
     - **post-event window** (`s-after` → `s-final`) — the stated
       lifecycle: accepted event first, participant edits later. The
       pre-pid is absent from both endpoints (no entry); the appended
       successor edit surfaces as `hash_changed`.
     - **spanning window** (`s-before` → `s-final`) — the retrospective
       case: the audit window straddles the original drift. The pre-pid
       reports `removed`; surviving successors report `added`.
  4. For each window, assert the reported `drift_participant_updates`,
     projected to `{person_id, change_type}`, equals the oracle's
     expected set for that window's endpoint states exactly (exact
     equality doubles as the quietness assertion for untouched
     participants), plus the null-ness contract and `drift_event_ids` on
     every entry.
- **P15.localization** (deterministic, `scan-history!`): states
  `[m0, edit-000002(m0), edit-000001(·)]` with monotone instants; sidecar
  authored for a synthetic split whose `pre-` participant is `000001` and
  whose `post-` participant is a never-ingested fresh pid. Expected: pair 1
  reports no updates; pair 2 reports exactly one `hash_changed` entry for
  `000001`; the never-present post pid yields no entry anywhere; summary
  `"drift_participant_updates"` = 1 = sum of per-pair counts.
- **P15.invalid-sidecar** (deterministic loop over the four faults): clean
  two-commit pair + corrupted sidecar dir. Contract: both entry points
  converge on the same `drift-index-map` validation boundary, so the full
  fault sample pins the shared boundary through `audit!`, and a single
  representative fault pins `scan-history!` propagation — exhaustive
  duplication across entry points adds no evidence. Concretely: all four
  faults through `audit!` must throw a clean `ex-info` carrying
  `:persons-dir` and `:failures`, and never a forbidden class
  (`NullPointerException`, `AssertionError`, `StackOverflowError`, raw
  `java.util.zip.ZipException`); one fault (`:schema-hash-mismatch`) is
  additionally driven through `scan-history!` to pin that the per-pair
  re-validation path (fact 5) is equally loud rather than absorbing the
  error into a pair entry.
- **P15.rerun** (deterministic): with a valid sidecar present, a reused
  `:work-dir` yields the same `oracle/semantic-report` as a fresh one —
  extends P14's hygiene contract to the sidecar path
  (`drift_participant_updates` entries carry no locator fields, so
  `semantic-report` retains them).

### 4. Suite wiring

None needed. The `-sim-test$` namespace pattern auto-includes the new test
namespace in the `:simulation` kaocha suite; CI seeds `[42 4242 424242]`
and `just sim-soak` apply unchanged. Expected added runtime: seconds
(small histories, P10-sized repos).

## Error handling

The properties themselves encode the two-tier taxonomy: sidecar-source
faults are loud `ex-info` with required keys (`:persons-dir`, `:failures`);
nothing in the sidecar path may absorb them into a normal-looking report;
forbidden exception classes are asserted even inside divergence-gated
cases so a wrong-behavior regression cannot hide behind an open divergence.

## Non-goals

- No production changes to `aozora_history_audit.clj`, `person_drift.clj`,
  or ingest. If a property demands one, it lands as a D7+ divergence and a
  separate fix cycle.
- No `--fail-on-drift-participant-updates` CLI exit-code property — the
  flag is CLI-layer logic already unit-tested; the sim asserts the counts
  the flag reads.
- No sidecar-absent property — every existing sim test already runs the
  audit without a sidecar dir, and the empty-list case is unit-tested.
- No coverage of `abc-…` local successor ids: in the sim, upstream itself
  performs the split/merge, so successors are numeric upstream pids. The
  real-world `abc-` successors never appear in generated corpora and would
  only ever produce no-entry rows; the never-present post pid in
  P15.localization covers that shape.
- No arbitrary-participant sidecar generator (scope decision above).

## Acceptance criteria

- `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` green from
  `abc/` with the checked-in seeds, including the four new P15 tests.
- P15.lifecycle passes for both forced variants with exact-set equality
  against the oracle on both audited windows (post-event and spanning),
  covering `hash_changed`, `removed`, and `added` between them.
- P15.invalid-sidecar demonstrates loud, clean failure for all four
  sampled faults through `audit!`, and for the representative
  `:schema-hash-mismatch` fault through `scan-history!`.
- No production namespace is modified; `abc.sim.model` and `abc.sim.gen`
  are unmodified.
- Any divergence discovered is recorded in `abc.sim.divergences/table`
  (D7+) with an `:open` status and a dated case description, and the
  gating test asserts desired behavior via `div/expected-failure`.

## Future work

- Pre-overwrite guard reuse of the participant-update check in a managed
  corpus command (needs its own ADR-level decision).
- Content-side evolution simulation (unchanged from the parent spec's
  Future Work).
