# ADR 0022: Upstream Ingest Drift Awareness

Status: Accepted
Date: 2026-04-30
Accepted: 2026-04-30
Validation scope: operational
Release authority: development

## Implementation Status

As of 2026-07-09, `abc.tools.aozora-history-audit` implements the live
drift-participant update report and failure gate while leaving
`abc.tools.aozora-csv` source-faithful. The audit contract is covered by
`test/abc/tools/aozora_history_audit_test.clj`.

## Context

ADR 0020 and ADR 0021 define person identity drift as validated audit
sidecars. Drift events do not flow into `manifest_identity_object`, and event
participants bind historical person snapshots by `person_id` plus
`person_record_hash`.

The realistic operational scenario is not authoring drift events during normal
ingest. The normal path is:

1. Aozora upstream changes.
2. ABC ingests the previous and current upstream refs into generated corpus
   snapshots.
3. `aozora-history-audit` compares those snapshots and flags split or merge
   candidates only when contributor-edge evidence exists.

Once an accepted drift event exists, a later upstream change may still edit a
person who participates in that event. The current ingester is source-faithful:
it rebuilds person files from the upstream CSV and has no reason to know that
`examples/v0/example-persons/_indexes/<person_id>.json` exists elsewhere. That
is correct for raw ingest, but it leaves one review gap: an upstream update can
change a drift participant's generated `person_record_hash` without producing a
split/merge candidate. That should be surfaced during the after-upstream audit.

Automatically rewriting Aozora contributor identities during ingest is out of
scope. The drift model is an audit and lineage layer, not an identity
normalizer. If upstream still emits one numeric person ID where ABC has a local
split, the ingester must preserve what upstream said; the audit must tell the
maintainer that this update touches drift-sensitive identity state.

## Decision

Add a drift-participant update check to the upstream history audit path.

`aozora-history-audit` will accept an optional `--drift-persons-dir DIR`
argument pointing at a validated persons directory that may contain
`_events/` and `_indexes/` drift sidecars. In the repository workflow this will
normally be:

```text
examples/v0/example-persons
```

When the option is present, the audit will:

1. Run `validate-drift-events!` against `DIR`.
2. If no drift artifacts are present, report zero drift-participant updates.
3. If drift artifacts are valid, collect every `person_id` mentioned by the
   drift indexes.
4. Compare the previous and current generated corpus snapshots for those
   `person_id`s.
5. Report a `drift_participant_updates[]` entry for every drift participant
   whose generated person record was added, removed, or whose
   `person_record_hash` changed.

The report entry shape is:

```json
{
  "person_id": "000123",
  "change_type": "hash_changed",
  "previous_hash": "sha256:...",
  "current_hash": "sha256:...",
  "drift_event_ids": ["sha256:..."]
}
```

For added and removed participants, the absent side uses `null`:

```json
{
  "person_id": "000123",
  "change_type": "removed",
  "previous_hash": "sha256:...",
  "current_hash": null,
  "drift_event_ids": ["sha256:..."]
}
```

The check is report-only by default. A new
`--fail-on-drift-participant-updates` flag makes the audit exit non-zero when
the list is non-empty.

The repository workflow exposes this as `nix run .#aozora-upstream-audit`,
which wraps `aozora-history-audit` with
`--drift-persons-dir examples/v0/example-persons`, `--fail-on-candidates`, and
`--fail-on-drift-participant-updates`. The lower-level
`aozora-history-audit` app remains available for exploratory scans and custom
drift sidecar directories.

The raw `aozora-ingest` command remains source-faithful and does not rewrite,
skip, or retire Aozora person IDs based on drift sidecars. A later managed
corpus command may reuse the same check as a pre-overwrite guard, but this ADR
does not add mutation behavior to ingest.

## Rejected Alternatives

### Rewrite upstream identities during ingest

The ingester could read drift events and map old upstream person IDs to ABC
local successors. This is rejected because it would make generated corpus
snapshots no longer be a transparent view of the upstream CSV. Split and merge
semantics also require editor judgment about work-level attribution; the
ingester cannot infer that safely.

### Add a `retired` marker to drift indexes

The index sidecar could carry explicit `retired: true` state for predecessor
IDs. This duplicates event semantics: predecessor/successor role is already
encoded by `prov.used` and `prov.was_generated_by` in the event. It also makes
indexes semantic artifacts instead of traversal artifacts, contradicting ADR
0021.

### Fail all ingests involving drift participants

This is too broad. Historical rebuilds and source-faithful audit snapshots must
still be possible. The failure mode belongs behind an explicit audit flag.

## Consequences

- After every upstream update, maintainers can run one audit command that
  checks for new split/merge candidates and for ordinary upstream edits to
  already drift-sensitive persons.
- Drift sidecars remain non-mutating audit artifacts.
- The generated corpus stays source-faithful by default.
- A non-empty `drift_participant_updates[]` list is not automatically a new
  drift event. It is a review queue: the maintainer decides whether the change
  is an ordinary bibliographic correction, evidence that an existing drift
  event needs a new snapshot, or evidence for a new split/merge event.

## Acceptance Criteria

- **ADR-0022-C1 — fixture-behavior:** the synthetic two-ref audit report includes `drift_participant_updates` and the matched event IDs when an indexed participant changes; `test/abc/tools/aozora_history_audit_test.clj` covers the integrated report.
- **ADR-0022-C2 — fixture-behavior:** invalid drift sidecars abort the tested audit path before a report value is returned; `test/abc/tools/aozora_history_audit_test.clj` covers the exception.
- **ADR-0022-C3 — fixture-behavior:** a complete synthetic audit report contains an empty `drift_participant_updates` list when no drift artifacts are present; `test/abc/tools/aozora_history_audit_test.clj` covers the report value.
- **ADR-0022-C4 — fixture-behavior:** a generated hash change for the tested indexed participant appears in `drift_participant_updates`; `test/abc/tools/aozora_history_audit_test.clj` covers the bounded case.
- **ADR-0022-C5 — operational-behavior:** with validation and candidate failures held false, `--fail-on-drift-participant-updates` makes the real audit process exit nonzero for a non-empty update list and does not make it fail for an empty list; other validation/candidate gates remain independent; `test/abc/tools/aozora_history_audit_test.clj` launches the subprocess and covers the exit matrix.
- **ADR-0022-C6 — fixture-behavior:** on the bounded synthetic CSV slice, adding drift sidecars does not change the generated person record, metadata record, or refreshed manifest; `test/abc/tools/aozora_ingest_test.clj` covers raw-ingest independence without claiming broader undefined “source-faithfulness.”

## Future Verification

The `aozora-upstream-audit` app remains a supported wrapper, but a passing operational claim requires a direct, reproducible Nix-app observation with fixed upstream refs; wrapper source text is not that observation.

## Rollback

If the drift-participant update check proves noisy or expensive, keep the raw
`aozora-ingest` source-faithful path and drop only the
`--drift-persons-dir` / `drift_participant_updates` surface from the audit. The
report-only default means removing the check does not change generated person
or work records, drift sidecars, or manifest identity. Do not reinterpret prior
audit reports that included drift-participant updates; dropping the check is an
audit-surface change, not an identity or drift-model change.

## References

- ADR 0020: Person identity drift data model.
- ADR 0021: Person identity drift harness contract.
- `src/abc/tools/aozora_history_audit.clj`
- `src/abc/tools/person_drift.clj`
- `src/abc/tools/person_drift_history.clj`
