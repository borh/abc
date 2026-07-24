# Person Identity Drift Data Model

## Implementation Status

As of 2026-07-09, the lineage-only model is live in
`schemas/person-drift-event.schema.json`,
`schemas/person-drift-index.schema.json`, and `abc.tools.person-drift`; its
identity, participant, and graph invariants are covered by
`test/abc/tools/person_drift_test.clj`.

## Context

The separated-person-records milestone handles bibliographic corrections to a
single person record: a typo fix or name edit changes the person record content,
rotates `person_record_hash`, and only invalidates works that reference that
person snapshot. That mechanism does not model identity drift, where the
meaning of a `person_id` changes because one identity is split into multiple
people, or multiple identities are merged into one.

Flavor 2 needs an explicit event model for that drift. A 2024 manifest may
validly reference `person_id` 000123 and `person_record_hash` H1; if 000123 is
split in 2026, consumers need a way to trace the old identity to its current
successors without retroactively invalidating the old manifest.

Re-attribution is out of scope. Moving a work's contributor from one person to
another is a `contributors[]` edit on a metadata record and rotates
`metadata_record_hash` through the existing Flavor 1 path. Recording that as a
lineage-only drift event would contradict the invariant chosen below.

Three questions determine the data model:

- Whether drift events rotate identity hashes.
- How split successors and merge survivors are identified.
- How consumers find multi-person events from a person reference.

## Decision

### Position L: lineage-only

Drift events do not rotate `person_record_hash`, `metadata_record_hash`, or
`manifest_identity_object`. Existing manifests remain valid. Consumers that need
the current reading of a person identity traverse the drift event log.

The rejected alternative is Position H, where a drift event updates successor
relations inside person records and deliberately rotates downstream hashes.
That is simpler to implement but breaks archival validation: old manifests could
not be revalidated after upstream identity drift.

### Identifier policy: ABC-local entity IDs

ABC may mint local person IDs for split successors and merge survivors. The
accepted `person_id` shape widens from six Aozora digits to:

```text
^([0-9]{6}|abc-[0-9a-f]{12})$
```

This pattern applies everywhere `person_id` appears, including
`schemas/person-record.schema.json` and contributor references in
`schemas/metadata-record.schema.json`.

ABC-local IDs use the IRI base:

```text
https://w3id.org/abc/persons/<person_id>
```

Aozora numeric IDs continue to use the existing Aozora person-page IRI. The RDF
`dcterms:identifier` datatype for ABC-local IDs is `xsd:string`; Aozora numeric
IDs continue to use `xsd:int`. `PersonRecordShape` must preserve the existing
`sh:minCount 1` and `sh:maxCount 1` cardinality, while widening the value
constraint to this canonical SHACL form:

```turtle
sh:property [
  sh:path dcterms:identifier ;
  sh:minCount 1 ;
  sh:maxCount 1 ;
  sh:or (
    [ sh:datatype xsd:int ; sh:pattern "^[0-9]{6}$" ]
    [ sh:datatype xsd:string ; sh:pattern "^abc-[0-9a-f]{12}$" ]
  )
] ;
```

The ABC-local suffix is 12 lowercase hex digits, not 8. Twelve hex digits give
roughly 2^48 values, which keeps accidental local-ID collision outside any
plausible corpus size while staying readable.

### Multi-person event indexing: stable event IDs plus participant indexes

Each drift event has a deterministic `drift_event_id`. Events live once, under
a per-event location. Per-person indexes point to event IDs so consumers can
traverse from a person to the events that mention it without duplicating the
event body into every participant's sidecar.

Each event binds the historical snapshots it mentions. A participant reference
is not a bare `person_id`; it records:

```json
{
  "snapshot_id": "pre-000123",
  "person_id": "000123",
  "person_record_hash": "sha256:..."
}
```

The `snapshot_id` is local to the event and is the target used by the JSON PROV
block. It must be unique within the event and must begin with `pre-` for
predecessor snapshots or `post-` for successor snapshots. `participants[]` is
stored in lexicographic `snapshot_id` order, so content-derived
`drift_event_id` values do not change when two editors list the same logical
participants in different orders.

The `person_record_hash` is the snapshot hash observed by the event, not a
value looked up from the current person record during validation. This is
required for Position L: a later bibliographic edit to a person record must not
silently rebind an old drift event to a new snapshot.

The event ID is content-derived:

1. Start from the full wrapper JSON object as it appears on disk.
2. Remove only the `drift_event_id` field.
3. JCS-canonicalize the remaining object.
4. SHA-256 the canonical bytes.
5. Prefix the digest with `sha256:`.

No `id_omitted_from_hash` marker is materialized. The omission rule is a fixed
protocol invariant, not an in-band flag. Validation re-derives the ID by
removing only `drift_event_id` and asserting equality.

### Drift-log identity: audit sidecar

The drift log is validated as part of the design bundle, but it is not an input
to `manifest_identity_object`. This is required for Position L: if the drift log
were a corpus artifact whose hash flowed into manifest identity, every drift
event would rotate manifests.

### Vocabulary: thin ABC profile over PROV-O

ABC defines a thin drift vocabulary over PROV-O:

- `abc:DriftEvent rdfs:subClassOf prov:Activity`
- `abc:DriftSplitEvent rdfs:subClassOf abc:DriftEvent`
- `abc:DriftMergeEvent rdfs:subClassOf abc:DriftEvent`
- `abc:driftEventType`
- `abc:driftEvidence`
- `abc:DriftEditor`

The closed event-type set is `split` and `merge`.

- A split has one predecessor snapshot and two or more successor snapshots.
- A merge has two or more predecessor snapshots and one successor snapshot.

`rename` is not part of the recommended decision. Pure name edits are
bibliographic corrections to one `person_record.json` and already rotate
`person_record_hash` through Flavor 1. A future ADR may add `rename` only if it
defines a case that is not a Flavor 1 edit.

`abc:DriftEditor` is the role attached to the `prov:Association` connecting the
drift Activity to the editor Agent. It is an ABC-local role term because the
role is specific to this drift-review workflow; the Agent itself remains a
plain PROV Agent IRI.

### Canonical PROV graph

The canonical RDF view for a split or merge is:

```text
Pre-snapshot E1
    --prov:wasInvalidatedBy--> A (drift Activity)
A
    --prov:used--> E1
    --prov:wasAssociatedWith--> editor Agent
    --prov:qualifiedAssociation--> [a prov:Association;
                                    prov:agent editor;
                                    prov:hadRole abc:DriftEditor]
    a abc:DriftEvent ;
      abc:driftEventType "split"|"merge" ;
      dcterms:date "..." ;
      abc:driftEvidence (...)
Post-snapshot E2
    --prov:wasGeneratedBy--> A
    --prov:wasDerivedFrom--> E1
```

For a split, there are two or more post-snapshots, each derived from the single
pre-snapshot. For a merge, there is one post-snapshot derived from two or more
pre-snapshots. `prov:wasDerivedFrom` is Entity-to-Entity; the Activity points to
pre-snapshots with `prov:used`, and post-snapshots point back to the Activity
with `prov:wasGeneratedBy`.

## Hard Rule

For Position L plus audit-sidecar drift logs:

- `person_record_hash` is not rotated by adding a drift event.
- `metadata_record_hash` is not rotated by adding a drift event.
- `manifest_identity_object` is not affected by drift events.
- Identifier policy `abc-[0-9a-f]{12}` does rotate
  `person_record_schema_hash` and `metadata_record_schema_hash`, because the
  existing person and metadata schemas widen their `person_id` contracts.

The schema-widening cascade is separate from the drift-event invariant. If the
identifier policy lands in the same implementation milestone, the cascade must
be verified as its own expected rotation.

## Consequences

- Consumers that need current identity must be drift-aware and traverse the
  audit sidecar.
- ABC gains a local person-ID space, which is more invasive than waiting for
  Aozora-issued IDs but allows real splits to be represented.
- The data model separates lineage events from bibliographic contributor edits,
  avoiding a mixed event log where some entries rotate manifest identity and
  others do not.

## References

- `docs/superpowers/specs/2026-04-28-separated-person-records-design.md` —
  Flavor 1 contract surface and Flavor 2 deferral.
- ADR 0001 — manifest identity and `manifest_identity_object`.
- ADR 0017 — vocabulary review discipline for new `abc:` terms.
- ADR 0018 — example of RDF/SHACL contract changes without JSON schema
  rotation.
- PROV-O recommendation: `prov:Entity`, `prov:Activity`, `prov:Agent`,
  `prov:used`, `prov:wasGeneratedBy`, `prov:wasDerivedFrom`,
  `prov:qualifiedAssociation`, `prov:wasAssociatedWith`,
  `prov:wasInvalidatedBy`, `prov:specializationOf`.
