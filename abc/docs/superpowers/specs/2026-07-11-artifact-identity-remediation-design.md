# Artifact Identity Remediation Design

Date: 2026-07-11
Status: Proposed design for review
Parent: `2026-07-11-adr-logical-remediation-program-design.md`
Depends on: `2026-07-11-adr-evidence-and-lifecycle-remediation-design.md`
Consumes optionally: generation mappings from the rights and temporal remediation designs

## Purpose

Give subject, derivation, bytes, release occurrence, and cross-generation
equivalence distinct names and contracts while preserving existing hashes.

## Current Fault

ADR 0001 correctly defines `artifact_id` as a hash of derivation coordinates,
not output bytes. Elsewhere the system calls it stable identity or a content
address. Schema rotations intentionally give the same underlying facts new
`artifact_id` values, but no cross-generation equivalence contract exists.

## Identity Lattice

| Identifier | Equality relation |
| --- | --- |
| `subject_id` | Same intellectual work, person, or modeled entity |
| `source_record_id` | Same versioned upstream record |
| `derivation_id` | Same complete identity-bearing production coordinates |
| `content_hash` | Same materialized bytes |
| `release_id` | Same signed publication occurrence |
| equivalence assertion ID | Same curated cross-generation assertion value |

### Component-coordinate layer

Derivation identity is composed from artifact-kind-specific component
coordinates such as `work_content_hash`, `metadata_record_hash`,
`parser_build_hash`, schema/profile hashes, tokenizer hashes, mapping hashes,
and recipe hashes. Each component hash means equality of the canonical value or
bytes named by its defining contract; it is not an outcome identifier.

A checked-in artifact-kind coordinate matrix is generated from the manifest
schema plus explicit applicability policy. It records for every artifact kind
whether each coordinate is required, null as not applicable, or forbidden.
Rotation claims are evaluated through this matrix. For example, metadata-record
changes rotate publication artifacts that carry `metadata_record_hash`, but do
not rotate analysis artifact kinds that deliberately set that coordinate null.

The acceptance requirement covers both layers: outcome identifiers in the
lattice and every component coordinate in the artifact-kind matrix.

The current `artifact_id` hash algorithm and identity object become the
`derivation_id` contract. `artifact_id` remains a deprecated exact alias during
migration. A manifest carrying both must use identical values.

## Subject and Generation Links

`subject_id` is non-derivational and stable across schema rotations. It does
not replace person-drift events: splits and merges remain explicit changes to
subject interpretation.

Cross-generation continuity is represented by separately versioned
equivalence assertions containing:

```json
{
  "subject_id": "...",
  "from_derivation_id": "sha256:...",
  "to_derivation_id": "sha256:...",
  "relation": "supersedes|equivalent-facts|migration-of",
  "basis": ["..."],
  "asserted_at": "..."
}
```

Equivalence assertions are provenance artifacts, not inputs to either linked
derivation ID. Updating curatorial equivalence therefore does not rewrite
history.

### Relation algebra

- `equivalent-facts` is symmetric and transitively closed for querying. Cycles
  are valid and are normalized into equivalence classes.
- `supersedes` is directed, acyclic, and has at most one latest successor per
  asserted lineage unless an explicit split relation is introduced by a future
  ADR.
- `migration-of` is directed and acyclic from older schema generation to newer
  generation; multiple independent migration families may link the same
  subject.

Contradictions are checked per relation, never by one generic cycle rule.
Chiasmus formalization is used during design review to verify closure and DAG
properties; the repository gate remains a deterministic Clojure validator.
The validator uses union-find for `equivalent-facts` classes and ordinary
visited-set/topological algorithms for directed relations. A logic engine is
not a runtime dependency: it would still require separate deterministic path,
ordering, and diagnostic construction.

## Compatibility

- Readers accept historical manifests containing only `artifact_id`.
- Writers emit both fields during one schema generation.
- Indexes normalize the old field to `derivation_id` internally.
- Conflict detection continues to mean one derivation ID mapping to multiple
  content hashes.
- Identical bytes from different derivations retain different derivation IDs
  and the same content hash.
- Public documentation never calls `derivation_id` a byte content address.

## Migration

Each rights or temporal migration independently supplies an old/new derivation
mapping. The identity migration validates and publishes each mapping family as
soon as that workstream completes. The generated cross-generation index merges
available families by subject and derivation ID without requiring both.

The migration report identifies:

- manifests with missing or ambiguous subject IDs;
- alias mismatches;
- one-to-many and many-to-one generation links;
- identical-content/different-derivation cases;
- reproducibility conflicts; and
- unresolved links quarantined from the equivalence index.

## Acceptance Criteria

- Every outcome identifier and every component coordinate in schemas and public
  documentation names its equality relation and artifact-kind applicability.
- `artifact_id` and `derivation_id` mismatch is rejected.
- historical manifests remain readable without rewriting.
- derivation conflict detection retains current behavior.
- byte equality and derivation equality have independent tests.
- rights and temporal schema rotations produce queryable generation links.
- equivalence changes do not alter linked derivation IDs.
- the generated index forms equivalence classes for `equivalent-facts` and
  rejects cycles or contradictory latest nodes for directed relations.

## Safe Fallback

Readers may continue exposing `artifact_id` as the compatibility field. Keep
equivalence artifacts and migration reports; deleting them would restore the
known longitudinal-continuity fault.
