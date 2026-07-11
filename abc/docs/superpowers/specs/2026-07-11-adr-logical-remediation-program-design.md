# ADR Logical Remediation Program Design

Date: 2026-07-11
Status: Proposed design for review

## Purpose

Repair the logical and scientific faults identified in `docs/adr` without
rewriting historical manifests or conflating document cleanup with semantic
migration. The program separates governance, rights, temporal knowledge,
artifact identity, and parser qualification into independently reviewable
workstreams joined by explicit dependencies.

## Problem Statement

The current architecture is strong at deterministic serialization and
contract conformance, but several ADRs make claims stronger than their
evidence:

- a Boolean source flag becomes a positive external rights assertion;
- unknown dates become identity `null` described as not applicable;
- comparative parser characterization is used to accept parser-viability
  criteria and framed as deciding ownership even though a project-owned custom
  parser is the practical production requirement;
- evidence-path existence is treated as sufficient acceptance evidence;
- Accepted decisions may depend on noncanonical Proposed contracts; and
- subject, derivation, byte, release, and cross-generation identity are
  described with overlapping terminology.

These are semantic faults. Passing the existing fixture and byte-drift gates
cannot repair them because those gates faithfully enforce the current rules.

## Program Structure

The program consists of this umbrella design and five workstream designs:

1. `2026-07-11-adr-evidence-and-lifecycle-remediation-design.md`
2. `2026-07-11-rights-assessment-remediation-design.md`
3. `2026-07-11-temporal-knowledge-state-remediation-design.md`
4. `2026-07-11-artifact-identity-remediation-design.md`
5. `2026-07-11-parser-qualification-remediation-design.md`

A shared-foundation execution slice implements the source-assertion envelope
before the five workstreams. It is not a sixth normative workstream: it owns no
domain interpretation and can be rejected only by revising the shared program
invariant that rights and temporal provenance use the same value shape.

Rights containment lands while governance is in audit mode. Governance is then
enforced before any semantic migration is promoted. Rights and temporal work
may proceed independently after that point. Identity consumes each completed
schema rotation separately; neither workstream blocks continuity links for the
other. Parser qualification consumes governance but does not depend on the
metadata-schema migrations.

```text
evidence + lifecycle governance
      |              |              |
    rights         temporal       parser qualification
      |              |
      +---- per-generation identity links
```

## Shared Vocabulary

- **Source assertion:** what an upstream source literally records.
- **Assessment:** an ABC interpretation supported by stated evidence, scope,
  jurisdiction, and time where relevant.
- **Claim:** a falsifiable statement made by an ADR acceptance criterion.
- **Evidence:** a scoped observation or authority offered for a claim.
- **Decision status:** whether a contract is canonical.
- **Validation scope:** fixture, smoke corpus, full corpus, or operational.
- **Release authority:** none, development, or publication.
- **Subject identity:** enduring intellectual or real-world entity identity.
- **Derivation identity:** equality of all declared production coordinates.
- **Content identity:** byte equality.
- **Release identity:** equality of a signed publication occurrence.

### Shared source-assertion value

Rights and temporal values share one source-assertion envelope:

```json
{
  "source": "aozora",
  "field": "作品著作権フラグ",
  "lexical_value": "...|null",
  "snapshot_hash": "sha256:..."
}
```

The envelope preserves provenance only. Domain values independently interpret
it as a rights assessment or temporal knowledge state. Missing source fields
still use an envelope with `lexical_value=null`; omission is not conflated with
an explicit unknown marker.

## Program-Wide Invariants

1. Negative, missing, or unknown source information MUST NOT become a positive
   external assertion without an explicit inference rule and evidence.
2. For any knowledge-bearing domain value, `unknown`, `not-recorded`, and
   `not-applicable` are distinct when those states are meaningful in that
   domain. Rights additionally distinguishes positive assessed statuses.
3. Every identifier states the equivalence relation it implements.
4. Custom-parser ownership is independent of neutral ecosystem comparison;
   parser measurement, compatibility admission, and release qualification are
   distinct transitions.
5. Accepted ADR dependency closure contains only Accepted contract scopes.
6. Acceptance evidence is typed and its observed result satisfies the claim's
   expected predicate.
7. Historical manifests and artifacts remain immutable.
8. Semantic migrations create new schema generations and explicit provenance
   or equivalence edges; they do not reinterpret old bytes.
9. Every migration reports old and new hashes, affected counts, quarantined
   records, semantic losses, and rollback procedure.
10. Derived RDF, TEI, JSON-LD, diagrams, and indexes never become identity
    inputs merely to make a migration convenient.
11. In-place manifest refresh is permitted only for unpublished development
    fixtures. A manifest that has entered a release or citation set is replaced
    by a new generation, never rewritten through `--refresh-manifest`.

## Delivery Policy

Each workstream produces:

- one or more Proposed ADRs with scoped amendment or supersession links;
- failing tests that demonstrate the current logical fault;
- the smallest schema/tool change that establishes the new contract;
- positive, negative, and migration fixtures;
- a checked-in machine-readable migration report where corpus data changes;
- updated generated views;
- focused checks and `just validate-migration` evidence; and
- an explicit promotion decision after its acceptance predicates pass.

Existing Accepted ADR Decisions are not edited in place. Errata may explain a
fault, but normative correction occurs through a new ADR.

## Sequencing and Release Gates

### Containment gate

Before public publication, and while the new governance validator is in audit
mode, stop emitting external rights statements not supported by the verified
upstream contract and stop describing the selected parser base as
release-qualified. These containment ADRs use the old governance gate, are
marked `Release authority: none`, and are revalidated under the enforced typed
evidence gate before any downstream promotion.

### Governance gate

No downstream remediation ADR becomes Accepted until typed evidence,
dependency closure, validation scope, and release authority are enforced.

### Semantic migration gate

Each rights or temporal schema rotation publishes its own complete old-to-new
derivation mapping. The identity workstream may publish links for either
rotation independently; it does not wait for a joint barrier.

### Final gate

The program is complete when every shared invariant is mechanically enforced,
the representative migration completes without unexplained loss, and
`just validate-migration` passes from the monorepo root.

## Non-Goals

- Rewriting historical manifests.
- Replacing RFC 8785 JCS or SHA-256.
- Selecting a different parser base without new comparative evidence.
- Providing legal advice or calculating copyright status independently.
- Introducing event sourcing or a database service.
- Treating every RDF or documentation view as a canonical artifact.

## Review Decisions

Reviewers approve this umbrella design only if they agree that:

- the five workstreams are independently rejectable at their normative
  boundaries, while the shared source-assertion envelope and per-generation
  mapping protocol make their real coordination explicit;
- governance precedes semantic promotion;
- historical data is migrated by new generations rather than mutation; and
- the workstream specifications contain no silent authority expansion.
