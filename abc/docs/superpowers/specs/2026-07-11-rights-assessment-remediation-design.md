# Rights Assessment Remediation Design

Date: 2026-07-11
Status: Proposed design for review
Parent: `2026-07-11-adr-logical-remediation-program-design.md`
Depends on: `2026-07-11-adr-evidence-and-lifecycle-remediation-design.md`

## Purpose

Preserve what Aozora states while preventing a Boolean source flag from being
published as a stronger, unsupported legal-status assertion.

## Current Fault

`metadata_record.clj` maps `copyright_expired=true` to Creative Commons Public
Domain Mark and `false` to RightsStatements.org In Copyright. The metadata and
person schemas expose only Booleans. The mapping therefore cannot represent
undetermined, unevaluated, jurisdiction-dependent, or source-only states.

## Required Investigation

Before selecting a durable mapping, record authoritative evidence for:

- the literal values and semantics of Aozora's work and person copyright
  fields;
- whether the assertion is current status, expiry, publication policy, or a
  source-maintainer flag;
- its jurisdiction and effective date;
- whether work and person flags have identical semantics; and
- the preconditions of each external rights statement considered.

The investigation has two valid outcomes:

1. **Exact semantic match:** emit the external IRI with source, jurisdiction,
   and assessment provenance.
2. **No exact match:** retain an ABC-local source assertion and omit the
   external rights IRI.

Silence or ambiguity selects outcome 2.

## Decision

Introduce a versioned `rights-assessment` value:

```json
{
  "source_assertion": {
    "source": "aozora",
    "field": "作品著作権フラグ",
    "lexical_value": "...",
    "snapshot_hash": "sha256:..."
  },
  "source_knowledge_state": "known|unknown|not-recorded|not-applicable",
  "assessment_status": "public-domain|in-copyright|undetermined|not-evaluated",
  "jurisdiction": "JP|unknown",
  "status_effective_at": "YYYY-MM-DD|null",
  "assessed_at": "YYYY-MM-DD|null",
  "basis": ["..."],
  "statement_iri": "https://...|null"
}
```

The source assertion uses the umbrella's shared envelope and is always
preserved. `source_knowledge_state=known` means a source value was supplied;
`unknown` means the source positively records uncertainty; `not-recorded`
means no source value was supplied; and `not-applicable` requires an explicit
domain rule. `assessment_status` is a separate ABC assessment: a present source
value can still yield `undetermined`, while a missing value normally yields
`not-evaluated`. This prevents source silence and failed interpretation from
collapsing into one state.

`status_effective_at` records when the asserted status applies;
`assessed_at` records when ABC or its cited source performed the assessment.
They are never substituted for each other.

`statement_iri` is optional and may be derived only through an evidence-backed
mapping from `public-domain` or `in-copyright`. `false`, null, an absent field,
or a parse failure never implies
`in-copyright`.

Work and person assessments use the same value schema but remain separate
facts. No person-level assertion is automatically projected to every work.

## RDF Contract

- Emit `dcterms:rights` only when `statement_iri` is non-null.
- Emit ABC provenance for source, snapshot, assessment date, jurisdiction, and
  basis.
- An undetermined assessment may map to an external `UND` statement only when
  its documented preconditions match.
- SHACL validates the conditional relationship; it does not force every record
  into a two-value rights set.

## Identity and Migration

Rights assessment is scholarly metadata and changes `metadata_record_hash` or
the corresponding person-record hash. The schema rotation creates new
derivation IDs. Historical manifests remain unchanged.

The migration report records:

- each old Boolean, source knowledge state, and assessment status;
- the evidence rule applied;
- counts by state and jurisdiction;
- records lacking sufficient source semantics;
- old/new record hashes and derivation IDs; and
- any external IRI added or removed.

No old-to-new mapping may silently default to `in-copyright`.

Migration totality is checked as a `clojure.test.check` partition property:
every input record appears exactly once in one output-state bucket or the
quarantine, and the union equals the input population. Hegel is not used
because it has no Clojure implementation.

## Containment

Before the full migration, an amendment disables unconditional external rights
IRI emission. This is a semantic safety change and is reviewed separately from
the later schema rotation.

## Acceptance Criteria

- Authoritative source-contract evidence is recorded with typed scope.
- Negative tests show that false, missing, malformed, and unknown values emit
  no positive external assertion.
- Missing source fields map to `not-recorded`; an explicit unresolved marker or
  assessed uncertainty maps to `unknown`; `not-applicable` requires a named
  rule.
- Positive mappings cite an exact semantic precondition and provenance.
- Work and person mappings are tested independently.
- SHACL admits source-only and undetermined records.
- The full migration is total or explicitly quarantines every unmapped record.
- Historical manifests validate under their original schema.

## Safe Fallback

Stop publishing derived rights IRIs and retain source assertions. Never roll
back by recreating the Boolean-to-`InC` inference.
