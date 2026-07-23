# ADR 0035: Rights Assessment and External Statements

Status: Proposed
Date: 2026-07-12
Validation scope: structural
Release authority: none
Amends: ADR 0018 [scope: Boolean-to-external-rights RDF mapping]
Depends on: ADR 0043
Source: `docs/superpowers/specs/2026-07-11-rights-assessment-remediation-design.md`

## Implementation Status

Containment is implemented: legacy Boolean, missing, nil, and malformed source
values emit no external `dcterms:rights` statement. Release-facing publication
and staging commands fail closed under the checked-in publication policy while
the source semantics and rights-assessment migration remain incomplete.

## Context

ADR 0018 mapped the Aozora `copyright_expired` Boolean directly to either the
Creative Commons Public Domain Mark or RightsStatements.org In Copyright. The
source flag does not carry the jurisdiction, effective date, assessment
provenance, or negative-state semantics required to justify that stronger
external assertion. In particular, false, null, and absent values previously
collapsed into the same In Copyright statement.

## Decision

This ADR proposes replacing the Boolean projection with a provenance-bearing
rights assessment as specified by the linked design. Until that assessment is
implemented and migrated:

- no legacy source value emits an external `dcterms:rights` IRI;
- metadata SHACL permits a source-only work with no external rights IRI;
- `data/publication-policy.edn` blocks release publication and staging with
  `:blocked-pending-assessment-migration`; and
- development fixture rendering remains available but conveys no release
  authority.

The temporary omission of legitimate public-domain statements is an explicit
completeness regression. It is safer than publishing unsupported legal-status
claims and must not be released as a complete rights view.

## Acceptance Criteria

- **ADR-0035-C1 — structural-invariant:** metadata RDF tests demonstrate that
  true, false, nil, absent, and malformed legacy values emit no external rights
  statement.
- **ADR-0035-C2 — fixture-behavior:** SHACL accepts a work without an external
  rights IRI while retaining the existing cardinality and IRI checks when one
  is present.
- **ADR-0035-C3 — operational-behavior:** materialize, full-build, and staging
  release boundaries fail before writing artifacts while containment is active.
- **ADR-0035-C4 — external-semantics:** As a promotion condition, authoritative
  evidence defines the Aozora source fields and every enabled external mapping.
- **ADR-0035-C5 — corpus-behavior:** As a promotion condition, migration is total or
  explicitly quarantines every record and publication accepts only valid
  assessments.

## Consequences

- Published RDF cannot silently strengthen an Aozora source flag into a legal
  status assertion.
- Existing development fixtures lose their Public Domain Mark triple during
  containment and continue to validate as source-only RDF.
- Release publication remains blocked until the new schema, migration, and
  conditional SHACL contract pass.

## Forward Recovery

Complete the evidence-backed assessment migration and change the publication
policy to `:assessment-required`. Never recover by restoring the Boolean-to-InC
inference.
