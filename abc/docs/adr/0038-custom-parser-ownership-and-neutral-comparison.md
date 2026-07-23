# ADR 0038: Custom Parser Ownership and Neutral Comparison

Status: Accepted
Date: 2026-07-12
Accepted: 2026-07-12
Amends: ADR 0002, ADR 0030, ADR 0032
Amended by: ADR 0043 [scope: ownership assessment freshness]
Depends on: ADR 0002, ADR 0023, ADR 0025, ADR 0030, ADR 0032
Validation scope: structural
Release authority: development

## Implementation Status

Accepted for development ownership. Exact-tuple admission and publication
authority remain outside this decision.

## Context

The project now owns a hard-detached parser implementation and the ABC
parser-IR and publication contracts it serves. Historical July studies remain
useful bounded evidence, but they were not a preregistered neutral comparison
and cannot establish exact-tuple admission or publication authority.

## Decision

Soranoha accepts project ownership of the custom-parser contract for
development, subject to the bounded owner assessment and its review date.
Admission remains exclusively controlled by ADR 0023, and publication remains
controlled by the publication contracts and a separately qualified release.

## Consequences

The project maintains the detached implementation and the owned contract as a
single development responsibility. A future neutral comparison may inform a
superseding decision, but comparison and citation records do not themselves
admit or release a parser.

## Acceptance Criteria

- **ADR-0038-C1 — domain-interpretation:** Given the enumerated owned parser/publication contract, current maintainer commitment, and bounded five-parser/build-fresh survey, the project-owner assessment supports Soranoha ownership of the custom-parser contract for development only; it grants neither tuple admission nor publication authority. Evidence boundary: `docs/evidence/external/custom-parser-ownership-assessment.md`.
- **ADR-0038-C2 — structural-invariant:** Comparison and citation records cannot bypass ADR 0023 exact-tuple admission, and this ADR has development rather than publication release authority. Evidence boundary: `test/abc/tools/parser_evidence_test.clj`.
- **ADR-0038-C3 — structural-invariant:** The historical Phase-5 evidence remains bound to its complete frozen C5 tuple and is distinct from the live parser-IR schema and current source-role relations. Evidence boundary: `test/abc/tools/parser_phase5_frozen_tuple_test.clj`.

## Historical Evidence

The July 2026 candidate survey and Phase-5 measurements are frozen historical
evidence. They did not evaluate a preregistered neutral contract and do not
prove current parser quality.

## Future Verification

A preregistered neutral comparison and any release qualification remain future
work. Review the ownership assessment after 2026-10-12 or earlier if a listed
falsifier occurs.

## Rollback

Supersede this ADR if the project retires the custom parser or can no longer
maintain the owned contract. Do not reinterpret historical evidence as current
admission evidence.
