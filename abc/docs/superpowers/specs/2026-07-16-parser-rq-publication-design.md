# Parser Release Qualification P2: Publication Structure Design

Date: 2026-07-16
Status: Approved design
Parent: `2026-07-15-parser-release-qualification-campaign-design.md`
Roadmap: `../plans/2026-07-15-parser-release-qualification-campaign.md`
Depends on: P0 Foundation
Feeds: P5 Admission and Promotion

## Purpose

Provide an honest, machine-derived observation for ADR 0039 predicate 6,
`publication_structure = 1.0`, over the pinned parser release-qualification
corpus. P2 validates the complete publication structure produced for every
successfully parsed work without duplicating the source-accountability claim
owned by R1 or the parser-IR schema claim owned by predicate 5.

P2 implements an instrument and its deterministic fixture. It does not perform
the authoritative release-candidate capture, registry admission, or ADR 0039
promotion; P5 owns those operations after all instrument plans have landed.

## Grounded Current State

The repository already has most of a deep publication validator:
`ab-validator/reports/parser-ir/publication-bundle-validate.py` checks generated
TEI, plaintext, preservation records, manifests, pointer joins, and body-only
plaintext. `abc.tools.materialize-publication` produces those artifacts and
validates TEI against the project Relax NG and Schematron profiles.

The validator cannot be used as predicate 6 authority unchanged:

- Its aggregate verdict includes `source_region_coverage_valid` and
  `parser_ir_schema_valid`. Those are supporting preconditions owned by R1 and
  predicate 5, not publication-structure facts. Reusing the aggregate would
  make one scientific predicate depend on two others.
- Its `preservation_schema_valid` check accepts schema version `0.2.0` and only
  checks identity plus record count. The authoritative
  `abc/schemas/parser-ir-publication-preservation.schema.json` is version
  `0.3.0`; predicate 6 requires validation against that actual schema.
- Batch membership is discovered by walking for `parser-ir.json`. Discovery
  cannot prove that every pinned work contributed exactly one record and cannot
  establish the release denominator.
- The three pinned qualification works are syntax fixtures and do not carry the
  metadata/person inputs required by the real materializer.

The pinned corpus has three members and declares each `:expected_status
:parsed`. Its corpus and list hashes remain unchanged by P2.

## Decision

Retain one deep publication validator, but make it emit two explicitly separate
relations:

1. **Publication structure** is the conjunction of a closed, versioned ABC-owned
   check set. It alone supplies predicate 6.
2. **Supporting preconditions** retain source-accountability and parser-IR
   schema observations for diagnosis. They remain visible but cannot alter the
   predicate-6 verdict.

This deepens the existing validator instead of duplicating its parsing and join
logic in a second release-only tool. The separation occurs in named data, not in
separate processes.

## Predicate Identity and Closed Policy

ABC owns a committed publication-structure policy. Its identity is the JCS hash
of the closed policy document. The policy names exactly these predicate-6
checks:

- `tei_profile_valid`
- `preservation_schema_valid`
- `tei_manifest_valid`
- `plaintext_manifest_valid`
- `tei_manifest_references_preservation`
- `tei_manifest_references_validation_result`
- `tei_abc_projection_resolves_to_sidecar`
- `preservation_tei_pointers_resolve`
- `preservation_source_pointers_resolve`
- `plaintext_body_only`

The following existing checks remain supporting preconditions and are excluded
from the publication-structure policy:

- `parser_ir_schema_valid`
- `source_region_coverage_valid`
- `source_region_sidecar_role_available`

An unknown, missing, duplicate, or extra policy check makes derivation
unavailable. Adding, removing, or renaming a predicate-6 check creates a new
policy version and identity; it cannot silently reinterpret an old capture.

`preservation_schema_valid` means full JSON Schema validation against the
authenticated authoritative `0.3.0` schema, including its schema id, version,
closed fields, record vocabulary, and record-count consistency. A string
comparison is insufficient. The validator records the schema's logical content
identity, not a trusted path.

## Per-Work Data Flow

For each pinned corpus member, the P2 capture flow:

1. Authenticates the source and parser-IR inputs against the qualification
   identity and corpus member.
2. Supplies committed, deterministic qualification-only metadata and person
   inputs for that work.
3. Runs the production `abc.tools.materialize-publication` path.
4. Runs the deep publication validator once over the materialized bundle.
5. Projects the detailed checks through the authenticated publication-structure
   policy.
6. Stores generated artifacts and detailed validation evidence in the external
   artifact store.
7. Emits one immutable work record and one closed record-index entry containing
   logical blob identities and the qualification identity reference.

The qualification-only metadata fixtures have no bibliographic or publication
authority. They exist solely to exercise the real materializer and TEI profile
for syntax fixtures. They use deterministic values, are content-authenticated,
and are named as qualification fixtures in their producer/source metadata.
Reusing unrelated example metadata is prohibited because it would create false
work-identity joins. Replacing the pinned corpus with production works is
outside P2 because it would invalidate prior corpus-bound captures.

## Immutable Contracts

### Publication policy

The closed policy carries:

- schema id and version;
- ordered unique predicate-6 check names;
- supporting-precondition check names;
- the authoritative preservation schema id and logical hash; and
- its own canonical policy identity.

### Publication work record

Every pinned corpus member has exactly one record, including parse failures and
timeouts. A record carries:

- work id and source content hash;
- qualification identity reference;
- parser disposition (`parsed`, `failed`, or `timeout`);
- publication policy identity;
- preservation-schema identity;
- for parsed records, logical blob references for materialization inputs,
  generated artifacts, and detailed validator evidence, the exact detailed
  check map, the projected publication-structure verdict, and bounded failure
  witnesses naming failed checks without embedding corpus-scale artifacts; and
- for failed or timed-out records, the authenticated parser disposition and no
  fabricated publication evidence.

### Closed record index

The index is a deterministic capture output, never hand-authored membership. It
is an exact projection of the pinned corpus and carries:

- corpus id, snapshot hash, and list hash;
- qualification identity reference;
- publication policy and schema identities; and
- one content-authenticated work-record reference for every corpus member.

Index order is canonical. Missing, duplicate, or extra members and cross-work
record substitution are rejected before aggregation.

## Denominator and Aggregation

The index makes four counts explicit:

- `expected_works`: all pinned corpus members;
- `successful_works`: records whose candidate parse disposition is `parsed`;
- `structure_eligible_works`: successful records with complete authenticated P2
  evidence; and
- `structure_passed_works`: eligible records passing every policy check.

The predicate value is:

```text
structure_passed_works / successful_works
```

Every corpus member remains in the index, so parser failures and timeouts cannot
disappear. Predicates 1 and 9 own their release effect; predicate 6 excludes
them because its predeclared dimension is “required publication structures
present per successful work.”

If `successful_works` is zero, the observation is unavailable rather than a
vacuous `1.0`. If any successful work is not structure-eligible, the observation
is unavailable rather than a reduced ratio: absence or unauthenticated evidence
is not a measured structural failure. A complete eligible record with one or
more failed policy checks contributes zero to the numerator and yields an honest
ratio below `1.0`.

The derived aggregate carries integer numerator and denominator counts. Decimal
rendering is descriptive; ADR 0039's equality-to-`1.0` decision is additionally
guarded by integer equality `structure_passed_works = successful_works`.

## Failure Semantics

P2 distinguishes measured failure from unavailable evidence:

- **Available failure:** a complete authenticated publication bundle fails a
  policy check. The work record names the failed checks and the aggregate ratio
  is below `1.0`.
- **Unavailable:** malformed policy or record; missing, duplicate, or extra
  member; zero successful denominator; qualification, corpus, policy, or schema
  identity mismatch; unknown check vocabulary; missing blob; hash/length/media
  mismatch; cross-work substitution; or incomplete evidence for a successful
  work.
- **Available pass:** every successful work has complete authenticated evidence
  and passes every closed policy check.

Runtime paths are locators below the configured external store and never
evidence identity. P0's authenticated single-read boundary supplies the bytes
used by derivation. A consumer may not authenticate a locator and reopen it.

## State, Time, and Identity

P2 introduces no database or mutable run state. Capture artifacts and indexes
are immutable values.

“Current” is not a time coordinate. A P2 result is valid for the exact
qualification identity, corpus hashes, publication policy hash, preservation
schema hash, materializer version, and validator version recorded by the
capture. Changing any coordinate requires a new capture and cannot mutate old
evidence.

Work equality uses work id plus source content hash. Artifact equality uses
logical SHA-256 identity plus byte length and media type. Run coherence uses the
full qualification identity reference. Policy and schema equality use canonical
content identity. No basename, directory position, timestamp, or human boolean
substitutes for these relations.

## Capture, Derive, Drift

- **Capture:** hinoki materializes and validates every indexed work and writes
  immutable blobs plus the closed work-record index.
- **Derive:** a pure analyzer authenticates the index and work records, checks
  every identity join, and produces the aggregate plus a
  `publication_structure` observation envelope.
- **Drift:** the committed small fixture regenerates its work records, aggregate,
  and observation byte-identically. P5 later performs the authoritative capture
  against its newly pinned candidate; P2 fixtures cannot be promoted by copying
  their scalar value.

Corpus-scale generated bundles remain outside Git. The repository commits
schemas, policies, qualification metadata fixtures, capture manifests, small
bounded witnesses, and deterministic drift fixtures only.

## Component Boundaries

1. **ABC policy and schemas** own the check vocabulary, immutable record/index
   contracts, and qualification metadata fixtures.
2. **The existing ab-validator deep validator** owns reading a materialized
   bundle once, evaluating detailed joins, actual preservation-schema
   validation, and emitting separate structure/supporting blocks.
3. **The P2 capture command** owns corpus traversal from the explicit index,
   materializer invocation, external-store writes, and deterministic record-index
   production. It does not calculate the final ratio.
4. **The pure P2 analyzer** owns authentication, closed-set validation, exact
   corpus folding, aggregation, and observation-envelope derivation. It does not
   invoke the parser or materializer and does not resolve arbitrary paths.
5. **The existing release bundle generator** consumes only the analyzer's
   authenticated observation envelope. It never accepts a hand-keyed scalar.

These pieces stop decomposing here: each has one authority, a data-in/data-out
interface, and an independently falsifiable test boundary.

## Verification Strategy

The implementation plan must cover:

- policy schema acceptance and rejection of missing, duplicate, extra, reordered
  where order is identity-bearing, and unknown checks;
- full `0.3.0` preservation-schema validation, including a fixture that would
  pass the old shallow `0.2.0` string/count check but fails the real schema;
- separate publication and supporting verdicts;
- the independence property that changing only R1 or predicate-5 supporting
  status cannot change `publication_structure`;
- deterministic materialization from qualification-only metadata;
- exact three-work index completeness and canonical ordering;
- missing/extra/duplicate member, empty denominator, wrong identity, stale
  policy/schema, blob corruption, locator escape, and cross-work substitution;
- a complete structural failure producing an available ratio below `1.0`;
- incomplete successful-work evidence producing unavailable rather than fail;
- exact numerator/denominator derivation and observation-envelope identity;
- byte-identical drift regeneration; and
- comment hygiene, schema drift, governance recapture, and
  `just validate-migration`.

Property-based tests are appropriate for record-order invariance, interval-free
corpus folding, duplicate injection, and numerator bounds. Example tests remain
the authority for named protocol failures and witness shapes.

## Delivery Slices

The focused implementation plan will contain independently reviewable TDD
slices for:

1. publication policy and record/index schemas;
2. deep-validator schema correction and verdict separation;
3. deterministic qualification metadata and batch materialization;
4. per-work capture records and closed corpus index;
5. pure aggregation and adversarial identity/completeness tests;
6. generated observation integration and production-shaped drift fixture; and
7. governance evidence recapture plus the full migration gate.

## Non-Goals

- Changing predicate 6 or its `= 1.0` threshold.
- Changing the pinned qualification corpus.
- Granting bibliographic authority to qualification metadata fixtures.
- Making predicate 6 depend on R1 or predicate 5 verdicts.
- Treating missing evidence as a measured structural failure.
- Performing P5's authoritative candidate capture, registry admission, or ADR
  0039 promotion.
- Committing corpus-scale publication bundles.
- Refactoring unrelated publication or source-accountability machinery.

## Acceptance Criteria

P2 is complete when:

- every pinned corpus member has exactly one authenticated work record;
- every successfully parsed work has a complete publication capture;
- predicate 6 is derived only from the closed publication-structure policy;
- preservation sidecars are actually validated against authenticated schema
  version `0.3.0`;
- supporting source/schema checks remain visible but cannot affect predicate 6;
- structural failures yield an available value below `1.0`, while trust and
  completeness failures yield unavailable;
- the observation is an identity-bearing envelope generated from committed
  manifests and external-store blobs, never a hand-authored scalar;
- the production-shaped fixture regenerates byte-identically;
- governance evidence is recaptured for every changed live read; and
- `just validate-migration` exits zero.

An honest predicate failure is a successful P2 outcome. P2 promises a real
verdict, not a passing verdict.

## Deferred to P5

P5 chooses and pins the final implementation revision, runs this instrument on
hinoki for that exact qualification identity, commits the new manifests, derives
the release bundle, and resolves admission. P2's fixture proves the protocol and
drift path; it is not release evidence for a later candidate.
