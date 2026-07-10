# ADR Governance Hardening Design

Date: 2026-07-10
Status: Approved design

## Purpose

Make the ABC ADR set a uniformly validated decision history whose lifecycle,
evidence, relationships, architecture views, and runtime diagrams cannot drift
silently while repository checks remain green.

This work repairs the existing ADR set and then applies one policy to every ADR.
It does not preserve permanent legacy exceptions.

## Current Problems

The existing diagram machinery reliably proves that committed Mermaid files are
byte-current relative to their immediate sources. It does not prove that those
sources are semantically complete or that ADR lifecycle metadata satisfies the
documented policy.

The review identified these concrete deficiencies:

- ADR 0028 has implementation commentary inside `Status:`. The parser keeps
  only the first token, so the malformed value passes lint.
- ADR 0016 is Accepted without an `Accepted:` date. Several Accepted ADRs do
  not have an `Implementation Status` section.
- The acceptance-criteria shell gate checks only for path-like text and skips
  allowlisted ADRs permanently. It does not prove that evidence paths exist or
  that a changed legacy ADR satisfies the current policy.
- The canonical architecture document omits identity coordinates required by
  the current manifest schema. The architecture diagram's manifest stage also
  omits ADRs that changed manifest identity.
- Accepted decisions can depend on Draft or Proposed decisions without an
  explicit, machine-readable scope or waiver.
- The runtime workflow diagram renderer does not validate its input against the
  existing workflow-run schema and semantic validator before rendering it.

## Design Principles

1. Markdown ADR files remain the lifecycle and decision source of truth.
2. One parser and validator define ADR validity for both governance checks and
   diagram consumers.
3. Invalid input is reported, never silently normalized.
4. Existing ADRs are repaired before uniform enforcement is activated.
5. Generated Mermaid remains a derived view, not an identity input or competing
   source of truth.
6. The semantic-relation EDN sidecar remains limited to relationships that ADR
   headers cannot express.
7. Proposed ADRs are not promoted merely because some implementation exists.

## Architecture

### ADR domain module

Add `abc.tools.adr` as the sole parser and governance validator for ADR
Markdown. It owns:

- ADR discovery and four-digit filename identity;
- exact header parsing without first-token truncation;
- title number and filename number agreement;
- lifecycle status and date validation;
- section discovery;
- ADR reference parsing and referential integrity;
- reciprocal amendment validation;
- dependency status and scope validation;
- acceptance-evidence extraction and filesystem resolution; and
- repository-wide validation that accumulates all problems.

The public boundary is a structured ADR value plus validation functions. The
value contains the original file path, number, title, exact status, dates,
relations, section names, and evidence references. Parsing preserves invalid
values so validation can describe them accurately.

`abc.tools.diagram.adr-graph` consumes these ADR values for nodes and
header-owned graph edges. It continues to own only graph construction and the
`adr-relations.edn` semantic-edge rules. It must not maintain a second ADR
header parser.

### Governance check

Replace `nix/check-acceptance-criteria.sh` with a Clojure entry point backed by
`abc.tools.adr`. The existing flake check becomes an ADR-governance check that
validates lifecycle, relations, sections, dependencies, and evidence in one
run. Delete `.acceptance-legacy-allowlist` after the repository is clean.

The root flake continues to expose the nested ABC check with its `abc-` prefix,
so root `nix flake check` and `just validate-migration` see the same policy.

### Architecture completeness

Keep `docs/architecture-stages.edn` as the source for the generated architecture
diagram, but add checks for semantic completeness rather than existence alone.

The manifest identity coordinate list in `docs/architecture.md` must equal the
required property list of `schemas/manifest.schema.json`'s `identityObject`.
The comparison is set-based for membership and separately verifies the
documented order when order is part of the presentation contract.

The manifest stage must cite every ADR explicitly recorded as introducing or
amending a manifest identity coordinate. Add this ownership to
`docs/architecture-stages.edn` as a top-level value:

```clojure
:manifest-identity-contract
{:schema "schemas/manifest.schema.json"
 :adrs [1 10 23 27 28]}
```

The architecture validator consumes that value and requires the manifest
stage's ADR list to include it. Ownership must not be inferred from arbitrary
prose searches. Proposed ADRs 0027 and 0028 appear because their implemented
schema changes are already present in the live manifest contract; inclusion in
this metadata does not promote their lifecycle status.

### Runtime workflow diagrams

Before `abc.tools.diagram.workflow-graph` constructs a graph, it validates the
input with `schemas/workflow-run.schema.json` and the existing workflow semantic
validator. Schema or semantic failures stop rendering and report actionable
errors. Unknown statuses are invalid; they are not mapped to `skipped`.

The per-run diagram remains an on-demand stdout artifact and is not added to the
committed diagram registry.

## Lifecycle Contract

### Exact header rules

- `Status:` equals exactly one of `Draft`, `Proposed`, `Accepted`,
  `Superseded`, or `Withdrawn`.
- `Date:` is required and has `YYYY-MM-DD` shape.
- `Accepted` requires `Accepted: YYYY-MM-DD` and an
  `## Implementation Status` section.
- Non-Accepted ADRs must not carry an `Accepted:` field.
- The date of acceptance must not precede the ADR date.
- The `# ADR NNNN:` title number equals the filename number.
- ADR numbers are unique across the directory.
- References use exactly `ADR NNNN` and resolve to an existing ADR.
- `Amends` and `Amended by` are reciprocal in both directions.

`Supersedes:` may continue to contain scoped prose, as ADR 0012 does. Any
well-formed `ADR NNNN` token found there is treated as a graph reference and
must resolve. A scoped supersession does not force the referenced ADR's entire
status to `Superseded`.

### Required sections

Every Accepted ADR has:

- `## Decision`;
- `## Implementation Status`; and
- `## Acceptance Criteria` with executable evidence.

Draft and Proposed ADRs may have Acceptance Criteria. When present, the same
evidence rules apply.

### Dependency status and scope

An Accepted ADR cannot depend on a non-Accepted ADR without an explicit scoped
dependency annotation. The machine-readable syntax is:

```markdown
Depends on: ADR 0002 [scope: source_span_coverage gate]
```

Each bracketed scope binds to the immediately preceding ADR reference. Scope is
a non-empty human-readable string. The validator rejects unscoped
Accepted-to-Draft and Accepted-to-Proposed dependencies.

Draft and Proposed ADRs may depend on non-Accepted ADRs without a scope because
their own contracts are not canonical. The graph label for a scoped dependency
includes the scope so the generated view does not erase it.

### Acceptance evidence

Each Acceptance Criteria section must contain at least one repository-relative
evidence reference under an allowed executable surface:

- `test/` for Clojure tests;
- `fixtures/` for validated fixtures;
- `nix/` for executable check scripts; or
- `fixtures/v0/facts/prolog/` for committed Prolog evidence.

Every referenced path must exist. Directory references are allowed only when
the criterion names the executable harness that consumes the directory. A path
substring in prose is not sufficient: references are extracted from Markdown
code spans and validated as paths relative to `abc/`.

The governance check proves evidence presence and addressability. Actual test
execution remains the responsibility of the focused and full Clojure/Nix test
checks; the ADR check does not build a second test runner.

## Existing-Set Migration

The migration repairs all current violations before removing the old gate:

- Move ADR 0028's status commentary into its existing Implementation Status
  section and leave its exact status `Proposed`.
- Recover ADR 0016's acceptance date from repository history or contemporaneous
  evidence and add the field. If history cannot establish a distinct date, use
  its recorded decision date and document the evidence in Implementation
  Status.
- Add Implementation Status sections to Accepted ADRs that lack one. These
  sections describe current repository evidence without rewriting immutable
  Decision or Hard Rule text.
- Add real evidence references to every Acceptance Criteria section currently
  covered by the legacy allowlist. Where no executable evidence exists, add the
  missing characterization test or fixture rather than inventing a reference.
- Add explicit scopes to Accepted-to-non-Accepted dependencies, including ADR
  0024's dependency on ADR 0002.
- Synchronize `docs/architecture.md` with the manifest schema's current identity
  coordinates and update the manifest-stage ADR attribution.
- Regenerate committed Mermaid after source repairs.
- Delete the allowlist and shell-only acceptance gate only after the new
  repository-wide validator reports no problems.

ADRs 0026, 0027, 0028, and 0029 remain Proposed unless their own stated
promotion criteria are separately demonstrated and ratified. This effort does
not bundle those status decisions.

## Error Reporting

Validation returns a vector of problem values and the CLI prints all problems
in one run. Each problem contains:

- ADR file and number when available;
- a stable problem kind;
- the field, section, relation, or evidence path involved; and
- a concise remediation message.

Malformed files must not fail with incidental exceptions such as
`NumberFormatException`. File-read or EDN/JSON parse failures may retain their
exception as cause data, but the CLI reports the affected source path.

The command exits zero only when the full repository is valid.

## Testing Strategy

Develop the validator with focused test-first cases for:

- exact versus decorated status values;
- required and forbidden acceptance dates;
- acceptance-date ordering;
- filename/title mismatch and duplicate numbers;
- malformed, dangling, and reverse-only relations;
- Accepted-to-Draft dependencies with and without scopes;
- missing Acceptance Criteria or Implementation Status;
- missing, invalid, and existing evidence paths; and
- accumulation of multiple problems from one repository scan.

Add a repository-wide test proving the migrated ADR set is clean.

Architecture tests prove:

- documented identity coordinates equal manifest schema requirements;
- manifest-stage ADR attribution equals the declared identity-contract owners;
- stage schemas, ADRs, and input IDs still resolve; and
- committed Mermaid remains byte-current.

Workflow diagram tests prove:

- the valid example renders deterministically;
- schema-invalid workflow input is rejected;
- semantic-invalid workflow input is rejected; and
- an unknown status is not rendered as `skipped`.

## Verification and Rollout

Implement and review the work in this order:

1. Introduce the ADR domain parser and validator with synthetic tests.
2. Repair the existing ADR set and add the clean-repository test.
3. Switch the flake governance check to the Clojure validator, then delete the
   shell gate and allowlist.
4. Add architecture semantic-completeness checks and repair the architecture
   sources.
5. Add workflow schema and semantic validation.
6. Regenerate diagrams and run the complete verification matrix.

The verification matrix includes focused Kaocha tests, diagram drift, Clojure
format/lint, the full ABC Clojure test derivation, the ADR-governance check, and
the root migration validation entry point.

Each step is committed independently so a reviewer can accept or reject ADR
parsing, document migration, gate replacement, architecture completeness, and
workflow validation separately.

## Non-Goals

- No structured ADR registry becomes canonical.
- No Markdown ADR generation is introduced.
- No automatic promotion of Proposed ADRs occurs.
- No changes are made to manifest artifact identity as a consequence of
  diagram regeneration.
- No per-run workflow diagram is committed.
- No general documentation-site generator is introduced.

## Success Criteria

The work is complete when:

- every current ADR satisfies one uniform lifecycle and evidence policy;
- no legacy ADR allowlist remains;
- malformed lifecycle metadata fails the same check that feeds the decision
  graph;
- the architecture document and diagram are complete against the current
  manifest identity contract;
- invalid workflow reports cannot produce plausible runtime diagrams;
- committed diagrams are byte-current; and
- the focused, ABC-wide, Nix, and root migration checks pass.
