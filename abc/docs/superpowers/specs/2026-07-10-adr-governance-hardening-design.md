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
header parser. This explicitly relocates the recently added malformed-reference
width parsing and linting from `adr_graph.clj`; it does not reimplement that
behavior in parallel.

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
The comparison is set-based because JSON Schema's `required` array order is not
semantic. `architecture.md` retains a human-readable presentation order without
claiming that order as a second contract.

The manifest stage must cite every ADR explicitly recorded as introducing or
amending a manifest identity coordinate. Add per-coordinate ownership to
`docs/architecture-stages.edn` as a top-level value:

```clojure
:manifest-identity-contract
{:schema "schemas/manifest.schema.json"
 :coordinates
 {"manifest_schema_hash" [1 10]
  "corpus_snapshot_hash" [1]
  "work_content_hash" [1]
  "metadata_record_hash" [1]
  "parser_build_hash" [1]
  "parser_config_hash" [1]
  "aat_parser_ir_mapping_hash" [23]
  "parser_ir_schema_hash" [1]
  "tei_profile_hash" [1]
  "tokenizer_build_hash" [1]
  "tokenizer_dictionary_hash" [1]
  "tokenizer_profile_hash" [27]
  "analysis_recipe_hash" [1]
  "annotation_policy_hash" [28]
  "output_format_spec_hash" [1]}}
```

The architecture validator requires the coordinate key-set to equal the
manifest schema's required identity-property set, derives the owning ADR set
from the union of all coordinate values, and requires the manifest stage's ADR
list to equal that derived set. Ownership must not be inferred from arbitrary
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

The header grammar is closed and line-oriented. An ADR begins with exactly one
`# ADR NNNN: Title` line, one blank line, and then a contiguous block of
single-line `Field: value` entries. The first blank line after a field ends the
header. Multi-line values are not legal; rationale belongs in the body.

The recognized field vocabulary is `Status`, `Date`, `Accepted`, `Supersedes`,
`Superseded by`, `Amends`, `Amended by`, `Depends on`, and `Source`. `Source`
accepts one non-empty line of human-readable source references. Within the
header block, an unknown field, a non-field line, duplicate field, or empty
value is a validation problem rather than ignored input.

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

`Amends`, `Amended by`, `Depends on`, `Supersedes`, and `Superseded by` use a
comma-separated relation grammar. Each item is exactly `ADR NNNN` optionally
followed by `[scope: non-empty text]`; no other prose or document paths are
legal in relation fields. `Supersedes: none` is the sole non-relation sentinel;
the other relation fields are omitted when empty. Brackets make scope
unambiguous rather than relying on parentheses that cannot be distinguished
reliably from ordinary prose. Reciprocal relation items must carry identical
scope text.

Every `Supersedes` relation requires a reciprocal `Superseded by` entry with the
same scope. A scoped supersession does not force the referenced ADR's entire
status to `Superseded`. An unscoped supersession does: its target must have
`Status: Superseded`. Every ADR with `Status: Superseded` must be the target of
at least one unscoped `Supersedes` relation. ADR 0012's current free prose is
migrated to the bracketed scoped form rather than grandfathered.

`Amends` records an intended relationship at any lifecycle state, but it changes
the canonical meaning of its Accepted target only when the amending ADR is also
Accepted. A Proposed amendment is pending. Its reciprocal `Amended by` entry is
navigational and does not make it effective. Live schema behavior introduced by
a Proposed ADR is an implemented provisional surface, not implicit ratification;
the architecture metadata records that implementation fact separately.

### Required sections

Every Accepted ADR has:

- `## Decision`;
- `## Implementation Status`; and
- `## Acceptance Criteria` with executable evidence.

Draft and Proposed ADRs may state Acceptance Criteria as future promotion
conditions. They are not required to cite already-existing executable evidence.
Any evidence path they do cite is validated for existence under the same path
grammar, so an explicit broken reference still fails.

### Dependency status and scope

An Accepted ADR cannot depend on a Draft or Proposed ADR without an explicit
scoped dependency annotation. The machine-readable syntax is:

```markdown
Depends on: ADR 0002 [scope: source_span_coverage gate]
```

Each bracketed scope binds to the immediately preceding ADR reference. Scope is
a non-empty human-readable string. The validator rejects unscoped
Accepted-to-Draft and Accepted-to-Proposed dependencies.

An Accepted ADR may not depend on a Withdrawn or wholly Superseded ADR, scoped or
otherwise; it must depend on the current successor or restate the required
contract. Draft and Proposed ADRs may depend on non-Accepted ADRs without a scope
because their own contracts are not canonical. The graph label for a scoped
dependency includes the scope so the generated view does not erase it.

### Acceptance evidence

Each Accepted ADR's Acceptance Criteria section must contain at least one
repository-relative evidence reference under an allowed executable surface:

- `test/` for Clojure tests;
- `fixtures/` for validated fixtures;
- `nix/` for executable check scripts.

Committed Prolog evidence under `fixtures/v0/facts/prolog/` is included by the
`fixtures/` rule.

Every referenced path must exist. A directory reference counts only when the
same criterion item also cites an existing `test/` or `nix/` harness path that
consumes it. A path substring in prose is not sufficient: references are
extracted from Markdown code spans and validated as paths relative to `abc/`.
Only code spans beginning with an allowed prefix are candidate evidence paths;
code spans naming tests, functions, or other identifiers are ignored. A
misspelled prefix is therefore not independently detectable, but an Accepted
ADR still fails unless at least one allowed-prefix path resolves.

The governance check proves evidence presence and addressability. Actual test
execution remains the responsibility of the focused and full Clojure/Nix test
checks; the ADR check does not build a second test runner.

## Existing-Set Migration

The migration repairs all current violations before removing the old gate:

- Update `docs/adr/README.md` to document the closed header grammar, structured
  relation scopes, supersession reciprocity, status-sensitive evidence rule,
  and removal of the legacy allowlist.
- Move ADR 0028's status commentary into its existing Implementation Status
  section and leave its exact status `Proposed`.
- Recover ADR 0016's acceptance date from repository history or contemporaneous
  evidence and add the field. If history cannot establish a distinct date, use
  its recorded decision date and document the evidence in Implementation
  Status.
- Add Implementation Status sections to Accepted ADRs that lack one. These
  sections describe current repository evidence without rewriting immutable
  Decision or Hard Rule text.
- Add real evidence references to every Accepted ADR's Acceptance Criteria
  section currently covered by the legacy allowlist. Where no executable
  evidence exists for an Accepted contract, add the missing characterization
  test or fixture rather than inventing a reference. The user independently
  accepted ADR 0030, which amended and accepted ADR 0002; this is not an
  incidental governance promotion. Only ADRs 0003–0005 remain Draft and keep
  their criteria as future promotion conditions without fictional evidence.
- Add explicit scopes to Accepted-to-non-Accepted dependencies, including ADR
  0024's dependency on ADR 0002.
- Rewrite ADR 0023 and ADR 0024 relation headers to contain only structured ADR
  relations; move their handoff-document references to `Source` or body prose.
- Rewrite ADR 0012's scoped supersession using `[scope: TEI stub language]` and
  add the reciprocal scoped `Superseded by` entry to ADR 0006 without changing
  ADR 0006's overall Accepted status.
- Collapse every wrapped `Source` or other header value to one physical line and
  repair any header that violates the closed field vocabulary.
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
- unknown, duplicate, empty, and multi-line header fields;
- required and forbidden acceptance dates;
- acceptance-date ordering;
- filename/title mismatch and duplicate numbers;
- malformed, dangling, reverse-only, scoped, and unscoped relations;
- scoped supersession versus whole-ADR supersession status behavior;
- Accepted-to-Draft dependencies with and without scopes;
- Accepted dependencies on Withdrawn and wholly Superseded ADRs;
- pending Proposed amendments into Accepted ADRs;
- missing Acceptance Criteria or Implementation Status;
- Accepted versus Draft/Proposed evidence requirements;
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
2. Repair ADR lifecycle headers, relations, sections, and Accepted evidence;
   then add the clean-repository test.
3. Switch the flake governance check to the Clojure validator, then delete the
   shell gate and allowlist.
4. Add per-coordinate architecture ownership and semantic-completeness checks;
   then repair `architecture.md`, `architecture-stages.edn`, and the generated
   architecture view.
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
