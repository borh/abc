# ADR Evidence Corpus Migration Design

Date: 2026-07-12
Status: Approved for implementation planning
Parent:
`2026-07-12-adr-artifact-backed-evidence-enforcement-design.md`

## Purpose

Migrate the Accepted ADR corpus from audit-only typed-evidence reporting to
strict artifact-backed enforcement without inventing observations, weakening
substantive decisions, or treating a successful test process as proof of
claims that the test does not assert.

The protocol checkpoint contains 26 Accepted ADRs and 146 Acceptance Criteria.
Its honest audit has 196 problems: 146 missing claim headers, 25 missing
validation scopes, and 25 missing release authorities. Those counts describe
the pre-review corpus. They are not targets to preserve after false,
historical, compound, or non-observable criteria are corrected.

## Governing Decisions

1. Evidence is grouped by the real observation boundary, not by ADR and not by
   an arbitrary family-wide command. Related claims may reference the same
   immutable bundle, but every claim retains its own typed registry entry and
   derived predicate.
2. Criterion correction precedes stable claim-ID assignment. Existing criteria
   have no claim IDs, so moving a review checklist or future guard out of
   Acceptance Criteria does not break an established identifier. After claim
   headers land, criterion order and claim IDs are stable.
3. Correction is narrower than demotion. Correct or split an overbroad
   criterion while preserving the ADR's Accepted core. Demote an ADR only when
   the central decision lacks support.
4. A passing focused namespace supports only assertions that namespace
   actually makes. Loading a file or function does not establish every
   statement about it.
5. Command-level behavior requires a direct operational observation. Helper
   tests cannot impersonate a Nix application, shell wrapper, CI job, or
   corpus command.
6. Historical measurements remain historical values under their exact
   identities. They are never silently promoted onto current parser, mapping,
   schema, corpus, source-bundle, or environment coordinates.
7. Validation scope and release authority describe different facts and are
   reviewed per ADR. Neither is inferred from evidence kind.
8. Audit mode remains active throughout family migration. Enforcement begins
   only in the atomic ADR 0034 transition.

## Artifact Model

### Migration decision ledger

`docs/adr/adr-claim-migration.edn` is the reviewed source ledger. Before claim
headers exist, entries are keyed by `[adr-number criterion-index]`. Each entry
records exactly one disposition:

- `:retain` — the current statement is observable and supportable;
- `:correct` — retain the decision but narrow or split the criterion;
- `:move-out-of-acceptance` — preserve the text as Decision, Consequence,
  Historical Evidence, or Future Verification rather than treating it as a
  satisfied observation;
- `:demote-adr` — the central decision cannot remain Accepted.

Every non-`:retain` entry carries a rationale. Every retained or corrected
criterion names its planned evidence boundary. The generated migration
inventory exposes these dispositions; it must not leave reviewed rows as
`null`.

### Capture descriptors and bundles

Checked-in descriptors live under `docs/evidence/adr-capture/`. Executable
bundles live under `docs/evidence/adr-runs/`. A descriptor is included in its
own explicit input set so the recorded command/profile cannot drift without
staling the bundle.

External-authority summaries and expert assessments live under
`docs/evidence/external/`. They are bounded documents with independently
hashed inputs and explicit review dates; prose in an ADR is not independent
evidence for that ADR.

The authoritative claim join remains `docs/adr/adr-evidence.edn`. Registry
entries never contain observations, input hashes, or stored verdicts.

### Generated reports

`docs/reports/adr-claim-migration-inventory.json` reports the reviewed corpus,
families, claim headers, citations, and dispositions.
`docs/reports/adr-evidence-migration.json` is the deterministic governance
report. Generated reports are outputs, not hand-edited sources.

## Family Transaction

Each family is migrated in two clean-tree stages.

### Stage A: correct and bind the claims

1. Review every family criterion against current source, tests, fixtures, and
   immutable reports.
2. Record its disposition in the migration ledger.
3. Repair a broken operational contract when it is central to an Accepted
   decision; do not rewrite the criterion merely to hide the break.
4. Narrow, split, or move unsupported wording as required.
5. Add missing focused assertions for observable core invariants using
   test-first development.
6. Add lifecycle headers and stable claim headers only after the corrected
   criterion set is final.
7. Commit and verify the family in audit mode. Missing evidence is expected at
   this intermediate commit; new parser, lifecycle, graph, or dependency
   problems are not.

### Stage B: capture and join evidence

1. Capture from the clean Stage A commit.
2. Bind namespace closure plus every runtime-read schema, fixture, registry,
   Markdown file, report, sidecar, and generated view explicitly.
3. Use direct commands for operational claims. A command returning zero is a
   Boolean observation only when zero is the command's semantic success
   condition.
4. Add bundles and claim-level registry entries, then regenerate audit and
   inventory reports.
5. Require the family audit debt to disappear without introducing a different
   problem kind.
6. Commit bundles, registry entries, and generated reports together.

## Shared Prerequisite

The advertised `nix run ./abc#validate-design-bundle` boundary currently
reaches the final Git-cliff check from a pinned Nix-store source that is not a
Git repository. Several Accepted criteria claim that this command succeeds.
The command is a core development validation boundary, so the first plan must
repair and test the Git-cliff/store seam rather than narrow every criterion
away from the supported Nix application.

The same prerequisite adds the migration-ledger schema/validation and teaches
the inventory generator to report dispositions. No family evidence is
captured before this shared boundary is green.

## Evidence Boundaries

Evidence boundaries are smaller than families and may support claims from
multiple ADRs.

- Focused Clojure boundaries use `clojure-test-v1`, with runtime data explicit.
- Rust or Python checks use `repo-files-v1` and a bounded command whose exit
  status is the observation.
- Nix applications and checks use operational bundles and the exact supported
  invocation.
- Corpus observations bind immutable reports, corpus/run-set identities, and
  the generator/checker inputs. Machine-local `/db/...` paths are locators,
  never identities.
- Expert assessments state their assumptions and falsifiers. They cannot
  satisfy structural, fixture, corpus, performance, or release predicates.

A generic family-suite observation is forbidden when the suite does not
assert every attached claim. Shared bundles should expose narrowly named
observations or be split at the actual assertion boundary.

## Lifecycle Assignments

Release authority describes what the decision may govern, not whether the
repository is currently releasable. Publication remains blocked by
`data/publication-policy.edn` until the rights assessment migration completes.

| ADR | Validation scope | Release authority |
| --- | --- | --- |
| 0001 | `fixture` | `publication` |
| 0002 | `smoke-corpus` | `development` |
| 0006 | `fixture` | `development` |
| 0007 | `fixture` | `development` |
| 0008 | `operational` | `development` |
| 0009 | `fixture` | `development` |
| 0010 | `fixture` | `publication` |
| 0011 | `fixture` | `development` |
| 0012 | `fixture` | `publication` |
| 0013 | `fixture` | `publication` |
| 0014 | `fixture` | `publication` |
| 0015 | `fixture` | `publication` |
| 0016 | `fixture` | `publication` |
| 0017 | `fixture` | `publication` |
| 0018 | `fixture` | `none` |
| 0020 | `fixture` | `publication` |
| 0021 | `fixture` | `publication` |
| 0022 | `operational` | `development` |
| 0023 | `fixture` | `publication` |
| 0024 | `fixture` | `publication` |
| 0025 | `fixture` | `publication` |
| 0029 | `fixture` | `none` |
| 0030 | `full-corpus` | `development` |
| 0031 | `full-corpus` | `none` |
| 0032 | `full-corpus` | `development` |
| 0033 | `full-corpus` | `publication` |

ADR 0018 receives `none` because its indivisible historical decision includes
the now-contained Boolean-to-external-rights mapping. ADR 0035 may later
restore publication authority to a corrected assessment contract. ADRs 0030
and 0032 receive `development`: they authorize which code is developed and
maintained while explicitly denying publication admission.

ADR 0034 uses `full-corpus` and `none` on promotion. Its binding acceptance
condition is complete Accepted-corpus conformance, not merely structural
implementation of the validator.

## Family Obligations

### Foundation, runtime, and identity

- Correct ADR 0001/0009 citations that name materialization tests as schema
  validation evidence when those tests do not validate the schemas.
- Narrow ADR 0001's reproducibility conflict to the implemented artifact-ID
  oracle and move the unsatisfied non-JVM JCS guard to Future Verification.
- Repair direct design-bundle execution before evidencing ADR 0008, ADR 0010,
  or ADR 0011 command claims.
- Split source-bundle criteria that combine Clojure fixture reproduction,
  Rust producer policy, simulation behavior, corpus measurement, and limit
  enforcement.
- Preserve source roles: `work_content_hash = bundle_hash`, independent
  `primary_text_hash`, exact archive identity, and exact parser/mapping
  coordinates.
- Narrow historical readability to the versioned fixtures actually exercised
  unless a bounded historical corpus sweep is captured.

### Schema, RDF, and TEI

- Remove the nonexistent claim that Schematron failure materializes a failure
  manifest unless that behavior is implemented and tested.
- Attribute ODD-derived RNG/Schematron byte parity to the Nix drift check, not
  to the design-bundle command.
- Replace warning-report materialization with the actual warning-finding
  behavior unless an end-to-end report test is added.
- Bound Linked Art and IIIF claims to their actual committed fixtures and
  conditional applicability contract.
- Correct ADR 0017 historical rotation/all-Turtle wording to exact current
  parity boundaries.
- Amend ADR 0018 for rights containment: zero or one rights IRI is permitted;
  when present it is from the closed set. The legacy Boolean emits no external
  rights assertion. Historical manifest-byte equality is not a current
  acceptance predicate.

### Temporal, person, and ingest

- Split schema-shape assertions from malformed-fixture behavior.
- Keep date-normalization evidence bounded to named cases and do not claim that
  legacy `nil` preserves temporal knowledge.
- Move ADR 0016's hard-rule/source-of-truth prose out of Acceptance Criteria
  unless an end-to-end identity test is added.
- Move ADR 0020 decision-review and canonical-graph checklists out of
  Acceptance Criteria; ADR prose cannot self-certify completeness.
- Do not pass the drift-sidecar/manifest identity or ABC-local schema-cascade
  claims without explicit before/after changed-set tests.
- Complete missing drift invariants only when they are core: duplicate and
  interleaved participant use, ordering, SHACL resources, associated-agent RDF,
  and committed example counts.
- Correct the upstream-audit failure claim: a non-empty drift update list is
  one failure condition, not the command's only possible nonzero condition.
- Separate raw-ingest independence from the broader undefined phrase
  “source-faithful.”

### Parser, IR, and publication

- Preserve the July parser reports as historical measurements and provenance.
  They were not preregistered under the neutral comparison protocol and cannot
  prove comparative victory or release qualification.
- Narrow ADR 0002's Accepted slice to file-contract, evidence-classification,
  and exact-tuple policy. Neutral comparison and release viability remain
  Proposed work.
- Correct ADR 0030: its cited reports prove that the historical study and
  inheritance decision occurred, not that the chosen parser met every ADR
  0002 must-pass gate.
- Introduce ADR 0038 as the corrective ownership decision. Its basis is a
  bounded expert assessment that maintaining the custom parser is a project
  constraint independent of rankings. It must forbid comparison evidence from
  satisfying admission or release claims.
- Keep neutral comparison and release qualification separate. ADR 0039 cannot
  be Accepted until the exact tuple and every declared release predicate pass.
- Strengthen parser citation tests to recompute logical file hashes and reject
  explanatory/compatibility evidence at selection/admission transitions.
- Preserve Phase 5 only under its frozen tuple:
  `ab-aozora 0.6.0` at
  `004deaf548f34a36abbc17d0f7a162df010a6292`, mapping `0.4.0` hash
  `sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30`,
  and parser-IR schema hash
  `sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2`.
  Do not join this evidence to current source-identity coordinates by an
  adapter name or “latest” mapping.
- Split tested furigana rendering from the now-obsolete “not Level 3 until”
  future condition.

### Diagrams and governance

- Split committed diagram byte parity from diagram API error semantics.
- Describe architecture and ADR graph checks as declared structural totality,
  not semantic proof that every declared owner or relation is correct.
- Split ADR 0031 parser/policy fixture behavior from current-corpus
  conformance. Corpus cleanliness requires a deterministic zero-problem
  governance report.
- Bind every runtime-read ADR, sidecar, architecture registry, workflow
  fixture, and generated Mermaid file explicitly.
- Regenerate derived views after lifecycle, dependency, or status changes.

## ADR 0034 Self-Certification

ADR 0034 has three distinct boundaries:

1. C1 is structural evidence for the typed artifact protocol: closed
   compatibility, artifact-backed observations, derived predicates, input
   freshness, and deterministic external dates.
2. C2 is structural evidence for lifecycle/dependency witness behavior and
   audit/enforce command semantics. Stubbed command tests do not establish
   real-corpus success.
3. C3 is a corpus measurement over the complete pre-promotion Accepted corpus
   with `ok = true`, zero problems, exact counts, explicit evaluation epoch,
   and bound validator inputs.

Audit exit zero is never evidence for C3 because audit intentionally exits zero
when `ok` is false. The measurement producer must inspect report content or use
a command whose exit code reflects `ok = true` and zero problems.

The C3 boundary excludes its own future registry entry and bundle identity;
otherwise the registry hash would depend on the bundle that depends on the
registry. This pre-promotion bootstrap observation is followed by live strict
enforcement on the final post-promotion tree.

The atomic final commit contains ADR 0034 Accepted, all three evidence entries,
valid artifacts, full registry coverage, regenerated views, and the Nix gate
in enforcement mode. No committed intermediate state may mark ADR 0034
Accepted while CI remains audit-only.

## Error Handling and Recovery

- A capture failure creates no registry entry.
- A stale shared artifact emits one root problem with all affected claim IDs.
- A family audit that replaces missing evidence with a different failure is
  not progress and blocks that family.
- If a core command cannot be repaired without expanding scope, keep audit
  mode and request review; do not narrow the claim silently.
- If the final transition fails, retain ADR 0034 Proposed and audit mode.
  Repair forward; do not restore path-only evidence, inline observations,
  scoped dependency waivers, or placeholder passes.

## Verification Strategy

Each family plan uses test-first changes for new behavior or repairs, focused
verification for every evidence boundary, artifact hash/input validation, and
an audit-delta assertion. Each task receives independent specification and
quality review.

The final verification set is:

- focused ADR/evidence/governance tests;
- all family-specific Clojure, Rust, Python, and Nix checks;
- direct `nix run ./abc#validate-design-bundle`;
- `nix build ./abc#checks.x86_64-linux.adr-governance` in enforcement mode;
- `nix build ./abc#checks.x86_64-linux.clj-kondo`;
- `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests`;
- regenerated diagram/schema drift checks;
- root `just validate-migration`.

Completion means the migrated corpus, including ADR 0034, conforms to the
implemented typed-evidence policy and enforcement is active. It does not mean
the governance policy proves its own semantic correctness.

## Implementation Plan Decomposition

Write six independently reviewable implementation plans:

1. shared prerequisite plus foundation/runtime/identity;
2. schema/RDF/TEI;
3. temporal/person/ingest;
4. parser/IR/publication and ADR 0038 ownership correction;
5. diagrams/governance;
6. ADR 0034 self-certification and enforcement.

Plans 2 through 4 may be reviewed independently after the shared prerequisite.
In one worktree they execute sequentially because they share the ADR registry,
migration ledger, generated reports, and derived ADR graph. Plan 5 consumes
the final corrected ADR set. Plan 6 consumes every prior family and is the only
plan authorized to switch enforcement on.
