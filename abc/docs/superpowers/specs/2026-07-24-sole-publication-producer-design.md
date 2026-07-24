# Sole Publication Producer

**Date:** 2026-07-24 · **Status:** Approved design

## Goal

Make Soranoha's publication architecture have one production authority:

```text
official Aozora checkout + build policy
                    |
                    v
         soranoha build-publication
              release lifecycle
                    |
                    v
            materialize-publication!
             per-work artifact writer
                    |
                    v
              publication root
       artifacts · manifests · reports
       provenance · failures · run trace
```

`soranoha build-publication` is the sole supported command that turns an
official source checkout into release-facing publication artifacts.
`abc.tools.materialize-publication/materialize-publication!` is the sole
implementation boundary that writes the per-work publication artifact set.

The word “sole” applies at two levels:

1. **One release workflow.** No second CLI, Nix app, rehearsal, batch command,
   or snapshot reproduction path independently claims production authority.
2. **One per-work writer.** Production, fixture validation, and parser
   qualification may call the same lower-level function, but none may
   reimplement its plaintext, TEI, preservation, validation-result, or manifest
   writes.

This is a subtractive refactor. It should remove alternative authority,
duplicate lifecycle code, and historical workflow surfaces. It must not replace
them with a generic pipeline framework, command registry, release database, or
new identity layer.

## Why this is the next high-leverage boundary

The current architecture has two coherent but competing publication systems.

The snapshot/rehearsal path in `abc.tools.soranoha` owns request-set resolution,
source snapshots, snapshot-index identity, per-work reproduction, reports,
staging, validation, and a rehearsal workflow. The direct
`abc.tools.soranoha-build-publication` path owns official-source discovery,
adapter execution, source materialization, per-work publication, caching,
failure handling, run records, and atomic promotion.

That duplication was not an oversight. The original
`2026-07-08-soranoha-build-publication-design.md` required the direct build to
delegate to `publication-rehearsal!`. Commit `587bd182` deliberately removed
the delegation because the rehearsal path was fixture/request-set-bound and
produced stub parser IR rather than real body text. The direct path became the
real full-corpus implementation.

The mistake would be to restore the old delegation. The useful production
behavior is in `build-publication`; the architectural repair is to make that
authority explicit and retire the superseded path.

The split currently makes basic questions need two answers:

- What command produces a publication?
- Which top-level identity and report describe it?
- Where are failure and atomicity semantics enforced?
- Which output tree is release-facing?
- Which workflow trace is authoritative?
- Where should a new publication capability be added?

Making `build-publication` the sole producer gives each question one owner. It
also creates the deletion seam needed to reduce `soranoha.clj`,
`materialize_publication.clj`, the CLI surface, workflow apparatus, tests, and
schemas without a rewrite.

## Architectural decision

### Production authority

`soranoha build-publication` owns:

- official-source checkout validation and source selection;
- source-bundle and parser input derivation;
- release-policy and rights-policy admission;
- build configuration and operational provenance;
- failure policy;
- concurrency and cache reuse;
- the call to the per-work artifact writer;
- corpus-level results and failure accounting;
- temporary-root handling and atomic promotion;
- the supported production output-root contract.

No other command may combine those responsibilities into an alternative
publication workflow.

This is authority ownership, not a requirement that one namespace implement
everything. The command is a narrow façade over cohesive capabilities:

```text
build-publication
  ├── source selection and source/parser derivation
  ├── per-work materialize-publication!
  ├── corpus result assembly
  └── output-root promotion
```

Each capability may live with its domain and be tested independently. What it
may not do is expose another source-to-release composition. The façade should
be deep—one production operation hiding lifecycle detail—while its internals
remain ordinary functions over explicit values.

### Per-work publication boundary

`materialize-publication!` owns exactly one work's publication values:

- visible-body plaintext;
- TEI;
- preservation sidecar;
- TEI validation result;
- plaintext manifest;
- TEI manifest.

Its input is an explicit record of parser IR, source manifest, metadata/person
records, output directory, and generation time. Its output is a record of the
written artifact paths. Renderers and validators remain behind this boundary;
callers do not reproduce their sequencing or file layout.

The function is an internal library capability, not a second release entry
point. These calls remain legitimate:

- `build-publication`, as the sole production caller;
- `validate-design-bundle` and focused tests, as fixture characterization;
- parser-RQ publication materialization, as a qualification instrument whose
  outputs are evidence rather than a release.

Calling the function does not confer release authority. Release admission,
corpus accounting, atomic promotion, and the top-level output contract belong
only to `build-publication`.

### Read-side capabilities

Validation, explanation, reporting, and staging are projections over a
completed production root. They do not mint a competing publication identity
or independently materialize the canonical per-work artifact set.

A read-side capability stays public only when it has a live operator or
consumer:

- validation checks values already present in a completed root;
- explanation prints identity and provenance already present;
- a report derives a view from the completed root;
- staging copies or packages already-authenticated artifacts without
  regenerating their contents or identities.

If staging is a required part of release, `build-publication` invokes it and
records its result. A standalone staging command may remain only as a
repeatable projection from the same completed root; its output is a deployment
view, not a second publication.

### Introspection is data, not another subsystem

The production root must answer operational questions from a small, closed set
of values on disk. The current likely set is:

```text
<output-root>/
  build-config.json
  build-plan.json
  source-selection-report.json
  workflow-plan.json
  workflow-run.json
  materialized-root/
    works/<work-slug>/...
  publications/
    publications-report.json
    <work-slug>/
      plain.txt
      plaintext.manifest.json
      tei.xml
      tei.manifest.json
      tei-validation-result.json
      preservation.json
      source_work_content_hash.txt
```

This list is a disposition input, not permission to preserve every file.
Implementation chooses the smallest sufficient authoritative set and labels
each retained value as one of:

- identity-bearing;
- release/admission-bearing;
- operational provenance;
- derived report;
- diagnostic trace.

Paths stored in retained values are relative to the production root where
possible. A user should be able to determine what was selected, what was
published, what failed, which inputs and policies applied, and which artifacts
carry the authoritative hashes without replaying the workflow.

Do not add a `release-index` schema merely to make the target diagram tidy.
First determine whether one existing value—most likely
`publications-report.json`—can become the closed corpus summary. A new value is
justified only if a live contract cannot be expressed by deleting or
strengthening an existing one.

## Current surface disposition

The following is the design disposition. Deletion still requires the closed
consumer/claim audit below.

| Current surface | Target disposition | Reason |
| --- | --- | --- |
| `soranoha build-publication` | Keep: sole production front door | It is the real full-corpus path and owns atomic promotion and failure policy. |
| `materialize-publication!` | Keep and deepen: sole per-work writer | It already centralizes the publication artifact set. |
| `materialize-release-publication!` | Retire as a release boundary | The release gate belongs at the sole corpus workflow, not a second per-work entry point. |
| `materialize-publication` Nix app/CLI | Retire or explicitly demote to development-only after consumer audit | As a supported app it is a second artifact-producing front door. |
| `materialize-publications-batch` app/CLI and manual workflow record | Retire after consumer audit | `build-publication` already owns bounded parallel corpus materialization; historical report command strings are frozen facts, not live consumers. |
| `publication-rehearsal!`, command, report, and workflow | Retire | It is the superseded end-to-end production composition. |
| `reproduce!` and `materialize-snapshot-root!` publication loop | Retire or split | Their artifact-writing role competes with the sole producer; pure request-set/analysis behavior may survive under its actual owner. |
| `source-snapshot`, request-set, and snapshot-index functions | Disposition by live capability | Keep pure identity/data transformations only where current analysis or audit consumers need them; do not preserve them to support the retired rehearsal. |
| snapshot `validate`, `explain`, reports, and staging | Rebase on the production root or retire | They are allowed only as read-side projections with an identified consumer. |
| `validate-workflow` in the Soranoha dispatcher | Move to its domain or retire | Generic run-record validation is not publication command dispatch. |
| parser-RQ publication materializer | Keep as non-release qualification capability | The warm qualification campaign needs the same renderer boundary without becoming a release path. |
| `validate-design-bundle` publication fixture | Keep as integration characterization | It exercises the writer over committed fixtures; it does not own production. |

The target dispositions deliberately do not classify every request-set,
snapshot, analysis, annotation, layout, and staging function as dead. Their
production-composition role is dead; their independent data-model role must be
proved or disproved by consumers and decision claims.

## The hardest design problem: identity disposition

The two paths do not merely sequence the same functions differently. They
publish different top-level identity stories.

The rehearsal path treats `request_set_id`, `source_snapshot_hash`, and
`snapshot_identity_hash` as the citable publication identity. The direct path
records a build-config hash, Aozora Git provenance, a corpus snapshot hash,
per-work source/content identity, per-work manifests, and a corpus publication
report, but does not currently produce the rehearsal's snapshot root.

The implementation must not silently crown both identity systems or combine
all their fields into a larger identity object. Before changing either path,
produce a field-by-field identity disposition:

| Field or value | Construction owner | Current consumers | Decision claim | Target role |
| --- | --- | --- | --- | --- |
| Aozora Git commit/dirty state | direct build | to audit | to audit | operational or source authority |
| `corpus_snapshot_hash` | source materialization | to audit | source-bundle claims | identity-bearing |
| `work_content_hash` | source bundle | manifests/cache | Accepted source-bundle claims | identity-bearing |
| parser and mapping hashes | source/parser boundary | manifests/RQ | Accepted parser decisions | identity-bearing |
| publication manifest identities | per-work writer | validators/release | Accepted manifest/rendering decisions | identity-bearing |
| `request_set_id` | request-set resolver | analysis/snapshot path | currently Proposed analysis decisions plus code consumers | keep only for a live selection/analysis capability |
| `source_snapshot_hash` | source-snapshot path | request-set/snapshot path | Accepted source-bundle validation may use the underlying capability | distinguish source-bundle authority from rehearsal composition |
| `snapshot_identity_hash` | snapshot-index path | rehearsal reports/staging | to audit | migrate only if a live release contract requires it |
| config hash and concurrency | direct build | build plan | operational | non-identity provenance |
| workflow run id/timestamps | workflow runner | diagnostics | none identified | non-authoritative trace |

“To audit” means enumerate repository consumers, Accepted decision claims, Nix
checks, active operator documentation, and known external integrations. Draft
or Proposed decisions are design input, not an obligation to keep unused
machinery warm.

The preferred outcome is not predetermined field survival. It is one explicit
identity chain from source authority through per-work manifests to the
corpus-level production summary, with operational values kept outside that
chain.

## Closed disposition audit

The first implementation artifact is a disposition table, not a code change.
It must be complete over:

1. Soranoha commands and Clojure entry points.
2. Root and `abc` flake apps, `deps.edn` aliases, launchers, and checks.
3. Output files and their schemas.
4. Accepted decision claims and their cited evidence.
5. Active documentation and runbooks.
6. In-monorepo consumers, including parser-RQ, analysis, annotation, and
   staging.
7. Known out-of-repository/public consumers.
8. Tests, separated into domain characterization and tests of the apparatus
   being retired.

Every row receives one disposition:

- **production authority** — must be owned by `build-publication`;
- **shared domain capability** — a lower-level function used by production and
  non-release instruments;
- **read-side projection** — consumes completed production values;
- **fixture characterization** — tests a retained boundary;
- **qualification instrument** — produces evidence, not releases;
- **frozen historical reference** — remains readable but is not executable
  support;
- **retire** — no owned capability;
- **unknown** — blocks deletion until resolved.

No compatibility command or dual output format is added to handle an unknown.
Resolve it, explicitly retain it, or stop that cut.

## Failure semantics and guardrails

Singular authority makes failure behavior easier to state, not optional:

- malformed or inadmissible source/parser identity fails before release
  promotion;
- a strict materialization failure leaves the temporary run inspectable and
  does not replace the prior completed root;
- best-effort mode records every known failed work, marks the corpus result
  non-admissible/partial, may promote that inspectable partial root under the
  existing Accepted source-bundle contract, and exits nonzero;
- cache reuse preserves the current `work_content_hash` match requirement; the
  disposition audit decides which additional retained manifest/reference checks
  are required before copied artifacts are trusted;
- a malformed corpus summary, broken relative reference, or missing required
  manifest fails closed;
- read-side validation and explanation do not mutate the completed production
  root.

The refactor must characterize the current distinction between strict failure
and best-effort partial promotion before moving code. It must not “simplify”
the distinction into an exception/log convention. The other bullets are target
guardrails to verify or close, not a claim that every check is already enforced
at the direct-build boundary.

## Migration sequence

### Cut 0: characterize the sole producer

Pin a small official-Aozora-shaped fixture through `build-publication` and
record:

- its complete output tree;
- which files are authoritative and which are diagnostic;
- byte hashes for deterministic values;
- exit behavior for success, strict failure, and best-effort partial failure;
- atomic replacement behavior;
- per-work artifact equivalence with a direct call to
  `materialize-publication!`.

This is characterization, not a permanent golden copy of volatile timestamps
or temporary paths. Normalize or exclude operational fields rather than making
them identity-bearing.

Complete the closed disposition and identity tables in the same review slice.

### Cut 1: deepen the retained boundaries

Make the direct build's ownership visible without adding layers:

- keep policy admission, temp-root lifecycle, cache reuse, corpus failure
  accounting, and promotion in `build-publication`;
- keep all six per-work publication writes in `materialize-publication!`;
- make required inputs explicit at the call boundary;
- keep qualification and fixtures on the same writer;
- choose one corpus summary and validate its references to per-work manifests;
- keep run traces operational and outside artifact identity.

Any release capability found only in the rehearsal path is either:

1. moved behind `build-publication` because a current contract requires it; or
2. retired because only the rehearsal itself consumes it.

It is never preserved by retaining the whole rehearsal.

### Cut 2: retire the secondary producer surfaces

Atomically remove each confirmed-dead vertical slice:

- command-table entry;
- Clojure public orchestration function;
- Nix app and `deps.edn` alias;
- workflow/report emitter;
- schema and fixtures owned only by that surface;
- apparatus-only tests;
- active documentation that advertises the command.

Historical reports keep their recorded command strings. Frozen evidence is not
rewritten to pretend the old command never existed.

The standalone batch materializer is a good first cut if the audit confirms no
live external consumer: its concurrency behavior already exists in
`build-publication`, and its hand-built workflow record is duplicate lifecycle
machinery.

The rehearsal/reproduce cut follows only after the identity table decides the
fate of request-set and snapshot values.

### Cut 3: collapse `soranoha.clj`

Deletion should expose the real remaining modules:

- thin CLI dispatch;
- request-set/analysis data operations, if retained;
- completed-root inspection/projection, if retained;
- production delegation to `soranoha-build-publication`.

Do not begin by splitting the 1,296-line file into several equally shallow
namespaces. Remove retired workflows first, then move only cohesive surviving
capabilities to their owners. The dispatcher should contain command
descriptions and delegation, not publication materialization or report
construction.

### Cut 4: re-evaluate the serial workflow runner

The workflow runner is an implementation detail, not a promised architecture.
After secondary workflows are gone:

- keep it if at least two live workflows require the same step dependency,
  failure, and trace semantics;
- otherwise inline the sole build's short stage sequence and emit the retained
  run value directly.

Do not keep a workflow abstraction solely to serialize three functions, and do
not replace it with the retired target-graph engine.

### Cut 5: shrink the integration gate

Once production ownership is singular, `validate-design-bundle` can stop
compensating for ambiguous integration boundaries:

- domain fixtures are materialized and checked by their domain owners;
- the bundle gate keeps one small cross-boundary publication smoke;
- Accepted claims are amended only where their stated evidence boundary must
  change;
- schema/path registries owned only by duplicated fixture orchestration are
  removed.

This follows the producer refactor. Doing it first would move complexity around
while both production models still exist.

## Change discipline

- Preserve current release behavior before improving output contracts.
- Separate behavior-preserving moves from intentional command/schema breaks.
- Make each retired surface an atomic vertical slice.
- Do not add compatibility wrappers for commands explicitly retired as
  non-authoritative.
- Do not treat historical reports or plans as live consumers.
- Do not delete pure identity or validation functions merely because their
current orchestration caller is deleted; prove their remaining reachability.
- Do not keep an orchestration function merely because its tests call it;
  distinguish boundary characterization from apparatus self-tests.
- Add a decision record only when the disposition changes a governed
  capability or claim. Do not mint an ADR just to bless a namespace layout.
- Measure commands, aliases, schemas, source/test LOC, namespace dependencies,
  and gate time before and after each cut.

## Failure modes of the refactor

The design has failed if it produces any of these outcomes:

- **A renamed dual system:** old commands remain supported but documentation
  merely calls them diagnostic.
- **A larger monolith:** publication authority is singular, but source
  derivation, artifact rendering, reporting, and promotion become inseparable
  private code in one file.
- **Identity accumulation:** direct-build and snapshot fields are combined into
  one larger identity object without a consumer/claim disposition.
- **A fake shared kernel:** both full workflows remain and call a new
  abstraction that centralizes syntax while their authority and lifecycle
  semantics still differ.
- **A new introspection subsystem:** a query engine, registry, or database is
  introduced to explain values already present in the output root.
- **Compatibility permanence:** aliases or wrappers keep retired production
  paths reachable indefinitely.
- **Capability loss by reachability alone:** request-set, annotation, analysis,
  qualification, or staging behavior with an actual owner is deleted because
  its present caller happened to be the rehearsal.
- **Apparatus preservation by tests alone:** workflow/report code stays because
  tests characterize that code rather than a retained domain contract.
- **Premature gate refactoring:** `validate-design-bundle` is reorganized before
  publication authority and identity have one owner.

## Acceptance criteria

The refactor is complete when:

1. Exactly one supported command turns an official source checkout into a
   release-facing publication root: `soranoha build-publication`.
2. Exactly one function writes the canonical per-work publication artifact
   set: `materialize-publication!`.
3. No other CLI/Nix app performs release admission, corpus publication
   materialization, or atomic promotion.
4. Qualification and fixture callers are explicitly non-release and reuse the
   same per-work writer.
5. One documented identity chain connects source authority, parser/mapping
   identity, per-work manifests, and the corpus-level summary.
6. Operational configuration, concurrency, workflow ids, timestamps, and
   cache decisions are not accidentally included in content identity.
7. A completed root is inspectable from its retained values without replaying
   the workflow.
8. The old rehearsal/reproduce production composition, its public commands,
   and its apparatus-only tests and schemas are absent.
9. `soranoha.clj` no longer implements a publication workflow.
10. The standalone batch materializer and duplicate workflow-record
    implementation are absent unless the disposition audit names a current
    external owner.
11. The integration gate retains only cross-boundary smoke behavior; domain
    validation lives with the owning domain.
12. The change introduces no generic orchestration engine, release registry,
    or parallel identity model.

## Expected leverage

The immediate deletion is useful, but the larger gain is architectural:

- publication changes acquire one obvious entry point;
- identity changes have one chain to inspect;
- release-policy changes have one enforcement boundary;
- corpus failures and atomicity have one owner;
- per-work rendering remains independently testable;
- parser qualification reuses production behavior without becoming production;
- the command dispatcher and integration gate can shrink after their
  compensating responsibilities disappear.

That is the intended refactorability: fewer places encode each decision, and
the remaining places are values and deep boundaries rather than alternative
workflows.
