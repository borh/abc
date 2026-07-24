# Sole Publication Producer

**Date:** 2026-07-24 · **Status:** Provisional design — architecture selected;
implementation blocked on the trust and cache repairs below

## Decision

Soranoha will have one authority that can assemble, install, and admit a
publication release:

```text
authenticated source + admitted parser/mapping/policies
                             |
                             v
                  soranoha build-publication
               sole release assembler/admitter
                    |                  \
                    | invokes           \ assembles
                    v                    v
         materialize-publication!    immutable release index
           one per-work renderer     refs · failures · identities
                /         \                    |
       qualification     fixtures       +-----+------+
         evidence       characterization |     |      |
                                     validate explain stage/package
```

`soranoha build-publication` is the sole release assembler and release
admission boundary. `materialize-publication!` is the sole renderer of the
canonical per-work publication artifact set.

This is intentionally more precise than saying that only one command may write
TEI or plaintext. Qualification and fixture tools need to render candidate
artifacts. They do not make releases. Candidate bytes become part of a release
only when `build-publication` closes over their identities in the authenticated
release index and the release passes admission.

The simplification is therefore:

1. one release authority;
2. one per-work renderer;
3. one authenticated release value;
4. no second source-to-release composition.

It is not “one process may ever produce an artifact.”

## Vocabulary

The design uses distinct words for distinct things:

- **Renderer** — derives one work's candidate publication artifacts and
  manifests. It has no corpus or release authority.
- **Release assembler** — selects authenticated inputs, invokes the renderer,
  collects results, and constructs the closed release value.
- **Release index** — the immutable, hash-authenticated closure of selected
  inputs, artifact references, policies, and failures. During this migration
  the existing `snapshot-index.json` contract is the release-index protocol.
- **Installation** — atomically makes a completed or diagnostic run root
  visible at an output place.
- **Admission** — declares a closed, successful index to be a release.
  Installing a partial diagnostic root is not release admission.
- **Operational trace** — plans, timestamps, concurrency decisions, cache
  events, temporary paths, and step records. These explain a run but do not
  identify a release.

These distinctions prevent three common conflations: bytes with releases,
places with values, and a successful filesystem move with release admission.

## Why this boundary has the highest leverage

The repository currently contains two end-to-end publication compositions.

The snapshot/rehearsal path in `abc.tools.soranoha` owns request-set resolution,
source snapshots, snapshot-index identity, per-work reproduction, reports,
staging, validation, and a rehearsal workflow. The direct
`abc.tools.soranoha-build-publication` path owns official-source discovery,
adapter execution, source materialization, per-work rendering, caching,
failure handling, run records, and atomic installation.

That duplication was not accidental. The original
`2026-07-08-soranoha-build-publication-design.md` required the direct build to
delegate to `publication-rehearsal!`. Commit `587bd182` removed the delegation
because the rehearsal was fixture/request-set-bound and produced stub parser IR
rather than real body text. The direct path became the real full-corpus
implementation.

Restoring the old delegation would preserve the wrong abstraction. The repair
is to move the useful authenticated value construction behind the real build
and retire the superseded composition.

At present, basic questions require answers from both systems:

- Which command makes a release?
- Which identity describes the corpus and its artifacts?
- Which parser and mapping were admitted?
- Where do strict and partial failure semantics live?
- Which output root is supported?
- Which trace is explanatory rather than authoritative?
- Where should a new publication capability be added?

After the refactor each question has one owner. That gives subsequent
decomposition of `soranoha.clj`, `materialize_publication.clj`, workflow
apparatus, schemas, and gates a reliable deletion seam.

## Architectural boundaries

### `build-publication`: sole release assembler and admitter

`soranoha build-publication` owns:

- validation of the official source checkout;
- construction and authentication of release inputs;
- exact parser-candidate, qualification, mapping, and policy admission;
- source selection and source/parser derivation;
- failure policy and release-admissibility accounting;
- concurrency and cache policy;
- calls to the per-work renderer;
- construction and validation of the release index;
- installation of the run root;
- admission of a completed index as a release.

No other command may combine those responsibilities into an alternative
source-to-release workflow.

This authority does not require one large namespace. The façade should be a
deep operation over explicit values:

```text
build-publication
  ├── authenticate release inputs
  ├── select and derive candidate works
  ├── render each work through materialize-publication!
  ├── assemble and validate the release index
  └── install the run root; admit only a closed success
```

The internal capabilities remain ordinary functions with domain ownership.
They may be reused independently. What must not reappear is another public
composition that selects source, renders a corpus, invents a top-level
identity, and calls it a release.

Release assembly does not transfer parser ownership to ABC. `ab-validator`
continues to own parser/adaptor measurement, AAT evidence, parser-IR conversion
evidence, and parser corpus reports. ABC consumes and authenticates those
values while retaining publication-schema, TEI, manifest, and registry
admission ownership. Parser outputs remain supporting evidence; source-authority
measurements remain authoritative for Aozora markup coverage.

### `materialize-publication!`: sole per-work renderer

`materialize-publication!` owns exactly one work's canonical publication
artifact set:

- visible-body plaintext;
- TEI;
- preservation sidecar;
- TEI validation result;
- plaintext manifest;
- TEI manifest.

Its input is an explicit value containing parser IR, source manifest,
metadata/person records, generation time, the identity inputs required by that
invocation, and an output directory. Its result describes the candidate
artifacts and manifests it wrote. The release caller is responsible for
authenticating those identity inputs. Renderers and validators stay behind this
boundary; callers do not reimplement their sequencing, layout, or manifest
construction.

The following callers are legitimate:

- `build-publication`, which may include the result in a release;
- parser qualification, which produces evidence;
- fixture and integration checks, which characterize the renderer;
- focused development tools that render candidate artifacts.

Only the first caller has release authority. The distinction is conferred by
authenticated inclusion and admission, not by preventing other code from
calling the renderer.

### Retained adapters are not release front doors

The current single-work materializer CLI and batch CLI have live
`ab-validator` and smoke-script consumers. They must not be deleted merely
because they can write artifacts.

Their target role is an explicitly non-release renderer adapter:

- it accepts already-selected inputs;
- it produces candidate artifacts or qualification evidence;
- it cannot authenticate an official source checkout;
- it cannot construct or admit the corpus release index;
- it cannot install a supported release root.

The batch adapter stays until its live consumers migrate. Its hand-built
workflow/run record may be simplified independently, but retirement is a
consumer migration, not the first architectural cut.

### Projections consume the release value

Validation, explanation, and reporting are pure or read-only projections from
the release index and referenced artifacts.

Staging and packaging are downstream derived producers: they copy, arrange,
archive, or emit deployment views. They are not read-only because they create
new values, but they must not regenerate canonical publication contents or mint
a competing publication identity. Their outputs cite the source release index.

The dependency direction is one-way:

```text
release index -> validate / explain / report / stage
```

No projection is allowed to call back into rendering in order to “verify” or
reconstruct the release.

## Authoritative release value

### Decision: use the existing snapshot-index protocol

The authoritative corpus value will be `snapshot-index.json`, constructed
directly from the actual results of `build-publication`.

This choice avoids adding a parallel `release-index` schema. The current
snapshot index already supplies the essential semantics:

- a schema-pinned identity object;
- source and request-set identity;
- an authenticated artifact-set hash;
- sorted artifact references with manifest and content hashes;
- failure and layout policy hashes;
- parser, schema, tokenizer, and analysis evidence hashes;
- a recomputable `snapshot_identity_hash`.

The direct build must construct those values from the source selection and
artifacts it actually used. It must not invoke the rehearsal, read a hand-built
snapshot plan, or synthesize fields merely to satisfy the schema.

The existing protocol is retained during the behavior-preserving migration.
If a required field cannot truthfully describe a direct production build, that
is a schema-version decision: remove or replace the field in a minimal
successor and supersede the old contract. Do not put an arbitrary value into
the field, and do not create a second corpus identity beside it.

`publications-report.json` remains a derived operational/corpus report. It is
not authoritative: it currently has no matching schema, no authenticated
artifact closure, and no identity equivalent to the snapshot index.

### Closed identity chain

The release index must authenticate this chain:

```text
clean official source identity
        +
exact admitted parser candidate / qualification tuple
        +
exact mapping document and release policies
        |
        v
selected source bundles and parser IR
        |
        v
per-work publication manifests
        |
        v
sorted artifact references + artifact-set hash
        |
        v
snapshot-index identity
```

The index and every identity-bearing manifest use relative locators. An output
directory, temporary root, cache directory, or absolute checkout path is a
place and must never enter artifact identity.

### Values excluded from release identity

The following remain operational facts unless a governed contract explicitly
states otherwise:

- concurrency and scheduling;
- cache hit/miss decisions;
- workflow/run identifiers;
- start and finish timestamps;
- temporary paths and absolute machine paths;
- command spelling;
- diagnostic logs.

They may be retained for introspection. Changing them must not change release
identity.

## Trust-boundary preconditions

The current direct build is not yet a trustworthy release boundary. These are
correctness repairs, not incidental cleanup, and must land separately from
structural deletion.

### Clean source identity

The build must fail closed when it cannot prove the relevant official source
state:

- reject dirty relevant source paths by default;
- treat failed Git inspection as unknown, never as clean;
- identify the selected source contents, not only the checkout commit and
  catalog;
- provide an explicit, recorded non-release override only if an operator needs
  diagnostic builds from dirty source.

The current behavior records dirty state without rejecting it and can interpret
unavailable status output as clean. That cannot authorize a release.

### Exact parser and mapping admission

A profile name or environment-selected binary is insufficient release
identity. The build must bind to:

- the exact parser candidate;
- the exact accepted qualification and admitted tuple;
- parser build and configuration hashes;
- the exact mapping document hash;
- the relevant parser-IR/schema and release-policy hashes.

This follows the Accepted parser qualification and owned AAT-to-parser-IR
mapping decisions. Authority must not float with `HEAD`, `PATH`, or an
environment variable.

The resulting hashes must reach the per-work manifests and the release index.
The current `nil` parser-build, parser-config, and mapping fields are a release
blocker, not optional provenance.

### Closed references

Before admission, the build must validate:

- the release index schema and recomputed identity;
- every required relative reference;
- every referenced manifest hash;
- every artifact content hash exposed by its manifest;
- agreement among source, parser/mapping, work, and corpus identities;
- absence of unexpected identity-bearing files outside the closed set.

An unreadable or malformed authority value is an error, never an accepted
status.

## Cache semantics

The render cache and release manifests are different values and must be treated
separately.

Reusable content bytes may be keyed by all inputs that determine those bytes.
A release manifest may be reused only when its complete identity-bearing input
object is identical. In particular, a matching `work_content_hash` plus the
existence of `tei.manifest.json` is insufficient.

For each artifact the implementation must choose one of two honest paths:

1. **Exact reuse:** content and manifest are reused only when the complete
   render and release identity matches.
2. **Content reuse:** deterministic content bytes are reused, while manifests
   and references are regenerated from the current release inputs.

A cache hit must not copy an old corpus snapshot, metadata identity, parser or
mapping identity, profile hash, timestamp policy, or locator into a new
release.

Cache keys are implementation details. The release index authenticates the
result and does not trust a cache-hit claim.

## Failure, installation, and admission semantics

Singular authority makes the state transitions explicit:

```text
candidate run root
    | validate closure
    +-- closed success --> atomic installation --> release admission
    |
    +-- strict failure --> no replacement of prior root
    |
    +-- best-effort partial --> optional diagnostic installation
                                + nonzero exit
                                + no release admission
```

The existing best-effort contract may keep an inspectable partial root. That
root must be visibly non-admissible and must not be mistaken for a release just
because it occupies the configured output path. If it contains a candidate
snapshot index, the index must encode its failures and fail release admission.

A strict failure does not replace the last admitted root. Malformed identities,
broken references, or inadmissible source/parser state fail before admission.

The implementation must characterize the current strict, partial, and atomic
replacement behavior before changing it. This spec separates the concepts; it
does not silently redefine an Accepted failure contract.

## Introspection surface

The installed root should answer operational questions from a small closed set
of values. The candidate set is:

```text
<output-root>/
  snapshot-index.json             # authoritative release value
  build-config.json               # operational input record
  source-selection-report.json    # selection explanation
  workflow-plan.json              # optional operational trace
  workflow-run.json               # optional operational trace
  publications/
    publications-report.json      # derived summary
    <work-slug>/
      plain.txt
      plaintext.manifest.json
      tei.xml
      tei.manifest.json
      tei-validation-result.json
      preservation.json
      source_work_content_hash.txt
```

This is a disposition input, not a promise to keep every file. Each retained
value is labeled:

- identity/release-bearing;
- operational provenance;
- derived report;
- diagnostic trace.

Relative paths must remain valid after temporary-root installation. The current
absolute `materialized_root` recorded from the temporary build place is a
place/value defect and must not become part of the supported contract.

A user should be able to determine what was selected, what was admitted, what
failed, and which hashes bind the source, parser, mapping, policies, manifests,
and artifacts without replaying a workflow.

## Current surface disposition

Deletion still requires a closed consumer and decision-claim audit.

| Current surface | Target disposition | Reason |
| --- | --- | --- |
| `soranoha build-publication` | Keep and deepen: sole release assembler/admitter | It is the real full-corpus path and owns failure policy and installation. |
| `materialize-publication!` | Keep and deepen: sole per-work renderer | It centralizes the canonical publication artifact set. |
| `materialize-release-publication!` | Retire as a release-named boundary; move any required rights check to `build-publication` | Per-work rendering cannot independently confer release authority. |
| Single-work materializer CLI/Nix app | Keep as a non-release renderer adapter while live consumers exist | `ab-validator` and publication smokes actively use it. |
| Batch materializer CLI/Nix app | Keep pending explicit consumer migration | It is live qualification/report support; simplify duplicate run-record code separately. |
| Parser-RQ publication materializer | Keep as a qualification instrument | It produces evidence through the shared renderer, not releases. |
| `publication-rehearsal!`, command, report, and workflow | Retire after direct build emits the release index | It is the superseded end-to-end composition. |
| `reproduce!` publication loop | Split and retire its competing composition | Retain only independently owned request-set/analysis/annotation capabilities. |
| Source-snapshot and request-set functions | Keep as pure domain values where the direct index or live analysis needs them | Their value semantics are useful; rehearsal orchestration is not. |
| Snapshot-index functions/schema | Keep as the release-index protocol, then simplify only by versioned contract change | It already provides the authenticated closure missing from the direct build. |
| Snapshot validate/explain/report | Rebase on the release index and retain only live projections | These should inspect values, not reproduce them. |
| Staging/packaging | Keep as a downstream derived producer where live | It creates deployment values that cite, but do not redefine, the release. |
| `validate-workflow` in the Soranoha dispatcher | Move to its domain or retire | Generic run-record validation is not publication dispatch. |
| `validate-design-bundle` publication fixture | Keep as small integration characterization | It exercises a retained boundary without owning release authority. |

The live adapter findings correct the earlier proposed “one artifact-producing
command” rule. Exclusive writing is not the invariant; exclusive release
assembly and admission is.

## Closed disposition audit

Before each deletion, complete a table over:

1. Soranoha commands and Clojure entry points.
2. Root and `abc` flake apps, `deps.edn` aliases, launchers, and checks.
3. Output files, schema contracts, fixtures, and mirrors.
4. Accepted decision claims and cited evidence.
5. Active documentation and runbooks.
6. In-monorepo consumers, including `ab-validator`, parser-RQ, analysis,
   annotation, and staging.
7. Known out-of-repository/public consumers.
8. Tests, separated into domain characterization and apparatus self-tests.

Every row receives one disposition:

- release authority;
- shared domain capability;
- non-release renderer adapter;
- projection or downstream derived producer;
- fixture characterization;
- qualification instrument;
- frozen historical reference;
- retire;
- unknown.

An unknown blocks deletion. It does not justify a permanent compatibility
wrapper.

## Migration sequence

The lanes below separate semantic correctness, behavior-preserving structure,
and intentional breaking deletion. They should not be combined into one
“simplification” commit.

### Lane 0: characterize and close the tables

Pin a small official-Aozora-shaped fixture through `build-publication` and
record:

- the output tree and relative references;
- identity-bearing versus operational values;
- deterministic hashes after normalizing operational fields;
- success, strict-failure, and best-effort-partial behavior;
- atomic replacement behavior;
- per-work equivalence with `materialize-publication!`;
- cache behavior when source, snapshot date, metadata, profiles, parser, and
  mapping change.

Complete the surface and identity disposition tables in the same review slice.

### Lane 1: repair the release trust boundary

Land separately reviewed correctness changes:

1. fail closed on dirty or unprovable official source identity;
2. bind the exact admitted parser candidate/qualification tuple;
3. bind and propagate parser build/configuration and mapping hashes;
4. repair cache reuse so old manifests cannot cross release identities;
5. remove stale absolute temporary-root paths from retained values.

These may intentionally tighten behavior. They require focused tests and
decision amendments where governed contracts change.

### Lane 2: make the direct build emit the release value

Construct `snapshot-index.json` from the actual direct-build selection and
render results:

- use the pure request-set/source/index value functions where they express true
  facts;
- do not call `publication-rehearsal!`;
- close over the actual per-work manifests;
- validate all references and hashes before admission;
- keep `publications-report.json` derived;
- keep run plans/traces operational.

If the existing snapshot-index schema cannot represent the direct build
truthfully, make one minimal versioned schema change and migrate projections.
Do not run two authoritative corpus identities in parallel.

### Lane 3: rebase consumers, then retire the secondary composition

Rebase validation, explanation, reports, and staging on the direct-build
release index. Then remove the confirmed-dead rehearsal/reproduction vertical
slice atomically:

- command-table entries;
- public orchestration functions;
- Nix apps and `deps.edn` aliases;
- workflow/report emitters;
- apparatus-only schemas and fixtures;
- apparatus-only tests;
- active documentation advertising the retired workflow.

Historical reports retain their recorded command strings.

### Lane 4: collapse the surviving modules

After deletion exposes the real seams:

- make `soranoha.clj` thin dispatch plus delegation;
- move retained request-set/analysis operations to their domain owners;
- keep completed-root projections together;
- remove duplicate run-record construction from
  `materialize_publication.clj`;
- consolidate repeated time/hash/schema helpers only where they share
  semantics.

Do not begin by splitting large files into several shallow namespaces. Delete
the obsolete compositions first, then deepen the surviving boundaries.

### Lane 5: reconsider workflow and integration apparatus

With one release composition:

- keep the serial workflow runner only if multiple live workflows need its
  dependency, failure, and trace semantics;
- otherwise inline the short build sequence and emit the retained operational
  trace directly;
- shrink `validate-design-bundle` to one cross-boundary publication smoke;
- leave domain validation with its domain owner.

Do not replace either subsystem with the retired target-graph workflow engine
or a new generic orchestrator.

### Lane 6: migrate or retain the batch adapter

Only after enumerating and migrating its active `ab-validator`, report, smoke,
and external consumers may the batch adapter be removed. If its capability
still earns its keep, retain a narrow qualification-oriented adapter without
release or workflow authority.

## Verification strategy

The migration needs tests at the semantic boundaries, not snapshots of
incidental orchestration.

### Characterization

- current strict, partial, and atomic installation behavior;
- current per-work artifact bytes and manifests;
- current active single/batch renderer consumers;
- release-index validation and projection behavior.

### Trust and identity

- dirty relevant source blocks admission;
- failed Git inspection blocks admission;
- parser candidate/qualification mismatch blocks admission;
- mapping or parser identity mismatch blocks admission;
- required identity hashes are non-null and propagate to manifests/index;
- absolute or temporary paths do not affect identity;
- tampered manifests, contents, references, policies, or index fail closed.

### Cache

- identical complete inputs permit exact reuse;
- changing corpus snapshot, metadata, parser, mapping, profile, or policy
  cannot reuse an old manifest;
- content-byte reuse regenerates current manifests;
- the final index validates independently of cache events.

### Architecture

- only `build-publication` can admit a release;
- all canonical per-work artifact sets pass through
  `materialize-publication!`;
- qualification and fixture adapters cannot emit an admitted release index;
- projections never call rendering;
- staging cites the source release identity and cannot replace it;
- retired commands and compositions are absent after their breaking lane.

## Change discipline

- Preserve behavior before structural cleanup; isolate intentional tightening.
- Separate correctness repairs, moves, and breaking retirements.
- Make each retired surface an atomic vertical slice.
- Do not add compatibility wrappers for deliberately retired release paths.
- Do not treat historical reports or plans as live consumers.
- Do not delete pure identity or validation functions merely because their
  orchestration caller is deleted; prove their remaining ownership.
- Do not keep orchestration merely because its own tests call it.
- Add or amend a decision only when a governed capability, identity, or claim
  changes.
- Measure commands, aliases, schemas, source/test LOC, namespace dependencies,
  and gate time before and after each cut.

## Non-goals

- no publication rewrite;
- no generic command registry, pipeline framework, target graph, or release
  database;
- no new corpus identity while the snapshot-index contract can state the truth;
- no deletion of live qualification or fixture rendering capability;
- no byte-for-byte drift gate over operational timestamps or paths;
- no opportunistic F4/F5 refactor mixed into trust-boundary repairs.

## Failure modes of this refactor

The design has failed if it yields:

- **A renamed dual system:** an old source-to-release workflow remains supported
  but is called diagnostic.
- **Exclusive-writer theater:** useful qualification adapters are deleted while
  more than one corpus release authority survives.
- **A larger monolith:** source authentication, rendering, indexing, and
  installation become inseparable code in one namespace.
- **Identity accumulation:** direct-build and snapshot fields are combined
  without a consumer and claim disposition.
- **A lying index:** required fields contain placeholders or unrelated values
  merely to reuse a schema.
- **A fake shared kernel:** both end-to-end workflows remain behind a new layer.
- **Cache laundering:** old manifests enter a new release because content bytes
  happened to match.
- **Place identity:** temporary or absolute paths become part of release
  identity or survive installation as broken references.
- **Partial-as-release:** a diagnostic root is called admitted because it was
  installed.
- **A new introspection subsystem:** a query engine or database explains values
  that should be explicit on disk.
- **Capability loss by reachability alone:** request-set, analysis, annotation,
  qualification, or staging behavior with an owner is deleted with rehearsal.
- **Premature gate refactoring:** integration apparatus is rearranged before
  release authority and identity have one owner.

## Acceptance criteria

The refactor is complete when:

1. `soranoha build-publication` is the only supported operation that
   authenticates release inputs, closes a corpus index, and admits a release.
2. `materialize-publication!` is the only function that writes the canonical
   per-work publication artifact set.
3. Qualification, fixture, and development adapters are explicitly
   non-release and reuse that renderer.
4. Active single/batch adapter consumers are preserved or deliberately
   migrated before their entry points change.
5. `snapshot-index.json` is constructed from actual direct-build results and is
   the sole authoritative corpus release value.
6. One documented identity chain binds clean source, exact admitted parser and
   mapping, policies, per-work manifests, and the artifact set.
7. Parser build/configuration and mapping hashes are present rather than null
   wherever the governed manifest contracts require them.
8. Cache reuse cannot carry a manifest across different identity-bearing
   inputs.
9. Operational configuration, concurrency, timestamps, cache events, workflow
   ids, and machine paths do not affect release identity.
10. A partial diagnostic installation is visibly non-admissible and cannot
    replace the meaning of an admitted release.
11. Validation and explanation consume the release index; staging/packaging
    produce derived values that cite it.
12. The old rehearsal/reproduce release composition, its public commands, and
    its apparatus-only tests and schemas are absent.
13. `soranoha.clj` no longer implements an end-to-end publication workflow.
14. The integration gate retains only cross-boundary smoke behavior.
15. No generic orchestration engine, release registry, or parallel identity
    model has been introduced.

## Expected leverage

The immediate deletion is useful, but the architectural gain is larger:

- publication changes have one release boundary;
- identity changes have one value chain;
- policy and parser admission have one enforcement point;
- failure, installation, and admission stop being conflated;
- per-work rendering remains independently useful and testable;
- parser qualification shares production rendering without becoming
  production;
- projections explain stored values instead of replaying derivations;
- the dispatcher, workflow runner, and integration gate can shrink after their
  compensating responsibilities disappear.

That is the intended refactorability: each decision has one owner, durable
facts are values, operational places remain places, and independently useful
capabilities compose without acquiring release authority.
