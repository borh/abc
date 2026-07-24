# Sole Publication Producer

**Date:** 2026-07-24 · **Status:** Approved design — implementation starts
with the trust and cache repairs below

## Decision

Soranoha will have one authority that can assemble and install a publication
root. Whether that root is release-admissible is a recomputable predicate, not
an event or receipt:

```text
authenticated source + admitted parser/mapping/policies
                             |
                             v
                  soranoha build-publication
               sole release assembler/installer
                    |                  \
                    | invokes           \ assembles
                    v                    v
         materialize-publication!    candidate release root
           one per-work renderer       + closed index
                /         \                    |
       qualification     fixtures              v
         evidence       characterization  release-admissible?
                                             /        \
                                          release   diagnostic
```

`soranoha build-publication` is the sole release assembler and installer.
`materialize-publication!` is the sole renderer of the canonical per-work
publication artifact set. `release-admissible?` is a pure verifier over the
closed index and explicit authoritative policy values.

This is intentionally more precise than saying that only one command may write
TEI or plaintext. Qualification and fixture tools need to render candidate
artifacts. They do not make releases. Candidate bytes belong to a release only
when `build-publication` closes over their identities in the authenticated
index and `release-admissible?` returns no problems.

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
  inputs, artifact references, policies, and failures. The direct publication
  protocol is the planned `snapshot-index.json` version 0.2.0 successor.
- **Installation** — atomically makes a completed or diagnostic run root
  visible at an output place.
- **Release admissibility** — the result of a pure, fail-closed predicate over
  a candidate root's closed index and explicit current decision, registry, and
  publication-policy values. It returns `{admissible?, problems}` and writes no
  receipt. Installation does not imply admissibility.
- **Operational trace** — plans, timestamps, concurrency decisions, cache
  events, temporary paths, and step records. These explain a run but do not
  identify a release.

These distinctions prevent three common conflations: bytes with releases,
places with values, and a successful filesystem move with release
admissibility.

Admissibility is deliberately a function of two values: the immutable release
root and an explicit current authority set. A later decision, registry, or
rights-policy change may change whether an old root is currently admissible
without changing that root's artifact identity. Verifier output names the
authority hashes and problems it used for explanation, but it is derived
output—not an admission receipt.

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

### `build-publication`: sole release assembler and installer

`soranoha build-publication` owns:

- validation of the official source checkout;
- construction and authentication of release inputs;
- exact parser-candidate, qualification, mapping, and policy authentication;
- source selection and source/parser derivation;
- failure policy and release-admissibility accounting;
- concurrency and cache policy;
- calls to the per-work renderer;
- construction and validation of the release index;
- installation of the run root;
- evaluation of the shared release-admissibility predicate before a successful
  release-facing result is reported.

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
  └── install the run root; report release success only for an admissible root
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

Only the first caller assembles the release root. The distinction is conferred
by authenticated inclusion and the release-admissibility predicate, not by
preventing other code from calling the renderer.

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

### Decision: version the snapshot-index protocol

The authoritative corpus value will remain `snapshot-index.json`, but direct
publication requires a minimal version 0.2.0 successor constructed from the
actual results of `build-publication`. This schema change is planned, not
contingent.

Version 0.1.1 supplies a useful closure shape:

- a schema-pinned identity object;
- an authenticated artifact-set hash;
- sorted artifact references with manifest and content hashes;
- failure and layout policy hashes;
- a recomputable `snapshot_identity_hash`.

It does not supply the required direct-publication semantics. Its required
`request_set_id`, request-set-derived tokenizer/analysis hashes, snapshot-label
pattern, and generated snapshot-plan fields belong to the fixture/rehearsal
model. The checked-in `full-corpus-publication-basic-ja` request set contains
two fixture subjects, while the direct build independently discovers the
official source corpus. Reusing that identity would be false. The 0.1.1
identity object also has no explicit candidate, qualification, parser-build,
parser-configuration, or mapping coordinates.

Version 0.2.0 therefore:

- replaces request-set identity with a hash-authenticated source-selection
  identity built from the actual selected source bundles;
- includes `candidate_ref`, `qualification_identity_ref`,
  `parser_build_hash`, `parser_config_hash`, `mapping_hash`, and the relevant
  parser-IR/schema coordinates;
- retains the artifact-set, failure-policy, layout-policy, and schema hashes
  that are actual publication inputs;
- omits tokenizer and analysis recipe hashes unless the published artifact set
  genuinely depends on them;
- adds an explicit schema version and truthful direct-build label semantics.

The direct build must not invoke the rehearsal, read a hand-built snapshot
plan, synthesize a `request_set_id`, or populate irrelevant arrays to satisfy
0.1.1. Version 0.1.1 artifacts remain frozen historical facts, not a live
producer or validation obligation, while all live release projections move
atomically to 0.2.0. There is never more than one authoritative publication
identity.

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

The index records the exact parser and mapping coordinates. It does not copy a
claim that they are “admitted.” Admissibility is recomputed from those
coordinates and the explicit current authority values described below.

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
- provide an explicit, recorded `fixture`/non-release trust mode for tests and
  diagnostics that cannot prove official Git provenance.

The current behavior records dirty state without rejecting it and can interpret
unavailable status output as clean. That cannot authorize a release.

The non-release mode is not “skip the check when the root is not a Git
repository.” It is an explicit input, is recorded in the build values, and
causes `release-admissible?` to return a problem. Existing temporary-directory
fixtures must opt into it in the same change that makes official mode
fail-closed.

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

Authority comes through `docs/adr/decisions.edn`, not a new
`"admitted": true` configuration field. The shared verifier must:

1. load exactly one decision corpus form and reject loader or shape problems;
2. resolve `custom-parser-release-qualification` by slug;
3. require `:status :accepted`, `:release-authority :publication`, and the
   validation scope required by publication policy;
4. verify the index's `candidate_ref` and `qualification_identity_ref` against
   the immutable parser-RQ candidate, capture/evaluation generation,
   compatibility registry, qualification report, and promotion evidence;
5. require the index's parser/mapping coordinates to equal that verified
   qualification identity.

The decision record authorizes the governed qualification capability; it does
not itself contain the exact tuple. Exact tuple identity comes from the
candidate/evaluation/registry values. Reuse the fail-closed decision and
promotion verification boundary introduced for `verify-promotion`; do not
duplicate a weaker status lookup in publication code. The existing helper must
be extended because it currently resolves only the ADR 0040/0041 dependency
slugs.

The resulting hashes must reach the per-work manifests and the release index.
The current `nil` parser-build, parser-config, and mapping fields are a release
blocker, not optional provenance.

### Closed references

`release-admissible?` must validate:

- the release index schema and recomputed identity;
- every required relative reference;
- every referenced manifest hash;
- every artifact content hash exposed by its manifest;
- agreement among source, parser/mapping, work, and corpus identities;
- absence of unexpected identity-bearing files outside the closed set.

An unreadable or malformed authority value is an error, never an accepted
status.

## Cache semantics

Lane 1 deletes direct-publication cache reuse. Every selected work rerenders
and every manifest and release-coupled sidecar is regenerated. The existing
`work_content_hash` plus `tei.manifest.json` check is not a complete cache key,
and inventing a second cache protocol during the identity cutover would add
more machinery than the saved rendering warrants.

This is not a permanent ban on caching. A later measured bottleneck may justify
content-byte reuse keyed by every byte-determining input, or exact manifest
reuse keyed by a complete 0.2.0 identity object. Such a cache remains an
implementation detail: the release index authenticates results and never
trusts a cache-hit claim.

## Failure, installation, and admissibility semantics

Singular authority makes the state transitions explicit:

```text
candidate run root
    | release-admissible?(root, decisions, registry, policies)
    +-- no problems --> atomic installation --> release-facing success
    |
    +-- strict failure --> no replacement of prior root
    |
    +-- best-effort partial --> optional diagnostic installation
                                + nonzero exit
                                + predicate remains false
```

The existing best-effort contract may keep an inspectable partial root. That
root must be visibly non-admissible and must not be mistaken for a release just
because it occupies the configured output path. If it contains a candidate
snapshot index, the index must encode its failures and make
`release-admissible?` return problems.

A strict failure does not replace the last admissible root. Malformed
identities, broken references, or inadmissible source/parser state prevent a
release-facing success.

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

A user should be able to determine what was selected, why the root is or is not
currently admissible, what failed, and which hashes bind the source, parser,
mapping, policies, manifests, and artifacts without replaying a workflow.

## Current surface disposition

Deletion still requires a closed consumer and decision-claim audit.

| Current surface | Target disposition | Reason |
| --- | --- | --- |
| `soranoha build-publication` | Keep and deepen: sole release assembler/installer | It is the real full-corpus path and owns failure policy and installation. |
| `materialize-publication!` | Keep and deepen: sole per-work renderer | It centralizes the canonical publication artifact set. |
| `materialize-release-publication!` | Retire as a release-named boundary; move any required rights check to `build-publication` | Per-work rendering cannot independently confer release authority. |
| Single-work materializer CLI/Nix app | Keep as a non-release renderer adapter while live consumers exist | `ab-validator` and publication smokes actively use it. |
| Batch materializer CLI/Nix app | Keep pending explicit consumer migration | It is live qualification/report support; simplify duplicate run-record code separately. |
| Parser-RQ publication materializer | Keep as a qualification instrument | It produces evidence through the shared renderer, not releases. |
| `publication-rehearsal!`, command, report, and workflow | Retire after direct-build trust/cache characterization and before index 0.2.0 | It is the superseded end-to-end composition; deleting first prevents a third transitional producer. |
| `reproduce!` publication loop | Split and retire its competing composition | Retain only independently owned request-set/analysis/annotation capabilities. |
| Source-snapshot and request-set functions | Keep as pure domain values where the direct index or live analysis needs them | Their value semantics are useful; rehearsal orchestration is not. |
| Snapshot-index value functions/schema | Version to 0.2.0 and keep as the release-index protocol | The closure mechanism is useful; request-set identity and missing parser coordinates make 0.1.1 unsuitable for direct publication. |
| Snapshot validate/explain/report | Rebase on the release index and retain only live projections | These should inspect values, not reproduce them. |
| Staging/packaging | Keep as a downstream derived producer where live | It creates deployment values that cite, but do not redefine, the release. |
| `validate-workflow` in the Soranoha dispatcher | Retire the command; keep `workflow/validate-run` with its domain tests | It has no live operator or automation consumer outside its own CLI test and archived handoffs. |
| `validate-design-bundle` publication fixture | Keep as small integration characterization | It exercises a retained boundary without owning release authority. |

The live adapter findings correct the earlier proposed “one artifact-producing
command” rule. Exclusive writing is not the invariant; exclusive release
assembly and installation plus one shared admissibility predicate is.

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

Add one root-flake CI check that invokes the real `soranoha` app over a tiny
committed Git-backed Aozora fixture. It must use the flake-exported adapter,
parser, converter, and mapping paths rather than binding `*env*` or replacing
the parser function. Unit tests may still inject faults; this check proves the
actual production wiring.

### Lane 1: repair the release trust boundary

Land separately reviewed correctness changes:

1. fail closed on dirty or unprovable official source identity, while requiring
   every non-Git fixture to request and record the non-release trust mode;
2. extract a shared decision/promotion verifier, resolve the shape-valid
   `custom-parser-release-qualification` decision from `decisions.edn`, and
   bind the exact candidate/qualification/registry tuple;
3. bind and propagate parser build/configuration and mapping hashes;
4. delete the direct-publication reuse path and rerender every selected work;
5. remove stale absolute temporary-root paths from retained values.

These may intentionally tighten behavior. They require focused tests and
decision amendments where governed contracts change. Content reuse can return
later only with measured need and a complete content-input key; this campaign
does not add that second protocol while changing release identity.

### Lane 2: retire the secondary producer before adding the new protocol

Remove the confirmed-dead rehearsal/reproduction vertical slice atomically
after Lane 1 protects the real direct build. In `soranoha.clj`, remove:

- `build-snapshot-index`;
- `snapshot-index!`;
- `materialize-snapshot-root!`;
- `reproduce!`;
- `publication-rehearsal!`.

Remove the associated command-table entries, public orchestration functions,
workflow/report emitters, apparatus-only snapshot plans and tests, and active
documentation advertising the retired workflow. Retain request-set, analysis,
and annotation domain values that have independent consumers. Retain
`read-valid-snapshot-index` and the live read-only 0.1.1 projections until Lane
3 cuts all of them to 0.2.0 in one change.

This ordering prevents Lanes 1–3 from leaving a third half-finished
composition. Historical reports retain their recorded command strings.

### Lane 3: emit index 0.2.0 and cut every live projection atomically

Version the snapshot-index schema and construct version 0.2.0 from the actual
direct-build selection and render results:

- replace request-set identity with source-selection identity;
- add the exact candidate, qualification, parser, mapping, and schema
  coordinates;
- remove request-set, tokenizer, and analysis fields that are not publication
  inputs;
- reuse pure source/index value functions only where they express true facts;
- do not call `publication-rehearsal!`;
- close over the actual per-work manifests;
- validate all references and hashes through `release-admissible?`;
- keep `publications-report.json` derived;
- keep run plans/traces operational.

Rebase validation, explanation, reports, and staging on the direct-build
release index. Staging already copies bytes and changes locators; adapt its
loose-locator input seam rather than rewriting it.

Migrate schema contracts, examples, validators, explanation, reports, and
staging in the same lane. Version 0.1.1 remains frozen history, not a supported
producer/projection protocol. Do not write both versions and do not run two
authoritative corpus identities in parallel.

Lanes 1 and 3 are release-correctness work, not simplification credit. They may
add validation code but must add it only inside the retained build, value, and
verification boundaries. Lane 2 is the deletion that earns the simplification.
Do not advertise a supported release path until Lane 3 is complete and the
real-wiring check is green.

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
- release-index validation and projection behavior;
- one CI path through the real flake adapter/mapping wiring and a committed
  Git-backed fixture.

### Trust and identity

- dirty relevant source makes the admissibility predicate fail;
- failed Git inspection makes the admissibility predicate fail;
- explicit fixture trust mode remains buildable but never admissible;
- malformed decision corpus, wrong decision slug/status/authority/scope, or
  parser-RQ promotion failure makes the predicate fail;
- parser candidate/qualification mismatch makes the predicate fail;
- mapping or parser identity mismatch makes the predicate fail;
- required identity hashes are non-null and propagate to manifests/index;
- absolute or temporary paths do not affect identity;
- tampered manifests, contents, references, policies, or index fail closed.

### Cache

- changing corpus snapshot, metadata, parser, mapping, profile, or policy
  cannot reuse an old manifest;
- Lane 1 regenerates all manifests and release-coupled sidecars;
- direct-publication reports and identities contain no cache-hit status;
- the final index validates independently of cache events.

### Architecture

- only `build-publication` can assemble and install a release root;
- every caller uses the same pure `release-admissible?` verifier;
- all canonical per-work artifact sets pass through
  `materialize-publication!`;
- qualification and fixture adapters cannot emit a release-admissible index;
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
- no parallel corpus identity: snapshot-index 0.2.0 supersedes 0.1.1 for live
  direct publication;
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
- **Partial-as-release:** a diagnostic root is treated as admissible because it
  was installed.
- **A new introspection subsystem:** a query engine or database explains values
  that should be explicit on disk.
- **Capability loss by reachability alone:** request-set, analysis, annotation,
  qualification, or staging behavior with an owner is deleted with rehearsal.
- **Premature gate refactoring:** integration apparatus is rearranged before
  release authority and identity have one owner.

## Acceptance criteria

The refactor is complete when:

1. `soranoha build-publication` is the only supported operation that
   authenticates release inputs, closes a corpus index, and installs a
   release-facing root.
2. `materialize-publication!` is the only function that writes the canonical
   per-work publication artifact set.
3. Qualification, fixture, and development adapters are explicitly
   non-release and reuse that renderer.
4. Active single/batch adapter consumers are preserved or deliberately
   migrated before their entry points change.
5. `snapshot-index.json` version 0.2.0 is constructed from actual direct-build
   results and is the sole authoritative corpus release value; no synthetic
   request-set identity remains.
6. One documented identity chain binds clean source, exact admitted parser and
   mapping, policies, per-work manifests, and the artifact set.
7. Parser build/configuration and mapping hashes are present rather than null
   wherever the governed manifest contracts require them.
8. The direct build contains no manifest/content reuse path.
9. Operational configuration, concurrency, timestamps, cache events, workflow
   ids, and machine paths do not affect release identity.
10. `release-admissible?` is a pure fail-closed predicate with no receipt, and
    a partial diagnostic installation cannot pass it merely by occupying the
    output path.
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
- policy and parser admission have one verification boundary;
- failure, installation, and release admissibility stop being conflated;
- per-work rendering remains independently useful and testable;
- parser qualification shares production rendering without becoming
  production;
- projections explain stored values instead of replaying derivations;
- the dispatcher, workflow runner, and integration gate can shrink after their
  compensating responsibilities disappear.

That is the intended refactorability: each decision has one owner, durable
facts are values, operational places remain places, and independently useful
capabilities compose without acquiring release authority.
