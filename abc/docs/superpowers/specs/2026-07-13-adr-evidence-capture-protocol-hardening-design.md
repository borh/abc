# ADR Evidence Capture Protocol Hardening Design

Date: 2026-07-13
Status: Draft for review
Parents:

- `2026-07-12-adr-artifact-backed-evidence-enforcement-design.md`
- `2026-07-12-adr-evidence-corpus-migration-design.md`

## Purpose

Close the protocol gaps discovered before authoring the foundation evidence
artifacts. The target is 37 independently captured observations supporting 42
claim-registration rows. Focused Clojure tests and operational Nix commands
have different trust and freshness properties. They must not share a nominal
closure mechanism that is strict enough for neither.

This design revises the implementation contract in
`2026-07-12-adr-evidence-migration-foundation.md`; it is not merely an
additional prerequisite. Before implementation resumes, that plan must be
amended so the two documents do not remain concurrently authoritative.

Specifically, this design supersedes:

- Task 2B's requirement that `--repo-root` equal the Git worktree root;
- Task 7's 42-descriptor/42-manifest count and its prohibition on observation
  key reuse when rows execute the same Var or command;
- Task 7's four independently named Nix descriptors; and
- Task 8's 42-bundle capture loop.

It also supersedes the `derive-nix-clojure-source-closure` instructions in the
unexecuted schema/RDF/TEI, temporal/person/ingest, and parser/publication family
plans. Their operational boundaries must use the entrypoint-namespace closure
protocol when those plans are revised for execution.

The replacement contract is the three-root model, 37 observation descriptors
and manifests, 37 run bundles, and 42 claim-support bindings described here.
The foundation implementation plan must record Task 2B's landed single-root
behavior as superseded, schedule its replacement in Task 6C, and update Tasks
6C, 7, and 8 before implementation resumes. The three later family plans must
be corrected before their own execution, but they do not block foundation
capture. This design itself does not create evidence bundles, register claims,
or change governance from audit mode.

## Grounded Failure Evidence

The original Task 7 table has internally consistent claim-support arithmetic:
42 rows covering 35 distinct claims. Thirty-eight rows name focused Clojure
tests and four name Nix operations. The four operational rows actually describe
only two distinct measured events: the original table attaches three claims to
one design-bundle run, and one claim uses the source-corpus run. Execution was
stopped before a descriptor commit because:

- the 38 Clojure rows identify 35 unique focused Vars, 14 of which do not yet
  exist;
- none of the other 21 unique Vars yet owns the required direct
  `with-validated-read-trace!` boundary;
- six namespaces containing planned focused Vars are absent from Task 7's file
  inventory;
- the source-form reader cannot parse valid auto-resolved keywords such as
  `::am/run-summary-events` because it does not establish the namespace alias
  map before reading later forms;
- real planned focus graphs expose reviewed pure operations and unsupported
  forms that the current exact analyzer does not yet classify, including
  `charred.api/write-json-str` and `clojure.core/re-pattern`;
- the four Nix claim-support rows name same-stem closure manifests, but
  version-1 descriptors
  cannot identify or mechanically validate those manifests; and
- the existing closure helper couples source freshness to the focused-test
  default-deny capability policy, so intended operational entrypoints fail on
  their legitimate filesystem and subprocess behavior.
- the capture CLI currently requires `--repo-root` to equal the Git worktree
  root, while Task 7 invokes it from `abc/` and names `bin/kaocha`; in the
  monorepo those are different roots, so the planned command cannot pass its
  own preflight.

The incomplete Task 7 prototype is not an accepted artifact. It remains
outside history until this prerequisite is implemented and independently
reviewed.

The corrected arithmetic is:

| Boundary | Observations | Claim bindings | Distinct claims |
|---|---:|---:|---:|
| Focused Clojure | 35 | 38 | 32 |
| Operational Nix | 2 | 4 | 3 |
| Total | 37 | 42 | 35 |

One event has one observation identity. Multiple bindings may select it; they
do not manufacture additional measurements.

## Decision

### Keep focused-test safety separate from operational freshness

Focused Clojure evidence retains the safety semantics of the version-2
protocol:

- one exact qualified `deftest` Var per focus;
- a direct, unconditional top-level call to
  `abc.tools.adr-evidence-runtime-inputs/with-validated-read-trace!`;
- exact equality between observed repository reads and the checked runtime
  input manifest;
- Var-level reachable-call analysis with default-deny treatment of unreviewed
  executable capabilities; and
- a repository-owned Kaocha runner whose summary proves that every requested
  focus ran exactly once.

Operational Nix evidence uses a new descriptor protocol. It binds the exact
Clojure namespace freshness closure and Nix determinants used by the command,
but does not claim that the operational entrypoint is capability-pure. A Nix
application that launches processes or reads the filesystem is not a focused
unit-test boundary and must not gain broad exemptions in the focused analyzer.

The executable run bundle remains `abc-adr-evidence-run-v1`. Descriptor and
closure coordinates are hashed inputs, so this strengthens capture
preconditions without changing the observation artifact consumed by
governance.

The existing test-only `:abc-adr-nix-clojure-closure-v1` shape is not retained
as a second accepted closure protocol. Task 6C deletes
`derive-nix-clojure-source-closure`, `validate-nix-clojure-closure!`, their
focused-Var manifest fixtures, and tests that accept that schema before adding
`:abc-adr-operational-closure-v1`. No committed descriptor or run bundle uses
the retired shape, so no historical artifact is migrated. References in the
three unexecuted family plans are planning debt corrected as stated above, not
compatibility consumers.

The unrelated use of `:invalid-nix-clojure-closure` for version-2 focused
runner validation is renamed to `:invalid-focused-evidence-runner` in the same
commit. Operational manifest problems use only
`:invalid-operational-closure`. The old name is not aliased to either new
problem kind.

### Generic observation catalog and foundation claim bindings

Observation policy lives in
`data/adr-evidence/foundation-observation-catalog.edn`, with schema version
`:abc-adr-evidence-observation-catalog-v1`. This is a family-neutral closed
protocol containing two sorted collections: focused observations and
operational observations. The catalog validator enforces row shape,
uniqueness, ordering, and binding resolution without embedding a family name
or fixed row count. Foundation-specific tests separately require 35 focused
rows, two operational rows, and 42 bindings. Every row contains:

- descriptor stem;
- observation ID and unique observation key; and
- either an exact qualified focus Var or an operational-command contract ID.

Claim-support policy lives separately in the existing registration template,
`docs/evidence/adr-entries/foundation.edn`. Every binding contains one claim
ID, one observation ID, the typed evidence kind, and the predicate expected by
registration. Observation IDs are unique; claim IDs and observation IDs may
each occur in multiple bindings. A join validator requires all 42 bindings to
resolve to one of the 37 observation rows and rejects unbound observations,
unknown observations, and duplicate `[claim-id observation-id]` identities.

Registration materializes one row per binding, but does not rename or rerun an
event to manufacture independence. Capture canonicalizes the selected
observation row as an observation-contract value and hashes that projection.
The descriptor stores the catalog path, observation ID, and expected contract
hash; offline validation recomputes the projection from the current catalog.

The hash never serializes generic EDN. A pure
`observation-contract-json-value` projection maps the closed catalog row to an
I-JSON value with schema version
`abc-adr-evidence-observation-contract-v1`. JSON object keys are fixed ASCII
field names; EDN keywords map through field-specific closed string enums;
qualified symbols become their full dotted/slashed strings; paths are
normalized repository-relative strings; vectors preserve their already
validated order; and no sets, arbitrary map keys, tagged values, ratios,
floating-point values, or integers outside the I-JSON range are admitted. The
contract hash is `sha256:` plus the SHA-256 of the RFC 8785 canonical JSON UTF-8
bytes produced by the repository's existing JCS implementation. Positive
known-answer and cross-order tests pin the exact bytes and digest.

The whole observation catalog and the registration template are not
observation inputs. Changing an unrelated row or a claim binding therefore
does not rewrite measurement history. Changing
the selected observation contract invalidates its descriptor and artifact
until its contract hash is deliberately updated and the observation is
recaptured. The Git-versioned catalog remains the current policy authority.

Before a descriptor is capturable, the catalog validator requires every
focused row to resolve to one `deftest`, every focus to own the direct trace
boundary, and every reachable graph to pass the source reader and capability
policy. One exact focus identifies one observation row and one descriptor;
when multiple claims are predicates over that same test event, the catalog
represents the event once and the registration template lists all bindings.
Repeating the same focus under another descriptor name is forbidden. Distinct
focused tests supporting different parts of one claim remain distinct
observations.

Task 6C checks the complete catalog but does not fabricate the 14 missing
claim tests. It writes the reviewed complete sorted finding vector to
`data/adr-evidence/foundation-capture-conformance-debt.edn`, with schema
version `:abc-foundation-capture-conformance-debt-v1`. The gate requires exact
equality with that value; “at least these findings” is not a passing state.
The vector contains normalized finding identities, not diagnostic messages,
line/column coordinates, analyzer traversal order, or stack data. Each identity
is a closed tuple of problem kind plus the stable semantic coordinates relevant
to that kind, such as focus Var, target Var/JVM head, or repository-relative
path. It includes the 14 unresolved Vars, the 21 existing Vars without direct
ownership, and every additional capability or source-reading admission blocker
produced by the completed Task 6C analyzer.

Human-readable diagnostics remain an accumulated derived report but are not
the equality key. An analyzer change that alters admission semantics requires
an explicit reviewed debt update; an edit that only changes messages or source
positions does not. Task 7 atomically changes the normalized debt value to `[]`
and requires observed identities to equal it before descriptors are admitted.
The debt value is a temporary migration control, not an observation input. No
production gate scrapes implementation-plan Markdown.

### Alias-aware source reading

The source reader first reads the `ns` form with reader evaluation disabled,
derives the current namespace and exact alias map from its `:require` clauses,
then reads remaining forms under that namespace identity and alias map.
Auto-resolved keywords and symbols therefore receive the same identities as
normal Clojure reading. Resolution uses an invocation-local reader resolver
value. It does not call `create-ns`, install aliases, mutate `*ns*`, or depend
on JVM-global namespace state.

Unknown aliases, malformed namespace declarations, reader evaluation,
conditional forms outside the supported `:clj` feature set, and disagreement
between reader coordinates and clj-kondo coordinates fail closed. The change
must preserve line and column metadata used by call-edge matching.

### Reviewed focused capabilities

The focused analyzer remains exact. It does not admit a namespace, package, or
class wholesale. Each newly required pure operation is added by qualified Var
or exact JVM head with a positive regression test and a negative neighboring
case. Pure serialization or value transformation may be admitted; file reads,
directory enumeration, archive access, library path-loads, network access,
dynamic resolution, reflection-based invocation, and subprocesses still
require the named protocol or fail.

Writes are not freshness inputs. Repository mutation is nevertheless rejected
by the capture tool's before-and-after clean-tree checks. Generated reads are
authorized only inside the ephemeral-root capability described below.

### Owned ephemeral roots

`abc.tools.evidence-io/with-ephemeral-root` already supplies the authorization
primitive: it requires an active read trace, rejects overlap with the identity
root, tracks ephemeral reads separately, and scopes one caller-supplied root.
It does not create or remove that root and does not currently know the outer
Git workspace when the identity root is `abc/`.

Task 6C retains that primitive and adds one reviewed owner,
`with-owned-ephemeral-root`. The read-trace context gains the canonical
workspace root, defaulting to the identity root for existing callers. The new
owner creates a temporary directory outside both roots, delegates
authorization to `with-ephemeral-root`, invokes one finite callback with the
root, and removes the directory in `finally`. Both helpers remain invalid
outside an active read trace.

The new owner is a registered higher-order capability with exactly one
callback position. It cannot:

- create or select a root inside the repository/workspace;
- suppress a repository read;
- authorize a sibling external path;
- turn a copied repository input into an unrecorded source; or
- leave the generated directory behind after success or failure.

Tests whose operation writes and rereads generated outputs use this helper.
Repository fixture and schema reads still flow through the traced adapters and
must appear in the runtime manifest.

### Repository, workspace, and staging roots

Capture accepts three distinct roots:

- `repo-root` is the ABC artifact/input identity root and defaults to the
  current directory;
- `workspace-root` is the exact Git worktree root used for cleanliness checks
  and defaults to `repo-root`; and
- `staging-root` is an existing output root outside both identity trees and is
  required for committed-evidence capture.

`repo-root` may be the `abc/` component below `workspace-root`; it need not be
the Git root. The command working directory is `repo-root`, preserving
`bin/kaocha` and `nix run .#...` as component-local interfaces. The workspace
must be the exact Git root containing the monorepo sentinels. Cleanliness is
checked at `workspace-root` with untracked files included.

Descriptor, catalog, manifest, and operational determinant coordinates are
relative to `repo-root`. The existing `component-clojure-test-v1` profile is
the one deliberate exception for focused evidence that consumes sibling
components: its bundle input keys remain `workspace-root`-relative (for
example `abc/...` and `ab-validator/...`) and its `component_root` identifies
`abc`. Capture and offline validation must preserve that existing profile
semantics; they must not reinterpret those keys as `repo-root`-relative.

For these two commands, flake resolution begins at `abc/`. The component
`abc/flake.nix` and `abc/flake.lock` are determinants expressed relative to
`repo-root`; the monorepo-root flake and lock are not evaluation inputs to
`nix run .#validate-design-bundle` or the component source-corpus check. The
workspace root affects cleanliness and provenance, not component-flake
identity.

Before command execution, capture canonicalizes `staging-root` and `output`,
requires the output to be a direct contained target below the staging root,
and rejects staging or output paths inside `repo-root` or `workspace-root`.
Symlink traversal in either direction fails. The output target must not already
exist.

One pure output-boundary function owns these relations and returns a closed
validated-destination value containing the canonical staging root, canonical
output target, and deterministic sibling-candidate relation. It performs no
filesystem allocation or mutation. It proves that the output parent is the
staging root, so a caller cannot reinterpret “contained” as arbitrary
descendant traversal. The atomic writer accepts that value, not raw path
arguments, and owns the filesystem transition: exclusive `CREATE_NEW`
allocation of a collision-free sibling, serialization, fsync, then atomic
exclusive publication with `Files/createLink(output, sibling)` followed by
sibling removal. The same-directory hard-link creation is the linearization
point: it exposes the already-complete inode and fails if `output` exists.
Unsupported hard-link semantics fail closed; the implementation must not fall
back to Java `ATOMIC_MOVE`, whose destination-exists behavior is provider
specific. This guarantees that a successful process does not publish a partial
artifact; it does not claim persistence across power loss. Failure removes the
temporary file and never replaces an existing artifact. Tests must show that
neither the capture path nor the writer can bypass the owner with an
unvalidated string or `Path`.

### Operational closure manifest

The closed EDN value is:

```clojure
{:schema-version :abc-adr-operational-closure-v1
 :owned-namespace-prefixes [abc]
 :entrypoint-namespaces [abc.tools.validate-design-bundle]
 :paths ["src/abc/tools/validate_design_bundle.clj" ...]}
```

For `:entrypoint-kind :clojure`, `entrypoint-namespaces` is a nonempty,
sorted, unique vector of dotted namespace symbols. `owned-namespace-prefixes`
is the closed sorted component
policy and is exactly `[abc]`. Prefix matching is by namespace segment:
`abc` and `abc.*` match, while `abcdef.*` does not. An unresolved namespace
under an owned prefix is always an error, never an external dependency.
`paths` is the exact sorted, unique, contained transitive repository-local
namespace closure derived from those namespaces' `ns` `:require`
declarations.

Resolution examines the Cartesian product of `[src test]` and `[.clj .cljc]`
in that deterministic enumeration order for each namespace coordinate.
Exactly one local candidate is required when the namespace has an owned
prefix. Finding more than one candidate is an error; enumeration order is not
selection precedence. Prefix libspecs are expanded using normal Clojure
`:require` semantics. Other external namespaces are represented by `deps.edn`
and `deps-lock.json`. Reader evaluation and in-body/dynamic `require` remain
unsupported and must instead be represented as reviewed determinants.

Operational namespace closure derivation is a freshness operation, not a
capability verdict. It does not use the focused Var analyzer and does not
reject an entrypoint because its intended behavior reads files or launches an
external command.

The design-bundle observation uses entrypoint namespace
`abc.tools.validate-design-bundle`. The pinned-corpus observation uses
`abc.tools.source-bundle-report`, the entrypoint invoked by the checked
`source-bundle-corpus` derivation contract.

### Operational-command contracts

Operational policy lives in the observation catalog, not in descriptor-authored
fields. Each closed contract contains an explicit `:entrypoint-kind`:

```clojure
{:command-id :validate-design-bundle
 :tool "bash"
 :argv ["bash" "--noprofile" "--norc" "-c"
        "nix run .#validate-design-bundle"]
 :environment-policy :nix-local-v1
 :entrypoint-kind :clojure
 :entrypoint-namespaces [abc.tools.validate-design-bundle]
 :determinant-paths ["flake.nix" "flake.lock" ...]
 :observation-key "design-bundle-operational-passes"}
```

The foundation catalog has exactly two operational observation contracts: the shared
design-bundle event above and the pinned source-bundle-corpus check. The latter
uses `nix build --no-link` and records its actual system-qualified check
coordinate. Three `:operational-behavior` claim bindings reference the one
design-bundle event because all three explicitly concern the behavior of that
supported app and the compatibility matrix requires an operational
observation. The narrow temporary-materialization focus remains the evidence
for `ADR-0009-C6`; it does not impersonate the Nix app for `ADR-0011-C1`.
Running an identical command under different names is forbidden.

`determinant-paths` is a closed sorted reviewed value. The design-bundle set
includes the component flake and lock, the app definition, schemas, fixtures,
and the exact `validate-design-bundle/evidence-input-paths` union. The
source-bundle-corpus set includes the component flake and lock, source-bundle
report fixture, and pinned-source identity.

Nix determinant completeness is a reviewed policy value, not claimed to be
automatically discovered from arbitrary Nix evaluation. Mechanical guarantees
are limited to exact agreement with that reviewed set, the recomputed namespace
closure, current hashes, and the bound locks. The executed command proves the
named flake app/check's result; the catalog's explanation that a particular
Clojure entrypoint implements it remains a reviewed relation. This design does
not claim that source-text matching can prove Nix evaluation semantics.
Changing a determinant path or entrypoint namespace changes the selected
observation-contract hash and stales the artifact.

### Operational descriptor

Task 7 focused descriptors use
`abc-adr-evidence-capture-v3`, which is version 2 plus required
`:catalog-path`, `:observation-id`, and
`:observation-contract-sha256`. Existing versions 1 and 2 remain closed and
unchanged.

Add descriptor schema version `abc-adr-evidence-capture-operational-v1` as a
closed discriminated union. The Clojure-backed shape is:

```clojure
{:schema-version "abc-adr-evidence-capture-operational-v1"
 :tool "bash"
 :argv ["bash" "--noprofile" "--norc" "-c"
        "nix run .#validate-design-bundle"]
 :entrypoint-kind "clojure"
 :clojure-closure-manifest
 "docs/evidence/adr-inputs/design-bundle-operational.edn"
 :catalog-path "data/adr-evidence/foundation-observation-catalog.edn"
 :observation-id :design-bundle-operational
 :observation-contract-sha256 "sha256:<64 lowercase hex digits>"
 :input-set-mode "exact-v1"
 :input-profile {:kind "repo-files-v1"
                 :roots []
                 :explicit [...]}
 :observation-key "design-bundle-operational-passes"}
```

The pure-Nix branch sets `:entrypoint-kind "nix-only"` and forbids
`:clojure-closure-manifest`. Its selected catalog row sets
`:entrypoint-kind :nix-only`, has no `:entrypoint-namespaces`, and supplies a
reviewed exact determinant set. This branch exists for commands such as
`tei-profile-drift` whose Nix check contains no Clojure invocation. It does not
pretend that a namespace closure exists; exactness is over descriptor,
catalog, flake/lock, check expression, and reviewed data determinants.

For the Clojure branch, the closure manifest basename equals the descriptor
basename and is itself an explicit input. `:tool`, `:argv`, entrypoint kind
and namespaces, observation key, and determinant
paths come from the catalog observation row; the descriptor must equal them
and `:tool` must equal the first argv element. Claim bindings are validated
separately and do not participate in the observation-contract hash. Version 1
continues to reject the new fields. Version 2 continues to require
`:runtime-input-manifest` and reject the new catalog and operational fields.
No existing descriptor is silently reinterpreted.

Generic `abc-adr-evidence-run-v1` validation retains the parent contract for
`repo-files-v1`: explicit paths are the minimum and additional bound inputs are
allowed and hash-checked. The stricter rule belongs to the operational
descriptor protocol, not to that profile kind. Every operational-v1 descriptor
requires `:input-set-mode "exact-v1"`; capture and the additional offline
operational validator require the run bundle's actual input keys to equal the
derived set. Dispatch is therefore by closed descriptor version and mode while
the governance-consumed run-bundle profile keeps its existing meaning.

For a Clojure-backed operational descriptor, the derived exact set is the
union of:

- the descriptor and operational closure manifest;
- `deps.edn`, `deps-lock.json`, and `tests.edn`;
- every recomputed closure path; and
- the command-specific Nix expression, flake lock, wrapper, schema, fixture,
  report, and other non-Clojure determinants named by the catalog policy.

For a Nix-only operational descriptor, omit the closure manifest, Clojure lock
files, and closure paths unless the catalog lists them as actual determinants;
the exact set is descriptor + catalog-selected reviewed determinants.

Missing and extra paths both fail. One design-bundle descriptor, manifest,
bundle, and observation support its three operational claim bindings. The
source-corpus observation remains a separate descriptor and bundle because it
is a different command and measurement boundary.

### Capture sequence

Operational capture performs these steps in order:

1. validate repository, workspace, staging, and output roots;
2. derive and validate the contained same-stem descriptor coordinate, then
   load the descriptor;
3. validate the catalog coordinate before loading its selected row and
   validating the closed descriptor shape and contract hash;
4. for `:clojure`, validate the closure-manifest coordinate before loading its
   value; for `:nix-only`, reject any closure-manifest field;
5. for `:clojure`, resolve namespace candidates through the contained-coordinate boundary,
   validating each candidate before reading it, then recompute the closure;
6. validate and canonicalize every determinant and explicit-input coordinate,
   derive the required set, and compare exact set equality before loading any
   bytes;
7. load and hash only those validated contained coordinates;
8. require a clean Git worktree, including untracked files;
9. execute the catalog-owned direct argv vector;
10. require the worktree to remain clean;
11. construct and schema-validate the run bundle; and
12. write it atomically below the external staging root.

All file-reading helpers in capture and offline validation accept validated
contained-coordinate values rather than raw strings or `Path` instances.
Containment failure therefore precedes file content access.

A command that executes and exits nonzero produces a valid observation with
`value: false` and its exit code. Descriptor, closure, containment,
infrastructure, or cleanliness failures produce no observation.

Operational observation details record `nix_system`, `nix_version`, and the
catalog command/check and environment-policy IDs. These are provenance
coordinates, not freshness claims about another host. The versioned
`:nix-local-v1` policy fixes locale and `HOME` to a dedicated capture-owned
temporary directory outside the repository and workspace, suppresses
login/interactive shell initialization, removes `NIX_CONFIG` and flake-override
variables, and inherits only the process-search and Nix-daemon coordinates
required to reach the installed `bash` and `nix`. The temporary home is removed
in `finally`. Future changes to that allowlist require a new policy ID. The
guarantee is a passing or failing observation over hash-bound repository
inputs on that recorded Nix system/version; Git snapshots alone are not
described as hermetic execution.

### Offline validation owns the same protocol

Evidence validation derives the descriptor path from the run-bundle artifact
stem, loads the descriptor and current catalog, and reruns all shape, same-stem,
closure, determinant-set, and exact-input checks without executing the command.
The descriptor, closure manifest, and all derived inputs must occur in the
bundle input map with current hashes. The validator also recomputes the
canonical selected observation row and requires its hash to equal the
descriptor's expected contract hash. The descriptor's own current file hash is
already a run-bundle input. The catalog's unrelated rows and claim bindings are
not observation inputs.

A bundle missing its descriptor or bound policy artifacts is invalid even if
its remaining hashes agree. This prevents hand-authored internally consistent
bundles from bypassing capture-time input completeness. Runtime trace equality
remains an execution-time observation; offline validation verifies the bound
manifest and static policy but does not claim to replay the command.

## Failure Taxonomy

New operational failures use `:invalid-operational-closure` with stable
coordinates such as descriptor path, manifest path, namespace, or input path.
It covers:

- invalid manifest shape or schema version;
- missing, duplicate, or unsorted entrypoint namespaces or paths;
- unresolved entrypoint namespace;
- missing, extra, absolute, traversal, or symlink-escaping closure path;
- mismatch between recorded and recomputed namespace closure; and
- mismatch between the exact required and declared determinant sets.

Current-file hash mismatches remain the existing `:input-hash-mismatch`
problem. A path value itself is not described as stale.

Invalid staging or output paths use `:invalid-evidence-output`; existing
targets use `:evidence-output-exists`. Atomic-write failures leave no accepted
artifact and report `:evidence-output-write-failed`.

Generic descriptor shape violations remain `:invalid-evidence-artifact`.
Focused-test source, capability, trace, and ownership failures retain their
existing specific kinds so operational freshness cannot mask a focused safety
failure.

## Migration and Sequencing

1. Amend foundation Tasks 2B, 6C, 7, and 8 to cite this design and remove the
   superseded root, count, duplicate-observation, and capture-loop instructions.
2. Land Task 6C: retire the test-only focused-Var Nix closure shape and old
   problem name; add the alias-aware reader, observation catalog, normalized
   exact conformance debt, owner around the existing ephemeral authorization
   primitive, operational manifest/descriptor protocols, root separation, and
   capture/offline validation under tests. Governance stays in audit mode and
   no evidence bundle is captured.
3. Amend Task 7's file inventory to include every namespace containing a
   planned focused Var and every newly authored narrow Var. Its commit stages
   all required test files as well as the descriptors, manifests, and
   registration template.
4. Task 7 authors the 14 missing narrow Vars, gives all 35 focused Vars direct
   trace ownership, resolves every catalog finding, and checks in exactly 37
   descriptors and manifests: 35 focused and two operational.
5. Task 8 captures those 37 artifacts from the clean Task 7 commit. It
   registers 42 claim-support rows because three focused observations each
   support two claims and the shared design-bundle observation supports three
   claims, then regenerates reports atomically.

The preserved pre-design Task 7 prototype is disposable input. Generated
placeholder descriptors are not promoted unless they satisfy the implemented
contracts from a clean tree.

## Alternatives Rejected

### Run operational commands through the focused-test protocol

Rejected because legitimate Nix/application I/O and subprocess behavior would
require broad exceptions in the strict focused analyzer. That would conflate
operational freshness with capability purity and weaken the stronger protocol.

### Keep version 1 and validate closure only in a corpus test

Rejected because capture itself could execute a descriptor whose closure
manifest is decorative or stale. This recreates the same-entry
self-certification fault that artifact-backed evidence was designed to remove.

### Keep both Nix/Clojure closure shapes

Rejected because `:abc-adr-nix-clojure-closure-v1` derives operational
freshness through the focused capability graph while the new closure derives
namespace freshness without a capability verdict. Accepting both would retain
two meanings for the same operational responsibility. The old shape has no
committed consumers, so retirement is cheaper and clearer than compatibility.

### Bind only Nix files for operational commands

Rejected because the Nix derivation executes Clojure whose source closure can
change independently of its wrapper and lock files.

## Acceptance Criteria

- Existing version-1 and version-2 descriptor fixtures retain their exact
  accepted and rejected shapes.
- The old `:abc-adr-nix-clojure-closure-v1` schema and functions have no
  accepted path, and focused-runner failures use
  `:invalid-focused-evidence-runner` rather than a closure problem kind.
- Alias-aware reading accepts known namespace aliases, rejects unknown aliases,
  preserves reader/kondo source-coordinate agreement, and is deterministic
  under repeated and concurrent analysis without global namespace mutation.
- The checked observation catalog reports all 35 focused rows
  deterministically and identifies all unresolved or nonconforming rows
  without first-error loss; the join validator separately reconciles 37
  observations with 42 claim bindings.
- The catalog schema/validator is family-neutral and accepts a closed valid
  non-foundation catalog with different counts; only the foundation admission
  test enforces 37/42.
- Task 6C requires exact equality with the checked transitional conformance
  debt, and Task 7 changes both expected and observed debt to the empty vector
  in one commit.
- Changing only a claim binding or unrelated observation row does not stale an
  artifact; changing its selected observation contract does.
- Observation-contract known-answer tests pin the exact I-JSON projection,
  RFC 8785 bytes, and SHA-256 independently of EDN map iteration order.
- Every focused pure-capability addition is exact and has a neighboring
  rejection test; no namespace-wide allowance exists.
- Existing `with-ephemeral-root` authorization behavior remains covered; the
  new owner cleans up on success and exception and cannot authorize repository,
  workspace, or unrelated external reads.
- Clojure-backed operational manifests are total, exact, reproducible functions of named
  entrypoint namespaces, the specified resolver algebra, and current repository
  namespace forms.
- Operational capture rejects every missing, extra, malformed, unresolved,
  absolute, traversal, symlink-escaping, or recorded-versus-recomputed
  closure coordinate before command execution.
- Both foundation operational observations have checked same-stem manifests and exact
  reviewed determinant sets; the one design-bundle observation has three
  explicit claim bindings without duplicate renamed runs.
- A `:nix-only` operational fixture forbids a closure manifest and namespace
  resolution while retaining exact reviewed determinant equality.
- Operational execution uses the catalog-owned versioned environment policy
  and records that policy with the observed Nix system and version.
- Offline validation rejects a bundle that omits, changes, or disagrees with
  its descriptor, selected observation-contract hash, closure manifest,
  reviewed determinant set, exact-v1 descriptor mode, or exact input set.
- Repository-local, workspace-local, symlinked, existing, partially written,
  unsupported-hard-link, and serialization-failing output cases cannot
  produce an accepted bundle; an existing destination is never replaced.
- No Task 6C commit creates run bundles, changes the evidence registry, promotes
  ADR 0034, or changes the governance mode.
- The focused suites, Clojure lint/format checks, Nix format checks, root
  governance audit, and `just validate-migration` pass from a clean tree.

## Recovery

Before evidence capture, recovery is forward-only and local: leave governance
in audit mode, remove no historical artifact, and repair the descriptor or
closure implementation in a new commit. If operational closure validation is
faulty, no operational-v1 descriptor is admitted until the validator and its
negative tests agree. Its successor would be operational-v2; focused versions
1, 2, and 3 and each operational version remain available only for their exact
contracts, and recovery never widens one silently.
