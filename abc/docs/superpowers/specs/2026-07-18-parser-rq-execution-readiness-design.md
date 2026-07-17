# Parser Release Qualification Execution Readiness Design

Date: 2026-07-18

Status: Proposed amendment to the P5 admission-and-promotion campaign

## Purpose

The P5 admission-and-promotion implementation is not yet safe to execute on
`hinoki.hyakutake-barbel.ts.net`. Preflight on 2026-07-18 stopped before a
candidate authorization was minted and found that the bounded transaction did
not establish a working production transaction:

- `parser-rq-campaign-provenance.py` exposes library functions but no command
  line interface, so the Task 10 `compare-builds` command exits without writing
  evidence;
- raw `nix build --json` output does not contain the executable, NAR, adapter,
  revision, or argument-template records consumed by `compare_builds`;
- the final provenance schema requires candidate and qualification identity
  bindings that can only be added after candidate derivation;
- `parser-rq-campaign-capture.sh` delegates to six caller-supplied command
  environment variables, but no production wiring supplies them;
- the shell takes the campaign lock while the core-attempt producer also tries
  to take the same lock, so straightforward production wiring would deadlock or
  fail closed;
- pre-window `verify-authorization` uses the current clock and therefore rejects
  a correctly future-dated authorization;
- the plan leaves several candidate inputs and evidence-tree commands dependent
  on the caller's current working directory;
- hinoki reports the kernel hostname `hinoki`, not the stable Tailscale name,
  and has no configured external replica failure domain.

These are pre-freeze defects. Repairing them changes the candidate Git revision,
so the current `main` revision is not a release candidate. No honest evidence or
authorization exists for it, and no unavailable attempt needs to be recorded.

This design adds one execution-readiness slice before the existing P5 candidate
freeze. It does not change the nine predicates, their thresholds, the pinned
corpus, registry authority, admission relation, or promotion criteria.

## Decision

Add a closed production transaction with three explicit boundaries:

1. a build-provenance tool turns two independently realized Nix outputs into a
   candidate-bindable provenance value;
2. one repository-owned orchestrator owns the fixed lane graph, campaign lock,
   and capture lifecycle;
3. a read-only site preflight authenticates host and storage configuration
   before authorization exists.

The one-shot authorization remains the transition from preparation to
execution. Nothing before that transition may execute a volatile predicate
lane, create a capture generation, or consume an authorization ordinal.

Readiness has two deliberately different values. A site-preflight report says
the machine can host a campaign. A final readiness receipt says one particular
candidate, provenance value, site-preflight report, and production graph are
ready now. Only the latter is authorization input.

## Why This Is One Slice

The defects appear in several files but describe one protocol: turning a
candidate source revision into one authorized, reproducible capture. Fixing only
the CLI leaves an unwired capture; fixing only the lane commands leaves
unverifiable executables; supplying environment variables leaves orchestration
authority with the operator. The slice is complete only when one dry-run can
prove every nonvolatile prerequisite without minting an authorization.

This is Protocol Design, not a convenience wrapper. It owns time, identity,
exclusive execution, executable provenance, closed lane membership, and the
transition into immutable evidence.

## Alternatives Considered

### Manually compose existing commands

An operator could assemble the six lane commands in a shell profile and build
the provenance EDN by hand. This is rejected because command selection and
evidence authorship would move into an unreviewed place. The campaign would
again trust a human-selected description of what ran.

### Keep six configurable lane command variables

This preserves the current shell shape but makes the fixed production graph a
site concern. It also permits a caller to replace one producer without changing
the candidate's instrument identity. This is rejected. Test injection belongs
behind an explicit test interface, not in the production authority path.

### Declare the current candidate unavailable

No capture was authorized or started, so there is no candidate attempt to mark
unavailable. More importantly, a new revision would encounter the same missing
transaction. This is a truthful stop condition, not a remedy.

## Build Provenance

### Staging value and final value

The provenance cycle is resolved with values, not mutation:

1. `capture-build` reads one Nix build result through its explicitly named store
   and emits a closed staging build record.
2. `compare-builds` compares build A and build B and emits an unbound
   reproducibility proof containing both named build rows and the common closed
   executable set.
3. candidate derivation hashes that proof while deliberately excluding the two
   later binding fields, matching the existing
   `executable-provenance-ref` rule.
4. `bind-provenance` adds `schema_id`, `schema_version`, `candidate_ref`, and
   `qualification_identity_ref`, then proves that the provenance core hash did
   not change.
5. only the bound, schema-valid `executable-provenance.edn` is committed.

The staging records are disposable. They must not be accepted by promotion or
stored under the immutable run directory.

### Independent realization

Each build uses a newly created, initially empty local Nix store with a distinct
store URI. The build record carries its store URI and build ID while staged.
The provenance tool reads NAR identity and executable bytes through that exact
store; it never maps a logical `/nix/store/...` path to a guessed host path.

The target output must be realized independently in both stores. A substituted
copy of the target output, two queries of the daemon store, or two reads of one
physical output do not qualify. Dependencies may come from authenticated
substituters, but the candidate target must be built in each empty store. The
hinoki preflight must demonstrate the exact Nix invocation that achieves this;
if the installed Nix cannot prove it, the campaign stops before authorization.

After comparison, one proven output may be copied into the daemon store for
execution. Its NAR identity and every bound executable byte identity are
rechecked after the copy. Copying is distribution of an already proven value,
not a third build.

### Executable closure

The default `ab-validator` flake package is the single candidate output. The
provenance manifest names every candidate executable used by the fixed graph,
including at least `ab-check`, `ab-aozora`, `ab-aat-to-parser-ir`,
`ab-parser-rq-source-accountability`, and
`ab-parser-rq-diagnostic-authorization`. For each executable it binds:

- logical Nix output path and NAR hash;
- executable SHA-256 and byte count;
- adapter and version coordinates;
- candidate Git revision; and
- the closed argument template used by the orchestrator.

Repository Python and Clojure instruments remain bound by their existing
semantic-closure and predicate instrument identities. They are invoked from the
detached candidate tree, never from the moving evidence checkout.

Unknown, missing, duplicate, or extra executable records make provenance
unavailable.

## Production Orchestration

### One owner

Replace the six production command environment variables with one
repository-owned orchestration entry point. Its public inputs are values and
places only:

- candidate, authorization, and bound provenance paths;
- detached candidate repository root;
- explicit corpus root;
- primary content-store root;
- campaign lock path; and
- fresh staging root.

It derives all policy, schema, mapping, fixture, executable, and analyzer paths
from the authenticated candidate and detached tree. Production callers cannot
supply a lane command, change lane order, omit a lane, or add a lane.

### Fixed graph

The orchestrator owns this serial graph:

1. authenticate candidate, bound provenance, authorization record, site
   preflight report, final readiness receipt, and executable bytes;
2. acquire the campaign lock once and retain the same locked file description;
3. record the real capture start and verify it is inside the authorized window;
4. run three complete core attempts and reduce them by maximum;
5. run the source-accountability producer;
6. run the shared predicate-hardening producer once;
7. derive diagnostic authorization from the authenticated shared raw values;
8. materialize and validate publication structure;
9. run resource capture serially, one work per transient service;
10. derive the seven member observations and compose exactly nine envelopes;
11. write the closed capture index; and
12. release the lock only after capture authentication finishes or an honest
    unavailable terminal record has been staged.

The existing core producer must accept an inherited lock capability or a
controller-owned lock handle. It may not reacquire the campaign lock. Other
producers do not own membership or lifecycle and cannot release or replace it.
The inherited descriptor is a private controller-to-child capability, not a
public configuration option; tests may inject an equivalent locked handle.

### Producer output contract

Every lane writes its detailed closed records beneath its own staging subtree.
The orchestrator alone installs these seven canonical campaign members:

- `core_attempt.json`;
- `source_recognition.json`;
- `diagnostic_gap.json`;
- `diagnostic_completeness.json`;
- `parser_ir_conformance.json`;
- `publication_structure.json`; and
- `resource.json`.

Each member contains only its already assigned identity-bearing observation
envelopes. The Clojure composer remains the authority for closed member
membership and the exact nine-key union.

### Failure semantics

Before the first candidate process, any defect exits without a capture attempt.
After the real capture start, interruption, lock loss, unknown producer status,
missing member, expired authorization, or instrument protocol failure is part of
the sole attempt and must be preserved as unavailable. The orchestrator never
retries, repairs an observed value, changes the authorization window, or starts
a sibling generation.

## Authorization Verification

Split record integrity from permission to execute:

- `verify-authorization-record` validates the closed schema, self-reference,
  ordinal, candidate and identity bindings, interval ordering, repetition count,
  reduction, and host-policy reference without consulting the clock.
- `verify-authorization` additionally requires an explicit real UTC instant and
  proves it lies inside the inclusive interval.

Task 10 uses the structural command before commit. The orchestrator uses the
temporal command exactly once immediately before the first volatile process,
with its recorded capture start. A timestamp chosen merely because it lies in
the interval cannot authorize execution.

The authorization schema rotates to version 2.0.0 and gains one required
`readiness_receipt_ref`. The self-reference covers that field. Existing bounded
fixtures migrate; no historical real authorization exists to reinterpret. The
structural verifier requires the receipt to authenticate the same candidate,
qualification identity, provenance core, site-preflight report, and production
graph. This is a protocol version change, not a predicate-set change.

## Site Configuration and Host Identity

### External site descriptor

Hinoki receives one explicit, untracked site descriptor path at runtime. The
descriptor contains no observation values and is not evidence authority. It
names:

- stable host label `hinoki.hyakutake-barbel.ts.net`;
- expected kernel hostname `hinoki`;
- qualification corpus root;
- primary store root;
- externally mounted replica root;
- campaign lock path; and
- primary and replica failure-domain identifiers selected from the committed
  site policy.

The repository provides a schema and example, not machine-local paths in active
code. The actual descriptor remains host configuration.

A committed site policy owns the accepted stable host label, short kernel
hostname, production graph version, primary failure-domain identity, and
approved external replica identity and mount class. The untracked descriptor
maps runtime places to those identities; it cannot introduce a new identity or
claim that a local block device is the approved external domain. Changing the
approved domain is reviewed pre-freeze policy work and therefore changes the
prospective candidate revision.

### Host check

The stable campaign label and the kernel hostname are different facts. Preflight
requires the kernel hostname to equal `hinoki`, the stable FQDN to resolve, and
at least one resolved address to be assigned to a local interface. It records
both facts. It does not require changing the host's system hostname merely to
make `hostname -f` echo the policy label.

This check is injected in bounded tests so DNS and interface state do not make
the test suite host-dependent.

### Failure-domain check

Different paths or devices on hinoki are not independent failure domains. The
replica must be mounted from a separately administered storage failure domain
that survives loss of hinoki. Preflight requires:

- the exact distinct identities and mount classes accepted by committed policy;
- different resolved roots;
- distinct mount sources and filesystem identities; and
- successful create, fsync, stream-read, and removal probes in disposable
  preflight namespaces.

The current `/db` and `/data` mounts do not satisfy the host-failure criterion.
P5 remains operationally blocked until an external replica is configured. The
replication verifier continues to authenticate every logical blob by streaming
both copies; mount identity is an additional precondition, not a substitute for
content verification.

## Site Preflight and Readiness Receipt

`preflight-site` is a read-only campaign command except for disposable probes in
explicit scratch and store roots. It emits a canonical site-preflight report
that binds:

- evidence-tree revision and cleanliness;
- host identity facts;
- site descriptor and committed site-policy hashes;
- independent-build capability result;
- primary/replica failure-domain facts;
- lock availability; and
- the exact production graph version.

`seal-readiness` runs only after provenance and candidate derivation. It
reauthenticates the current site facts and emits a final canonical receipt that
binds:

- the site-preflight report hash;
- candidate-tree Git revision and cleanliness;
- candidate and qualification identity references;
- bound executable-provenance reference and executable closure;
- corpus snapshot and list hashes;
- policy and schema hashes;
- evidence-tree base revision and cleanliness before publication;
- lock availability; and
- production graph version.

Neither value contains an authorization interval or predicate observations.
Authorization binds the final receipt hash. Any relevant site, candidate, or
graph change after sealing requires a new receipt before authorization, not an
edit to an existing value. The site report and final receipt are immutable
values; only the final receipt is committed in the candidate run directory.
Disposable build and preflight staging values are removed after the committed
values authenticate; they are never treated as a second evidence generation.

## Ordering

The amended campaign order is:

1. implement and test this execution-readiness slice on `main`;
2. update the P5 plan and bounded drift fixture;
3. run the complete pre-freeze validation and push `main`;
4. on hinoki, configure and validate an external replica;
5. run `preflight-site` without an authorization or candidate execution;
6. perform two independent builds and create bound provenance;
7. derive the candidate and run `seal-readiness`;
8. mint, commit, and push the sole authorization;
9. execute the unchanged capture, admission, and promotion sequence.

Any implementation, schema, policy, corpus, predicate, or instrument change
after step 3 creates a new prospective candidate. Any readiness failure before
step 8 is preparation failure, not a capture attempt. Any failure after step 8
follows the existing one-shot terminal semantics.

## Migration and Rollback

The bounded fixture and authorization schema migrate atomically with the new
verifiers. Before a real authorization is committed, rollback is an ordinary
revert followed by the full pre-freeze gate; no release evidence points at the
new protocol. After a real authorization is committed, reverting or rewriting
its protocol is forbidden for that candidate. The candidate instead reaches
its honest terminal state, and any correction begins with a new implementation
revision and candidate.

## Verification Strategy

### Build-provenance tests

- CLI help and dispatch are exercised, including atomic output creation.
- Raw Nix JSON is rejected unless captured through its explicit store.
- Empty-store provenance is distinguished from repeated daemon-store reads.
- NAR, output, executable membership, bytes, revision, or argument-template
  disagreement exits nonzero and emits unavailable staging evidence only.
- Binding candidate fields preserves the provenance core hash and produces a
  schema-valid final value accepted by the Clojure verifier.
- A final proven output copied into the daemon store reauthenticates byte for
  byte.

### Orchestration tests

- A bounded real graph runs every producer once in the fixed order.
- Candidate executable paths come only from bound provenance.
- The parent lock remains held across all lanes and the core producer does not
  reacquire it.
- Omitted, extra, repeated, reordered, or caller-replaced lanes fail.
- Every terminal status maps to the existing observation algebra.
- Composition produces exactly the seven members and nine envelopes.
- No analyzer or composition command starts a candidate executable.

### Authorization and site tests

- Structural verification accepts a future valid record while temporal
  verification rejects execution before its window.
- The real capture start is the timestamp authenticated in the capture index.
- Short kernel hostname plus locally assigned stable-FQDN address passes; a
  remote-only DNS answer, wrong short hostname, or ambiguous identity fails.
- same path, same mount, same device, or same host failure domain fails replica
  preflight.
- missing site fields and ambient fallback values fail closed.
- an untracked descriptor cannot introduce or relabel a failure domain absent
  from committed site policy.

### Hinoki gates

Before candidate freeze, run a no-authorization dry-run that proves:

- detached clean candidate and evidence trees;
- production CLI availability;
- independent target realization in two fresh stores;
- exact executable closure;
- external replica independence and writeability;
- campaign lock availability; and
- no candidate process or capture directory was created.

The full repository migration gate then runs from the pushed revision. Only
after both gates pass may Task 10 begin.

## Acceptance Criteria

1. Every Task 10 and Task 11 command exists and has a checked `--help` or
   equivalent usage path.
2. Two raw Nix result files cannot be mistaken for compared build evidence.
3. Final executable provenance is candidate-bound, schema-valid, and accepted
   by `verify-provenance`.
4. Production capture accepts no caller-supplied lane command.
5. One lock owner spans the entire fixed graph.
6. Structural authorization verification performs no clock authorization.
7. The first volatile process cannot start before real-time authorization.
8. All candidate and policy paths resolve from the detached candidate tree; all
   evidence Git operations resolve from the explicit evidence tree.
9. Host identity does not depend on `hostname -f` synthesizing the Tailscale
   name.
10. A second disk on hinoki is rejected as the external replica.
11. Authorization v2 binds the final candidate readiness receipt.
12. Hinoki site preflight and candidate readiness sealing are green before the
    sole authorization is minted.
13. The existing nine predicates, corpus, admission relation, registry, and ADR
    promotion semantics are byte-unchanged by this slice.

## Falsifiers

The design must be reconsidered if:

- Nix cannot independently realize the target in two inspectable fresh stores;
- the default package does not contain the closed executable set used by the
  graph;
- a producer cannot participate without discovering membership or reacquiring
  lifecycle authority;
- publication materialization requires an unbound executable or moving-tree
  input;
- the host identity cannot be authenticated without changing campaign policy;
- no external replica failure domain can be made available; or
- the bounded production graph cannot prove that each candidate executable ran
  at most the authorized number of times.

Any falsifier stops before authorization. It is evidence that the campaign
boundary is wrong, not permission to weaken the check.

## Non-Goals

- Changing predicate semantics, thresholds, or observed keys.
- Changing the qualification corpus.
- Admitting a registry tuple or accepting ADR 0039/0040.
- Running the authoritative capture while implementing this slice.
- Supporting arbitrary hosts, remote execution schedulers, or multiple storage
  backends.
- Treating a second local disk as disaster-independent evidence storage.
- Adding retries or operator-selected capture generations.

## Open Operational Dependency

The code and local tests can implement this design, but the authoritative P5 run
cannot proceed until an external replica mount and its site descriptor are
available on hinoki. This is deliberately an operational dependency, not a
repository default. The implementation plan must separate code completion from
the later site-readiness gate and must never report the P5 campaign complete
while that dependency remains unsatisfied.
