# P5 Parser Admission and Promotion Design

Date: 2026-07-17

Status: Revised after depth-4 review; implementation pending

Depends on: P0 Foundation, P1/P4A source accountability and diagnostic gap
partition, P2 publication structure, P3 process-tree resource qualification,
and P4B predicate hardening

## Purpose

P5 turns the bounded P0-P4 instruments into one authoritative release-candidate
campaign. It selects an immutable custom-parser implementation revision, runs
every release instrument against that one identity on
`hinoki.hyakutake-barbel.ts.net`, resolves ADR-0023 compatibility admission,
derives the nine-predicate gate report, and conditionally promotes ADR 0039.

The deliverable is an honest, reproducible verdict. Promotion is possible but
is not the definition of success. A measured predicate failure, an admission
conflict, or an unavailable observation is preserved as evidence and leaves ADR
0039 Proposed.

P5 does not execute the neutral third-party-parser study, make that study a
release dependency, or retire third-party parser lanes. Those remain later
Track-S publication and retirement work.

## Grounded state

The repository already provides:

- the closed qualification identity, identity reference, observation envelope,
  coherence precondition, registry-derived `admitted?`, exact predicate
  evaluator, and conditional ADR-0039 status projection in
  `abc.tools.parser-release-qualification`;
- logical blob identity, authenticated reads, closed membership, capture
  generation identity, and fail-closed status mapping in
  `abc.tools.parser-rq-capture`;
- ledger-authoritative source accountability and diagnostic-gap derivation for
  predicates 2 and 3;
- publication-structure derivation for predicate 6;
- process-tree cgroup-memory derivation for predicate 8;
- diagnostic-envelope completeness and Parser-IR conformance derivation for
  predicates 4 and 5;
- `ab-check` batch reports that distinguish `fatal_error` and
  `adapter_timeout`, the declared authorities for predicates 1 and 9;
- the declared GNU `time(1)` batch wall-time authority for predicate 7;
- `ab-aat-to-parser-ir audit-corpus --compat-edn-out`, which produces the
  compatibility candidate consumed by `abc.tools.aat-parser-ir-compat`; and
- ADR governance that forbids Accepted status without executable evidence.

P3 and P4B are implemented and drift-checked. The campaign roadmap's earlier
P3 “choose one” wording is stale; cgroup-v2 `memory.peak` under a transient
systemd user service is the implemented and governed mechanism.

Three pre-freeze gaps remain and belong to P5:

1. Predicates 1, 7, and 9 still need identity-bound Capture -> Derive records;
   historical scalar values are not authoritative P5 evidence.
2. Predicates 2 and 3 still describe their instruments as “no committed
   instrument” in the live predicate contract. Those strings participate in
   `predicate_set_hash` and must be corrected before candidate freeze.
3. ADR 0039 says “HEAD build,” but evidence, registry, and promotion commits
   necessarily advance repository HEAD after the measured parser binary was
   built. Literal moving-HEAD identity would be circular.

## Decision

Use a candidate-centric staged transaction.

The candidate is an immutable implementation commit, not the repository HEAD at
the time evidence is reviewed or ADR status changes. P5 builds the parser from a
detached worktree at that commit. Later commits publish evidence about that
candidate without changing its `parser_git_rev`.

The campaign publishes immutable values and derives state from them. It does
not maintain a mutable campaign-status flag. The authoritative facts are:

- a closed candidate descriptor and its content hash;
- closed instrument capture manifests, indexes, aggregates, policies, and
  observation envelopes;
- a conversion-audit candidate row and its admission report;
- a deterministic nine-observation measurement bundle;
- a deterministic gate report; and
- governance commits that accept the instrument-binding ADR before freeze and
  may accept ADR 0040 and ADR 0039 only when their independently checked
  preconditions hold.

Capture, admission, evaluation, and promotion are separate boundaries. No
capture program edits the registry or an ADR. No admission program changes an
observation. No report generator changes governance state. Promotion is a
human-reviewed repository commit guarded by a pure verifier.

## Considered alternatives

### One commit containing capture, admission, report, and promotion

Rejected. It hides the unadmitted precondition state, makes honest failing runs
awkward to retain, and braids execution with governance mutation. It also makes
reviewers reason about volatile host execution and ADR authority in one diff.

### Treat the evidence-bearing commit as the candidate HEAD

Rejected. Committing evidence advances HEAD, which would change the candidate
revision and require another build and capture. No finite commit can contain a
truthful self-reference to its own Git object ID.

### Candidate-centric staged transaction

Selected. The parser revision is frozen once; later values refer to it. Failed
or conflicting outcomes remain publishable, and promotion stays a small,
auditable state transition.

## Identity model

### Candidate identity

The candidate descriptor is a closed value containing the existing full
`qualification-identity-keys` tuple:

- the nine ADR-0023 admission coordinates;
- `parser_git_rev` for the detached implementation commit;
- qualification corpus snapshot and list hashes;
- the final predicate-set hash; and
- the complete instrument-version map.

It additionally carries its computed `qualification_identity_ref` beside the
identity value. The verifier recomputes the reference and rejects disagreement.
The reference is never typed into downstream records independently; generators
project it from the authenticated candidate descriptor.

`parser_git_rev` identifies the implementation being executed. Evidence commit
IDs, capture timestamps, hinoki boot ID, and promotion commit IDs are attempt or
governance context, not parser identity.

### Executable provenance

Checking out the candidate SHA is insufficient proof that the invoked binary
came from it. Before capture, P5 builds the flake outputs from the detached
candidate worktree and records:

- the candidate Git SHA;
- the Nix derivation/output identity for each invoked executable;
- a streaming SHA-256 and byte count for each executable;
- each executable's self-reported adapter/version coordinates; and
- the exact argv templates used by each capture lane.

The candidate verifier requires the baked adapter revision to equal
`parser_git_rev` and the remaining self-reported coordinates to equal the
candidate descriptor. A prebuilt binary whose labels merely claim the candidate
identity is unavailable unless its recorded Nix output and byte identity match
the built candidate outputs.

Bit-reproducible candidate builds are an explicit eligibility precondition.
Before authorization, hinoki performs two clean, independent Nix rebuilds of
every measured executable and compares output/NAR identities, executable byte
counts, and SHA-256 digests. Any disagreement makes the candidate unavailable
and blocks capture. The accepted output closure is then preserved by logical
identity in the replicated external store; recovery first re-resolves those
preserved bytes. Rebuilding is a valid recovery only because the candidate has
already passed the two-build reproducibility check.

### Instrument and policy identity

The final `instrument_versions` map is closed over every producer used by the
nine predicates. It distinguishes at least:

- core `ab-check` attempt capture for predicates 1, 7, and 9;
- source recognition and R1 derivation;
- diagnostic authorization and R2 derivation;
- diagnostic-envelope completeness;
- Parser-IR schema conformance;
- publication structure; and
- process-tree cgroup memory.

Policy, schema, semantic-closure, host-capability, and census hashes remain in
their instrument-specific records and manifests. The full qualification
identity names instrument versions; instrument records authenticate the deeper
semantic identities. P5 does not flatten every policy hash into the admission
projection.

### Predicate-set correction before freeze

P5 first binds the implemented R1/R2 instruments and the new core-attempt
capture in the predicate contract without changing any dimension, comparator,
threshold, observed key, or unit. Because `:instrument` participates in
`predicate_set_hash`, this is an identity rotation even where predicate meaning
is unchanged.

The rotation requires a separately evidenced ADR (next available number at
implementation time; currently 0041). Its bounded structural and derivation
evidence must pass and the ADR must be Accepted before candidate freeze. It must
state:

- exactly which instrument descriptions changed;
- that no threshold, comparator, observed key, dimension, or unit changed;
- that every old observation remains historical and is never relabeled; and
- that the final candidate is selected only after the new hash is committed.

If implementation discovers a semantic predicate change rather than an
instrument binding, P5 stops. It must not smuggle that change through the
metadata ADR.

## Artifact model

P5 separates measurement time from registry time. One candidate has one
authorized capture, but that capture may be evaluated before and after an
append-only registry admission. Keying a mutable directory only by
qualification identity would erase that evaluation succession.

The repository may preserve captures for many candidate identities, but P5
authorizes exactly one authoritative volatile capture for any one candidate.
Before execution, a committed capture authorization binds the candidate
identity, hinoki host policy, fixed execution window, fixed repetition schedule,
and ordinal `1`. The capture tool refuses to start outside that window, and the
capture index must reference the authorization hash. Production-shaped probes
use fixture identities and finish before candidate freeze; they cannot become
authoritative captures by relabeling.

The planned committed layout is:

```text
abc/docs/reports/parser-rq/runs/<identity-ref-without-sha256-prefix>/
  candidate.edn
  authorizations/<authorization-ref-without-prefix>.edn
  captures/<capture-generation-ref-without-prefix>/
    capture-index.edn
    measurements.edn
    capture-manifests/
    aggregates/
  evaluations/<evaluation-generation-ref-without-prefix>/
    evaluation-index.edn
    admission-candidate.edn
    admission-report.edn
    qualification-report.json
```

Large parser outputs, decoded texts, Parser-IR documents, and publication
artifacts stay in the configured external content-addressed store on hinoki.
Committed manifests contain logical identity (`sha256:<digest>`, bytes, media
type) and a runtime-resolved relative locator. The locator is not identity.

Every manifest-referenced blob, including accepted executable closures, must
exist in two independently configured failure domains before an evidence commit
or promotion is eligible. P5 records a replication receipt containing the same
logical identities and independently re-hashes both replicas. One replica may
be offline, but it cannot share hinoki's storage failure domain. Missing or
mismatched replication makes the owning capture unavailable. Garbage collection
requires both that no committed manifest references the blob and that the
replica-retention policy permits deletion; reference counting alone is not a
durability policy.

Every capture and evaluation directory is append-only after publication.
Corrections and retries create new generation references or, before
publication, replace the entire staged directory.
The stable historical paths
`docs/reports/parser-release-qualification-measurements.edn` and
`docs/reports/parser-release-qualification-report.json` become deterministic
canonical projections, not operator selections or independent hand-edited
authorities. Measurements project the candidate's one authorized capture. The
report projects the unique evaluation whose `registry_ref` equals the hash of
the current committed registry. Their bytes must equal the corresponding
immutable generation artifacts.

The capture index is a closed set of named references. It names exactly one
candidate descriptor, one core-attempt aggregate, one R1 aggregate, one R2
aggregate, one diagnostic-completeness aggregate, one Parser-IR-conformance
aggregate, one publication aggregate, one resource aggregate, and one
measurement bundle. Its `capture_generation_ref` is the JCS hash of the closed
index with that field removed. Unknown, missing, duplicate, or differently
identified members make the capture unavailable.

An evaluation index names exactly one capture generation, the logical hash of
the registry bytes used for evaluation, one admission candidate, one admission
report, and one qualification report. Its `evaluation_generation_ref` uses the
same projected-hash rule. Registry admission therefore creates a new evaluation
of unchanged measurements rather than mutating the capture or pretending the
registry had always contained the candidate.

The promotion verifier enumerates the candidate's generation directory rather
than trusting a selected path. It requires exactly one capture bearing the
committed authorization, no second authoritative capture for that candidate,
and exactly one valid evaluation for the current registry hash. A second
volatile capture makes that candidate permanently ineligible for promotion;
the next attempt requires a new implementation candidate and a new baked
`parser_git_rev`. This deliberately trades retry convenience for removal of the
favorable-sample selection seam.

## Core attempt capture for predicates 1, 7, and 9

P5 adds one small instrument rather than preserving three legacy scalars.

One authorized core capture runs exactly three serial repetitions of the
declared `ab-check` batch command over the exact qualification membership, with
the committed per-work timeout and job count. Repetition count and reduction are
part of the instrument-binding ADR and policy; the operator cannot stop after a
favorable repetition. The Nix-pinned GNU `time(1)` wraps each batch under
`LC_ALL=C` and writes one `%e` elapsed-real value to a dedicated output. The
analyzer rejects an empty, multi-line, negative, non-finite, or otherwise
non-decimal timing record and emits the maximum of the three accepted values as
the Clojure double required by the live `:<=` comparator.

The orchestrator acquires the exclusive hinoki campaign lock before any
volatile lane starts. Core repetitions and the resource lane run serially; no
other P5 lane may execute concurrently. Lock identity, competing parser-related
user units, load, and memory pressure are recorded before and after every
repetition. Failure to acquire or retain the lock makes the capture unavailable.
Load and pressure remain disclosed context rather than operator-controlled
rejection criteria; the fixed maximum-of-three reduction absorbs ordinary
jitter conservatively.

The capture records:

- exact expected work IDs and source hashes;
- one authenticated `ab-check` report per work per repetition;
- adapter and adapter-version coordinates;
- per-work disposition `parsed`, `fatal_error`, `adapter_timeout`, or
  `protocol_error`;
- the timeout policy;
- GNU time executable identity and raw timing record;
- batch argv, exit status, start/end attempt context, and host label; and
- all three wall times and the fixed reduction rule.

The pure analyzer authenticates the closed index and derives three independent
envelopes:

- `fatal_failures` = maximum per-repetition count of `fatal_error`
  dispositions;
- `wall_time_seconds` = maximum authenticated GNU-time elapsed-real value; and
- `timeouts` = maximum per-repetition count of `adapter_timeout` dispositions.

Protocol errors, missing work reports, malformed timing output, command-level
failure that prevents a closed report set, identity mismatch, or unknown
disposition make all affected core observations unavailable. Fatal errors and
timeouts themselves are available measurements and normally yield predicate
failure; they are not transformed into unavailability.

The shared blast radius is intentional: an unknown protocol status or
incomplete repetition means the core instrument did not establish a closed
execution set, so none of its three projections is trusted. It cannot retain a
favorable wall time while discarding an unclassifiable work outcome.

The wall-time observation is host-sensitive but does not reuse P3's memory
attempt as if both came from one OS execution. Both captures bind the same
candidate identity. Exact elapsed values are volatile captured evidence and are
not byte-identical drift fixtures; only the pure analyzer's synthetic fixture is
drift-tested.

## Capture topology

P5 uses one coherent campaign, not necessarily one process invocation.
Measurement lanes may execute separately when their authorities require it:

1. core `ab-check` batch for fatal failures, wall time, and timeouts;
2. source capture, classified-source ledger, recognition, and diagnostic-gap
   authorization for R1/R2;
3. diagnostic completeness and Parser-IR conformance;
4. publication materialization and structure validation; and
5. serial transient-service cgroup memory capture.

Each lane consumes the same authenticated candidate descriptor and closed
corpus membership. Lane-specific indexes cannot discover corpus members from
output directories. P5 composes only completed, authenticated aggregates whose
full qualification identity reference equals the campaign target.

This is deliberate parallel evidence, not duplicated authority. A concern has
one owner:

- parser execution records what happened;
- instrument analyzers derive their own measurements;
- the P5 composer authenticates membership and identity, then assembles
  envelopes;
- the gate evaluates predicates and registry admission; and
- ADR governance changes release authority.

## Capture transaction on hinoki

The operational sequence is:

1. commit every implementation, predicate metadata, policy, schema, and plan
   change;
2. select that implementation commit as the candidate;
3. create a detached, clean worktree at the candidate on hinoki;
4. perform two independent clean builds, require byte-identical outputs, and
   verify baked coordinates;
5. generate and commit the candidate descriptor and its sole capture
   authorization from repository values and observed build identities;
6. run each capture lane into a fresh staging root and external blob store;
7. authenticate every blob and derive every aggregate without re-executing the
   parser;
8. compose the measurement bundle and publish an immutable capture generation;
9. run the independent full-corpus conversion audit and produce the admission
   candidate;
10. generate and publish a pre-admission evaluation generation; and
11. resolve registry admission and publish a new evaluation generation from the
    unchanged committed capture.

The detached candidate worktree remains unchanged. Evidence is copied by
content identity into a separate clean evidence worktree based on current main.
No capture command writes into the candidate source tree or commits from
hinoki's mutable staging directory.

P5 has no authoritative retry for one candidate. Once the first volatile lane
starts under its committed authorization, an interruption, partial result,
failed predicate, unavailable instrument, or host disturbance resolves that
candidate's one capture honestly. Diagnostic bytes may remain outside source,
but the capture index records the unavailable result; it is not silently
discarded. Another production-shaped attempt requires a new implementation
candidate, authorization, and baked revision. P5 never merges records from
attempts.

## Admission transaction

The conversion audit runs against the same candidate executable, mapping,
schema, and adapter coordinates as the qualification identity, but over the
full ADR-0023 admission corpus. It emits one candidate registry generation.

Before registry mutation, P5 runs `admission-report` against the committed
registry:

- `admitted`: a byte-equal entry already exists; no registry edit is needed;
- `missing`: append the exact candidate entry as a new registry entry, then run
  admission again and require `admitted`;
- `conflict`: stop promotion, commit the conflict report, and do not overwrite
  either entry; or
- invalid registry/candidate: stop and report unavailable governance input.

A conflict has the live `abc.tools.aat-parser-ir-compat` meaning: a registry row
and candidate have equal values for all nine `match-keys`, but their complete
entries differ, including evidence scope or compatibility evidence. This is not
an admitted result.

The two relations remain named and separate, but conflict has precedence. The
gate owns both derivations: it projects the qualification identity through
`admission-query` and calls `compatible?` for membership, then calls
`admission-report` with the authenticated audit candidate for full-evidence
resolution. It reports `admitted` only when membership is true and the strict
report is `:admitted`. A nine-field match plus full-entry disagreement is
`conflict`; it makes the coherence/admission precondition false even though
`compatible?` alone is true. Missing is `unadmitted`; malformed candidate or
registry input is `invalid`.

This requires the live gate report contract to accept the authenticated
admission candidate value and expose the closed admission statuses `admitted`,
`unadmitted`, `conflict`, and `invalid`. It does not accept a caller-supplied
admission boolean. The gate recomputes both relations from registry and
candidate values.

Registry updates are append-only. Existing rows and their evidence scopes are
never edited to make the candidate match. Because the registry is an ADR
evidence-closure input, every append is followed by real evidence recapture on
hinoki and `just validate-migration`.

The current affected closure is explicitly budgeted, not left for execution to
discover. It includes the four observation catalogs that name the registry and
the eight current focused runs: ADR-0009 C5 conversion compatibility; the base,
parser-publication, schema/RDF/TEI, and temporal-person design-bundle runs;
parser-import-boundary; parser-mapping-admission; and
parser-phase5-frozen-tuple. The pre-promotion governance snapshot is refreshed
when its generated closure includes the registry. The implementation plan must
derive this affected set from the live catalogs before recapture and fail on
either an omitted live read or an unexpected new one.

## Bundle composition and gate evaluation

The P5 composer is pure after authenticated reads. It accepts the candidate
descriptor, capture index, and instrument aggregates. It rejects:

- any scalar observation not carried by an identity-bearing envelope;
- any observation whose identity reference differs from the candidate;
- incomplete or extra predicate membership;
- mismatched corpus generation, source identity, instrument version, policy,
  schema, census, or host capability where the owning instrument requires it;
- overwrite of an already installed observation;
- an aggregate that reports unavailable while carrying trusted measurement
  totals; and
- a canonical report path whose bytes differ from its uniquely resolved immutable
  capture or evaluation generation.

The composer produces exactly the nine observed keys declared by the live
predicate set. It does not derive admission or verdicts.

`abc.tools.parser-release-qualification` remains the sole verdict owner. It
recomputes corpus and predicate hashes, validates every envelope, checks full
observation coherence, projects the nine admission keys, queries the committed
registry, resolves full-evidence conflict against the authenticated audit
candidate, evaluates exact comparators, and derives `gate_status` and
`adr_0039_status`.

An authenticated value that misses a threshold is `fail`. Trust, identity,
membership, or availability failure is `unavailable`. Neither is patched into a
pass.

## Governance ordering

P5 uses distinct reviewable commits:

Decision content and ADR status are different time coordinates. Both ADR 0040's
process-tree-memory content and the instrument-binding ADR's content are
committed before freeze, and those content bytes determine the predicate-set
hash. No predicate identity field depends on an ADR status string. The
instrument-binding ADR can be Accepted before freeze because its criteria are
bounded structural/derivation evidence. ADR 0040 remains Proposed until after
capture only because ADR-0040-C3 explicitly consumes the fresh all-nine-envelope
run. Its later status stamp cannot change the already captured predicate
identity.

1. **Instrument-binding governance.** Land the core-attempt instrument and the
   separately evidenced predicate metadata rotation; accept that ADR after its
   bounded evidence and governance closure pass. All old envelopes remain
   superseded historical evidence.
2. **Candidate freeze.** Select the last implementation commit. No code, schema,
   policy, predicate, corpus, or instrument change may occur for that candidate.
3. **Evidence publication.** Commit the immutable capture generation and an
   honest pre-admission evaluation generation. ADR 0039 remains Proposed.
4. **Admission.** Append a missing exact registry row, recapture its governance
   closure, and commit a new admitted evaluation generation. The capture remains
   byte-identical. A conflict is committed without a registry rewrite.
5. **ADR 0040 resolution.** Once all nine envelopes have been freshly captured
   under the post-0040 predicate identity, promote ADR 0040 if its existing
   criteria and governance closure pass. This accepts the process-tree memory
   predicate decision independently of whether the parser stays below 2 GiB.
6. **ADR 0039 promotion.** Only a committed, byte-reproducible report with
   `gate_status = release-qualified`, `adr_0039_status = Accepted`, coherence
   `ok`, admission `admitted`, nine `pass` verdicts, and zero unavailable/fail
   entries permits the ADR status change. At promotion, ADR 0039 depends on the
   accepted ADR 0040 and instrument-binding ADR because its live predicate set
   incorporates both decisions.

The promotion verifier reads committed artifacts and exits nonzero for every
other state. It never edits ADR files. The status edit, Accepted date, validation
scope, release authority, dependency, implementation status, and evidence paths
remain an ordinary reviewed patch followed by governance recapture.

If the report is not release-qualified, P5 stops after publishing the evidence
and names the blockers. ADR 0039 stays Proposed and development-only authority
continues under ADR 0038.

## Failure and recovery model

| Condition | Recorded outcome | Allowed next action |
|---|---|---|
| Candidate binary identity mismatch or non-reproducible rebuild | campaign unavailable | re-resolve preserved bytes; otherwise fix the build and select a new candidate |
| Missing/mismatched blob or replica | owning observation unavailable | repair replica publication and re-authenticate; never edit the digest |
| Predicate value outside threshold | available `fail` | publish result and inspect attempt context; any new measurement requires a new candidate |
| Authorized capture interrupted or host lock lost | unavailable capture | publish the outcome; any new measurement requires a new candidate |
| Unknown/malformed instrument status | unavailable | fix instrument under a new candidate |
| Admission missing | precondition unadmitted | append exact audited row, recapture governance, re-evaluate |
| Admission conflict | conflict report | investigate; never rewrite either row |
| Governance recapture failure | promotion blocked | fix governance evidence without changing candidate measurements |
| Gate not qualified | ADR 0039 Proposed | retain evidence and blocker |
| Gate qualified | promotion eligible | review and commit ADR transition |

Rollback never mutates a published generation. Operational rollback selects a
prior generation for inspection; release-authority rollback uses a separately
governed ADR transition. External unreferenced blobs may be garbage-collected
under the P0 retention policy only after no committed manifest names them and
the two-replica retention rule authorizes deletion.

## Trust and composition review

### Values and places

Candidate descriptors, manifests, aggregates, admission reports, capture
generations, evaluation generations, and gate reports are immutable values. Git
branches, hinoki staging roots, the external store root, canonical report paths,
and the compatibility registry are places.
P5 never treats a place name as evidence identity. Stable paths either resolve a
content identity or are verified projections of immutable capture/evaluation
generations.

### Time

The parser candidate precedes its evidence commits. The evidence generation
precedes admission. Admission precedes gate qualification. Qualification
precedes ADR promotion. These are causal relations, not one mutable status
field, and each is reconstructible from committed values.

### Authority

- instrument analyzers own measurement meaning;
- the campaign composer owns closed composition only;
- ADR 0023's registry owns compatibility admission;
- the qualification gate owns release verdicts; and
- ADR governance owns release authority.

No component is allowed to assert a downstream authority's result as an input
boolean. In particular, the bundle does not carry
`admitted_tuple_matches = true`, and the promotion patch does not carry a
hand-authored gate override.

### Falsifiers

The design is wrong and must be reopened if:

- a candidate cannot be built and its baked coordinates verified without using
  mutable evidence-tree state;
- two independent clean builds of the candidate do not produce byte-identical
  executable outputs;
- any predicate requires combining records from different candidate identities;
- promotion can choose among two volatile captures for one candidate rather
  than rejecting the candidate;
- a full, honest failed run cannot be committed without also mutating an ADR;
- the composer must execute a parser, discover corpus membership, or decide
  admission;
- registry admission cannot be separated from full-evidence conflict checking;
  or
- promotion eligibility cannot be proven from committed artifacts alone; or
- a manifest-referenced blob cannot be re-hashed from two independent failure
  domains.

## Verification strategy

Implementation must provide:

- closed-schema and hash tests for candidate descriptors, capture/evaluation
  indexes, core attempt records, aggregates, and promotion inputs;
- authorization tests proving exactly one authoritative capture per candidate,
  rejecting a sibling capture, and selecting the unique current-registry
  evaluation without a caller-provided path;
- fixed-repetition tests requiring exactly three complete core repetitions and
  deriving the maximum fatal, timeout, and wall-time values independent of
  record order;
- two-build reproducibility and two-replica mutation tests;
- a test that evidence commit HEAD may differ from `parser_git_rev` while every
  observation remains coherent with the candidate;
- executable-provenance mutation tests for Git revision, Nix output, binary
  digest, self-reported adapter version, and argv;
- core-attempt tests covering parsed, fatal, timeout, protocol-error, missing,
  duplicate, extra, and malformed timing records;
- an evaluator-facing test proving all three core observations use the numeric
  types expected by the live comparators;
- composition tests for exact nine-key membership, cross-candidate substitution,
  policy/schema/corpus mismatch, scalar bypass, and overwrite;
- admission tests for already admitted, missing then appended, conflict, invalid
  candidate, and invalid registry;
- a test proving `compatible?` uses the nine-key projection while
  `admission-report` requires full evidence equality, plus an end-to-end gate
  test proving a matching projection with conflicting evidence is not qualified;
- capture/evaluation generation tests proving that registry changes produce a
  new evaluation without changing the measurement capture;
- canonical-projection drift tests for measurements and qualification report;
- promotion-verifier tests that reject unadmitted, incoherent, unavailable,
  failing, extra/missing predicate, non-reproducible, and stale report inputs;
- a positive promotion fixture with exactly nine passes and admitted coherence;
- candidate-tree cleanliness and no-machine-local-path checks;
- an external-store rebinding verification on hinoki; and
- root `just validate-migration`, language-specific checks, comment hygiene,
  schema validation, and every required ADR evidence recapture.

Volatile production timings and memory values are not golden drift fixtures.
Their records are content-addressed captured facts. Deterministic analyzers and
bundle/report projections regenerate byte-identically from fixed inputs.

## Delivery sequence

1. Correct the roadmap's P3 status.
2. Implement and fixture-test the core-attempt Capture -> Derive instrument.
3. Govern the final predicate instrument bindings and rotate the predicate-set
   identity before candidate selection.
4. Implement the candidate descriptor, executable-provenance verifier,
   capture/evaluation indexes, composer, canonical projections, and promotion
   verifier.
5. Run all bounded tests and repository validation.
6. Freeze the final implementation candidate, prove two-build reproducibility,
   and commit its one capture authorization.
7. Execute the fixed authoritative capture and full-corpus audit on hinoki.
8. Verify two independent blob replicas, publish the immutable capture and
   evaluation generations, resolve admission,
   and recapture governance.
9. Regenerate and drift-check the report.
10. Resolve ADR 0040, then conditionally promote ADR 0039.

Steps 6-10 are a separate execution phase after implementation review. Any code,
schema, policy, corpus, predicate, or instrument change after step 6 selects a
new candidate and invalidates unfinished authoritative capture. Documentation or
governance commits may advance repository HEAD without changing the candidate,
provided they do not alter any candidate input or executable closure.

## Non-goals

- Guaranteeing that the custom parser passes.
- Weakening or rounding a predicate.
- Reusing the superseded pre-ADR-0040 measurement bundle.
- Treating fixture captures as release evidence.
- Treating a Git label, branch, path, or host name as content identity.
- Hand-keying observations, admission, or gate status.
- Rewriting a compatibility registry row.
- Promoting ADR 0039 in the same operation that captures measurements.
- Retrying or selecting among volatile captures for one candidate.
- Making Track S or third-party parser availability gate custom-parser release.
- Retiring third-party parsers before their comprehensive report/publication.

## Acceptance criteria

P5 is complete, whether or not ADR 0039 promotes, when:

1. one immutable custom-parser candidate is identified independently of later
   evidence and governance commit HEADs, and two clean builds reproduce its
   executable bytes;
2. the live predicate contract truthfully names every committed instrument and
   its final hash was fixed before capture;
3. all nine observations are generated from the one pre-authorized capture for
   exactly one qualification identity, with no hand-authored scalar or
   operator-selected sibling generation;
4. every predicate is `pass` or `fail`, or any remaining `unavailable` has a
   committed, machine-derived reason;
5. the full-corpus compatibility audit resolves to admitted, conflict, missing,
   or invalid without rewriting history, and full-evidence conflict overrides a
   matching nine-field membership projection;
6. the gate derives coherence and admission from committed authorities;
7. the immutable capture/evaluation generations and canonical projections
   regenerate byte-identically, and every manifest-referenced blob is re-hashed
   from two independent failure domains;
8. the instrument-binding ADR is Accepted before candidate freeze, and ADR 0040
   is resolved before ADR 0039 can depend on their predicate decisions;
9. ADR 0039 is Accepted if and only if the committed gate is release-qualified;
10. an honest non-qualifying result remains published with ADR 0039 Proposed;
    and
11. `just validate-migration` and all focused checks exit zero after the final
    governance state.
