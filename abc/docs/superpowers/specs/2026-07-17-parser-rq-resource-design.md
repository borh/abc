# P3 Parser Resource Qualification Design

**Date:** 2026-07-17  
**Status:** Approved direction; design complete  
**Parent:** `2026-07-15-parser-release-qualification-campaign-design.md`  
**Roadmap:** `../plans/2026-07-15-parser-release-qualification-campaign.md`

## Purpose

Implement Track R's R4 instrument so predicate 8 measures the peak cgroup-v2
memory charge required to produce Parser-IR for every pinned qualification
work. The fixed threshold remains:

```text
peak_cgroup_memory_bytes <= 2147483648
```

The measured subject is the complete per-work production process tree: parser,
adapter, conversion process, and their descendants. The quantity includes the
memory controller's anonymous-memory, page-cache, kernel-structure, and socket
charges. It is deliberately not called RSS. Measuring only the custom parser
executable would answer a different and weaker question.

The current predicate key `peak_rss_bytes` is technically false for this
process-tree authority. P3 changes the memory predicate to
`peak_cgroup_memory_bytes`, updates the dimension and instrument, and recomputes
the predicate-set hash before producing any authoritative observation. This is
a semantic predicate change, not a local instrument rename: it changes the
qualification identity and invalidates every current observation envelope.

ADR 0039 requires a separately evidenced ADR for a predicate change. P3
therefore starts with a new predicate-amendment ADR and its governance evidence.
That ADR records the unchanged 2 GiB threshold, the stronger process-tree
cgroup-memory quantity, and the required full-bundle recapture. P5 then
recaptures all nine observation envelopes under the new predicate-set identity;
no old envelope is rebased or silently reinterpreted.

P3 proves the instrument with bounded fixtures and lands the separately
evidenced predicate amendment. It does not perform P5's authoritative
full-corpus capture, modify the compatibility registry, or promote ADR 0039.

## Decision

Run each work as a separate transient systemd user service with cgroup-v2
memory accounting. A small measurement wrapper executes the production command
inside that service and reads the service cgroup's exact `memory.peak` value
before the wrapper exits.

The service is created with:

- `MemoryAccounting=yes`;
- `MemoryMax=3221225472` (3 GiB);
- `MemorySwapMax=0`;
- `OOMPolicy=continue`;
- `Delegate=no`;
- one unique, non-identity-bearing unit name per attempt; and
- no `--scope`, because a transient service provides a stable process-tree
  owner and service result.

The 3 GiB safety ceiling is deliberately above the 2 GiB qualification
threshold. It cannot turn a qualifying run into a failure. If a descendant is
killed at the ceiling while the wrapper survives, the wrapper still reads
`memory.peak`; the work is an available failure because the measured value is
already above 2 GiB. If accounting or the wrapper itself is lost, the work is
unavailable rather than assigned a fabricated number.

Swap is prohibited for the service. Without `MemorySwapMax=0`, anonymous demand
could be displaced to swap and make the resident cgroup charge pass under host
pressure. Host swap configuration remains disclosed context, but it cannot
alter the work's qualification value.

This mechanism was checked with a disposable Hinoki probe on systemd 261 and
cgroup v2. A transient user service reported a cgroup memory peak for an
allocated child process. That probe is design evidence, not a committed
qualification observation.

## Why cgroup accounting

### Rejected: GNU `time -v`

The repository already parses `Maximum resident set size (kbytes)` in
`measure-parser-performance.py`. That is useful performance-study evidence but
is not the P3 authority. `ru_maxrss` does not establish the simultaneous
aggregate peak of a parent and multiple live descendants. It could therefore
pass a process tree whose combined resident memory exceeded the original
predicate.

### Rejected: `getrusage`

Embedding `getrusage` in the adapter has the same boundary problem and moves a
host measurement concern into production parser code. It also cannot account
for descendants that are not waited for by the instrumented process.

### Rejected: manually managed cgroups

Directly creating cgroups and writing controller files would provide the same
kernel counter, but would duplicate systemd's delegation, lifecycle, cleanup,
and failure handling. P3 uses systemd only as the cgroup lifecycle owner; the
authoritative number still comes from the kernel's `memory.peak` file.

## Measurement semantics

### Quantity

For work `w`, `peak_cgroup_memory_bytes(w)` is the maximum value of the cgroup-v2
`memory.current` counter observed by the kernel during the lifetime of the
transient service, exposed through that cgroup's `memory.peak` file. It includes
the measurement wrapper and every descendant in the production command.
These semantics follow the Linux kernel's authoritative cgroup-v2 memory
controller documentation:
`https://docs.kernel.org/admin-guide/cgroup-v2.html#memory-interface-files`.

The wrapper's own fixed overhead is intentionally included. The release
question is operational capacity for the real per-work path, not an estimate of
parser heap usage or POSIX per-process RSS.

Swap is not added to `peak_cgroup_memory_bytes`; cgroup `memory.current` and
`memory.peak` are total memory-controller charges excluding the separately
reported swap counter. `memory.swap.peak`, when available, is recorded as a
disclosed supporting field and never subtracted from or added to the predicate
value.

### Per-work isolation

Exactly one expected work runs in each transient service. Qualification works
are measured serially. Parallel execution would make host contention and shared
cache pressure part of an individual work's value and would make OOM attribution
ambiguous.

The work command is the same pinned parser-to-Parser-IR command contract used by
the capture campaign. P3 may parameterize input and output paths, but must not
replace the production adapter with a memory-specific implementation.

### Process-tree closure

After the command's direct child exits, the wrapper waits for the service cgroup
to contain only the wrapper itself. The wait is bounded. If another descendant
remains, the work is unavailable with `lingering_descendant`; P3 does not read a
prematurely low peak and call the work measured.

`Delegate=no` prevents the service from receiving delegated cgroup-controller
ownership. Ordinary forks, double-forks, and daemonization remain in the
service cgroup. P3 does not claim that a deliberately hostile process with
access to the user manager could never request a different unit. The measured
command is a pinned, project-owned release candidate, and deliberate cgroup
migration is outside the trusted-command contract. The wrapper detects
lingering descendants; it does not claim to detect an already migrated process.

The wrapper then reads `memory.peak`, optionally reads `memory.swap.peak`, writes
its record atomically, and exits. systemd collects the unit afterward. Unit
names and cgroup paths are runtime locators, not evidence identity.

## Components and boundaries

### 1. Cgroup measurement wrapper — ab-validator

`ab-validator/reports/parser-ir/parser-rq-resource-wrapper.py` owns only the
inside-service measurement boundary:

- verify cgroup v2 and a readable `memory.peak` for its own cgroup;
- execute the exact command after `--` without a shell;
- preserve the child's exit code and termination signal;
- wait for descendant closure;
- read exact integer byte counters;
- emit one raw attempt record atomically.

It does not discover corpus membership, compare against 2 GiB, calculate an
aggregate, or know whether the candidate is admitted.

### 2. Resource capture orchestrator — ab-validator

`ab-validator/reports/parser-ir/parser-rq-resource-capture.py` owns:

- exact membership from the pinned capture input, never filesystem discovery;
- serial creation of one transient user service per work;
- the fixed systemd properties above;
- timeout and service-result collection;
- host-coordinate capture;
- one closed work record per expected work; and
- a closed index plus capture manifest stored through P0's content-addressed
  store contract.

All P3 records are small witnesses and remain in the committed tier. P3 uses
P0's manifest and logical-blob contracts for authentication but does not create
corpus-scale external blobs or exercise locator rebinding.

The orchestrator never derives the gate verdict. A nonzero parser exit,
timeout, or memory-limit event remains evidence in a per-work record.

### 3. Pure resource analyzer — ABC

`abc.tools.parser-rq-resource` authenticates the manifest, index, work records,
candidate identity, instrument policy, and host identity before deriving an
observation envelope. It does not execute systemd or reopen unauthenticated
paths after capture.

It produces:

```clojure
{:value <corpus maximum peak cgroup memory in bytes>
 :identity_ref <qualification identity ref>
 :counts {:expected N :measured N :within_limit M :over_limit K}
 :max_work_id <work id>
 :right_censored_work_ids [...]
 :limit_exceeded_work_ids [...]}
```

Any unavailable work makes the aggregate unavailable. A measured value above
2 GiB remains available and produces a normal predicate failure; a capped value
is also listed in `right_censored_work_ids`.

## Data contracts

P3 adds four closed JSON Schemas:

- `abc/schemas/parser-rq-resource-policy.schema.json`;
- `abc/schemas/parser-rq-resource-work.schema.json`;
- `abc/schemas/parser-rq-resource-index.schema.json`; and
- `abc/schemas/parser-rq-resource-aggregate.schema.json`.

Every schema has `additionalProperties: false`. Every identity-bearing policy
or aggregate uses a projected JCS hash in the same manner as P1 and P2.

### Policy

The committed policy pins:

- `instrument_id = "abc/parser-rq-resource/systemd-cgroup-v1"`;
- qualification threshold `2147483648` bytes;
- safety ceiling `3221225472` bytes;
- serial execution;
- cgroup counter `memory.peak`;
- `MemoryAccounting=yes`, `MemorySwapMax=0`, `OOMPolicy=continue`, and
  `Delegate=no`;
- descendant-closure timeout;
- wrapper semantic-closure identity; and
- the required host-coordinate field names.

Changing any of these values creates a new policy identity. Runtime unit names,
temporary directories, and cgroup paths are excluded.

The wrapper semantic-closure identity is not a hand-maintained version string.
It is the JCS hash of a generated, reviewed manifest containing:

- the wrapper and every repository-local transitive Python import discovered
  from its AST import graph;
- the resolved Python executable's SHA-256;
- `sys.implementation.name`, `sys.implementation.cache_tag`, and the exact
  Python version; and
- the Nix derivation identity that supplies the wrapper runtime.

A guard test derives the import closure and requires exact equality with the
committed manifest, failing on additions and removals. Capture authenticates
the manifest hash before starting a service. This binds both behavior and the
Python overhead included in the measured cgroup.

### Work record

Every expected work has exactly one record containing:

- `work_id` and `source_sha256`;
- `qualification_identity_ref`;
- policy, candidate, corpus-list, and corpus-snapshot identities;
- `measurement_status`;
- exact `peak_cgroup_memory_bytes` when measured;
- optional exact `peak_swap_bytes` when the kernel exposes it;
- child exit code or signal;
- systemd service result and OOM indication;
- host identity reference; and
- bounded failure details for unavailable outcomes.

Closed statuses are:

- `measured`: exact `peak_cgroup_memory_bytes` is present, with
  `measurement_kind` equal to `exact` or `right_censored`;
- `timeout`: no complete measurement, aggregate unavailable;
- `command_failed`: the production command failed before producing its normal
  result, aggregate unavailable;
- `accounting_unavailable`: counter absent, unreadable, or non-integer;
- `wrapper_lost`: the service killed the wrapper or no atomic record exists;
- `lingering_descendant`: process-tree closure was not established;
  and
- `identity_conflict`: work or candidate coordinates disagree.

A child killed by `MemoryMax` is `measured` only when the surviving wrapper
records a peak above the qualification threshold and the service reports the
memory-limit event. Its `measurement_kind` is `right_censored`: the recorded
counter is exact for the capped execution, but is only a lower bound on the
uncapped workload's demand. This is sufficient for a fail verdict and must not
be presented as an uncapped peak. Otherwise the status is `wrapper_lost`.

### Index and aggregate

The index declares exact expected work IDs and exactly one logical blob
reference per work. Extras, omissions, duplicates, locator/hash mismatches, or
source-hash mismatches make derivation unavailable.

The aggregate maximum is computed from integer bytes, not displayed units. The
human report may render MiB/GiB, but display rounding never reaches the gate.

## Host identity

Resource values are host-sensitive. Capture records a committed host descriptor
and its JCS identity containing:

- stable host label `hinoki.hyakutake-barbel.ts.net`;
- machine ID hash, not the raw machine ID;
- system architecture;
- cgroup-v2 filesystem and memory-controller availability;
- readable `memory.peak`, `memory.swap.peak`, and `memory.events` interfaces;
- support for the five pinned transient-service properties;
- total physical memory;
- CPU model identity.

Exact kernel release, systemd version, NixOS system revision, swap
configuration, boot ID, uptime, current load, free memory, unit name, and
timestamps are disclosed attempt context, not host identity. The identity pins
the semantic capabilities P3 consumes without making routine security patching
an automatic identity break. A missing or changed required capability is still
an identity mismatch.

P5 must run on a host descriptor whose identity equals the one pinned for the
authoritative campaign. A host mismatch is unavailable, not a comparable
measurement.

## Capture, derive, drift

P3 follows the campaign boundary exactly:

1. Capture runs the production command and stores raw work records, index,
   policy, host descriptor, and manifest as immutable blobs.
2. Derive authenticates those blobs and computes the corpus maximum without
   executing a parser or consulting mutable filesystem state.
3. Drift regenerates a pure bounded analyzer fixture and compares canonical
   output byte-for-byte.

The separate systemd/cgroup integration smoke contains at least two concurrent
allocating child processes. Its assertion requires the cgroup peak to exceed
either child's individual allocation by a tolerance that covers wrapper/runtime
overhead. A parent-only or max-of-children implementation therefore fails the
test.

Because exact allocator behavior varies, the integration smoke tests an
inequality, not a byte-for-byte memory value. Its volatile measured values are
never described as drift-tested constants.

Pure analyzer fixtures use fixed synthetic byte counts and are byte-identical.

P3 memory capture and predicate 7's batch wall-time capture are separate
executions. They remain coherent because both bind the same candidate, corpus,
predicate-set, and instrument-version tuple. The parent campaign's “one
capture” goal means one authenticated campaign generation, not one operating
system process invocation for every instrument.

## Availability and verdict rules

The aggregate is available only when:

- expected membership is nonempty and exact;
- every work has one authenticated record;
- every record has status `measured`;
- all work, candidate, corpus, policy, host, and instrument identities cohere;
- every peak is a nonnegative integer byte value; and
- the index and manifest close over the same logical blobs.

When available, `value` is the maximum exact `peak_cgroup_memory_bytes`. The
existing predicate evaluator compares it numerically with `2147483648`:

- maximum at or below 2 GiB: pass;
- maximum above 2 GiB: fail, with the maximum work and all over-limit works as
  witnesses; and
- any unavailable condition: unavailable.

No averaging, percentile, sampling, or successful-work denominator is allowed.
One over-limit work fails the predicate; one missing work makes it unavailable.

## Failure handling

- systemd user manager unavailable: stop before capture; no partial aggregate;
- cgroup v1 or absent `memory.peak`: unavailable;
- localized systemd output: irrelevant, because the authoritative counter is
  read as an integer from cgroupfs;
- timeout: terminate the transient service, retain bounded diagnostics, mark
  the work unavailable;
- service OOM with surviving record: accept only an over-threshold capped peak,
  label it `right_censored`, and require an explicit memory-limit result;
- service OOM without record: unavailable;
- stale transient unit: fail before reuse; unit names are unique and collected;
- host mutation: host-identity mismatch, unavailable;
- command mutation: policy or wrapper semantic hash mismatch, unavailable.

## Verification strategy

The implementation plan must include:

1. wrapper unit tests for cgroup-path parsing, integer counters, atomic records,
   exit/signal preservation, and lingering descendants;
2. a Linux-only systemd/cgroup integration smoke using two concurrent child
   allocations;
3. capture tests for exact corpus membership, serial services, timeout, OOM,
   absent record, and host-coordinate closure;
4. closed-schema positive and adversarial fixtures;
5. pure analyzer tests for exact maximum, equality at 2 GiB, one-byte failure,
   unavailable propagation, identity mutation, extras, omissions, duplicates,
   and order independence;
6. observation-envelope installation tests proving the gate receives an
   envelope rather than a scalar;
7. a byte-identical pure Capture→Derive→Drift fixture;
8. predicate-amendment ADR evidence plus tests proving the predicate-set hash,
   qualification identity, and all old envelopes rotate together; and
9. the repository's Python, Clojure, Nix, comment-hygiene, and full migration
   gates.

## Scope exclusions

P3 does not:

- measure latency or throughput;
- compare third-party parsers;
- tune parser memory;
- change the 2 GiB threshold after the predicate amendment fixes its meaning;
- perform the authoritative corpus capture;
- admit a registry row;
- promote ADR 0039; or
- make resource qualification depend on publication or admission verdicts.

Those remain respectively Track S, later optimization work, or P5 concerns.

## Consequences

The instrument is Linux, cgroup-v2, and systemd dependent. That is intentional:
predicate 8 is a host-pinned operational claim, not a portable microbenchmark.
Development platforms without this boundary can run pure tests but cannot
produce authoritative P3 evidence.

The measurement includes small wrapper overhead and cold/warm host effects.
Serial execution, exact host identity, attempt context, and P5 recapture make
those effects visible without pretending they can be eliminated.
