# Parser Release Qualification: Portable Evidence Integrity

**Date:** 2026-07-18

**Status:** Proposed

## Purpose

Remove capture-site identity and backup topology from parser release
qualification. Qualification must authenticate what was measured, not prescribe
where the measurement ran or how the evidence store is backed up.

The current execution-readiness implementation crosses that boundary by
committing the hinoki hostname, DNS label, filesystem allowlist, failure-domain
names, and prospective replica authority as release policy. It also makes a
two-domain replication receipt a promotion prerequisite. Those checks attempt
to implement storage durability inside qualification while making the campaign
less portable.

This correction keeps the parts that protect measurement integrity and deletes
the infrastructure policy.

## Decision

Parser release qualification owns exactly three storage concerns:

1. captured artifacts are named by logical content identity;
2. every referenced artifact can be read from the configured evidence store;
3. the bytes are streamed and re-hashed before promotion.

Storage reliability, backup scheduling, restore testing, media choice, mount
topology, and failure-domain placement are infrastructure responsibilities.
They do not enter parser qualification policy, candidate identity, predicate
identity, admission, or promotion.

The qualification transaction retains a readiness receipt. The receipt binds
the candidate, qualification identity, executable provenance, production graph,
corpus identity, clean source revisions, and the minimal volatile prerequisites
for the authorized run. Successful sealing attests that those prerequisites were
checked; the receipt does not carry or indirectly bind the runtime descriptor,
machine identity, paths, or storage implementation.

## Decision Record

This decision reverses the site-policy and two-domain replication portions of
the 2026-07-18 execution-readiness design. It also removes the unprovisioned
replica as a parser-qualification blocker. That is a consequence, not the
motivation: even if the replica existed, hostname and storage topology would
still describe infrastructure places rather than parser evidence.

Because this changes a promotion protocol, implementation records the decision
in ADR 0042 before rotating schemas or fixtures. The ADR distinguishes content
integrity, which promotion enforces, from evidence retention, which operations
owns.

## Open Operational Dependency: Evidence Retention

The repository currently has no named operational owner or runbook for backing
up parser-RQ evidence. Removing the replication gate must not silently claim
that this responsibility is already discharged.

Implementation adds one short operator-owned record and runbook at
`docs/reports/parser-rq-evidence-retention.md`. Before the authoritative
campaign, the repository operator must record that the configured evidence
store is covered by the established reliable-storage and external-backup
process, name the applicable operational procedure, and confirm a current
restore test. Until that record exists, the authoritative campaign is
operationally blocked even though the portable qualification code may merge.

This record is deliberately not a qualification artifact. It is not hashed into
the candidate, readiness receipt, capture, evaluation, admission, or promotion;
the parser-RQ application does not inspect backup systems. Its purpose is to
give retention a real owner and an explicit pre-run stop rather than pretending
that a qualification check can prove infrastructure durability.

The stop is deliberately process-enforced by the named operator, not encoded as
another parser-RQ status document. A program could validate that a JSON field
says `restore_test: passed`, but it could not thereby verify the external backup
or restore. Treating that self-attestation as a fail-closed machine fact would
recreate the removed `replica_status` fake seam under a new name. Qualification
therefore verifies stored bytes; the operator owns the independent decision not
to mint authorization until the runbook is complete.

## Boundary

### Reusable qualification facts

These may affect release qualification:

- candidate and qualification identity;
- predicate and instrument policy;
- corpus snapshot and closed work membership;
- executable provenance and reproducible-build result;
- production graph identity;
- capture and evaluation generations;
- logical blob identity: SHA-256, byte count, and media type;
- successful streaming re-hash of every referenced blob;
- clock synchronization when evaluating an authorization interval;
- exclusive campaign-lock acquisition;
- required measurement capabilities, such as the resource instrument's cgroup
  contract.

Measurement-specific capabilities are checked by the instrument that gives
them meaning. In particular, the resource instrument owns its cgroup contract.
This does not create a generic host-capability profile or a second committed
site-policy object.

### Runtime places and attempt context

These must not affect the meaning or admissibility of a measurement:

- hostname or FQDN;
- DNS and interface addresses;
- absolute corpus, scratch, lock, or store paths;
- filesystem type, source, device, or filesystem ID;
- mount or remote authority;
- primary or replica failure-domain names;
- backup provider or backup topology.

Runtime paths remain explicit local configuration. Diagnostic attempt context
may disclose non-sensitive host facts in logs, but those facts are not required
schema fields, policy inputs, comparator inputs, or promotion conditions.

## Values and Flow

### Runtime configuration

One untracked runtime descriptor supplies only the places needed to execute:

- `corpus_root`;
- `evidence_store_root`;
- `scratch_root`;
- `campaign_lock_path`.

The descriptor is schema-validated for shape and safe path use. Its bytes and
paths do not enter candidate or qualification identity. There is no committed
site-policy value.

### Preflight and readiness

Preflight is read-only except for bounded create, fsync, read, and remove probes
inside the configured evidence and scratch roots. It verifies only:

- runtime configuration is valid;
- required roots are usable;
- the campaign lock is available;
- the clock is synchronized;
- independent-build capability passed;
- the production graph and committed candidate inputs authenticate.

Preflight neither resolves a stable hostname nor inspects mount topology. It is
a check-only command: diagnostics go to stdout/stderr, and it writes no report
or authorization input.

Sealing readiness rechecks the volatile prerequisites and emits one immutable
receipt without copying the runtime facts into it. Authorization binds that
receipt. Immediately before capture, the orchestrator rechecks the lock, clock,
and bounded store probe. A preparation failure before the recorded capture start
consumes no authorization attempt.

### Evidence integrity

After capture, a single-purpose command reads the closed capture index, resolves
every logical blob beneath `evidence_store_root`, streams each file, and emits an
evidence-integrity receipt containing:

- candidate and capture-generation references;
- the exact closed blob membership;
- each logical blob identity;
- the observed re-hash and byte count;
- `status: verified` or `status: unavailable` with a reason.

The command never trusts filesystem metadata as evidence of content. A missing,
escaping, unreadable, size-mismatched, or hash-mismatched blob makes integrity
unavailable. It does not inspect or describe backup storage.

Promotion requires a self-authenticating `verified` receipt whose membership is
exactly the manifest-referenced blob set. This replaces the replication receipt.

## Deletions

Delete, rather than generalize, the following concepts:

- committed parser-RQ site policy;
- `replica_status` and the configured/unconfigured transition;
- stable-host and kernel-hostname policy;
- DNS-to-local-interface checks;
- filesystem and mount-source allowlists;
- remote-authority and failure-domain comparison;
- application-level replica copying and verification;
- `host_policy_ref` in capture authorization;
- replication as a promotion precondition.

Do not replace them with a host profile, site class, storage provider interface,
backup attestation, or pluggable policy layer.

## Schema and Protocol Migration

The campaign has no real authorization or authoritative capture generation yet,
so the protocol may rotate without reinterpreting historical release evidence.
Bounded fixtures migrate atomically.

- Delete the site-policy schema and committed value. Reduce the existing
  site-descriptor schema in place to the four runtime paths. There is no
  committed descriptor instance.
- Delete the site-preflight report schema. Remove `site_preflight_report_ref`
  and `site_facts` from the readiness schema so runtime places are not bound
  through an indirect hash.
- Remove `host_policy_ref` from authorization. The existing
  `readiness_receipt_ref` is the sole readiness binding.
- Replace `parser-rq-replication-receipt` with
  `parser-rq-evidence-integrity-receipt`.
- Replace `verify-replicas` with one-store `verify-evidence`.
- Remove replica and site-policy arguments from the orchestrator and runbook.
- Replace the bounded drift fixture's two-copy invariant with the closed
  evidence-integrity-receipt invariant. This is an explicit governance
  re-baseline, not a byte-preserving fixture edit.

Names must describe the remaining responsibility. Retaining replication names
for compatibility would preserve a fake seam and is rejected.

## Authority

- The runtime descriptor chooses places but asserts no release fact.
- Preflight observes whether the current runtime can safely start.
- The readiness receipt authenticates preparation for one candidate and graph.
- Capture producers author observations only.
- The evidence-integrity verifier authenticates stored bytes only.
- The evaluator owns predicate verdicts.
- The registry owns admission.
- The promotion verifier composes these values and owns no backup policy.

No component accepts a caller-supplied boolean for readiness, integrity,
admission, or qualification.

## Scope Fence

This correction changes only runtime-site configuration, readiness bindings,
stored-evidence authentication, and their bounded fixtures and runbook steps.
It does not change:

- executable two-build reproducibility or provenance semantics;
- the production graph's parser operations;
- predicate IDs, dimensions, comparators, thresholds, or predicate-set identity;
- corpus identity or closed work membership;
- observation capture or evaluation semantics;
- admission-query or conflict precedence;
- the one-authorized-capture and canonical-generation rules;
- ADR 0039 or ADR 0040 acceptance criteria except for replacing the storage
  integrity input named by promotion.

## Failure Semantics

- Invalid runtime configuration or failed preflight: preparation failure; no
  capture attempt.
- Lost lock, unsynchronized clock, or unusable store before capture start:
  preparation failure; no capture attempt.
- The same failures after capture start: honest unavailable attempt.
- Missing or mismatched evidence bytes: integrity unavailable; no promotion.
- Missing evidence-retention ownership or restore-test record: operational
  campaign block before authorization; it is not a qualification verdict.
- Backup failure: handled by the named infrastructure owner and procedure,
  outside qualification.

No retry, alternate store, or operator-selected generation is introduced.

## Verification

Tests must prove:

1. candidate and qualification references are unchanged when runtime paths or
   disclosed host context change;
2. no active parser-RQ code, current schema, committed runtime policy, fixture,
   or P5 runbook contains a hinoki requirement, filesystem allowlist, remote
   authority, or failure-domain field;
3. authorization binds readiness and has no host-policy field;
4. preflight rejects an unusable store, unavailable lock, unsynchronized clock,
   or failed build capability;
5. preflight accepts the same valid facts under different hostnames, paths, and
   filesystem implementations;
6. evidence verification rejects missing, escaping, unreadable, truncated,
   extra, or hash-mismatched blobs;
7. evidence verification accepts exactly the closed manifest membership from
   one configured store;
8. promotion requires the verified integrity receipt and does not read a
   replica or site policy;
9. the bounded production-wiring smoke runs from an unrelated working directory;
10. the full migration and governance gates pass after evidence recapture.

Implementation adds a non-goal guard that searches active code, current schemas,
data, fixtures, and the P5 runbook for the removed field and protocol names. It
does not scan historical reports or superseded designs: a historical attempt may
truthfully disclose that it ran on hinoki, and rewriting evidence would violate
the same time-and-identity discipline this decision protects.

## Alternatives

### Keep a simplified two-store receipt

This would require fewer edits but would preserve application-level backup
policy and imply that two configured paths prove independent durability. It is
rejected as a misleading abstraction.

### Replace hinoki with a generic host capability profile

This would improve portability while retaining a committed site-policy layer.
The only necessary capabilities already belong to the resource instrument or
minimal preflight checks. A second policy object adds indirection without a
second present use. It is rejected under YAGNI.

### Keep the current topology checks

This provides detailed operational ceremony but couples release authority to a
specific machine and storage implementation. It is rejected because it models
backup infrastructure as parser semantics.

## Falsifiers

Reopen this decision if:

- a predicate's meaning genuinely depends on a named physical host rather than
  a measured capability;
- qualification must itself provide a contractual durability guarantee not
  supplied by the named storage operator and procedure;
- the evidence store cannot provide stable content-addressed reads through
  runtime configuration;
- removing site identity permits a measurement-affecting capability mismatch
  that no instrument policy or preflight check detects.

Absent a falsifier, the implementation must delete site identity and backup
topology rather than relocate them.

## Acceptance

The correction is complete when parser qualification can run on any host that
satisfies its measurement prerequisites, all release-relevant artifacts are
authenticated by logical content identity, and no qualification or promotion
decision depends on hinoki, a filesystem implementation, or backup topology.
The authoritative campaign additionally remains blocked until the separate
evidence-retention runbook has a named owner, applicable backup procedure, and
current restore-test record.
