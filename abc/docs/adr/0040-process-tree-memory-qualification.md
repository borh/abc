# ADR 0040: Qualify Process-Tree Cgroup Memory

Date: 2026-07-17

Status: Accepted

## Context

Predicate 8 named per-process peak RSS, but the release subject is the complete
parser-to-Parser-IR process tree. Per-process RSS and the simultaneous cgroup-v2
memory charge can yield different verdicts. ADR 0039 requires a separately
evidenced decision when a predicate changes.

## Decision

Predicate 8 measures `peak_cgroup_memory_bytes` for the complete per-work
production process tree. The bound remains `2147483648` bytes. The authority is
the `memory.peak` counter of a transient cgroup-v2 systemd user service with
swap prohibited. Its instrument identifier is `parser-rq-resource-v1`.

This semantic amendment rotates the predicate-set hash and therefore the full
qualification identity. P5 must recapture all nine observation envelopes under
that identity. No old envelope may be relabeled or rebased. Registry admission
is unchanged, and ADR 0039 remains Proposed until the complete gate passes.

The existing
`docs/reports/parser-release-qualification-measurements.edn` and the
ADR-0039-cited `docs/reports/parser-release-qualification-report.json` are
**superseded-pending-P5**. They remain historical evidence for the former
predicate set, are intentionally incoherent with the live predicate set after
this decision, and are forbidden as current gate evidence.

## Consequences

P3 may prove the bounded instrument and its derivation. It does not perform the
authoritative corpus capture, alter compatibility admission, or promote ADR
0039. A missing or unauthenticated cgroup measurement remains unavailable.

## Acceptance Criteria

- **ADR-0040-C1 — structural-invariant:** The live predicate, committed
  resource policy, and generated wrapper identity jointly bind the
  process-tree cgroup-memory key, unchanged 2 GiB threshold, zero-swap service
  property, and exact wrapper semantic identity. Evidence boundary:
  `abc.tools.parser-rq-resource-test/process-tree-memory-policy-contract`.
