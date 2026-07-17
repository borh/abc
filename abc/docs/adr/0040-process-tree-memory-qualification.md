# ADR 0040: Qualify Process-Tree Cgroup Memory

Status: Proposed
Date: 2026-07-17
Depends on: ADR 0002
Validation scope: structural
Release authority: development

## Implementation Status

The bounded instrument, pure analyzer, identity policy, and Hinoki smoke
witness are implemented. The authoritative P5 corpus recapture remains future
work, and ADR 0039 remains Proposed.

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

- **ADR-0040-C1 — structural-invariant:** Predicate 8 measures process-tree cgroup memory with the unchanged 2 GiB threshold, zero swap, and a content-addressed wrapper identity. Evidence boundary: `test/abc/tools/parser_rq_resource_test.clj`.
- **ADR-0040-C2 — operational-behavior:** A Hinoki transient-service smoke run
  records both a measured small workload and a right-censored ceiling-clipped
  workload. Evidence boundary:
  `docs/superpowers/reports/2026-07-17-parser-rq-resource-hinoki-smoke.json`.
- **ADR-0040-C3 — structural-invariant:** The predicate-set rotation does not
  relabel old observations; P5 must recapture all nine envelopes before ADR
  0039 can be promoted.

## Evidence

The focused structural observation in
`docs/evidence/adr-runs/parser-rq-resource-policy.json` binds the live
predicate, committed resource policy, process-tree cgroup-memory key, unchanged
2 GiB threshold, zero-swap service property, and content-addressed wrapper
semantic identity. The Hinoki integration witness is recorded at
`docs/superpowers/reports/2026-07-17-parser-rq-resource-hinoki-smoke.json`.
