# ADR 0039: Custom Parser Release Qualification

Status: Proposed
Date: 2026-07-15
Validation scope: smoke-corpus
Release authority: development
Depends on: ADR 0002, ADR 0023, ADR 0030, ADR 0038
Amended by: ADR 0040 [scope: predicate 8 process-tree cgroup-memory dimension]

## Implementation Status

Proposed. The release-qualification gate machinery is committed and executable:
a pinned qualification corpus (`data/parser-release-qualification-corpus.edn`),
a predeclared predicate set with exact thresholds fixed before any run
(`data/parser-release-qualification-predicates.edn`), and a
measurement-agnostic evaluator (`abc.tools.parser-release-qualification`) that
emits, per predicate, exact observed and expected values and a derived verdict.

This ADR STAYS Proposed until every release predicate verdict is `pass` from
captured current evidence. The captured run
(`docs/reports/parser-release-qualification-report.json`) does not pass: several
predicates have no committed release instrument and are reported `unavailable`
(never `pass`), and the running parser build is not the admitted tuple (below).
Per the promotion rule these outcomes keep the gate at `not-qualified` and this
ADR at Proposed.

## Context

ADR 0002 records must-pass parser viability gates; ADR 0030 reported a 0.969
coverage observation and three failed `must` vectors while selecting a
consolidated-parser base. ADR 0038 accepts project ownership of the custom
parser for development only and states explicitly that ownership grants neither
tuple admission nor publication authority, and that comparison/citation records
cannot release a parser. Release qualification was left as future work.

Admission and release qualification are distinct gates:

- **Admission** is the exact parser/adapter/mapping tuple through ADR 0023's
  compatibility registry (`data/aat-parser-ir-compatibility.edn`). The admitted
  custom-parser row is `ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3
  (git 004deaf548f34a36abbc17d0f7a162df010a6292)`, mapping `0.4.0`
  (`sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30`).
- **Release qualification** is this gate: predeclared predicates over a pinned
  corpus, evaluated with exact numeric comparison.

Neither comparison nor neutral evidence (ADR 0030 selection rows, the ADR 0038
neutral comparison) can satisfy either gate; the release gate consumes the
Task-6 structural evidence-class boundary in `abc.tools.parser-evidence`
(`assert-release-evidence!`, release class `#{:conversion-compatibility}`) so a
comparison/neutral citation is structurally incapable of qualifying a release.

### Admission resolution for the release candidate (ADR 0023)

`adapter_version` bakes the source revision into the exact-match registry
coordinate (`AB_AOZORA_GIT_REV = self.rev`, baked by `build.rs`). The admitted
row pins `git 004deaf…`. The release candidate at the current branch HEAD is a
different source revision (the `ab-aozora` crate changed after `004deaf`, e.g.
the C5 identity-bump and clippy/fmt commits), so its baked git coordinate is not
`004deaf`. The release candidate is therefore NOT the admitted tuple, and the
old admission does not silently qualify current HEAD. Admitting the HEAD build
requires a new exact ADR-0023 registry row backed by a fresh full-corpus
conversion audit (`ab-aat-to-parser-ir audit-corpus` over the aozora-full
corpus), which is a separate measurement campaign from this smoke-scale
qualification corpus and is not performed here. Until that row exists, admission
for the HEAD build is `unavailable`, which independently keeps this gate
`not-qualified`.

## Decision

Qualify the project-owned parser for release ONLY through the predeclared
predicate set evaluated over the pinned qualification corpus with exact numeric
comparison. Promote this ADR to Accepted (and move release authority toward
publication) ONLY when every predicate verdict is `pass` from captured current
evidence for an admitted build. A predicate with no valid instrument is
`unavailable`, never `pass`; a `0.969` observation fails a `1.0` predicate
mechanically. Rounded composite coverage is descriptive and never substitutes
for a must-pass predicate.

## Consequences

The gate is reproducible and honest about its own limits: it reports exact
observed values where a committed instrument exists and `unavailable` (with the
named blocker) where one does not, instead of fabricating a number. Weakening a
failing or unavailable predicate to force a pass is prohibited; changing a
predicate requires a separately evidenced ADR. Because the current captured run
is `not-qualified`, the custom parser continues under development release
authority without publication authority (the ADR 0038 safe fallback).

## Acceptance Criteria

- **ADR-0039-C1 — structural-invariant:** The qualification corpus is pinned and hash-addressed; its recomputed `corpus_snapshot_hash` and `list_hash` equal the committed values, and tampering with a member identity is rejected. Evidence: `test/abc/tools/parser_release_qualification_test.clj`.
- **ADR-0039-C2 — structural-invariant:** The predicate set predeclares the fatal, span, silent-drop, diagnostic, IR, publication-structure, wall-time, memory, and timeout dimensions with exact thresholds fixed before measurement. Evidence: `test/abc/tools/parser_release_qualification_test.clj`.
- **ADR-0039-C3 — fixture-behavior:** Predicate evaluation is exact and integrity-preserving: a `0.969` observation fails a `1.0` predicate, and a predicate with no instrument is `unavailable`, never `pass`. Evidence: `test/abc/tools/parser_release_qualification_test.clj`.
- **ADR-0039-C4 — structural-invariant:** Comparison and neutral citations are structurally rejected as release evidence through the `abc.tools.parser-evidence` release class boundary. Evidence: `test/abc/tools/parser_release_qualification_test.clj`.
- **ADR-0039-C5 — operational-behavior:** The gate is release-qualified only when every predicate verdict is `pass` for an admitted build; otherwise it stays `not-qualified` and this ADR stays Proposed. Evidence: `test/abc/tools/parser_release_qualification_test.clj`.

## Future Verification

Promotion to Accepted requires a full-corpus admission row for the HEAD build
under ADR 0023 and a captured qualification run whose every predicate verdict is
`pass`. Committed release instruments for the currently `unavailable`
predicates (source-span coverage, silent drops, publication structure, and
per-work memory) are required before those predicates can move off
`unavailable`.

## Rollback

Withdraw this ADR if release qualification is redefined, or supersede it once a
committed instrument set and an admitted HEAD build produce a fully passing
captured gate. Do not repurpose comparison or neutral evidence as release
evidence.
