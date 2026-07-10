# ADR 0002: Parser Evaluation Criteria

Status: Accepted
Date: 2026-04-26
Accepted: 2026-07-10
Supersedes: none
Amended by: ADR 0030
Source: `docs/high-level-architecture-note.md` v0.5

## Implementation Status

2026-07-10: Accepted via ADR 0030. The producer-side parser comparison study
(`ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`
and its 2026-07-09 follow-ups) is recorded as `:citable` parser-selection
evidence in `data/parser-evidence-citations.edn`, resolving the citation
question that kept this ADR Draft. ADR 0030 selects `aozora-pipeline`
(`P4suta/aozora`) as the consolidated-parser fork base and enacts the
evidence-policy acceptance slice from
`docs/handoffs/parser-evidence-citation-contract.md`. This ADR's gates remain
the evaluation criteria of record; the notes below are retained as history of
the Draft period.

Historical (pre-acceptance): parser candidate comparison remains in `../ab-validator`. ABC now
owns the downstream publication consumer contract: parser-IR can be validated,
materialized, and rendered to publication artifacts without executing parser
candidates locally.

2026-07-04 evidence note: ab-validator reports now show corpus-clean
AAT-to-parser-IR conversion for the admitted aozora-rs and aozora2html
adapter/version tuples and mapping hash. See
`docs/handoffs/measurement-probes-2026-07-04.md`. That evidence supports the
parser-IR boundary, but this ADR remains Draft until ABC decides how to cite
ab-validator's measured parser-candidate reports as parser-selection evidence
rather than only downstream conversion evidence.

Follow-up citation note:
`docs/handoffs/parser-evidence-citation-contract.md` defines the ABC-side
citation contract for producer measurement reports. The provisional
machine-readable index `data/parser-evidence-citations.edn` is now validated by
`nix run .#validate-design-bundle` and separates conversion compatibility,
parser selection, and comparator/oracle evidence. ABC may cite the July 4
conversion reports as publication-boundary evidence, but should not claim that
ADR 0002 has selected a canonical parser until parser-selection entries move
from provisional evidence to an accepted policy decision.

Policy boundary update: while this ADR remains Draft, ABC treats
`docs/handoffs/parser-evidence-citation-contract.md` as the controlling
producer-evidence citation policy for downstream publication work. Evidence is
citable only when it has a logical workspace-relative path, SHA-256 report
hash, exact adapter/version identity, mapping identity, and explicit evidence
class. Compatibility evidence can justify ABC admission and publication
rendering. Parser-selection evidence must still come from parser-candidate
reports that address this ADR's gates directly.

Monorepo note: future repository consolidation should remove only the physical
checkout boundary, not the logical producer/consumer boundary. The provisional
machine-readable index `data/parser-evidence-citations.edn` therefore records
logical workspace-relative evidence paths plus report hashes, while current
`../ab-validator` paths remain temporary locators. Monorepo naming and
component labels are tracked in
`docs/handoffs/monorepo-component-boundaries.md`; they do not imply an accepted
`abc` namespace or vocabulary rename.

Mapping transition note: ABC now admits mapping version `0.2.1` as a separate
conversion-compatibility identity with fresh producer report hashes and exact
adapter-version registry rows for `aozora-epub3`, `aozora-rs`, and
`aozora2html`. The earlier `0.2.0` rows remain historical admitted evidence and
were not rewritten.

TEI comparison note: the TEI-EAJ all-work comparison artifacts are downstream
publication evidence, not parser-selection evidence. They identify publication
gaps that require parser-IR support, especially paragraph and source-note
structure, but they do not by themselves select a parser candidate. See
`docs/handoffs/tei-eaj-aozora-comparison.md` and the machine-readable workset
export `docs/handoffs/tei-eaj-aozora-workset-export.json`.

## Evidence Policy Boundary

ABC separates producer evidence into three classes:

- **Compatibility evidence**: measured adapter/version/mapping/parser-IR tuples
  that ABC can admit through `data/aat-parser-ir-compatibility.edn`.
- **Parser-selection evidence**: candidate reports that evaluate parser behavior
  against this ADR's criteria: source-span coverage, unsupported syntax,
  diagnostics, fatal failures, performance, packaging, and reversibility.
- **Comparator/oracle evidence**: rendered-output or residual-bucket evidence
  that explains disagreement but does not select a direct source parser.

The accepted downstream publication boundary may cite compatibility evidence
now. Parser selection stays open until at least one serious parser candidate
has a parser-selection report recorded by logical path and hash in
`data/parser-evidence-citations.edn` or its accepted successor.

## Context

ABC should not assume the parser is greenfield. Candidate implementations
include `aozora-rs`, `aozora-core`, `aozora2`, `aozora2html`,
`aozora-parser.js`, the existing Clojure code, and the separate Rust parser
work in progress.

The parser choice should be made against boundary behavior, not language
preference.

## Decision

Parser candidates must pass these gates before they are considered viable:

- License and redistribution terms are compatible with ABC artifacts.
- Ruby scope, gaiji, editor notes, layout constructs, and unknown syntax are
  represented or reported.
- Output can be emitted directly as ABC parser IR or mapped into it without
  losing source spans.
- Parser IR documents include `schema_id` and `schema_hash`.
- Parser IR manifests reference warning/error sidecar artifacts.
- Diagnostics use stable severity levels and error/warning codes.
- Unsupported syntax is emitted as structured warnings or errors, not silently
  dropped.
- Candidate can process the smoke corpus without unacceptable fatal failures.

Before comparing candidates, "unacceptable" means:

- `fatal_failure_rate` is at or below the ABC-defined gate threshold for the
  smoke corpus. The candidate report records the *measured* rate, not the gate
  threshold: a candidate cannot pass by declaring its own threshold, because
  the threshold is ABC's acceptance gate, not a self-declared value. The gate
  threshold is recorded in this ADR's smoke-corpus contract
  (`docs/v0-design-bundle/ci-smoke-corpus.md`); if no explicit threshold is
  recorded there, the gate defaults to a zero fatal-failure rate for the smoke
  corpus during Draft status.
- `source_span_coverage` is 100% for parsed source bytes, excluding documented
  ignored regions.
- `unsupported_constructs` has zero silent drops.
- every warning and error has a stable code, severity, and source span when
  available.
- 100% of successful parses validate against the parser IR schema.

Tiebreakers:

- Compatibility with `aozora2html` for comparable HTML/rendered constructs.
- Performance on a fixed benchmark corpus, measured locally.
- Maintainability and release cadence.
- CLI/library/WASM availability.
- Ease of packaging in Nix or the chosen v0 runner.

## Benchmark Shape

The first benchmark corpus is the smoke corpus defined in
`docs/v0-design-bundle/ci-smoke-corpus.md`. It should include ruby, gaiji,
editor notes, images/captions, metadata-sensitive works, large-work behavior,
and expected warning/failure cases.

Measurements:

- Parse success/failure count.
- Warning/error count by stable code and severity.
- Corpus-level aggregation by severity, code, parser version, and work ID.
- IR round-trip validation result.
- Source span coverage.
- Wall-clock parse time.
- Peak memory if the candidate reports it cheaply.

Candidate reports use this minimum template:

```text
candidate_name:
candidate_version:
source_hash:
license_spdx:
redistribution_notes:
input_corpus_hash:
fatal_failures:
recoverable_errors_by_code:
warnings_by_code:
source_span_coverage_percent:
unsupported_constructs:
ir_validation_pass:
roundtrip_loss_notes:
aozora2html_construct_comparison:
wall_time:
peak_rss:
nix_packaging_status:
recommendation:
```

## Acceptance Criteria

- A candidate report exists for each serious parser option.
- Parser-selection reports cited by ABC have a logical workspace-relative path,
  SHA-256 hash, exact parser/adapter version, corpus label, success/failure
  counts, and explicit caveats.
- Conversion-compatibility reports are cited as compatibility evidence, not as
  parser-selection acceptance.
- Comparator/oracle reports are cited as explanatory evidence, not as direct
  parser-selection acceptance.
- At least one candidate can produce or map into `schemas/parser-ir.schema.json`.
- Candidate reports include warning sidecar references and diagnostic
  aggregation output.
- The advertised `aozora-rs` speed claim is treated as a claim to re-measure,
  not as accepted evidence.
- Parser choice can be reversed without changing manifest identity rules.

## Rollback

If no existing parser satisfies the must-pass criteria, use this ADR as the
requirements document for the Rust parser effort.
