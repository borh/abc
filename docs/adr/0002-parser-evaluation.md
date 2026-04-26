# ADR 0002: Parser Evaluation Criteria

Status: Draft
Date: 2026-04-26
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5

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

- `fatal_failure_rate` is at or below the threshold recorded in the candidate
  report for the smoke corpus.
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
- At least one candidate can produce or map into `schemas/parser-ir.schema.json`.
- Candidate reports include warning sidecar references and diagnostic
  aggregation output.
- The advertised `aozora-rs` speed claim is treated as a claim to re-measure,
  not as accepted evidence.
- Parser choice can be reversed without changing manifest identity rules.

## Rollback

If no existing parser satisfies the must-pass criteria, use this ADR as the
requirements document for the Rust parser effort.
