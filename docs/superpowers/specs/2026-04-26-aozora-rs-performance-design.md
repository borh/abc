# Aozora-rs Performance and Simplicity Design

## Context

The parser validation harness now validates the full Aozora Bunko corpus with
two adapters:

- `aozora2`: 17,894 reports, 0 failures, 32.983328 seconds.
- `aozora-rs`: 17,894 reports, 0 failures, 1690.014773 seconds.
- Comparison: 17,894 common reports, 0 result differences.

The current `aozora-rs` adapter is correct by the validation harness, but it is
too slow and the implementation is increasingly entangled. Parsing,
body-selection, source-visible cleanup, AAT construction, fallback handling, and
projection checking all live in `adapters/aozora-rs/src/lib.rs`. Some fallback
behavior is implicit: a malformed parser projection or a very large body can
cause the adapter to emit source-visible fallback blocks, but that decision is
not surfaced as a first-class metric in the AAT metadata or benchmark summary.

## Goal

Improve the performance and design quality of the existing `aozora-rs` adapter
without reducing validation strictness or changing the external adapter
protocol. The next implementation should explain where time is spent, make
fallback behavior observable, and simplify the adapter into composable phases.

## Non-Goals

- Do not add a new parser adapter in this phase.
- Do not change the AAT schema unless measurement proves a schema limitation is
  blocking observability.
- Do not weaken shared `ab-check` properties to make one adapter faster.
- Do not implement HTML/render diffing.
- Do not replace `aozora-rs-core`; treat it as the parser under evaluation.

## Success Criteria

The phase is complete when:

- Full-corpus comparison still produces 17,894 `aozora-rs` reports with 0
  validation failures.
- `ab-compare` still reports 17,894 common reports, 0 only-a, 0 only-b, and 0
  result differences between `aozora2` and `aozora-rs` validation reports.
- The `aozora-rs` adapter emits per-work metrics for decode/body selection,
  tokenize, scopenize, retokenize, AAT mapping, projection checking, fallback,
  and final root construction. Runner-side summaries add artifact write timing
  when they persist AAT files.
- The full comparison summary includes enough aggregate timing and fallback data
  to identify the top slow works and the top costly stages.
- At least one concrete performance improvement is implemented and measured
  against the checked-in parser comparison baseline.
- The measured full-corpus `aozora-rs` runtime is reduced by at least 25% from
  the checked-in 1690.014773 second baseline, or the implementation produces a
  stage-level report showing that at least 90% of the remaining time is inside
  `aozora-rs-core` parser stages that this phase intentionally does not
  replace.
- `adapters/aozora-rs/src/lib.rs` is reduced to orchestration and public API
  glue; parser invocation, AAT building, projection checking, and metrics live
  in focused modules.
- The code passes `cargo fmt --check`, `cargo test --workspace`, workspace
  clippy, adapter clippy, benchmark compile checks, and `nix flake check`.

## Design Principles

This phase should be reviewed with the Rich Hickey design lens:

- **Simplicity over ease:** avoid quick performance hacks that braid parsing,
  validation, and reporting more tightly together.
- **Values over places:** each phase should return a value describing what
  happened. Avoid mutating a JSON tree across unrelated concerns when a typed
  result can carry the same information.
- **Composition:** parser output, AAT output, projection validation, and metrics
  should be separable. A future parser adapter should be able to reuse the
  measurement and summary model without copying adapter internals.
- **Explicit time modeling:** benchmark and adapter metrics should preserve the
  succession of stage timings rather than only the final wall-clock time.
- **Protocols:** keep the external adapter protocol stable, but make the
  internal protocol between phases explicit through typed structs.

## Internal Protocol Types

The implementation should make these phase boundaries explicit. Field names may
change only if the replacement carries the same information with narrower
responsibility.

```rust
use std::time::Duration;

pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
    pub source_bytes: usize,
}

pub struct BodySelection<'a> {
    pub parser_body: &'a str,
    pub validation_body: &'a str,
    pub strategy: BodySelectionStrategy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum BodySelectionStrategy {
    ParserMeta,
    SeparatorFallback,
}

pub struct ParsedSource<'a> {
    pub body: BodySelection<'a>,
    pub retokenized: Vec<aozora_rs_core::Retokenized<'a>>,
    pub warnings: Vec<String>,
    pub tokenized_count: usize,
    pub retokenized_count: usize,
    pub timings: ParseTimings,
}

pub struct ParseTimings {
    pub body_selection: Duration,
    pub tokenize: Duration,
    pub scopenize: Duration,
    pub retokenize: Duration,
}

pub struct InitialAatBuildResult {
    pub blocks: Vec<serde_json::Value>,
    pub timings: AatBuildTimings,
}

pub struct AatBuildResult {
    pub blocks: Vec<serde_json::Value>,
    pub fallback: FallbackDecision,
    pub timings: AatBuildTimings,
    pub projection: ProjectionSummary,
}

pub struct AatBuildTimings {
    pub build: Duration,
    pub projection_check: Duration,
    pub fallback_build: Duration,
}

pub struct ProjectionSummary {
    pub source_visible_chars: usize,
    pub projected_visible_chars: usize,
    pub in_source_order: bool,
}

pub struct AdapterMetrics {
    pub decode: Duration,
    pub body_selection: Duration,
    pub tokenize: Duration,
    pub scopenize: Duration,
    pub retokenize: Duration,
    pub aat_build: Duration,
    pub projection_check: Duration,
    pub fallback_build: Duration,
    pub source_bytes: usize,
    pub validation_body_bytes: usize,
    pub parser_body_bytes: usize,
    pub tokenized_count: usize,
    pub retokenized_count: usize,
    pub fallback: FallbackDecision,
}

pub struct StageTiming {
    pub stage: &'static str,
    pub elapsed: Duration,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FallbackReason {
    None,
    ProjectionMismatch,
    LargeBody,
    Other(String),
}

pub struct FallbackDecision {
    pub used: bool,
    pub reason: FallbackReason,
}
```

`DecodedSource` owns the decoded text and source metadata. Downstream phases may
borrow string slices from it for the duration of one adapter invocation. No
phase may store borrowed values in global state, background work, or persisted
metrics. This keeps zero-copy parsing local without turning lifetimes into the
adapter's architectural boundary.

## Current Entanglements

The current adapter has these design issues:

- `parse_with_aozora_rs` chooses parse body and validation body, invokes parser
  stages, and collects warnings in one function.
- `retokenized_to_aat_blocks` builds blocks, normalizes visible text, strips
  source notes, injects supplemental ruby/gaiji nodes, checks projection order,
  and decides fallback behavior.
- Fallback use is invisible except through the resulting AAT shape.
- The expensive projection check is always tied to AAT construction, so it is
  hard to measure or disable for controlled profiling.
- Stage durations are not represented as values, so full-corpus performance
  regressions can only be inferred from coarse runner timings.

## Proposed Architecture

Refactor `adapters/aozora-rs` into these modules:

- `src/lib.rs`
  - Public adapter API: `decode_source_bytes`, `aat_json_from_bytes`,
    `html_from_bytes`.
  - Orchestrates decode -> parse -> build AAT -> serialize.
  - Owns no parser-stage or projection logic.

- `src/source.rs`
  - Source decoding and body extraction.
  - Defines owned `DecodedSource` and borrowed `BodySelection<'a>`.
  - Keeps parser body and validation body distinct and explicit.

- `src/parser.rs`
  - Invokes `aozora-rs-core` stages.
  - Defines `ParsedSource<'a>` with retokenized output, warnings, counts, and
    stage timings.
  - Converts parser errors into adapter errors without relying on implicit
    `anyhow` conversion.

- `src/aat.rs`
  - Converts `ParsedSource` into a typed `InitialAatBuildResult`.
  - Builds block values, not the final AAT root.
  - Builds fallback blocks when the orchestrator selects a fallback path.

- `src/projection.rs`
  - Owns source-visible text normalization, visible projection, note stripping,
    and subsequence checks.
  - Exposes pure functions that accept values and return values.

- `src/metrics.rs`
  - Defines `AdapterMetrics`, `StageTiming`, `FallbackReason`, and
    `MetricsSummary`.
  - Uses `std::time::Duration` internally and converts durations to millisecond
    `f64` fields only at JSON boundaries.
  - Provides aggregation helpers used by the comparison runner.

The internal data flow should be:

```text
bytes
  -> DecodedSource
  -> BodySelection { parser_body, validation_body, strategy }
  -> ParsedSource { retokenized, warnings, counts, parse timings }
  -> InitialAatBuildResult { blocks, build timings }
  -> ProjectionSummary from projection::check(...)
  -> final AatBuildResult { blocks, fallback, projection summary }
  -> AAT JSON { blocks, meta.metrics, meta.warnings }
```

The orchestrator calls `projection::check(...)` after `aat::build_initial(...)`.
`aat.rs` does not call `projection.rs`. If projection fails or large-body policy
applies, the orchestrator asks `aat.rs` to build fallback blocks and records the
fallback decision. This keeps projection validation composable and independently
testable.

## Metrics Model

Each `aozora-rs` AAT should include adapter-private metadata under
`meta.metrics`. The AAT schema allows additional metadata fields, so this does
not require a schema change.

Required fields:

```json
{
  "meta": {
    "metrics": {
      "decode_ms": 0.0,
      "body_selection_ms": 0.0,
      "tokenize_ms": 0.0,
      "scopenize_ms": 0.0,
      "retokenize_ms": 0.0,
      "aat_build_ms": 0.0,
      "projection_check_ms": 0.0,
      "source_bytes": 0,
      "validation_body_bytes": 0,
      "parser_body_bytes": 0,
      "tokenized_count": 0,
      "retokenized_count": 0,
      "fallback_used": false,
      "fallback_reason": "none"
    }
  }
}
```

The adapter metadata intentionally does not include `json_serialize_ms`: a
payload cannot observe its own final serialization time without a second
serialization pass. Runner-side summaries may record `aat_artifact_write_ms`
when AAT files are persisted.

`fallback_reason` is serialized from `FallbackReason`. Known values are:

- `projection_mismatch`
- `large_body`
- `none`

The Rust enum includes `Other(String)` so profiling can surface new recovery
causes without changing the wire shape. A new named enum variant should be added
only when a test demonstrates the reason with a real or synthetic fixture.

## Performance Measurement

The comparison runner should add a post-processing step that reads persisted
`aozora-rs` AAT metrics and writes an aggregate metrics file, for example:

```json
{
  "adapter": "aozora-rs",
  "works": 17894,
  "fallbacks": 0,
  "stage_totals_ms": {
    "tokenize": 0.0,
    "scopenize": 0.0,
    "retokenize": 0.0,
    "aat_build": 0.0,
    "projection_check": 0.0,
    "aat_artifact_write": 0.0
  },
  "slowest_works": [
    {
      "work_id": "001529_50685",
      "total_ms": 0.0,
      "dominant_stage": "retokenize",
      "fallback_used": false,
      "stages_ms": {
        "decode": 0.0,
        "body_selection": 0.0,
        "tokenize": 0.0,
        "scopenize": 0.0,
        "retokenize": 0.0,
        "aat_build": 0.0,
        "projection_check": 0.0,
        "aat_artifact_write": 0.0
      }
    }
  ]
}
```

The summary should be committed only for stable full-corpus baseline runs, not
for every local profiling experiment.

## Performance Hypotheses

The implementation plan should test these hypotheses in order:

1. The 5-minute outliers are dominated by `aozora-rs-core` parse stages rather
   than JSON serialization.
2. The adapter's projection validation is costly on large bodies and can be
   made cheaper by using fragment-level values or early exits.
3. Fallback construction currently repeats source-visible regex passes and can
   reuse computed source-visible text.
4. Persisting full AAT artifacts is useful for comparison but expensive for
   routine benchmarking; a metrics-only mode may be warranted after profiling.

Only implement an optimization after a metric points to it.

## Composition Boundaries

`parser.rs` is allowed to be specific to `aozora-rs-core` and may expose
`Retokenized<'a>` because this phase is about the existing parser path.
`metrics.rs` and `projection.rs` must not depend on `aozora-rs-core` token
types. `aat.rs` may depend on `Retokenized<'a>` because it maps parser output to
AAT blocks.

This means the reusable protocol is the metrics/projection boundary, not a
generic parser trait. A trait for all future parser adapters is out of scope
until a third adapter shows the shared shape.

## Error Handling

Parser-stage failures should remain adapter fatal errors. Non-fatal scopenize
and retokenize errors should remain warnings and should not cause
`parse_complete=false` unless the adapter cannot produce an AAT.

Fallback is not an error. It is an explicitly recorded recovery mode used when
parser-derived visible projection cannot be reconciled with the validation
source body or when a known pathological large body would make projection
checking too costly.

## Testing Strategy

Add focused tests before implementation:

- Metrics are present in a small ruby/gaiji AAT.
- Body selection distinguishes parser body from validation body for a fixture
  with Aozora separator lines.
- Projection mismatch fallback records `fallback_used=true` and
  `fallback_reason="projection_mismatch"`.
- Large-body fallback records `fallback_reason="large_body"` without requiring a
  multi-megabyte test fixture.
- Stage timing fields are present and numeric.
- Full-corpus runner summaries include aggregate `aozora-rs` metric totals.
- The top slow works summary includes full per-stage timing breakdowns, not only
  `total_ms` and `dominant_stage`.
- Existing full-corpus validation remains at 0 failures.

## Review Checklist

Before implementation is considered complete, perform a Hickey-style review:

- Are parsing, AAT construction, projection validation, fallback, and metrics
  separate concerns?
- Are stage results represented as values rather than hidden mutations?
- Can projection checking be tested without invoking `aozora-rs-core`?
- Can metrics aggregation be tested without running the full corpus?
- Is fallback explicit in metadata and summary output?
- Did any optimization introduce special cases that obscure the data flow?

## Open Constraints

- `references/aozorabunko` remains a local corpus mirror and is not committed.
- `references/parsers/aozora-rs` remains a local code-search/reference copy and
  path dependency source.
- Full-corpus benchmark numbers are same-machine baselines, not portable
  promises.
