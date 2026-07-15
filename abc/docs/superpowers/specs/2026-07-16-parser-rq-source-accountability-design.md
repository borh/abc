# Parser Release Qualification P1: Source Accountability Design

**Status:** Approved conversational design; written-spec review pending  
**Parent design:** `2026-07-15-parser-release-qualification-campaign-design.md`  
**Consumes:** P0 capture manifests, observation envelopes, external-store
verification, and qualification identity  
**Produces:** R1 source-span coverage instrument and the pure R2 reconciliation
engine; R2 remains unavailable until P4 validates diagnostic capture

## Objective and boundary

P1 makes source accountability a deterministic measurement rather than an
assertion. It measures how much of each pinned work's decoded UTF-8 source is
covered by emitted Parser-IR spans, using bytes as the denominator and a
versioned ignored-region taxonomy. It separately proves that every pinned work
contributed exactly one record.

P1 also implements the pure join that classifies uncovered regions as diagnosed
or silent. It does not publish an R2 gate observation yet. P4 must first supply a
schema-validated, identity-bound diagnostic capture; an absent, empty, or
unvalidated diagnostic stream cannot produce a false zero.

This focused design does not pin a release candidate, perform a corpus-scale
authoritative capture, admit a registry row, or promote ADR 0039. P5 owns those
barrier actions after P1-P4 are complete.

## Coordinate and authority model

The authoritative R1 input is Parser-IR JSON, not AAT JSON. Parser-IR is the
claimed publication intermediate representation, and its live schema defines
`span.start` and `span.end` as UTF-8 byte offsets into the decoded source string.
Measuring AAT spans would prove coverage at the wrong layer.

For every work, the instrument consumes:

1. the exact decoded UTF-8 source buffer used by the parser conversion;
2. the Parser-IR document emitted for that source;
3. the pinned corpus entry and its original-source SHA-256;
4. the versioned ignored-region taxonomy.

The original source hash and declared encoding remain provenance coordinates.
They do not change the arithmetic coordinate system. Every measured interval is
half-open `[start,end)` in decoded UTF-8 bytes.

## Architecture

A new Rust crate,
`ab-validator/crates/ab-parser-rq-source-accountability/`, owns the measurement.
It remains separate from both the parser implementation and the older generic
`ab-coverage` taxonomy. The crate contains five narrow units:

- `interval`: validate, sort, merge, intersect, subtract, and complement
  half-open byte intervals;
- `taxonomy`: load the closed v1 taxonomy and derive ignored intervals from the
  decoded source;
- `analyze`: extract Parser-IR node spans and produce one deterministic work
  record;
- `reconcile`: join uncovered intervals to validated diagnostic spans without
  deciding whether the diagnostic capture is admissible;
- `aggregate`: authenticate the work set, calculate R1, and conditionally expose
  R2 only when its diagnostic precondition is satisfied.

The CLI exposes two commands:

```text
ab-parser-rq-source-accountability analyze-work \
  --source-utf8 <path> --parser-ir <path> --corpus-entry <path> \
  --taxonomy <path> --out <path>

ab-parser-rq-source-accountability aggregate \
  --corpus <path> --work-records <directory> --taxonomy <path> \
  [--diagnostic-capture <path>] --out <path>
```

`analyze-work` is pure apart from file I/O. `aggregate` requires exactly one
authenticated record per corpus member and rejects extras.

ABC does not repeat the Rust interval arithmetic. A small Clojure derivation
boundary consumes a verified aggregate summary, confirms its denominator unit,
taxonomy identity, and qualification identity, then creates the
`:source_span_coverage` observation envelope through P0's contract.

## Ignored-region taxonomy v1

The v1 taxonomy is closed and deliberately narrow. It ignores only a leading
Unicode BOM (`U+FEFF`, three decoded UTF-8 bytes) when present at byte offset
zero. The parser's decoding contract already identifies that marker as
transport framing.

No other bytes are ignored in v1. In particular, whitespace, blank lines,
markup delimiters, bibliographic headers, body separators, source notes,
terminal provenance, malformed constructs, and unknown regions remain eligible.
If a future region cannot be proven to be transport framing from the parser's
existing preprocessing contract, it remains eligible. Expanding the taxonomy
requires a new version and hash and therefore a new instrument identity.

The committed taxonomy record contains:

```json
{
  "schema_version": "abc/parser-rq-ignored-regions/v1",
  "taxonomy_version": "parser-rq-ignored-regions-v1",
  "coordinate_system": "decoded_utf8",
  "rules": [{"kind": "leading-unicode-bom", "bytes": "efbbbf"}]
}
```

Its canonical SHA-256 and version are included in the instrument version and in
every work and aggregate record.

## Work-record contract

Each work record contains:

- work ID, original source hash, decoded-source hash, source encoding, and
  decoded byte length;
- Parser-IR schema ID/hash and document hash;
- taxonomy version/hash and coordinate system;
- ignored, eligible, covered-eligible, and uncovered interval lists;
- `eligible_bytes`, `covered_eligible_bytes`, and `uncovered_eligible_bytes`;
- a status of `ok` or `unavailable` with structured errors.

Nested and overlapping Parser-IR spans are normalized into a union before
counting. Container and child spans therefore never double-count bytes. A span
with `start > end`, an endpoint beyond the decoded buffer, a coordinate-system
mismatch, a malformed Parser-IR document, or a source-identity mismatch makes
the work record unavailable.

The following identity must hold for every `ok` record:

```text
covered_eligible_bytes + uncovered_eligible_bytes = eligible_bytes
eligible_bytes + ignored_bytes = decoded_source_bytes
```

Zero eligible bytes is unavailable, not a vacuous coverage value.

## Aggregate R1 contract

Aggregation compares work IDs to the pinned qualification corpus as sets and
counts. Missing, duplicate, unexpected, unavailable, source-hash-mismatched, or
taxonomy-mismatched records make the aggregate unavailable. Work completeness
is reported independently from the byte ratio.

When every record authenticates:

```text
source_span_coverage =
  sum(covered_eligible_bytes) / sum(eligible_bytes)
```

The serialized aggregate retains the integer numerator and denominator. The
gate value is derived from those integers; it is not accepted as a separately
supplied floating-point claim. A result below `1.0` is an honest gate failure
with uncovered witnesses, not an instrumentation failure.

The committed P1 capture manifest uses P0's logical blob references and records
the denominator as:

```clojure
{:value <sum-eligible-bytes> :unit "decoded_utf8_bytes"}
```

Corpus-scale work records remain in the external store. The repository commits
the manifest, hashes, aggregate summary, and only small diagnostic witnesses.

## R2 reconciliation and deferral

The pure reconciliation function accepts an uncovered interval set and a
diagnostic interval set in the same coordinate system. An uncovered interval is
diagnosed when at least one validated diagnostic interval overlaps it. Otherwise
it is silent. The output enumerates both classes and their counts; it does not
infer validity from a nonempty stream.

The aggregate accepts an optional diagnostic capture carrying:

- the same qualification identity reference;
- `coordinate_system = decoded_utf8`;
- a diagnostic schema version;
- a P4-owned validation marker and validator version;
- exactly one authenticated diagnostic record per work.

Until P4 defines and produces that validated capture, P1 serializes the R2
reconciliation capability but derives the `:silent_drops` observation as
`:instrument-missing`. An empty unvalidated stream is not evidence of zero
silent drops.

## Failure semantics

All contract failures fail closed:

- invalid or out-of-bounds interval -> work unavailable;
- source, schema, taxonomy, or qualification identity mismatch -> unavailable;
- missing, duplicate, or unexpected work -> aggregate unavailable;
- zero eligible-byte denominator -> aggregate unavailable;
- missing or unvalidated diagnostics -> R2 unavailable while leaving valid R1
  measurable;
- unavailable R1 -> no numeric R1 observation envelope.

The tool never repairs spans, clips endpoints, drops malformed records, imputes
missing work, or substitutes work count for bytes.

## Testing strategy

Ordinary Rust tests pin exact JSON contracts and failures for malformed spans,
out-of-bounds endpoints, a leading BOM, nested spans, missing/duplicate work,
zero eligible bytes, and unvalidated diagnostics. Clojure tests prove that only
a verified aggregate with unit `decoded_utf8_bytes`, matching taxonomy identity,
and matching qualification identity can become a source-span envelope.

Hegel property tests live beside the interval module and exercise evidence-based
properties:

1. interval union is invariant under input ordering and duplication;
2. union output is sorted, disjoint, and bounded by source length;
3. subtraction partitions eligible bytes without overlap;
4. for valid generated inputs,
   `covered + uncovered = eligible` and neither count exceeds decoded length.

The Hegel dependency is development-only. If it cannot be represented by the
repository's Nix/Cargo lock and checks, that is a planning blocker to resolve;
the implementation must not silently replace the approved property-test stack.

## Delivery and sequencing

P1 is complete when:

1. the v1 taxonomy and its hash are committed;
2. the Rust work analyzer, aggregate, CLI, and pure R2 reconciler pass unit and
   Hegel property tests;
3. the ABC derivation boundary passes focused Clojure tests;
4. a small fixture capture proves Capture -> Derive -> Drift locally;
5. R1 is capturable under P0 and R2 remains explicitly unavailable pending P4;
6. comment hygiene and `just validate-migration` pass.

P4 subsequently supplies the validated diagnostic-capture contract. P5 pins the
final implementation revision and performs authoritative corpus capture on
`hinoki.hyakutake-barbel.ts.net`.
