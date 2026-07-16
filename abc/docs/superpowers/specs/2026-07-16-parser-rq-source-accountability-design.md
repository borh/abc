# Parser Release Qualification P1: Source Accountability Design

**Status:** Revised after hammock-driven design and Rich Hickey review;
written-spec review pending
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

For every work, the instrument consumes immutable values:

1. the exact original source bytes pinned by the qualification corpus;
2. the Parser-IR document emitted for that source;
3. the pinned corpus entry and its original-source SHA-256;
4. the full P0 qualification identity and its canonical identity reference;
5. the versioned ignored-region taxonomy.

The instrument does not trust a separately supplied decoded-text file. It calls
`ab_aozora_aat::decode_source_bytes`, the same public decoder that establishes
the decoded value and offset maps used by the owned parser; downstream span
emission performs the actual rebasing through those maps. P1 measures the
resulting `DecodedSource.text`. This prevents two decoding implementations from
silently creating different coordinate spaces.

Lossy decoding is not a valid measured value. If `DecodedSource.encoding` is
`windows-31j-lossy`, the work is unavailable even if Parser-IR spans cover every
replacement byte. U+FFFD replacement destroys source information before the
denominator exists and therefore cannot support a source-accountability pass.
The original source hash and non-lossy detected encoding remain provenance
coordinates. They do not change the arithmetic coordinate system.

The load-bearing coordinate invariant is explicit:

```text
Every accepted Parser-IR node span is a half-open [start,end) interval into the
exact full DecodedSource.text produced from the pinned original bytes.
```

It is not body-relative or sanitized-text-relative. P1 characterizes this by
round-tripping rebased body and tail node spans against the full decoded text,
including BOM, CRLF, body-offset, sanitize-map, and terminal-provenance fixtures.

## Architecture

A new Rust crate,
`ab-validator/crates/ab-parser-rq-source-accountability/`, owns the measurement.
It remains separate from both the parser implementation and the older generic
`ab-coverage` taxonomy. The crate contains five narrow units:

- `interval`: validate, sort, merge, intersect, subtract, and complement
  half-open byte intervals;
- `taxonomy`: authenticate the closed v1 taxonomy identity; v1 has no executable
  rule language and derives no ignored intervals;
- `analyze`: extract Parser-IR node spans and produce one deterministic work
  record;
- `reconcile`: partition uncovered intervals using P4-authorized source-loss
  intervals without interpreting raw diagnostics;
- `aggregate`: authenticate the work set, calculate R1, and emit R2 as
  unavailable until P4 supplies its authorization boundary.

The CLI exposes focused fixture and aggregate commands:

```text
ab-parser-rq-source-accountability analyze-work \
  --source <path> --parser-ir <path> --corpus-entry <path> \
  --qualification-identity <path> --taxonomy <path> \
  --diagnostics-locator <locator> --out <path>

ab-parser-rq-source-accountability aggregate \
  --corpus <path> --work-record-index <path> --taxonomy <path> \
  --qualification-identity <path> --store-root <path> --out <path>
```

The corpus-scale producer is a third command:

```text
ab-parser-rq-source-accountability analyze-corpus \
  --corpus <path> --source-root <path> --parser-ir-root <path> \
  --qualification-identity <path> --taxonomy <path> \
  --store-root <path> --index-out <path>
```

`analyze-work` is pure apart from file I/O. `aggregate` consumes an explicit,
closed record index rather than enumerating a mutable directory. The index names
exactly one work record and logical blob identity per corpus member; ordering is
irrelevant and extras are rejected.

`analyze-corpus` is the only production producer of that index. It iterates the
pinned corpus entries—not a directory glob—runs the work analyzer once per entry,
writes content-identified records to the configured external store, and emits the
closed index in canonical corpus order. Regeneration from identical inputs must
be byte-identical. Tests may construct index values directly; committed or
authoritative indexes are never hand-authored.

ABC does not repeat the Rust interval arithmetic. A small Clojure derivation
boundary consumes a verified aggregate summary, confirms its denominator unit,
taxonomy identity, and qualification identity, then creates the
`:source_span_coverage` observation envelope through P0's contract.

## Ignored-region taxonomy v1

The v1 taxonomy is closed and empty: every byte in `DecodedSource.text` is
eligible. A UTF-8 BOM is already stripped by `decode_source_bytes` before that
value exists, so a BOM rule in decoded coordinates would be unreachable policy,
not an ignored region. Whitespace, blank lines, markup delimiters,
bibliographic headers, body separators, source notes, terminal provenance,
malformed constructs, and unknown regions are all eligible.

V1 deliberately has no generic taxonomy rule engine. Adding one now would create
configuration without a current use. If a future region can be proven to be
transport framing inside `DecodedSource.text`, that change requires a new
taxonomy version/hash and an explicit instrument implementation change.

The committed taxonomy record contains:

```json
{
  "schema_version": "abc/parser-rq-ignored-regions/v1",
  "taxonomy_version": "parser-rq-ignored-regions-v1",
  "coordinate_system": "decoded_utf8",
  "rules": []
}
```

Its canonical SHA-256 and version are included in the instrument version and in
every work and aggregate record.

## Work-record contract

Each work record contains:

- qualification identity reference and instrument version;
- work ID; original-source blob hash/length; decoded-source hash, non-lossy detected
  encoding, and decoded byte length;
- Parser-IR schema ID/hash and document hash;
- taxonomy version/hash and coordinate system;
- ignored, eligible, covered-eligible, and uncovered interval lists;
- `eligible_bytes`, `covered_eligible_bytes`, and `uncovered_eligible_bytes`;
- a status of `ok` or `unavailable` with structured errors.

Only `Parser-IR.nodes[*].span` contributes coverage. Paragraph and sentence spans
are derived views and would double-count or allow synthesized aggregate ranges
to hide missing primary nodes. Nested and overlapping node spans are normalized
into a union before counting. A node span with `start > end`, an endpoint beyond
the decoded buffer, a coordinate-system mismatch, a malformed Parser-IR
document, a missing or non-`decoded_utf8` node `coordinate_system`, a lossy
decode, a P0 identity mismatch, or a source-identity mismatch makes the work
record unavailable. Although the Parser-IR schema permits omission for legacy
compatibility, the current converter emits the coordinate explicitly and release
evidence fails closed rather than relying on the schema's documented default.
P1 measures whether source bytes have an explicit Parser-IR node claim; semantic
span accuracy remains Track S3.

The analyzer validates every qualification coordinate represented in
Parser-IR—schema ID/hash and all `derived_from` adapter/mapping fields—against
the supplied identity. It cannot prove which executable produced an already
materialized file. P5 owns that operational provenance: its authoritative
capture must invoke and hash the pinned binaries, bind their machine-readable
versions to `parser_git_rev` and the mapping identity, and place the resulting
input/output blob identities in the capture manifest. P1 records the full
identity reference but does not mislabel that reference as producer attestation.

The following identity must hold for every `ok` record:

```text
covered_eligible_bytes + uncovered_eligible_bytes = eligible_bytes
eligible_bytes + ignored_bytes = decoded_source_bytes
```

Zero eligible bytes is unavailable, not a vacuous coverage value.

## Aggregate R1 contract

Aggregation authenticates the explicit record index and compares its work IDs
to the pinned qualification corpus as sets and counts. Missing, duplicate,
unexpected, unavailable, source-hash-mismatched, taxonomy-mismatched, blob-
identity-mismatched, or qualification-identity-mismatched records make the
aggregate unavailable. Work completeness is reported independently from the
byte ratio.

When every record authenticates:

```text
source_span_coverage =
  sum(covered_eligible_bytes) / sum(eligible_bytes)
```

The serialized aggregate retains the integer numerator and denominator. The
gate pass condition is exact integer equality, not floating-point equality:
`covered_eligible_bytes == eligible_bytes`. The report's numeric display value
is derived in Clojure by decimal division with scale
`decimal_digits(eligible_bytes) + 1` and `RoundingMode/DOWN`. Exact integer
equality is the pass guardrail; rounding down prevents the display projection
from contradicting it, while the chosen scale keeps a one-byte deficit visible.
The numeric display is not a separately trusted claim. A result below `1.0` is
an honest gate failure with uncovered witnesses, not an instrumentation failure.

The committed P1 capture manifest uses P0's logical blob references and records
the denominator as:

```clojure
{:value <sum-eligible-bytes> :unit "decoded_utf8_bytes"}
```

Corpus-scale work records remain in the external store. The repository commits
the manifest, hashes, aggregate summary, and only small diagnostic witnesses.

## R2 reconciliation and deferral

The pure reconciliation function accepts an uncovered interval set and a set of
**P4-authorized source-loss intervals** in the same coordinate system. It does
not accept raw parser diagnostics and does not interpret diagnostic codes,
severity, or validity. P4 owns the adapter from its validated diagnostic capture
to these authorized intervals.

Reconciliation partitions bytes rather than laundering whole regions by
overlap:

```text
diagnosed = uncovered ∩ authorized-diagnostic-intervals
silent    = uncovered − authorized-diagnostic-intervals
```

The result enumerates maximal connected diagnosed and silent intervals. Each
maximal silent interval is one `silent_drop`; therefore fragmentation is
canonical and independent of input ordering. Until P4 supplies authorized
intervals, P1 exposes the pure function for tests but derives `:silent_drops` as
`:instrument-missing`. An empty raw diagnostic stream is never passed to this
boundary as evidence of zero.

`silent_drop` is an interval witness count, not a dropped-construct census. Two
adjacent dropped constructs with no covered byte between them form one maximal
silent interval. This is sufficient for the gate's `0` versus `> 0` predicate;
P4 and downstream reports must not reinterpret the count as construct prevalence.

`DecodedSource.sanitize_diagnostics` is captured and content-identified as raw
P4 input because its spans already participate in the parser's sanitize-to-
decoded rebase. P1 does not treat those diagnostics as authorization. Bytes
not claimed by Parser-IR nodes remain uncovered for R1 regardless of sanitizer
diagnostics, and R2 remains unavailable until P4 explicitly maps validated
sanitizer diagnostics to authorized source-loss intervals.

## Failure semantics

All contract failures fail closed:

- invalid or out-of-bounds interval -> work unavailable;
- `windows-31j-lossy` decode -> work unavailable;
- absent or non-`decoded_utf8` node coordinate system -> work unavailable;
- source, schema, taxonomy, or qualification identity mismatch -> unavailable;
- missing, duplicate, or unexpected work -> aggregate unavailable;
- zero eligible-byte denominator -> aggregate unavailable;
- absent P4-authorized diagnostic intervals -> R2 unavailable while leaving
  valid R1 measurable;
- unavailable R1 -> no numeric R1 observation envelope.

The tool never repairs spans, clips endpoints, drops malformed records, imputes
missing work, or substitutes work count for bytes.

## Testing strategy

Ordinary Rust tests pin exact JSON contracts and failures for malformed spans,
out-of-bounds endpoints, lossy Shift-JIS replacement, absent coordinate systems,
BOM stripping before decoded coordinates, full-text rebasing across body/tail and
sanitize maps, nested spans, derived-view exclusion, deterministic index
regeneration, missing/duplicate work, zero eligible bytes, and absent P4
authorization. Clojure tests prove that only a verified aggregate with unit
`decoded_utf8_bytes`, exact numerator/denominator integrity, matching taxonomy
identity, and matching qualification identity can become a source-span envelope.

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

## Deepening record

### Problem, evidence, and assumptions

The problem is not “we need a coverage CLI.” The problem is that the release
gate currently has no reproducible value proving that every eligible decoded
source byte is claimed by the candidate's Parser-IR output, and no safe boundary
for deciding whether uncovered bytes were diagnosed.

| Claim | Type | Evidence | Confidence | Impact if wrong |
|---|---|---|---|---|
| Parser-IR spans use decoded UTF-8 byte offsets. | Observation | Live `parser-ir.schema.json` span contract. | High | The denominator and spans would be incomparable. |
| `decode_source_bytes` is the owned parser's decode/rebase boundary. | Observation | Live `ab-aozora-aat` implementation and `DecodedSource` contract. | High | A separate decoder could manufacture gaps or coverage. |
| UTF-8 BOM bytes are absent from `DecodedSource.text`. | Observation | BOM branch slices `bytes[3..]` before constructing `text`. | High | The original v1 taxonomy rule was unreachable. |
| Lossy Shift-JIS decoding inserts U+FFFD into `DecodedSource.text`. | Observation | `SHIFT_JIS.decode` plus the `windows-31j-lossy` encoding tag. | High | Replacement bytes could receive spans and manufacture a 1.0 over corrupted content. |
| Emitted node spans are full-decoded-text coordinates after rebasing. | Observation requiring characterization | `SpanContext` maps sanitized/body offsets back to decoded offsets; current converter emits `decoded_utf8`. | High | A body-relative edge case would silently corrupt coverage. |
| Parser-IR nodes are primary output claims; paragraph/sentence spans are derived views. | Observation + inference | Schema structure and converter implementation derive paragraph spans from nodes. | High | Counting derived views could hide missing nodes. |
| Existing source-region coverage machinery measures occurrence dispositions, not byte-span accountability. | Observation | `source-region-coverage.schema.json` counters and `reports/lib/source_region.py`. | High | Reusing it would conflate different units and authorities. |
| Raw diagnostic overlap is sufficient to excuse uncovered bytes. | Rejected assumption | Counterexample: a one-byte diagnostic overlapping a large uncovered range. | High | A large silent loss could be reported as diagnosed. |
| Hegel 0.28 is available with Rust 1.86 minimum. | Disposable probe | `cargo info hegeltest` on 2026-07-16. | Medium | Nix sandbox/runtime integration can still fail and must be tested in the plan. |
| A supplied Parser-IR file proves which binary produced it. | Rejected assumption | Content alone carries schema/mapping coordinates but no executable attestation. | High | Records could be coherently labeled with the wrong build. P5 must bind execution provenance. |

### Glossary

| Term | Definition |
|---|---|
| decoded source | `DecodedSource.text`: full decoded text after BOM removal, before body selection/sanitized parser views. |
| eligible byte | A byte in decoded source not excluded by the pinned taxonomy; in v1 every decoded byte is eligible. |
| coverage claim | A valid `Parser-IR.nodes[*].span` interval. It claims source association, not semantic accuracy. |
| uncovered interval | A maximal connected eligible interval outside the union of coverage claims. |
| authorized diagnostic interval | A decoded-source interval that P4 has validated as evidence of source loss; not a raw warning span. |
| silent drop | A maximal connected uncovered interval remaining after subtracting authorized diagnostic intervals. |
| work completeness | Set/count equality proving exactly one authenticated work record per pinned corpus member; never the byte denominator. |

### Prior art and alternatives

[LLVM source-based coverage](https://clang.llvm.org/docs/SourceBasedCodeCoverage.html)
distinguishes mapped code regions from explicit gap or skipped regions and keeps
the region mapping bound to the instrumented build. The
[TC39 source-map specification](https://tc39.es/source-map/) likewise represents
generated material with no original mapping explicitly rather than assigning a
convenient source location. These are analogies, not imported formats: P1 adopts
their separation of mapped, unmapped, and identity-bound evidence while retaining
decoded-byte coordinates. Unicode permits but does not require a UTF-8 BOM and
recommends consuming it as a signature where present; in this codebase it is
already removed before the measured decoded value
([Unicode BOM guidance](https://www.unicode.org/faq/utf_bom.html)).

| Criterion | Status quo | Reuse occurrence-level source-region report | Measure AAT spans | Dedicated Parser-IR byte instrument (chosen) |
|---|---|---|---|---|
| Measures the release claim | No value exists. | No: counts classified occurrences, not byte coverage. | No: proves an upstream adapter layer. | Yes: measures the claimed Parser-IR output. |
| Denominator integrity | Absent. | Different unit. | Could use bytes, but at wrong layer. | Eligible decoded bytes plus separate work completeness. |
| Identity coherence | Absent. | Publication-policy identity, not full P0 tuple. | Could be added. | Full qualification identity on every record and aggregate. |
| Complexity | No new code but predicate stays unavailable. | Superficially easy; complects two meanings of “source coverage.” | Reuses nearby data but creates a false proof. | One new focused crate and explicit protocol schemas. |
| What would change the choice | A pre-existing exact byte instrument is discovered. | Its contract would need exact intervals, decoded-byte totals, and P0 identity. | The release predicate would have to be redefined as AAT coverage. | Revisit only if Parser-IR ceases to be the release output layer. |

### Components, values, state, time, and identity

| Component | Purpose | Input values | Output values | State / time / identity |
|---|---|---|---|---|
| decoder boundary | Produce the exact measured coordinate value. | Original source bytes. | `DecodedSource.text`, detected encoding, original hash. | Pure per invocation; implementation version belongs to instrument identity. |
| interval algebra | Normalize and partition half-open intervals. | Bounded interval values. | Sorted disjoint interval values and lengths. | Stateless; union is order-independent and idempotent. |
| work analyzer | Authenticate one source/IR pair and measure nodes. | Source, Parser-IR, corpus entry, taxonomy, qualification identity. | Closed work record. | No mutable shared state; record identity includes every input blob and contract identity. |
| corpus analyzer/index producer | Analyze exactly the pinned work set and name its records. | Corpus entries, runtime roots, contracts, qualification identity. | Work-record blobs and closed immutable index. | Iterates corpus values, never directory membership; canonical output is byte-identical for identical inputs. |
| aggregate | Authenticate completeness and sum exact integers. | Corpus, index, records, taxonomy, qualification identity. | Closed aggregate summary. | Deterministic fold; no clock or host fields enter the result identity. |
| R2 reconciler | Partition uncovered bytes using already-authorized intervals. | Two interval values. | Diagnosed and silent interval values. | Pure; does not own diagnostic trust policy. |
| ABC derivation | Bind a verified aggregate to the gate. | Verified blob, aggregate, qualification identity. | Observation envelope or unavailable result. | Trust boundary; recomputes identities and exact equality rather than trusting reported ratio/status. |

ABC owns the closed wire schemas for the taxonomy, work record, record index,
and aggregate under `abc/schemas/`. Rust structs are producers/consumers of those
contracts, not the protocol authority. This follows the repository boundary that
ABC owns publication schemas and manifest identity while `ab-validator` owns
measurement execution.

```text
corpus entries --drive--> original bytes + Parser-IR + qualification identity
                 |
                 v
      [corpus producer + work analyzer] --commutative per work--> work record blobs
                                                        |
                      generated logical-blob index -----+
                                                        v
                                               [pure aggregate]
                                                        |
                                           aggregate summary blob
                                                        |
                                P0 re-hash + ABC derivation boundary
                                                        |
                                      source_span_coverage envelope

uncovered intervals + P4-authorized intervals --> [pure R2 reconcile]
                                                --> silent regions/count
```

Per-work analysis is parallel and order-independent. Aggregation is idempotent
for the same immutable inputs. No filesystem scan, timestamp, hostname, or
completion order affects a content value.

### Hickey review findings

| Severity | Classification | Observation, risk, and resolution |
|---|---|---|
| Blocker | Mitigated | A separately supplied decoded file braided measurement with an untrusted decoding place. Consume original bytes and reuse `decode_source_bytes`. |
| Blocker | Mitigated | Lossy Shift-JIS replacement created eligible U+FFFD bytes that could still receive spans. `windows-31j-lossy` makes the work unavailable. |
| Blocker | Mitigated | The BOM taxonomy rule lived outside the decoded coordinate value and could never match. V1 is now an explicit empty taxonomy. |
| Strong suggestion | Mitigated | Full-text accounting depended on an unstated sanitize/body rebase invariant. The invariant is normative and receives body, tail, BOM, CRLF, and sanitizer characterization tests. Sanitizer diagnostics remain raw P4 input. |
| Blocker | Mitigated | Whole-region diagnostic overlap allowed a tiny warning to excuse a large loss. R2 now uses interval intersection/subtraction and retains residual silent bytes. |
| Blocker | Mitigated | Raw diagnostics mixed P1 interval arithmetic with P4 trust policy. P1 accepts only P4-authorized intervals; P4 owns code/severity/schema interpretation. |
| Blocker | Mitigated | Directory enumeration made the record set a time-varying place. A closed, content-identified work-record index is now the aggregate input. |
| Strong suggestion | Mitigated | An unnamed index producer allowed human corpus selection to re-enter. `analyze-corpus` derives membership only from the pinned corpus and must regenerate byte-identically. |
| Blocker | Mitigated by P5 boundary | A full identity reference on a prebuilt Parser-IR file is a claim, not proof of producer identity. P1 checks all embedded coordinates; P5 must invoke/hash pinned binaries and bind operational provenance. |
| Strong suggestion | Mitigated | Counting paragraph/sentence spans braided primary claims with derived views. Only node spans count. |
| Strong suggestion | Mitigated | A generic ignored-region rule engine was configuration without a v1 use case. V1 is a closed empty value; later policy needs an explicit versioned change. |
| Strong suggestion | Mitigated | A supplied floating ratio could diverge from integer evidence. ABC derives the pass condition from exact numerator/denominator equality. |
| Question | Decided fail-closed | Legacy omission of node `coordinate_system` is schema-valid but insufficient for release evidence. P1 requires explicit `decoded_utf8`. |
| Question | Clarified | `silent_drop` counts maximal byte intervals for a zero/nonzero gate; it is not a construct census. |
| Nit | Corrected | Decimal scale preserves display visibility; integer equality, not scale, is the pass guarantee. |
| Question | Accepted | A dedicated crate adds a workspace member, but keeps release measurement separate from parser behavior and occurrence-level publication policy. |
| Question | Unknown, plan blocker | Hegel is available from crates.io, but its native engine must pass the repository's offline Nix checks. The implementation plan begins with a disposable packaging probe. |

### Decision log

| Decision | Status | Reversibility | Evidence / rejected alternative | Revisit trigger |
|---|---|---|---|---|
| Measure full `DecodedSource.text` in decoded UTF-8 bytes. | Accepted | Contract-breaking after capture. | Matches live span schema; original-byte offsets require a reverse map. | Parser-IR changes coordinate systems. |
| Reject lossy decoded values. | Accepted | New instrument version to relax, requiring separate evidence. | U+FFFD is destroyed source content, unlike removable BOM framing. | Only if a future loss map can account for every original byte without imputation. |
| Require explicit node `coordinate_system = decoded_utf8`. | Accepted | New instrument version. | Current converter emits it; legacy default is too weak for release evidence. | Parser-IR makes another explicit coordinate system authoritative. |
| V1 taxonomy has no ignored regions or rule engine. | Accepted | New taxonomy version. | BOM is already outside decoded value; no other framing is proven. | A decoded region is proven non-content by parser contract. |
| Count only Parser-IR node spans. | Accepted | New instrument version. | Paragraph/sentence spans are derived; AAT is wrong authority. | Parser-IR adds an explicit primary source-mapping relation. |
| Bind every work record to the full qualification identity. | Accepted | Additive schema migration only before first authoritative capture. | Prevents mixing records from different builds/instruments. | Never silently relax; only supersede with an equally strong identity relation. |
| Generate the record index from the pinned corpus. | Accepted | Producer implementation can change under new instrument identity. | Removes human selection and filesystem membership from the trust path. | Capture orchestration adopts an equally deterministic corpus-driven index. |
| Defer diagnostic authorization to P4. | Accepted | R2 interface can be extended by version. | Keeps trust policy out of interval algebra. | P4's diagnostic contract cannot emit exact authorized intervals. |
| Use Hegel for interval properties. | Proposed pending probe | Easily reversible before tests land. | User requested skill-driven testing; crate exists and Rust floor is compatible. | Nix/offline integration fails or introduces an unacceptable runtime dependency. |

### Incubation and remaining unknowns

The direction is substantially stronger, but one implementation-environment
unknown remains: Hegel's native engine under the offline Nix test sandbox. The
plan must resolve it with a disposable minimal test before creating production
modules. Failure pauses implementation for an explicit testing decision; it does
not authorize a silent fallback.

The other tempting unknown—whether current corpus coverage will equal 1.0—is
deliberately not resolved during design. P1 must be capable of reporting an
honest failure. Sampling the result to tune taxonomy or span selection would
contaminate the preregistered predicate.

## Delivery and sequencing

P1 is complete when:

1. the v1 taxonomy and its hash are committed;
2. the Rust work analyzer, aggregate, CLI, and pure R2 reconciler pass unit and
   Hegel property tests;
3. the ABC derivation boundary passes focused Clojure tests;
4. a small fixture capture proves Capture -> Derive -> Drift locally, and its
   corpus-produced record index regenerates byte-identically;
5. R1 is capturable under P0 and R2 remains explicitly unavailable pending P4;
6. comment hygiene and `just validate-migration` pass.

P4 subsequently supplies the validated diagnostic-capture-to-authorized-interval
adapter contract. P5 pins the final implementation revision and performs
authoritative corpus capture on `hinoki.hyakutake-barbel.ts.net`.
