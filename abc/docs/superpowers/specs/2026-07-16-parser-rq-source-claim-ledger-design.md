# Parser RQ Source-Claim Ledger Design

**Status:** Approved design; written-spec review pending

**Scope:** The custom `ab-aozora` parser only. Third-party parsers remain
temporary research instruments until their comprehensive report/publication is
complete. Their later retirement is a separate milestone.

**Revises:** P1's R1 numerator authority and P4A's relationship between
diagnostics and R2. Existing captured P1 values remain immutable evidence of the
old node-span instrument; they are not reinterpreted.

## Problem and observed falsifier

P1 currently treats the union of Parser-IR `nodes[*].span` as the bytes claimed
by the parser. That is valid evidence about emitted publication nodes, but it is
not source accountability. A parser routinely consumes Aozora delimiters,
directives, and structural syntax without emitting a node whose span covers
those source bytes.

The disposable P4A characterization exercised all 21 live diagnostic codes and
joined raw diagnostics to R1 over the same source. After correcting the probe to
the live AAT-v2 conversion tuple and exact adapter identity, all 17 source repros
completed with an available R1 value. The result falsified the proposed
diagnostic policy:

- `source-contains-pua`, the sole proposed authorizing code, intersected no R1
  uncovered byte;
- eleven proposed `observe_only` diagnostics intersected R1 uncovered bytes;
- three documented repros did not emit their named code;
- ordinary successfully parsed Aozora syntax remained uncovered by Parser-IR
  node spans.

Broadening diagnostic authorization would make error policy compensate for a
wrong numerator. Reconstructing claims from AAT or Parser-IR would move the same
fake seam. The parser must instead state which exact source bytes it recognized
and consumed, while an independent ABC policy determines which public claim
semantics are acceptable.

## Objective

For every authenticated decoded source value, capture an immutable source-claim
ledger whose entries state:

1. the exact half-open decoded-byte interval consumed;
2. an ABC-owned source role;
3. an ABC-owned consumption result;
4. evidence connecting the claim to the parser path and resulting value, when
   one exists.

R1 derives claimed coverage from the validated ledger. Parser-IR node-span
coverage remains a separately named supporting observation and drift witness.
R2 partitions R1's remaining gaps into authorized recovery and silent loss;
diagnostics never increase R1 coverage.

## Authority model

Authority is deliberately split:

- `ab-aozora` observes consumption at the earliest layer that knows why bytes
  were consumed and emits candidate claim entries.
- ABC owns the closed claim vocabulary, permitted role/result combinations,
  conditional evidence requirements, schema, and immutable policy generation.
- The RQ validator authenticates source, ledger, policy, and qualification
  identity, then validates every claim before performing interval arithmetic.
- Parser-IR and AAT are evidence targets, not claim authorities.
- Raw diagnostics are recovery observations, not claims.

Parser completion does not imply full coverage. A parser cannot authorize a new
claim class by adding an internal enum or evidence code. Unknown vocabulary or
policy drift makes the ledger unavailable.

## Public claim model

The vocabulary has two independent axes.

### Source role

V1 must enumerate concrete Aozora source roles. Its initial candidate families
are:

- visible text and structural whitespace;
- ruby base, reading, and delimiters;
- gaiji source notation;
- typography and layout directives;
- headings, breaks, separators, and container syntax;
- source apparatus and annotations;
- terminal provenance and publication metadata.

These are families for characterization, not wildcard runtime values. The
frozen policy contains exact closed role identifiers. It has no `other`,
`unknown`, parser namespace, prefix rule, or extension bag.

### Consumption result

V1 supports four results:

- `emitted_semantic_value`: source contributes to an emitted AAT, Parser-IR, or
  approved publication-side value;
- `preserved_sidecar_value`: source is preserved in an authenticated custom or
  provenance sidecar;
- `structural_control`: syntax controls parsing or document structure without
  becoming visible text;
- `lossless_normalization`: source is transformed under an approved reversible
  mapping.

Role and result remain separate. A label such as `markup` cannot conceal whether
the information was emitted, preserved, used only as control, or transformed.

### Claim entry

Every entry contains exactly:

```text
start, end
source_role
consumption_result
result_pointer?       # conditional
parser_evidence_code  # audit evidence, never authority
construct_witness?    # conditional closed token/construct value
normalization_proof?  # conditional
```

All spans are half-open `decoded_utf8` byte intervals into the exact
authenticated `DecodedSource.text`. `start < end`; both endpoints must be UTF-8
boundaries and within the decoded byte length.

Evidence requirements are policy-owned:

- emitted and preserved results require a typed pointer into an authenticated
  output artifact;
- structural-control results require an approved evidence code plus a closed
  token/construct witness containing the recognized source form and exact span;
  the validator independently checks that the witness bytes equal the
  authenticated decoded slice and that its closed shape is permitted for the
  named role;
- lossless normalization requires an approved reversible transformation record
  binding source interval, source bytes, normalized bytes, and inverse rule.

`parser_evidence_code` helps characterize and debug parser paths. It cannot
select a disposition, relax a requirement, or create a claim class.

ABC does not implement a second Aozora parser to prove the semantic meaning of
each witness. The pinned parser implementation, path-level tests, and
characterization evidence remain part of the measurement trust base. ABC
independently controls the public semantics, identities, exact source slice,
and conservation rules so that trust is explicit and bounded rather than
laundered through an output artifact.

## Ledger protocol

One work ledger binds:

- ledger schema ID/version/hash;
- qualification identity reference;
- parser and instrument versions;
- original-source and decoded-source logical blob identities;
- decoded encoding, byte length, and `decoded_utf8` coordinate system;
- claim-policy ID and canonical hash;
- parser completion status;
- the ordered audit entries;
- stable unavailable reasons, if any.

The audit entry order is deterministic but not semantically significant.
Coverage normalizes the accepted interval union after every entry validates.
Overlaps are permitted because one source interval may participate in more than
one semantic fact; duplicates with identical complete claim identity are
rejected as producer ambiguity. Empty spans never contribute coverage.

The ledger contains no precomputed authoritative totals. Totals and normalized
witnesses are derived values so a producer cannot disagree with its entries.

## Capture and derivation

```text
authenticated source bytes
  -> decode and sanitize
  -> custom parser
  -> AAT + raw diagnostics + source-claim ledger
  -> independently authenticate and validate each artifact
  -> derive R1 from accepted claim union
  -> authorize diagnostic recovery independently
  -> partition R1 gaps into authorized recovery and silent intervals
  -> aggregate exact corpus membership
```

Claims originate at the earliest component that knows the consumption reason.
Sanitizer and parser claims retain distinct producer-stage evidence. They are
rebased through the same span context into full decoded-source coordinates
before capture. Emission is integrated with the typed token/construct creation
path rather than a second post-parse visitor: one accepted construct value and
its claim witness are produced together, preventing the claim stream from
silently drifting away from actual consumption.

Claims must not be reconstructed from broad parent spans, Parser-IR nodes,
rendered text, diagnostics, or the absence of parser errors. A parent construct
may claim its complete extent only when the policy defines that precise syntax
role and the parser evidence identifies the matching consumed extent; it cannot
blanket-cover unknown bytes between children.

Capture owns parser invocation and immutable artifact publication. Derivation
is pure: it receives authenticated values and cannot invoke the parser, invent a
claim, enumerate a mutable directory, or interpret parser-private rule names.
Corpus membership comes only from P1's authenticated record index.

## R1 semantics

The v1 eligible set remains every byte in `DecodedSource.text`. BOM bytes are
absent because decoding consumes the UTF-8 BOM before this value exists. Lossy
decoding remains unavailable.

For each work:

```text
claimed = normalize(union(all validated claim spans))
unclaimed = eligible - claimed
claimed_bytes + unclaimed_bytes = eligible_bytes
coverage = claimed_bytes / eligible_bytes
```

The gate uses exact integer equality `claimed_bytes == eligible_bytes`; decimal
coverage is reporting only. A valid ledger with gaps is an available failing
measurement. An invalid ledger has no trusted numerator or gap value.

Parser-IR node-span coverage is renamed as a non-authoritative supporting
observation. It may detect drift such as a claimed emitted value lacking a
corresponding node, but it cannot reduce or expand the claim ledger.

## R2 semantics

R2 classifies but never erases the R1 gap set:

```text
authorized_recovery = intersect(unclaimed, authorized diagnostic intervals)
silent = unclaimed - authorized_recovery
authorized_recovery union silent = unclaimed
```

Diagnostic authorization remains closed, identity-bound, and exact-span. It
cannot turn an unclaimed byte into a claimed byte. R1 can therefore fail while
R2 reports zero silent intervals; the release gate sees both independent facts.

An authenticated empty diagnostic stream is available and vacuous. With gaps,
all gaps remain silent; without gaps, both partitions are empty. Unknown or
invalid diagnostics make R2 unavailable without changing R1.

`silent_drop_count` remains the count of maximal connected silent intervals per
work, not a construct census. Intervals never merge across works.

## Failure semantics

The ledger and R1 are unavailable for:

- source, decoded-source, policy, schema, or qualification identity mismatch;
- lossy decoding;
- incomplete or failed parser execution;
- malformed, reversed, empty, out-of-bounds, or mid-codepoint spans;
- unknown source roles, consumption results, or forbidden combinations;
- missing or invalid conditional evidence;
- pointers that do not resolve into the authenticated named artifact;
- lossless-normalization proofs that do not round-trip;
- duplicate complete claim identities;
- vocabulary drift between the parser adapter, ABC policy, and validator.

Unavailable records carry stable errors and no trusted claim totals. Valid gaps
remain ordinary available evidence and are never patched to green.

## Characterization before vocabulary freeze

A disposable probe must enumerate every live sanitizer, lexer, and parser
consumption path and join:

```text
consumed decoded-byte interval
internal parser evidence path
candidate ABC role/result
emitted or preserved target, when applicable
raw diagnostic, when applicable
```

It compares the union of observed consumption events with the proposed public
ledger over representative and generated sources. The vocabulary cannot freeze
if:

- a successfully consumed byte has no approved mapping;
- a claim covers a byte the named path did not consume;
- a broad claim hides an unknown or rejected subregion;
- event order changes canonical ledger bytes;
- a claim depends on a diagnostic or reconstructed Parser-IR span;
- a claimed emitted/preserved value lacks its required authenticated target;
- a normalization proof fails its inverse.

The probe is design evidence, not production authority. Its small matrix and
counterexamples may be committed; its instrumentation is removed before the
protocol implementation commit.

## Testing and drift

Permanent examples and property tests cover:

- UTF-8 boundary, bounds, overlap, adjacency, duplicate, and ordering rules;
- every accepted and forbidden role/result combination and every closed
  structural witness shape;
- source, policy, vocabulary, and qualification identity drift;
- R1 conservation and exact integer gate behavior;
- R2 partition conservation and vacuity;
- empty source, empty diagnostics, successful full coverage, honest gaps,
  parser failure, BOM, CRLF, sanitizer transformations, and lossy decoding;
- pointer authentication, decoded-slice/witness agreement, and normalization
  round trips;
- exact corpus membership and absence of cross-work merging;
- end-to-end production fixture regeneration;
- mutations of source, claim, target, policy, diagnostic, and identity values.

Drift checks regenerate the complete small fixture through production capture
paths. Comparing hand-authored summaries or string inequality alone is not a
trust-path test.

## Program decomposition and migration

P4A becomes three focused deliverables:

- **P4A1:** characterize consumption paths; define the ABC source-claim
  protocol/policy; emit ledgers from the custom parser.
- **P4A2:** migrate R1 derivation from Parser-IR node spans to authenticated
  claim ledgers; retain node-span coverage under a new supporting name.
- **P4A3:** apply the independently reviewed diagnostic policy only to partition
  ledger gaps for R2.

Historical P1 work records, manifests, and aggregates remain immutable. They do
not become ledger evidence through schema migration or inferred claims. The new
instrument generation starts unavailable until an explicit recapture.

P5 still pins the final implementation commit and performs the authoritative
full-corpus capture on `hinoki.hyakutake-barbel.ts.net`. No registry admission,
release qualification, or ADR promotion occurs during P4A1-P4A3.

## Rejected alternatives

### Derive claims from AAT spans

Rejected as authority. AAT transforms and groups source after sanitizer and
parser decisions; delimiters, recovery, and removed regions can already have
lost their independent meaning. AAT remains a valid claim target and drift
witness.

### Expose raw parser event identifiers as the public vocabulary

Rejected. It couples release evidence to implementation structure and lets the
parser define its own accountability semantics. Internal events may feed the
adapter and characterization probe only.

### Expand diagnostic authorization until R1 is green

Rejected. Diagnostics describe exceptional recovery and cannot account for
ordinary successfully consumed syntax. This would complect errors with coverage
and reward adding warnings.

### Treat successful parse completion as complete consumption

Rejected. Completion is one fact about control flow, not evidence about every
eligible byte.

## Decision record

| Decision | Reason | Falsifier |
|---|---|---|
| Parser emits source claims at the consumption boundary. | That layer knows why exact bytes were consumed. | A production path cannot expose exact decoded coordinates without reconstructing them downstream. |
| ABC owns the closed role/result vocabulary. | Parser vocabulary cannot authorize itself. | Independent validation cannot describe a required construct without depending on parser-private identity. |
| Ledger claims replace node spans as R1 numerator authority. | Live characterization proved node spans measure emitted output, not consumed source. | Claim-ledger capture cannot be authenticated or conserved against decoded bytes. |
| Diagnostics affect R2 only. | Recovery explanation and ordinary consumption are different facts. | A diagnostic is proven to be the only possible evidence of ordinary successful consumption. |
| Existing evidence is not migrated. | Reinterpretation would manufacture observations under a new authority model. | A byte-identical old artifact already contains the complete authenticated new protocol. |
