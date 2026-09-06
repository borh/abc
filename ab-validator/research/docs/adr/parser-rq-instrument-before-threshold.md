# Fix the Instrument Before Setting the Residual Threshold

## Implementation Status

Not implemented. This record fixes the rule and the diagnosis; the instrument
work it authorizes is carried by
[parser-rq-source-region-partition](parser-rq-source-region-partition.md).

## Context

`source-span-coverage := 1.0` is one of the nine release predicates and the only
one stated as an exact contract. Measured against 299 real works through the
built `ab-parser-rq-source-accountability` binary, **not one reaches 1.0** — fold
0.9640, worst 0.2466 — while the governed three-work qualification corpus returns
exactly 1.0 from the same binary, the same taxonomy, and the same policy hash.

That corpus is 177 bytes. It is small enough that the instrument's defect does
not show, which is why the divergence survived until a real-source probe ran.

The mechanism was stated three times and was wrong twice. First a missing
`publication_metadata` policy rule, inferred from the policy file declaring a
`source_role` that none of its 31 rules implements. Then a corrected byte split
after a frame detector was found to match overlapping positions inside a single
separator rule. Both inferred a cause from a correlation.

The traced answer is a coordinate mismatch. The classified-source ledger is built
by lexing `decoded.span_text`, which is the `aozora_body_range` body projection,
while the recognition instrument reports `eligible_bytes` from the whole decoded
file. The header and `底本：` colophon are not unclassified; they are outside the
coordinate space every classified-source fact comes from. Demonstrated on a
synthetic work with a standard header, one body line, and a colophon: three
entries covering 51 of 375 bytes, with the entire 216-byte header and 108-byte
tail carrying no entry of any kind — not `plain_text`, not `newline`. A missing
policy rule cannot produce that, because `plain_text` alone would have covered
those lines had they been lexed.

## Decision

**The instrument is incomplete. Neither the threshold nor the ratio moves on
account of this gap.**

Three readings were available:

| Reading | Verdict |
| --- | --- |
| Fix the instrument | **Adopted** |
| Redefine the ratio as `accounted / eligible` | Rejected in kind |
| Lower the threshold | Rejected for now; admissible later |

Redefining the ratio is rejected on the merits rather than for insufficiency.
`recovered_verbatim` means *the parser could not type this construct*, which is
precisely what a recognition predicate should count against. Folding it into the
numerator would make the predicate easier to pass by making it say less.

Lowering the threshold is rejected *for now*. Some residual is genuine parser
limitation that no instrument work will remove, so a threshold will eventually be
right — but only once the instrument measures its declared contract.

The rule this encodes:

> A predicate threshold may be relaxed only once its instrument is known to
> measure what it claims.

Relaxing first converts an instrument defect into a permanent governed allowance,
and a corpus small enough to hide the defect would have kept that allowance
invisible indefinitely.

## Consequences

The confirmatory tier 2 campaign is gated until the instrument is fixed. Reading
any tier 2 coverage observation as a parser verdict before then is unsound. The
architecture, tier 1, tier 3, and the exploratory campaign are not blocked — the
exploratory campaign's purpose is to measure exactly this.

The superseded causal claims are deliberately not preserved as claims of this
record. They survive as clearly marked history in the design document, because
the record of having been wrong twice is worth keeping and the instructions
derived from it are not.

Mechanism claims in this area now require a source-line trace. Correlation with
the policy file's vocabulary is what produced both wrong answers.

## Evidence

No claim of this record carries an evidence path. The measurements behind it were
taken under a synthesized qualification identity that authenticates nothing, and
no committed test asserts them. They establish instrument behaviour, not a
qualified campaign. Promotion to Accepted requires evidence for each claim.

Design and diagnosis:
`docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.
