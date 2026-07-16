# Classified-source provenance characterization

Status: **characterized; Task 3 blocked on the existing `ClassifiedSpan` seam**.

The disposable probe recorded 36 deterministic cases at three levels: pair
events, classifier-local branch observations, and emitted `ClassifiedSpan`s.
Each case also records input bytes, all diagnostics, normalized and verbatim
SHA-256 hashes, and final registry `NodeKind`s. The complete machine-readable
matrix is `2026-07-16-classified-source-provenance.json`.

## Empirical result

Recovery identity exists at the individual classifier branches. The probe
observed, before pending-state convergence:

- ordinary top-level and replayed plain events, including `Solo` and
  `Unmatched` variants;
- held solo refmarks;
- literal Quote/Tortoise stream-through;
- skipped synthetic `Unclosed` events during declined-frame replay;
- recognizer decline, including gaiji decline before its refmark becomes plain.

These observations are not present in the emitted `ClassifiedSpan`. The cases
show distinct event/branch histories converging to `SpanKind::Plain`; adjacent
paths may fuse into one source interval. Recovery provenance is therefore
discarded when the branches seed or reuse `pending_plain_start` /
`pending_refmark`, before `flush_plain_up_to` emits a reasonless plain span.
This is a limitation of the unchanged output seam, not an absence of a viable
pre-flush observation point.

No AAT or Parser-IR reconstruction was used. Reconstructing pair history is
necessary only if the classifier remains unchanged. A durable downstream
contract would instead require a classifier-local provenance split at these
branches.

## Case and node coverage

The matrix covers ordinary text, newline, Quote/Tortoise punctuation, solo
refmark/bar/hash, unclosed open, unmatched close, gaiji decline, unknown
directive, block open, block close, a balanced block, and representative live
node producers.

Of the 26 declared `NodeKind` tags, 23 were observed through the live pipeline:
`ruby`, `bouten`, `combineUpright`, `gaiji`, `indent`, `alignEnd`, `center`,
`lineGothic`, `lineFontSize`, `pageBreak`, `sectionBreak`, `bodyEnd`,
`forcedBreak`, `heading`, `headingHint`, `illustration`, `kaeriten`, `directive`,
`angleQuote`, `emphasis`, `marginNote`, `containerOpen`, and `containerClose`.
The probe did not manufacture coverage for `warichu`, `framed`, or `container`:
their attempted directive shapes surfaced through other live representations
(`Directive` or block-container sentinels), and no classifier case in this
matrix emitted those three node tags.

## Determinism and authorization

Both ignored-probe runs passed and `cmp` reported identical JSON. The observer,
probe, and temporary dependencies were deleted after generating the evidence.

The observations do not support freezing a closed recovery-reason enum: they
identify branch families but do not establish that the temporary labels are a
durable exhaustive vocabulary. Task 3 is therefore not authorized against the
existing `ClassifiedSpan` seam. No `Other` variant or Task 3 implementation was
added.
